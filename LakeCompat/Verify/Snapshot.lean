import Std.Data.HashMap
import Std.Data.HashSet
import LakeCompat.Concurrent

open System
namespace LakeCompat.Verify

/-- Both paths must be absolute and canonical apart from the destination's final component. -/
def isInside (root path : FilePath) : Bool :=
  root == path || path.toString.startsWith (if root.toString.endsWith "/" then root.toString else root.toString ++ "/")

def metadata? (p : FilePath) : IO (Option IO.FS.Metadata) := do
  try return some (← p.symlinkMetadata) catch
    | .noFileOrDirectory .. => return none
    | e => throw e

private def excluded (component : String) := component == ".git" || component == ".lake"

/-- Walk cache components without following links. The OS can return a different spelling
for a real directory on a case-insensitive filesystem. -/
def isolatedDirectory (root relative : FilePath) : IO (Option FilePath) := do
  let mut path := root
  for part in relative.components do
    path := path / part
    unless (← metadata? path).any (·.type == .dir) do return none
  return some (← IO.FS.realPath path)

private def readLinkTargets (links : Array (FilePath × UInt64)) : IO (Array String) := do
  if links.size > 1 then
    let cmd := IO.Process.run {cmd := "readlink", args := links.map (·.1.toString)}
    if let .ok raw ← cmd.toBaseIO then
      let targets := ((raw.dropSuffix "\n").copy.splitOn "\n").toArray
      -- BSD readlink also uses a target's own final newline as the output separator.
      if targets.size == links.size && (targets.zip links).all (fun (target, _, n) => target.utf8ByteSize == n.toNat) then
        return targets
  -- Ask for the exact bytes when a target contains a newline or multiple operands are unsupported.
  links.mapM fun (link, _) => do
    try IO.Process.run {cmd := "readlink", args := #["-n", link.toString]}
    catch e => throw <| IO.userError s!"Cannot read source symlink {link}: {e}"

/-- Resolve each component of a copied symlink so even an intermediate escape followed by ".."
is rejected. Return its directory target, if any, for the cycle check. -/
private def directoryTarget? (root link path : FilePath) : IO (Option FilePath) := do
  try
    if path.isAbsolute then throw <| IO.userError "absolute path"
    let mut current := link.parent.getD root
    for part in path.components do
      if part.isEmpty then continue
      if excluded part then throw <| IO.userError "uses an excluded directory"
      current ← IO.FS.realPath (current / part)
      unless isInside root current do throw <| IO.userError "leaves the project"
      if (current.components.drop root.components.length).any excluded then
        throw <| IO.userError "uses an excluded directory"
    -- Also enforce the OS traversal rules, including a trailing slash on a non-directory.
    return if (← link.metadata).type == .dir then some current else none
  catch e => throw <| IO.userError s!"Cannot isolate source symlink {link}: {e}"

private abbrev Edges := Std.HashMap FilePath (Array FilePath)

private def checkDirectoryCycles (edges : Edges) : IO Unit := do
  let mut counts : Std.HashMap FilePath Nat := ∅
  for (dir, children) in edges do
    counts := counts.insertIfNew dir 0
    for child in children do counts := counts.insert child (counts[child]?.getD 0 + 1)
  let mut ready := counts.toArray.filterMap fun (dir, n) => if n == 0 then some dir else none
  let mut visited := 0
  while let some dir := ready.back? do
    ready := ready.pop
    visited := visited + 1
    for child in edges[dir]?.getD #[] do
      let n := counts[child]! - 1
      counts := counts.insert child n
      if n == 0 then ready := ready.push child
  if visited != counts.size then throw <| IO.userError "Directory symlinks create a traversal cycle."

private def validateSnapshot (root : FilePath) (copiedFiles : Option (Array FilePath)) : IO Unit := do
  let paths ← match copiedFiles with
    | some files => pure files
    | none => do
      -- For a large snapshot, find can filter ordinary entries by directory entry type.
      let names ← IO.Process.run {cmd := "find", args := #[root.toString, "!", "-type", "d", "!", "-type", "f", "-print0"]}
      pure <| ((names.splitOn (String.singleton (Char.ofNat 0))).filter (!·.isEmpty)).toArray.map FilePath.mk
  let mut links := #[]
  for path in paths do
    let info ← path.symlinkMetadata
    if info.type == .file then continue
    unless info.type == .symlink do throw <| IO.userError s!"Cannot isolate a special file: {path}."
    links := links.push (path, info.byteSize)
  let mut edges : Edges := ∅
  let mut seen : Std.HashSet FilePath := ∅
  for start in [0:links.size:32] do
    let batch := links.extract start (start + 32)
    for (link, _) in batch, target in ← readLinkTargets batch do
      let some dir ← directoryTarget? root link ⟨target⟩ | continue
      let parent := link.parent.get!
      edges := edges.alter parent fun dirs => some ((dirs.getD #[]).push dir)
      -- Only a directory link and its ancestors can participate in a traversal cycle.
      for endpoint in [parent, dir] do
        let mut child := endpoint
        while child != root && !seen.contains child do
          seen := seen.insert child
          let ancestor := child.parent.get!
          edges := edges.alter ancestor fun dirs => some ((dirs.getD #[]).push child)
          child := ancestor
  unless edges.isEmpty do checkDirectoryCycles edges

private def copyBatches (batches : Array (Array String × FilePath)) : IO Unit := do
  -- Each batch has distinct destination files. cp preserves modes and copies links verbatim.
  -- macOS cp can clone data on copy-on-write volumes and falls back to copying otherwise.
  let (cmd, flags) := if Platform.isOSX then ("/bin/cp", #["-c", "-Pp"]) else ("cp", #["-Pp"])
  discard <| mapConcurrent batches fun (files, toDir) => do
    discard <| IO.Process.run {cmd, args := (flags ++ files).push toDir.toString}

def sourceByteLimit := 1024 ^ 3

/-- Copy the trial sources and report whether they include previously compiled Lean modules. -/
def copyTrialSources (source destination packagesDir : FilePath)
    (managedPackages : Std.HashSet String) : IO Bool := do
  if isInside source destination then throw <| IO.userError "The temporary directory must be outside the project."
  let packageCache ← if managedPackages.isEmpty then pure none else isolatedDirectory source packagesDir
  let foldedNames := managedPackages.fold (init := (∅ : Std.HashMap String String)) fun m name =>
    m.insert name.toLower name
  IO.FS.createDir destination
  let mut stack := #[(source, destination)]
  let mut totalBytes := 0
  let mut hasModuleCache := false
  -- Inspect small copies directly. Large copies avoid retaining one path per file.
  let mut copiedFiles : Option (Array FilePath) := some #[]
  let mut batches := #[]
  while let some (dir, toDir) := stack.back? do
    stack := stack.pop
    let mut files := #[]
    let mut argBytes := 0
    let inCache := packageCache == some dir
    let entries ← dir.readDir
    let onDisk : Thunk (Std.HashSet String) := Thunk.mk fun _ =>
      entries.foldl (init := ∅) fun names e => names.insert e.fileName
    let cacheNames := if inCache then foldedNames else ∅
    for entry in entries do
      if excluded entry.fileName || (inCache && managedPackages.contains entry.fileName) then continue
      if inCache || entry.fileName.startsWith "." then
        let lower := entry.fileName.toLower
        let spelling := if excluded lower then some lower else cacheNames[lower]?
        if let some spelling := spelling then
          -- Distinct spellings can coexist on a case-sensitive filesystem. Check the source
          -- lookup before treating an entry as an alternative spelling of an excluded name.
          if !onDisk.get.contains spelling && (← metadata? (dir / spelling)).isSome then continue
      if batches.size >= 16 then
        copyBatches batches
        batches := #[]
      let info ← entry.path.symlinkMetadata
      match info.type with
      | .dir =>
        let subDir := toDir / entry.fileName
        IO.FS.createDir subDir
        stack := stack.push (entry.path, subDir)
      | .other => throw <| IO.userError s!"Cannot isolate a special file: {entry.path}."
      | .file | .symlink =>
        if let some paths := copiedFiles then
          copiedFiles := if paths.size < 8192 then some (paths.push (toDir / entry.fileName)) else none
        if !hasModuleCache then
          hasModuleCache := entry.fileName.endsWith ".ltar" || (entry.fileName.endsWith ".olean" &&
            onDisk.get.contains ((entry.fileName.dropSuffix ".olean").copy ++ ".trace"))
        if info.type == .file then
          totalBytes := totalBytes + info.byteSize.toNat
          if totalBytes > sourceByteLimit then throw <| IO.userError "Project sources exceed the 1 GiB temporary-copy limit."
        files := files.push entry.path.toString
        argBytes := argBytes + entry.path.toString.utf8ByteSize + 1
        if argBytes >= 32768 then
          batches := batches.push (files, toDir)
          files := #[]
          argBytes := 0
    unless files.isEmpty do batches := batches.push (files, toDir)
  copyBatches batches
  validateSnapshot destination copiedFiles
  return hasModuleCache

def copySources (source destination : FilePath) (packagesDir : FilePath := ".lake/packages")
    (managedPackages : Std.HashSet String := ∅) : IO Unit :=
  discard <| copyTrialSources source destination packagesDir managedPackages

end LakeCompat.Verify
