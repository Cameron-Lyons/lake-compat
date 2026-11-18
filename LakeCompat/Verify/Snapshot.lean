import Std.Data.HashMap

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

/-- Resolve each component of a copied symlink so even an intermediate escape followed by ".."
is rejected. The snapshot has stopped changing before links are inspected. -/
private def linkTarget (root link : FilePath) : IO FilePath := do
  try
    let raw ← IO.Process.run {cmd := "readlink", args := #[link.toString]}
    let path : FilePath := (raw.dropSuffix "\n").copy
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
    discard link.metadata
    return current
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

private def validateSnapshot (root : FilePath) : IO Unit := do
  let mut stack := #[root]
  let mut edges : Edges := ∅
  let mut directoryLinks := false
  while let some dir := stack.back? do
    stack := stack.pop
    let mut children := #[]
    for entry in ← dir.readDir do
      match (← entry.path.symlinkMetadata).type with
      | .file => pure ()
      | .dir =>
        stack := stack.push entry.path
        children := children.push entry.path
      | .symlink =>
        let resolved ← linkTarget root entry.path
        if ← resolved.isDir then
          children := children.push resolved
          directoryLinks := true
      | .other => throw <| IO.userError s!"Cannot isolate a special file: {entry.path}."
    edges := edges.insert dir children
  if directoryLinks then checkDirectoryCycles edges

private def copyBatch (files : Array String) (toDir : FilePath) : IO Unit := do
  unless files.isEmpty do
    -- cp preserves executable bits and copies the link itself, including dangling symlinks.
    discard <| IO.Process.run {cmd := "cp", args := (#["-Pp"] ++ files).push toDir.toString}

def copySources (source destination : FilePath) : IO Unit := do
  if isInside source destination then throw <| IO.userError "The temporary directory must be outside the project."
  IO.FS.createDir destination
  let mut stack := #[(source, destination)]
  let mut totalBytes := 0
  while let some (dir, toDir) := stack.back? do
    stack := stack.pop
    let mut files := #[]
    let mut argBytes := 0
    for entry in ← dir.readDir do
      if excluded entry.fileName then continue
      let info ← entry.path.symlinkMetadata
      match info.type with
      | .dir =>
        let subDir := toDir / entry.fileName
        IO.FS.createDir subDir
        stack := stack.push (entry.path, subDir)
      | .other => throw <| IO.userError s!"Cannot isolate a special file: {entry.path}."
      | .file | .symlink =>
        if info.type == .file then
          totalBytes := totalBytes + info.byteSize.toNat
          if totalBytes > 1024 ^ 3 then throw <| IO.userError "Project sources exceed the 1 GiB temporary-copy limit."
        files := files.push entry.path.toString
        argBytes := argBytes + entry.path.toString.utf8ByteSize + 1
        if argBytes >= 32768 then
          copyBatch files toDir
          files := #[]
          argBytes := 0
    copyBatch files toDir
  validateSnapshot destination

end LakeCompat.Verify
