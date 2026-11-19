import LakeCompat.Manifest
import LakeCompat.Source
import LakeCompat.Verify.Snapshot

open Lean System

namespace LakeCompat.Verify

abbrev Pins := Std.TreeMap String String

structure Trial where
  manifest : Json
  pins : Pins
  paths : Array (String × FilePath)
  packagesDir : FilePath := ".lake/packages"
  copiedPackages : Std.HashSet String := ∅
  gitSubDirs : Array (String × FilePath) := #[]
  localConfigs : Array FilePath := #["lakefile"]
  hasModuleCache : Bool := false
  deriving Inhabited

private def parts (p : FilePath) : List String :=
  p.components.filter fun part => !part.isEmpty && part != "."

private def normalParts (p : FilePath) : Except String (Array String) := do
  if p.isAbsolute || p.toString.contains (Char.ofNat 0) then throw "expected an in-tree path"
  let mut result := #[]
  for part in parts p do
    if part == ".." then
      if result.isEmpty then throw "path leaves its package"
      result := result.pop
    else result := result.push part
  return result

private def relative (value : Json) (label : String) : Except String FilePath := do
  let some value := value.getStr?.toOption | throw s!"Invalid {label} in the manifest."
  if value.isEmpty then throw s!"Invalid {label} in the manifest."
  let path : FilePath := value
  unless (normalParts path).isOk do throw s!"Cannot isolate {label} outside its package: {repr value}."
  return path

private def packageCacheDir (path : FilePath) : Except String FilePath := do
  let components := parts path
  if path.isAbsolute || path.toString.contains (Char.ofNat 0) || components.isEmpty || components.contains ".." ||
      components.contains ".git" || components == [".lake"] || (components.drop 1).contains ".lake" then
    throw s!"Cannot isolate packagesDir {path}; expected a project-relative cache directory."
  return mkFilePath components

/-- Lake resolves existing local Git remotes before fetching. Anchor a relative remote in the
original workspace, including when the snapshot omits a nested repository's Git metadata. -/
private def localGitRemote? (project : FilePath) (url : String) : IO (Option String) := do
  let some path := Source.localGitPath? url | return none
  if path.isAbsolute then return none
  let remote := project / path
  unless ← remote.pathExists do return none
  return some (← IO.FS.realPath remote).toString

private def readTrial (project : FilePath) (revisions : Pins) (expectedManifest : Option Json := none) :
    IO (Trial × String) := do
  let project ← IO.FS.realPath project
  let hasLakefile ← ["lakefile.lean", "lakefile.toml"].anyM fun (name : String) => do
    return (← (project / name).metadata.toBaseIO).toOption.any (·.type == .file)
  unless hasLakefile do
    throw <| IO.userError "Expected a lakefile.lean or lakefile.toml at the project root."
  let file := project / manifestName
  let resolved ← IO.FS.realPath file
  let info ← resolved.metadata
  unless isInside project resolved && info.type == .file && info.byteSize.toNat ≤ sourceByteLimit do
    throw <| IO.userError "Expected an in-tree manifest within the source snapshot size limit."
  let contents ← IO.FS.readFile resolved
  let raw ← IO.ofExcept <| (Json.parse contents).mapError fun error =>
    if expectedManifest.isSome then s!"The project manifest became unreadable since the metadata scan: {error}"
    else s!"{file}: {error}"
  if let some expected := expectedManifest then
    unless raw == expected do
      throw <| IO.userError "The project manifest changed since the metadata scan. Rerun the check with the current lock."
  let lock ← IO.ofExcept <| Lock.ofJson file raw
  if parts lock.lakeDir != [".lake"] || (FilePath.mk lock.lakeDir).isAbsolute then
    throw <| IO.userError "Build verification currently supports the standard .lake metadata directory."
  let packagesDir ← IO.ofExcept <| packageCacheDir (lock.packagesDir.getD ".lake/packages")
  let mut pins : Pins := ∅
  let mut paths := #[]
  let mut copiedPackages := ∅
  let mut gitSubDirs := #[]
  let mut localConfigs := #[FilePath.mk "lakefile"]
  -- Lock.ofJson already validated the package list and its defaults.
  let mut entries := (raw.getObjValD "packages").getArr?.toOption.getD #[]
  for i in [:entries.size] do
    let (kind, fields, tagged) ← IO.ofExcept <| unpackEntry entries[i]!
    let name ← IO.ofExcept <| (fields.getObjValD "name").getStr? >>= packageName
    unless tagged do
      for key in ["configFile", "manifestFile"] do
        let value := fields.getObjValD key
        unless value == .null do discard <| IO.ofExcept <| relative value (name ++ "." ++ key)
    match lock.packages[name]!.source with
    | .path dir copy =>
      paths := paths.push (name, ← IO.ofExcept <| relative (.str dir) (name ++ ".dir"))
      let config := if tagged then "lakefile" else (fields.getObjValD "configFile").getStr?.toOption.getD "lakefile"
      localConfigs := localConfigs.push (FilePath.mk dir / config)
      if copy then copiedPackages := copiedPackages.insert name
    | .git url rev subDir _ =>
      if let some subDir := subDir then
        let dir ← IO.ofExcept <| relative (.str subDir) (name ++ ".subDir")
        gitSubDirs := gitSubDirs.push (name, dir)
      let revision := revisions[name]?.getD rev
      unless Source.isFullRevision revision do
        throw <| IO.userError s!"{name} needs a full 40- or 64-character Git commit in the trial manifest."
      pins := pins.insert name revision.toLower
      let mut pinned := fields
      if let some remote ← localGitRemote? project url then
        pinned := pinned.setObjVal! "url" (.str remote)
      if revisions.contains name then
        pinned := pinned.setObjVal! "rev" (.str revision)
        pinned := pinned.setObjVal! (if tagged then "inputRev?" else "inputRev") (.str revision)
      entries := entries.set! i (if tagged then Json.mkObj [(kind, pinned)] else pinned)
  let unknown := revisions.keys.filter (!pins.contains ·)
  unless unknown.isEmpty do
    throw <| IO.userError s!"Trial revisions must name locked Git packages: {String.intercalate ", " unknown}."
  let manifest := if entries.isEmpty then raw else raw.setObjVal! "packages" (.arr entries)
  return ({manifest, packagesDir, pins, paths, copiedPackages, gitSubDirs, localConfigs}, contents)

/-- Keep the original JSON representation so future Lake fields, tagged entries and package order
survive trial revision edits. -/
def loadTrial (project : FilePath) (revisions : Pins) : IO Trial := do
  return (← readTrial project revisions).1

/-- The manifest exists. A second spelling that resolves without a separate directory entry
distinguishes a case-insensitive workspace. -/
private def hasCaseAliases (root : FilePath) : IO Bool := do
  let upper := manifestName.toUpper
  unless (← metadata? (root / upper)).isSome do return false
  let entries ← root.readDir
  return !(entries.any (·.fileName == manifestName) && entries.any (·.fileName == upper))

/-- Read the cache location before copying and confirm the copied lock matches this input.
Only real directories may lead to the freshly materialized checkouts. -/
def prepareTrial (project workspace : FilePath) (revisions : Pins) (configuredDir : IO FilePath)
    (expectedManifest : Option Json := none) : IO Trial := do
  let (initial, contents) ← readTrial project revisions expectedManifest
  let mut trial := initial
  let managed := trial.pins.foldl (init := trial.copiedPackages) fun names name _ => names.insert name
  if !managed.isEmpty && (trial.manifest.getObjValD "packagesDir").isNull then
    let dir ← IO.ofExcept <| packageCacheDir (← configuredDir)
    trial := {trial with packagesDir := dir, manifest := trial.manifest.setObjVal! "packagesDir" (.str dir.toString)}
  let hasModuleCache ← copyTrialSources project workspace trial.packagesDir managed
  trial := { trial with hasModuleCache }
  if (← IO.FS.readFile (workspace / manifestName)) != contents then
    throw <| IO.userError "The project manifest changed while taking the source snapshot."
  if (← hasCaseAliases workspace) then
    discard <| IO.ofExcept <| packageCacheDir trial.packagesDir.toString.toLower
  let mut cachePrefix := workspace
  for part in trial.packagesDir.components do
    cachePrefix := cachePrefix / part
    let some info ← metadata? cachePrefix | return trial
    unless info.type == .dir do throw <| IO.userError s!"Cannot isolate packagesDir through {cachePrefix}."
  for name in managed do
    if (← metadata? (workspace / trial.packagesDir / name)).isSome then
      throw <| IO.userError s!"Cannot isolate the managed package directory for {name}."
  return trial

/-- Preserve the spelling and symlink resolution of the existing prefix. The rest of a path
into a managed checkout may only exist once Lake fetches it. -/
private def resolveExistingPrefix (root relative : FilePath) : IO FilePath := do
  let mut path := root
  let mut resolving := true
  for part in relative.components do
    path := path / part
    if resolving then
      resolving := (← metadata? path).isSome
      if resolving then path ← IO.FS.realPath path
  return path

def Trial.checkLocalPaths (trial : Trial) (root : FilePath) (materialized : Bool) : IO Unit := do
  if materialized then
    for (name, subDir) in trial.gitSubDirs do
      let checkout := root / trial.packagesDir / name
      let package := checkout / subDir
      if ← package.isDir then
        if isInside (← IO.FS.realPath checkout) (← IO.FS.realPath package) then continue
      throw <| IO.userError s!"The package directory {subDir} for {name} must resolve to a directory in its locked checkout."
    for name in trial.copiedPackages do
      if (← isolatedDirectory root (trial.packagesDir / name)).isSome then continue
      throw <| IO.userError s!"Lake did not materialize an isolated copy of {name} at {trial.packagesDir / name}."
  if trial.paths.isEmpty then return
  let foldCase ← hasCaseAliases root
  let key := fun (s : String) => if foldCase then s.toLower else s
  let managed := trial.pins.foldl (init := trial.copiedPackages) fun names name _ => names.insert name
  let names := managed.fold (init := (∅ : Std.HashMap String String)) fun map name => map.insert (key name) name
  let cache ← resolveExistingPrefix root trial.packagesDir
  let physicalParts := cache.components.map key
  let cacheParts := trial.packagesDir.components.map key
  let owner? := fun (base path : List String) => do
    guard (base.isPrefixOf path)
    names[← path[base.length]?]?
  for (name, path) in trial.paths do
    let components := (parts path).map key
    if components.contains ".git" then throw <| IO.userError s!"{name} references a Git metadata directory."
    let normalized := (← IO.ofExcept <| normalParts path).toList.map key
    -- A symlink inside a fetched checkout can change the interpretation of a following '..'.
    let mut owners := [owner? cacheParts components, owner? cacheParts normalized].filterMap id
    if !managed.isEmpty && owners.isEmpty then
      let physical ← resolveExistingPrefix root path
      owners := (owner? physicalParts (physical.components.map key)).toList
    if !owners.isEmpty then
      unless materialized do continue
    else if components.contains ".lake" then
      throw <| IO.userError s!"{name} has a cache path outside the materialized packages."
    let resolved ← IO.FS.realPath (root / path)
    if trial.copiedPackages.contains name && isInside resolved (cache / name) then
      throw <| IO.userError s!"Cannot copy {name} into its own source directory at {path}."
    let inside ← if owners.isEmpty then pure (isInside root resolved) else
      owners.anyM fun (owner : String) => do return isInside (← IO.FS.realPath (root / trial.packagesDir / owner)) resolved
    unless inside && (← resolved.isDir) do
      throw <| IO.userError s!"Missing or external local path dependency {name} at {path}."

end LakeCompat.Verify
