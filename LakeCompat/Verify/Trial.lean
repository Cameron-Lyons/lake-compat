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

private def readTrial (project : FilePath) (revisions : Pins) : IO (Trial × String) := do
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
  let raw ← IO.ofExcept <| Json.parse contents
  let lock ← IO.ofExcept <| Lock.ofJson file raw
  if parts lock.lakeDir != [".lake"] || (FilePath.mk lock.lakeDir).isAbsolute then
    throw <| IO.userError "Build verification currently supports the standard .lake metadata directory."
  let packagesDir ← IO.ofExcept <| packageCacheDir (lock.packagesDir.getD ".lake/packages")
  let mut pins : Pins := ∅
  let mut paths := #[]
  let mut copiedPackages := ∅
  -- Lock.ofJson validated the array if present. Lake defaults an omitted array to empty.
  let mut entries := (raw.getObjValD "packages").getArr?.toOption.getD #[]
  for i in [:entries.size] do
    let (kind, fields, tagged) ← IO.ofExcept <| unpackEntry entries[i]!
    let name ← IO.ofExcept <| (fields.getObjValD "name").getStr? >>= packageName
    for key in [if tagged then "subDir?" else "subDir", "configFile", "manifestFile"] do
      let value := fields.getObjValD key
      unless value == .null do discard <| IO.ofExcept <| relative value (name ++ "." ++ key)
    match lock.packages[name]!.source with
    | .path dir copy =>
      paths := paths.push (name, ← IO.ofExcept <| relative (.str dir) (name ++ ".dir"))
      if copy then copiedPackages := copiedPackages.insert name
    | .git _ rev .. =>
      let revision := revisions[name]?.getD rev
      unless Source.isFullRevision revision do
        throw <| IO.userError s!"{name} needs a full 40- or 64-character Git commit in the trial manifest."
      pins := pins.insert name revision.toLower
      if revisions.contains name then
        let pinned := fields.setObjVal! "rev" (.str revision)
        let pinned := pinned.setObjVal! (if tagged then "inputRev?" else "inputRev") (.str revision)
        entries := entries.set! i (if tagged then Json.mkObj [(kind, pinned)] else pinned)
  let unknown := revisions.keys.filter (!pins.contains ·)
  unless unknown.isEmpty do
    throw <| IO.userError s!"Trial revisions must name locked Git packages: {String.intercalate ", " unknown}."
  let manifest := if revisions.isEmpty then raw else raw.setObjVal! "packages" (.arr entries)
  return ({manifest, packagesDir, pins, paths, copiedPackages}, contents)

/-- Keep the original JSON representation so future Lake fields, tagged entries and package order
survive trial revision edits. -/
def loadTrial (project : FilePath) (revisions : Pins) : IO Trial := do
  return (← readTrial project revisions).1

/-- Read the cache location before copying and confirm the copied lock matches this input.
Only real directories may lead to the freshly materialized checkouts. -/
def prepareTrial (project workspace : FilePath) (revisions : Pins) (configuredDir : IO FilePath) : IO Trial := do
  let (initial, contents) ← readTrial project revisions
  let mut trial := initial
  let managed := trial.pins.foldl (init := trial.copiedPackages) fun names name _ => names.insert name
  if !managed.isEmpty && (trial.manifest.getObjValD "packagesDir").isNull then
    let dir ← IO.ofExcept <| packageCacheDir (← configuredDir)
    trial := {trial with packagesDir := dir, manifest := trial.manifest.setObjVal! "packagesDir" (.str dir.toString)}
  copySources project workspace trial.packagesDir managed
  if (← IO.FS.readFile (workspace / manifestName)) != contents then
    throw <| IO.userError "The project manifest changed while taking the source snapshot."
  let mut cachePrefix := workspace
  for part in trial.packagesDir.components do
    cachePrefix := cachePrefix / part
    let some info ← metadata? cachePrefix | return trial
    unless info.type == .dir do throw <| IO.userError s!"Cannot isolate packagesDir through {cachePrefix}."
  for name in managed do
    if (← metadata? (workspace / trial.packagesDir / name)).isSome then
      throw <| IO.userError s!"Cannot isolate the managed package directory for {name}."
  return trial

def Trial.checkLocalPaths (trial : Trial) (root : FilePath) (materialized : Bool) : IO Unit := do
  let cacheParts := trial.packagesDir.components
  if materialized then
    for name in trial.copiedPackages do
      let destination := root / trial.packagesDir / name
      if (← metadata? destination).any (·.type == .dir) then
        if (← IO.FS.realPath destination) == destination then continue
      throw <| IO.userError s!"Lake did not materialize an isolated copy of {name} at {trial.packagesDir / name}."
  let managedOwner? := fun (parts : List String) => do
    guard (cacheParts.isPrefixOf parts)
    let name ← parts[cacheParts.length]?
    guard (trial.pins.contains name || trial.copiedPackages.contains name)
    return name
  for (name, path) in trial.paths do
    let components := parts path
    if components.contains ".git" then throw <| IO.userError s!"{name} references a Git metadata directory."
    let normalized := (← IO.ofExcept <| normalParts path).toList
    -- A symlink inside a fetched checkout can change the interpretation of a following '..'.
    let owners := [managedOwner? components, managedOwner? normalized].filterMap id
    if !owners.isEmpty then
      unless materialized do continue
    else if components.contains ".lake" then
      throw <| IO.userError s!"{name} has a cache path outside the materialized packages."
    let resolved ← IO.FS.realPath (root / path)
    let inside := if owners.isEmpty then isInside root resolved else
      owners.any fun owner => isInside (root / trial.packagesDir / owner) resolved
    unless inside && (← resolved.isDir) do
      throw <| IO.userError s!"Missing or external local path dependency {name} at {path}."

end LakeCompat.Verify
