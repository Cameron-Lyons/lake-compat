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

/-- Keep the original JSON object so future Lake fields, tagged entries, and package order survive
revision edits. Validate the copy with the same manifest decoder used by metadata analysis. -/
def loadTrial (project : FilePath) (revisions : Pins) : IO Trial := do
  let hasLakefile ← ["lakefile.lean", "lakefile.toml"].anyM fun (name : String) => do
    return (← (project / name).metadata.toBaseIO).toOption.any (·.type == .file)
  unless hasLakefile do
    throw <| IO.userError "Expected a lakefile.lean or lakefile.toml at the project root."
  let file := project / manifestName
  let raw ← IO.ofExcept <| Json.parse (← IO.FS.readFile file)
  let lock ← IO.ofExcept <| Lock.ofJson file raw
  let packagesDir : FilePath := lock.packagesDir.getD ".lake/packages"
  if parts lock.lakeDir != [".lake"] || (FilePath.mk lock.lakeDir).isAbsolute ||
      parts packagesDir != [".lake", "packages"] || packagesDir.isAbsolute then
    throw <| IO.userError "Build verification currently supports the standard .lake/packages layout."
  let mut pins : Pins := ∅
  let mut paths := #[]
  -- Lock.ofJson validated the array if present. Lake defaults an omitted array to empty.
  let mut entries := (raw.getObjValD "packages").getArr?.toOption.getD #[]
  for i in [:entries.size] do
    let (kind, fields, tagged) ← IO.ofExcept <| unpackEntry entries[i]!
    let name ← IO.ofExcept <| (fields.getObjValD "name").getStr? >>= packageName
    for key in [if tagged then "subDir?" else "subDir", "configFile", "manifestFile"] do
      let value := fields.getObjValD key
      unless value == .null do discard <| IO.ofExcept <| relative value (name ++ "." ++ key)
    match lock.packages[name]!.source with
    | .path dir _ => paths := paths.push (name, ← IO.ofExcept <| relative (.str dir) (name ++ ".dir"))
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
  return {manifest, pins, paths}

def Trial.checkLocalPaths (trial : Trial) (root : FilePath) (materialized : Bool) : IO Unit := do
  for (name, path) in trial.paths do
    let components := parts path
    if components.contains ".git" then throw <| IO.userError s!"{name} references a Git metadata directory."
    let mut boundary := root
    if components.contains ".lake" then
      let ".lake" :: "packages" :: owner :: _ := (← IO.ofExcept <| normalParts path).toList
        | throw <| IO.userError s!"{name} has a cache path outside the pinned Git checkouts."
      unless trial.pins.contains owner do
        throw <| IO.userError s!"{name} has a cache path outside the pinned Git checkouts."
      boundary := root / ".lake" / "packages" / owner
      unless materialized do continue
    let resolved ← IO.FS.realPath (root / path)
    unless isInside boundary resolved && (← resolved.isDir) do
      throw <| IO.userError s!"Missing or external local path dependency {name} at {path}."

end LakeCompat.Verify
