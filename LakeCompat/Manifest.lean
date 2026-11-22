import Lean.Data.Json.Parser
import Lean.Data.Json.Printer
import Lake.Load.Manifest
import Std.Data.HashSet
import Std.Data.TreeMap

namespace LakeCompat
open Lean System

private def wordChar (c : Char) : Bool :=
  c.isAlphanum || c == '_' || Lean.isLetterLike c

/-- Expand official Elan release and channel abbreviations while keeping linked toolchains intact. -/
def normalizeToolchain (value : String) : Except String String := do
  let value := value.trimAscii.copy
  let invalid := s!"invalid Lean toolchain {repr value}"
  let (origin, version) ← match value.splitOn ":" with
    | [version] => pure (none, version)
    | [origin, version] => do
      let [owner, repo] := origin.splitOn "/" | throw invalid
      unless [owner, repo].all (fun p => !p.isEmpty && p.all (fun c => wordChar c || ".-".contains c)) do
        throw invalid
      pure (some origin, version)
    | _ => throw invalid
  unless !version.isEmpty && version.all (fun c => wordChar c || ".+-".contains c) do throw invalid
  if let some origin := origin then
    if origin != "leanprover/lean4" then return value
  let bare := (version.dropPrefix "v").copy
  let core := (bare.takeWhile (fun c => c.isDigit || c == '.')).copy
  let suffix := (bare.dropPrefix core).copy
  let numbers := core.splitOn "."
  let isRelease := numbers.length == 3 &&
    numbers.all (fun s => !s.isEmpty && s.all Char.isDigit) &&
    (suffix.isEmpty || (suffix.length > 1 && (suffix.front == '-' || suffix.front == '+')))
  if isRelease then return "leanprover/lean4:v" ++ bare
  if origin.isSome || ["stable", "nightly", "beta"].contains version || version.startsWith "nightly-" then
    return "leanprover/lean4:" ++ version
  return value

/-- Directory spelling of a package name as serialized by Lake. -/
def packageName (value : String) : Except String String := do
  let invalid := s!"invalid package name {repr value}"
  let name ← if value.contains '«' || value.contains '»' then do
    let parsed := value.toName
    if parsed.isAnonymous then throw invalid
    pure (parsed.toString (escape := false))
  else pure value
  if name.isEmpty || name == "." || name == ".." ||
      name.any (fun c => c == '/' || c == '\\' || c == Char.ofNat 0) then
    throw invalid
  return name

inductive LockedSource where
  | git (url revision : String) (subDir inputRev : Option String)
  | path (dir : String) (copy : Bool)
  deriving Inhabited

def LockedSource.revision? : LockedSource → Option String
  | .git _ rev .. => some rev
  | .path .. => none

structure LockedPackage where
  name : String
  scope : String
  inherited : Bool
  manifestFile : String
  source : LockedSource
  deriving Inhabited

abbrev PackageMap := Std.TreeMap String LockedPackage

structure Lock where
  name : Option String
  lakeDir : String
  packagesDir : Option String
  fixedToolchain : Bool
  packages : PackageMap

def manifestName := "lake-manifest.json"

/-- An empty Lake overrides manifest leaves the root lock in effect. -/
def packageOverridesIssue? (lakeDir : FilePath) : IO (Option String) := do
  let file := lakeDir / "package-overrides.json"
  try
    unless (← file.metadata).type == .file do
      return some s!"Cannot inspect package overrides at {file}: expected a regular file."
    let entries ← IO.ofExcept (Lake.Manifest.parseEntries (← IO.FS.readFile file))
    if entries.isEmpty then return none
    return some s!"{file} contains package overrides that can replace entries from the root lock."
  catch
    | .noFileOrDirectory .. => return none
    | e => return some s!"Cannot inspect package overrides at {file}: {e}"

private def stringField (obj : Json) (key : String) : Except String String := do
  let some s := (obj.getObjValD key).getStr?.toOption | throw s!"expected a non-empty string for {key}"
  if s.isEmpty || s.contains (Char.ofNat 0) then throw s!"expected a non-empty string for {key}"
  return s

private def optionalString (obj : Json) (key : String) : Except String (Option String) :=
  if obj.getObjValD key == .null then pure none else some <$> stringField obj key

private def boolField (obj : Json) (key : String) (fallback : Option Bool := none) : Except String Bool :=
  match obj.getObjValD key, fallback with
  | .null, some b => pure b
  | .bool b, _ => pure b
  | _, _ => throw s!"expected a boolean for {key}"

/-- Unwrap a package without discarding fields needed to write an updated trial lock. -/
def unpackEntry (raw : Json) : Except String (String × Json × Bool) := do
  let obj ← raw.getObj?.mapError fun _ => "expected a package object"
  if obj.size == 1 then
    let (k, v) := obj.minEntry!
    if k == "path" || k == "git" then return (k, v, true)
    throw "expected a package with a source type"
  return (← stringField raw "type", raw, false)

private def decodePackage (raw : Json) : Except String LockedPackage := do
  let (kind, fields, legacy) ← unpackEntry raw
  let name ← packageName (← stringField fields "name")
  let inherited ← boolField fields "inherited"
  let scope ← match fields.getObjValD "scope" with
    | .null => pure ""
    | v => v.getStr?.mapError fun _ => s!"expected a string for {name}.scope"
  -- Tagged v5/v6 entries never recorded the package manifest path. Inspect the
  -- conventional file when present. Newer entries can override it.
  let manifestFile ← if legacy then pure manifestName
    else pure ((← optionalString fields "manifestFile").getD manifestName)
  let source ← match kind with
    | "git" => do
      let subDir ← optionalString fields (if legacy then "subDir?" else "subDir")
      let inputRev ← optionalString fields (if legacy then "inputRev?" else "inputRev")
      pure <| LockedSource.git (← stringField fields "url") (← stringField fields "rev") subDir inputRev
    | "path" => pure <| LockedSource.path (← stringField fields "dir") (← boolField fields "copy" (some false))
    | _ => throw s!"unknown source type for {name}: {kind}"
  return { name, scope, inherited, manifestFile, source }

private def supportedVersion : Json → Bool
  | (n : Nat) => n >= 5
  | .str version =>
    (Lake.StdVer.parse version).toOption.any fun v => v.major == 1 || (v.major == 0 && v.minor >= 5)
  | _ => false

/-- Decode all entries, optionally retaining only the direct requirements of a nested manifest. -/
def Lock.ofJson (file : FilePath) (raw : Json) (directOnly := false) : Except String Lock := Except.mapError (s!"{file}: {·}") <| do
  unless raw.getObj?.isOk do throw "expected a Lake manifest object"
  let version := (raw.getObjVal? "version").toOption.getD (raw.getObjValD "schemaVersion")
  unless supportedVersion version do throw s!"unsupported Lake manifest version {version.compress}"
  let name ← (← optionalString raw "name").mapM packageName
  let lakeDir := (← optionalString raw "lakeDir").getD ".lake"
  let packagesDir ← optionalString raw "packagesDir"
  let fixedToolchain ← boolField raw "fixedToolchain" (some false)
  let entries ← match raw.getObjValD "packages" with
    | .null => pure #[]
    | v => v.getArr?.mapError fun _ => "expected a packages array"
  let mut packages : PackageMap := ∅
  let mut names := Std.HashSet.emptyWithCapacity entries.size
  for entry in entries do
    let p ← decodePackage entry
    let (duplicate, updated) := names.containsThenInsert p.name
    if duplicate then throw s!"duplicate package {repr p.name}"
    names := updated
    unless directOnly && p.inherited do packages := packages.insert p.name p
  return ⟨name, lakeDir, packagesDir, fixedToolchain, packages⟩

def Lock.parse (file : FilePath) (contents : String) (directOnly := false) : Except String Lock := do
  let raw ← (Json.parse contents).mapError (s!"{file}: {·}")
  Lock.ofJson file raw directOnly

end LakeCompat
