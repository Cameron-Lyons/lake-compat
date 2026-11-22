import LakeCompat.Verify.Trial
import Lake.Build.ModuleArtifacts
import Lake.Util.Log
import Lake.Util.Name

open Lean System

namespace LakeCompat.Verify

def workspaceInspector := include_str "../../scripts/inspect-workspace.lean"

/-- A TOML key is either literal or contains a backslash escape. When neither a
directory setting nor an escape occurs, all package output directories are defaults. -/
private def defaultTomlOutputs (config : FilePath) : IO Bool := do
  let mut toml := config
  if config.extension.isNone then
    if ← (config.addExtension "lean").pathExists then return false
    toml := config.addExtension "toml"
  if toml.extension != some "toml" then return false
  let .ok text ← (IO.FS.readFile toml).toBaseIO | return false
  return !text.contains '\\' && !["buildDir", "leanLibDir", "nativeLibDir", "binDir", "irDir"].any (text.contains ·)

/-- Local declarative packages with default outputs build under their fresh .lake
directories. Other packages need the evaluated configuration of the target Lake. -/
def Trial.needsOutputInspection (trial : Trial) (workspace : FilePath) : IO Bool := do
  if trial.hasModuleCache || !trial.pins.isEmpty || !trial.copiedPackages.isEmpty then return true
  return !(← trial.localConfigs.allM fun file => defaultTomlOutputs (workspace / file))

private structure PackageOutputs where
  name : String
  hasDefaultTargets : Bool
  buildDir : FilePath
  leanLibDir : FilePath
  nativeLibDir : FilePath
  binDir : FilePath
  irDir : FilePath
  deriving FromJson

/-- Existing directory symlinks and '..' are resolved in filesystem order. A new
output directory must also be inside the copy before Lake is allowed to create it. -/
private def outputInside (workspace path : FilePath) : IO Bool := do
  unless isInside workspace path do return false
  let mut dir := workspace
  for part in path.components.drop workspace.components.length do
    if part.isEmpty || part == "." then continue
    if part == ".." then
      if dir == workspace then return false
      dir := dir.parent.getD workspace
    else
      dir := dir / part
      if (← metadata? dir).any (·.type == .symlink) then
        dir ← IO.FS.realPath dir
        unless isInside workspace dir do return false
  return true

def checkedLibraryDirs (workspace : FilePath) (result : Json) (defaultPackages : Array String := #[]) :
    IO (Array FilePath) := do
  let packages ← IO.ofExcept (fromJson? (α := Array PackageOutputs) result)
  if packages.isEmpty then throw <| IO.userError "Lake reported an empty trial workspace."
  let requested := Std.HashSet.ofArray <| defaultPackages.map fun name =>
    (Lake.stringToLegalOrSimpleName name).toString (escape := false)
  for pkg in packages do
    -- Lake can silently fall back to the root defaults when a package has none.
    if requested.contains pkg.name && !pkg.hasDefaultTargets then
      throw <| IO.userError s!"Package {pkg.name} has no default build targets. Select one with --build-target @{pkg.name}/NAME."
    for path in #[pkg.buildDir, pkg.leanLibDir, pkg.nativeLibDir, pkg.binDir, pkg.irDir] do
      unless ← outputInside workspace path do
        throw <| IO.userError s!"Package {pkg.name} configures a build output directory outside the trial workspace: {path}"
  return packages.map (·.leanLibDir)

/-- Validate the fields of a Lake build trace with module outputs. Using the individual
Lake field decoders keeps the build engine out of the standalone scanner. -/
private def moduleTrace? (contents : String) : Except String Lake.ModuleOutputDescrs := do
  let obj ← Lake.JsonObject.fromJson? (← Json.parse contents)
  let outputs ← obj.getAs Lake.ModuleOutputDescrs "outputs"
  if obj.contains "schemaVersion" then
    discard <| obj.getAs Lake.Hash "depHash"
  else
    unless (Lake.Hash.ofDecimal? (← obj.get "depHash")).isSome do throw "invalid legacy trace hash"
  discard <| obj.getAs? (Array (String × Json)) "inputs"
  discard <| obj.getAs? Lake.Log "log"
  discard <| obj.getAs? Bool "synthetic"
  return outputs

/-- Discard genuine module traces only from active local package output directories. -/
def invalidateModuleCaches (workspace : FilePath) (trial : Trial) (libraries : Array FilePath) : IO Unit := do
  unless trial.hasModuleCache do return
  let mut gitDirs := #[]
  for (name, _) in trial.pins do
    let path := workspace / trial.packagesDir / name
    if ← path.isDir then gitDirs := gitDirs.push (← IO.FS.realPath path)
  let localDir := fun path => isInside workspace path && !gitDirs.any (isInside · path)
  let visited ← IO.mkRef (∅ : Std.HashSet FilePath)
  let enter := fun path => do
    let dir ← IO.FS.realPath path
    if !localDir dir || dir.components.contains ".git" || (← visited.get).contains dir then return false
    visited.modify (·.insert dir)
    return true
  for dir in libraries do
    unless ← dir.isDir do continue
    for file in ← dir.walkDir enter do
      -- A packed module can have only its .ltar and the saved trace left on disk.
      unless file.extension == some "trace" do continue
      let actual ← IO.FS.realPath file
      unless localDir actual && (← actual.metadata).type == .file do continue
      let some contents := String.fromUTF8? (← IO.FS.readBinFile actual) | continue
      if (moduleTrace? contents).isOk then IO.FS.removeFile file

end LakeCompat.Verify
