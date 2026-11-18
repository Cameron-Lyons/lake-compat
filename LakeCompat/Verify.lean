import LakeCompat.Verify.Process
import LakeCompat.Verify.Trial

open Lean System

namespace LakeCompat.Verify

private def report (status message : String) (detail : Json := Json.mkObj []) : Json :=
  detail.mergeObj <| Json.mkObj [("status", .str status), ("message", .str message)]

private def run (args : Array String) (cwd : FilePath) (env : Environment) (ms : Nat)
    (detail : IO.Ref Json) : IO CommandResult := do
  let result ← runCommand args cwd env ms
  if result.timedOut then
    unless result.output.isEmpty do detail.modify (·.setObjVal! "output" (.str result.output))
    let limit := if ms % 1000 == 0 then s!"{ms / 1000}-second" else s!"{ms}-millisecond"
    throw <| IO.userError s!"Verification exceeded its {limit} command timeout."
  return result

private def confirmGit (packagesDir : FilePath) (pins : Pins) (env : Environment) (detail : IO.Ref Json) : IO Unit := do
  for (name, expected) in pins do
    let repo := packagesDir / name
    unless (← (repo / ".git").symlinkMetadata.toBaseIO).toOption.any (·.type == .dir) &&
        (← IO.FS.realPath repo) == repo do
      throw <| IO.userError s!"Lake did not materialize an isolated checkout of {name}."
    let head ← run #["git", "rev-parse", "--verify", "HEAD"] repo env 10000 detail
    if head.exitCode != 0 || head.output != expected then
      throw <| IO.userError s!"The checkout for {name} does not match its locked revision {expected}."
    let diff ← run #["git", "diff", "--quiet", "--no-ext-diff", "HEAD", "--"] repo env 30000 detail
    if diff.exitCode != 0 then
      throw <| IO.userError s!"Could not confirm that the tracked files in {name} match its locked revision."

private def writeInput (dst : FilePath) (value : String) : IO Unit := do
  -- Replace links and read-only inputs in the copy without changing their targets.
  if (← metadata? dst).isSome then IO.FS.removeFile dst
  IO.FS.writeFile dst value

/-- Older locks can leave the packages directory to the project configuration. Ask the target
Lake to evaluate it in a separate copy before allowing Lake to materialize any dependencies. -/
private def configuredPackagesDir (project parent : FilePath) (target : String) (lake : Array String)
    (env : Environment) (detail : IO.Ref Json) : IO FilePath := do
  let probe := parent / "config"
  if (← metadata? (project / "lakefile.lean")).isSome then
    copySources project probe
  else
    -- Declarative TOML needs only the lakefile. A custom cache can be large or contain links.
    let config ← IO.FS.realPath (project / "lakefile.toml")
    let info ← config.metadata
    unless isInside project config && info.type == .file && info.byteSize.toNat ≤ sourceByteLimit do
      throw <| IO.userError "Cannot read an in-tree lakefile.toml to determine packagesDir."
    IO.FS.createDir probe
    IO.FS.writeFile (probe / "lakefile.toml") (← IO.FS.readFile config)
  writeInput (probe / "lean-toolchain") (target ++ "\n")
  let output := parent / "resolved-config.toml"
  let result ← run (lake ++ #["translate-config", "toml", output.toString]) probe env 30000 detail
  if result.exitCode != 0 then
    detail.modify (·.setObjVal! "output" (.str result.output))
    throw <| IO.userError "Lake could not determine packagesDir from the configuration. Regenerate the manifest with its packagesDir recorded."
  -- Lake's TOML printer emits top-level keys first and uses JSON-compatible quoted strings.
  for line in (← IO.FS.readFile output).splitOn "\n" do
    if line.trimAsciiStart.startsWith "[" then break
    let some sep := line.find? '=' | continue
    if (line.sliceTo sep).trimAscii == "packagesDir".toSlice then
      let path ← IO.ofExcept <| Json.parse (line.sliceFrom sep.next!).copy >>= Json.getStr?
      return ⟨path⟩
  return ".lake/packages"

private def verifyTrial (parent project : FilePath) (target : String) (revisions : Pins)
    (buildTimeoutMs : Nat) (detail : IO.Ref Json) : IO Json := do
  let env ← trialEnvironment target parent
  let lake := #["elan", "run", target, "lake"]
  let version ← run (lake.push "--version") parent env 20000 detail
  if version.exitCode != 0 then
    return report "unavailable" s!"Cannot run Lake with installed toolchain {target}."
      (Json.mkObj [("output", .str version.output)])
  let workspace := parent / "project"
  let trial ← prepareTrial project workspace revisions (configuredPackagesDir project parent target lake env detail)
  trial.checkLocalPaths workspace false
  let pinFile := workspace / "lean-toolchain"
  let lockFile := workspace / manifestName
  writeInput pinFile (target ++ "\n")
  writeInput lockFile (trial.manifest.pretty ++ "\n")
  let build ← run (lake.push "build") workspace env buildTimeoutMs detail
  detail.set <| Json.mkObj [("output", .str build.output), ("lake_version", .str version.output)]
  let check ← if build.exitCode == 0 then some <$> run (lake.push "check-build") workspace env 30000 detail else pure none
  if (← IO.ofExcept <| Json.parse (← IO.FS.readFile lockFile)) != trial.manifest ||
      (← IO.FS.readFile pinFile).trimAscii != target.toSlice then
    return report "inconclusive" "The build changed its trial manifest or toolchain." (← detail.get)
  if build.exitCode != 0 then
    let hint := if revisions.isEmpty then "" else
      " If the candidate needs different dependencies, resolve them with lake update on your upgrade branch and rerun the check."
    return report "failed" s!"lake build exited with code {build.exitCode}.{hint}" (← detail.get)
  if let some result := check then
    if result.exitCode != 0 then
      let output := (build.output ++ "\n" ++ result.output).toUTF8
      let tail := String.fromUTF8Lossy (output.extract (output.size - logLimit) output.size)
      detail.modify (·.setObjVal! "output" (.str tail.trimAscii.copy))
      return report "inconclusive" "Lake could not confirm the project has default build targets." (← detail.get)
  if (← metadata? (workspace / ".lake" / "package-overrides.json")).isSome then
    throw <| IO.userError "The build created package overrides; the trial manifest cannot confirm effective pins."
  confirmGit (workspace / trial.packagesDir) trial.pins env detail
  trial.checkLocalPaths workspace true
  let checked := Json.mkObj (trial.pins.toList.map fun (name, rev) => (name, .str rev))
  return report "passed" s!"Default targets built on {target} with the trial pins."
    ((← detail.get).setObjVal! "checked_revisions" checked)

/-- Builds can leave read-only directories. Restore owner access only on directories in the
temporary tree, so hard-linked files and symlink targets keep their permissions. -/
private partial def removeTrialTree (path : FilePath) (repairPermissions := false) : IO Unit := do
  let some info ← metadata? path | return
  unless info.type == .dir do return ← IO.FS.removeFile path
  unless repairPermissions do
    try return ← IO.FS.removeDirAll path catch
      | .permissionDenied .. => pure ()
      | e => throw e
  IO.setAccessRights path { user := { read := true, write := true, execution := true } }
  for entry in ← path.readDir do
    removeTrialTree entry.path true
  IO.FS.removeDir path

end Verify

def defaultBuildTimeoutSeconds : Nat := 600

/-- Verify the default build on an installed Elan toolchain using an isolated working-source copy. -/
def verifyBuild (project : FilePath) (target : String) (revisions : Array (String × String) := #[])
    (buildTimeoutMs : Nat := defaultBuildTimeoutSeconds * 1000) : IO Json := do
  if Platform.isWindows then return Verify.report "unavailable" "Build verification requires a POSIX host."
  if target.isEmpty || target.startsWith "-" || target.any Char.isWhitespace || target.contains (Char.ofNat 0) then
    return Verify.report "inconclusive" "Invalid elan toolchain name."
  let detail ← IO.mkRef (Json.mkObj [])
  try
    let source ← IO.FS.realPath project
    if (← Verify.metadata? (source / ".lake" / "package-overrides.json")).isSome then
      throw <| IO.userError "This workspace has package overrides; its root manifest alone cannot reproduce them."
    let mut pins : Verify.Pins := ∅
    for (name, revision) in revisions do
      if let some old := pins[name]? then
        unless old == revision.toLower do throw <| IO.userError s!"Two different trial revisions for {name}."
      pins := pins.insert name revision.toLower
    let temp ← IO.FS.createTempDir
    try
      Verify.verifyTrial (← IO.FS.realPath temp) source target pins buildTimeoutMs detail
    finally
      Verify.removeTrialTree temp
  catch e => return Verify.report "inconclusive" (toString e) (← detail.get)

end LakeCompat
