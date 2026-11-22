import LakeCompat.Verify.Process
import LakeCompat.Verify.Outputs
import LakeCompat.Options

open Lean System

namespace LakeCompat.Verify

private def report (status message : String) (detail : Json := Json.mkObj []) : Json :=
  detail.mergeObj <| Json.mkObj [("status", .str status), ("message", .str message)]

private def run (args : Array String) (cwd : FilePath) (env : Environment) (ms : Nat)
    (detail : IO.Ref Json) (operation := "Verification") : IO CommandResult := do
  let result ← runCommand args cwd env ms
  if result.timedOut then
    unless result.output.isEmpty do detail.modify (·.setObjVal! "output" (.str result.output))
    let limit := if ms % 1000 == 0 then s!"{ms / 1000}-second" else s!"{ms}-millisecond"
    throw <| IO.userError s!"{operation} exceeded its {limit} command timeout."
  return result

/-- Lake initializes fresh repositories using Git's default object format. A 64-character
lock needs a SHA-256 repository before Lake fetches it, even alongside SHA-1 packages. -/
private def initializeSha256Checkouts (workspace : FilePath) (trial : Trial)
    (env : Environment) (detail : IO.Ref Json) : IO Unit := do
  for (name, revision) in trial.pins do
    if revision.utf8ByteSize == 64 then
      let repo := workspace / trial.packagesDir / name
      let result ← run #["git", "init", "--quiet", "--object-format=sha256", "--", repo.toString] workspace env 10000 detail
      if result.exitCode != 0 then
        detail.modify (·.setObjVal! "output" (.str result.output))
        throw <| IO.userError s!"Git could not initialize a SHA-256 checkout for {name}."

private def confirmCheckout (cache : FilePath) (name expected : String)
    (env : Environment) (detail : IO.Ref Json) : IO Unit := do
  let repo := cache / name
  unless (← metadata? repo).any (·.type == .dir) && (← metadata? (repo / ".git")).any (·.type == .dir) do
    throw <| IO.userError s!"Lake did not materialize an isolated checkout of {name}."
  -- A clean porcelain v2 status includes the full HEAD and no changed-file records.
  let status ← run #["git", "--no-optional-locks", "status", "--porcelain=v2", "--branch", "--no-ahead-behind",
    "--untracked-files=no", "--no-renames"] repo env 30000 detail
  let lines := status.output.splitOn "\n"
  if status.exitCode == 0 && lines.head? == some s!"# branch.oid {expected}" then
    if lines.all (·.startsWith "# ") then return
  else
    -- A large dirty status can scroll its HEAD out of the bounded command log.
    let head ← run #["git", "rev-parse", "--verify", "HEAD"] repo env 10000 detail
    if head.exitCode != 0 || head.output != expected then
      throw <| IO.userError s!"The checkout for {name} does not match its locked revision {expected}."
  -- An index change and an unstaged change can cancel out in the actual build inputs.
  let diff ← run #["git", "diff", "--quiet", "--no-ext-diff", "HEAD", "--"] repo env 30000 detail
  if diff.exitCode != 0 then
    throw <| IO.userError s!"Could not confirm that the tracked files in {name} match its locked revision."

private def confirmGit (root packagesDir : FilePath) (pins : Pins) (env : Environment) (detail : IO.Ref Json) : IO Unit := do
  if pins.isEmpty then return
  let some cache ← isolatedDirectory root packagesDir
    | throw <| IO.userError s!"Lake did not materialize an isolated package directory at {packagesDir}."
  let failure ← Std.Mutex.new (none : Option (Nat × Json))
  let check := fun ((name, expected), index) => do
    -- Each worker owns its output. Preserve the same first failure and diagnostic
    -- even when several independent checkout checks fail in a different order.
    let output ← IO.mkRef (Json.mkObj [])
    try
      confirmCheckout cache name expected env output
    catch e =>
      let value ← output.get
      failure.atomically <| modify fun previous =>
        if previous.all (fun old => index < old.1) then some (index, value) else previous
      throw e
  try
    discard <| mapConcurrent pins.toArray.zipIdx check
  catch e =>
    if let some (_, output) ← failure.atomically get then detail.modify (·.mergeObj output)
    throw e

private def writeInput (dst : FilePath) (value : String) : IO Unit := do
  -- Replace links and read-only inputs in the copy without changing their targets.
  if (← metadata? dst).isSome then IO.FS.removeFile dst
  IO.FS.writeFile dst value

/-- An unqualified filename can also name a root declaration. Make paths with a
directory explicit so a missing source cannot select a dependency's named target. -/
private def resolveBuildTarget (workspace : FilePath) (spec : String) : IO String := do
  let base := (spec.takeWhile (· != ':')).copy
  if base.endsWith ".lean" && !base.startsWith "@" then
    if base.startsWith "./" then return spec
    if base.contains '/' then return "./" ++ spec
    if (← (workspace / base).metadata.toBaseIO).toOption.any (·.type == .file) then return "./" ++ spec
    return "@/" ++ spec
  return spec

/-- Check the inputs again after Lake runs, including when it deletes or replaces a file. -/
private def readBuiltInput (workspace file : FilePath) : IO (Option String) := do
  let some path := (← (IO.FS.realPath file).toBaseIO).toOption | return none
  unless isInside workspace path do return none
  let some info ← metadata? path | return none
  unless info.type == .file && info.byteSize.toNat ≤ sourceByteLimit do return none
  return (← (IO.FS.readFile path).toBaseIO).toOption

private def checkTrialInputs (workspace : FilePath) (trial : Trial) (lockContents target : String) : IO Unit := do
  let lock ← readBuiltInput workspace (workspace / manifestName)
  let pin ← readBuiltInput workspace (workspace / "lean-toolchain")
  unless pin.any (·.trimAscii == target.toSlice) && lock.any (fun contents =>
      contents == lockContents || (Json.parse contents).toOption == some trial.manifest) do
    throw <| IO.userError "The trial manifest or toolchain changed during verification."

private def confirmPackages (workspace : FilePath) (trial : Trial) (env : Environment) (detail : IO.Ref Json) : IO Unit := do
  if let some message ← packageOverridesIssue? (workspace / ".lake") then
    throw <| IO.userError message
  confirmGit workspace trial.packagesDir trial.pins env detail
  trial.checkLocalPaths workspace true

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

/-- The target Lake resolves every active package in the copied workspace. A null
result records a workspace load error. A failure to run the inspector is inconclusive. -/
private def prepareOutputs (workspace : FilePath) (trial : Trial) (lake : Array String)
    (env : Environment) (detail : IO.Ref Json) (timeoutMs : Nat) (defaultPackages : Array String) : IO Bool := do
  let parent := workspace.parent.get!
  let inspector := parent / "inspector"
  IO.FS.createDir inspector
  IO.FS.writeFile (inspector / "lakefile.lean") workspaceInspector
  IO.FS.writeFile (inspector / manifestName) r#"{"version":"1.0.0","packages":[]}"#
  let output := parent / "outputs.json"
  let probe ← run (lake ++ #["-d", inspector.toString, "run", "inspect", output.toString])
    workspace env timeoutMs detail "Inspecting Lake output directories"
  if probe.exitCode != 0 then
    detail.modify (·.setObjVal! "output" (.str probe.output))
    if (← (IO.FS.readFile output).toBaseIO).toOption == some "null" then return false
    throw <| IO.userError "The target Lake could not inspect the package output directories."
  let result ← IO.ofExcept <| Json.parse (← IO.FS.readFile output)
  invalidateModuleCaches workspace trial (← checkedLibraryDirs workspace result defaultPackages)
  return true

private def verifyTrial (parent project : FilePath) (target : String) (revisions : Pins)
    (buildTimeoutMs : Nat) (detail : IO.Ref Json) (expectedManifest : Option Json)
    (targetSpecs : Array String) (testArgs? : Option (Array String)) : IO Json := do
  let env ← trialEnvironment target parent
  let lake := #["elan", "run", target, "lake"]
  let version ← run (lake.push "--version") parent env 20000 detail
  if version.exitCode != 0 then
    return report "unavailable" s!"Cannot run Lake with installed toolchain {target}."
      (Json.mkObj [("output", .str version.output)])
  let workspace := parent / "project"
  let trial ← prepareTrial project workspace revisions (configuredPackagesDir project parent target lake env detail)
    expectedManifest
  trial.checkLocalPaths workspace false
  initializeSha256Checkouts workspace trial env detail
  let pinFile := workspace / "lean-toolchain"
  let lockFile := workspace / manifestName
  let lockContents := trial.manifest.pretty ++ "\n"
  writeInput pinFile (target ++ "\n")
  writeInput lockFile lockContents
  detail.modify (·.setObjVal! "lake_version" (.str version.output))
  let defaultPackages := targetSpecs.filterMap defaultPackage?
  if !defaultPackages.isEmpty || (← trial.needsOutputInspection workspace) then
    let loaded ← prepareOutputs workspace trial lake env detail buildTimeoutMs defaultPackages
    checkTrialInputs workspace trial lockContents target
    unless loaded do return report "failed" "Lake could not load the trial workspace." (← detail.get)
    if let some message ← packageOverridesIssue? (workspace / ".lake") then throw <| IO.userError message
  let buildTargets ← targetSpecs.mapM (resolveBuildTarget workspace)
  let build ← run (lake.push "build" ++ buildTargets) workspace env buildTimeoutMs detail
  detail.set <| Json.mkObj [("output", .str build.output), ("lake_version", .str version.output)]
  let check ← if build.exitCode == 0 && (targetSpecs.isEmpty || testArgs?.isSome) then
      some <$> run (lake.push (if testArgs?.isSome then "check-test" else "check-build")) workspace env 30000 detail
    else pure none
  checkTrialInputs workspace trial lockContents target
  if build.exitCode != 0 then
    let hint := if revisions.isEmpty then "" else
      " If the candidate needs different dependencies, resolve them with lake update on your upgrade branch and rerun the check."
    return report "failed" s!"lake build exited with code {build.exitCode}.{hint}" (← detail.get)
  if let some result := check then
    if result.exitCode != 0 then
      let tail := boundedLogTail (build.output ++ "\n" ++ result.output)
      detail.modify (·.setObjVal! "output" (.str tail.trimAscii.copy))
      if testArgs?.isSome then
        return report "inconclusive" "Lake could not confirm a configured test driver for --test." (← detail.get)
      return report "inconclusive"
        "Lake could not confirm the project has default build targets. Use --build-target NAME to verify a named target." (← detail.get)
  confirmPackages workspace trial env detail
  if let some args := testArgs? then
    let command := lake.push "test"
    let command := if args.isEmpty then command else command.push "--" ++ args
    let test ← run command workspace env buildTimeoutMs detail "lake test"
    let output := boundedLogTail (build.output ++ "\n" ++ test.output)
    detail.modify fun d =>
      (d.setObjVal! "test_exit_code" (toJson test.exitCode.toNat)).setObjVal! "output" (.str output.trimAscii.copy)
    checkTrialInputs workspace trial lockContents target
    if test.exitCode != 0 then
      return report "failed" s!"lake test exited with code {test.exitCode}." (← detail.get)
    confirmPackages workspace trial env detail
  let checked := Json.mkObj (trial.pins.toList.map fun (name, rev) => (name, .str rev))
  let built := if targetSpecs.isEmpty then "Default targets" else String.intercalate ", " targetSpecs.toList
  let message := if testArgs?.isSome then s!"The build and Lake test driver passed on {target} with the trial pins."
    else s!"{built} built on {target} with the trial pins."
  return report "passed" message
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

/-- Build Lake targets and optionally run the test driver on an installed Elan toolchain in an isolated source copy. -/
def verifyBuild (project : FilePath) (target : String) (revisions : Array (String × String) := #[])
    (buildTimeoutMs : Nat := defaultBuildTimeoutSeconds * 1000) (expectedManifest : Option Json := none)
    (buildTargets : Array String := #[]) (runTests := false)
    (testArgs? : Option (Array String) := if runTests then some #[] else none) : IO Json := do
  if Platform.isWindows then return Verify.report "unavailable" "Build verification requires a POSIX host."
  if target.isEmpty || target.startsWith "-" || target.any Char.isWhitespace || target.contains (Char.ofNat 0) then
    return Verify.report "inconclusive" "Invalid elan toolchain name."
  let detail ← IO.mkRef (Json.mkObj [])
  try
    let specs ← IO.ofExcept (buildTargets.mapM buildTargetSpec)
    let source ← IO.FS.realPath project
    if let some message ← packageOverridesIssue? (source / ".lake") then
      throw <| IO.userError message
    let mut pins : Verify.Pins := ∅
    for (name, revision) in revisions do
      if let some old := pins[name]? then
        unless old == revision.toLower do throw <| IO.userError s!"Two different trial revisions for {name}."
      pins := pins.insert name revision.toLower
    let temp ← IO.FS.createTempDir
    try
      Verify.verifyTrial (← IO.FS.realPath temp) source target pins buildTimeoutMs detail expectedManifest specs testArgs?
    finally
      Verify.removeTrialTree temp
  catch e => return Verify.report "inconclusive" (toString e) (← detail.get)

end LakeCompat
