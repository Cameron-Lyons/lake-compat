import LeanTests.Support
import LakeCompat.Report

open Lean System LeanTests.Support

namespace LeanTests.Cli

private def oldRev := String.ofList (List.replicate 40 '1')
private def newRev := String.ofList (List.replicate 40 '2')

private def projectLock (packages : Array Json) (name := "app") : Json :=
  Json.mkObj [("name", name), ("version", "1.2.0"), ("packagesDir", ".lake/packages"), ("packages", .arr packages)]

private def dependency (rev := oldRev) (url := "https://github.com/example/lib") : Json :=
  Json.mkObj [("name", "lib"), ("type", "git"), ("url", url), ("rev", rev), ("inherited", false)]

private def withProject (test : FilePath → IO Unit) : IO Unit := withTempDir fun temp => do
  let project := temp / "app"
  let lib := project / ".lake/packages/lib"
  IO.FS.createDirAll lib
  IO.FS.writeFile (project / "lean-toolchain") "v4.19.0"
  IO.FS.writeFile (lib / "lean-toolchain") "v4.19.0"
  writeJson (lib / "lake-manifest.json") (projectLock #[] "lib")
  writeJson (project / "lake-manifest.json") (projectLock #[dependency])
  test project

private def invoke (project : FilePath) (args : Array String)
    (env : Array (String × Option String) := #[]) : IO IO.Process.Output :=
  cli (#["4.20.0", "--project", project.toString] ++ args) env

private def expectCode (out : IO.Process.Output) (code : UInt32) : IO Unit :=
  check (out.exitCode == code) s!"CLI exited with {out.exitCode}; expected {code}\n{out.stdout}{out.stderr}"

private def jsonResult (out : IO.Process.Output) (code : UInt32) : IO Json := do
  expectCode out code
  check out.stderr.isEmpty out.stderr
  let report ← IO.ofExcept (Json.parse out.stdout)
  check (report.getObjValD "schema_version" == toJson (1 : Nat)) report.compress
  return report

private def expectError (project : FilePath) (args : Array String) (detail : String) : IO String := do
  let report ← jsonResult (← invoke project (#["--json"] ++ args)) 2
  let message := LakeCompat.Report.string report "error"
  check (!message.isEmpty && message.contains detail) report.compress
  return message

private def scan : IO Unit := withProject fun project => do
  let offline := #[("PATH", some "")]
  let out ← invoke project #[] offline
  expectCode out 0
  for text in ["app -> lib", "Build not run", "Use --suggest"] do check (out.stdout.contains text) out.stdout
  let report ← jsonResult (← invoke project #["--json", "--strict"] offline) 1
  let mismatched := fun r => (LakeCompat.Report.rows r "findings").any fun f =>
    f.getObjValD "kind" == "toolchain_mismatch"
  check (mismatched report) report.compress
  let lib := project / ".lake/packages/lib"
  discard <| git lib #["init", "--quiet"]
  discard <| git lib #["add", "lean-toolchain", "lake-manifest.json"]
  discard <| git lib #["commit", "--quiet", "-m", "pin"]
  writeJson (project / "lake-manifest.json") (projectLock #[dependency (← git lib #["rev-parse", "HEAD"])])
  IO.FS.writeFile (lib / "lean-toolchain") "v4.20.0"
  let locked ← jsonResult (← invoke project #["--json", "--strict"]) 1
  check (mismatched locked &&
    (LakeCompat.Report.rows locked "packages")[0]!.getObjValD "toolchain" == "leanprover/lean4:v4.19.0") locked.compress

private def inputErrors : IO Unit := withProject fun project => do
  let missing ← expectError project #["--project", (project / "missing").toString] "does not exist"
  check (missing.contains "lakefile.toml" && !missing.contains "lake update") missing
  discard <| expectError project #["--project", (project / "lean-toolchain").toString] "--project expects a directory"
  discard <| expectError project #["--try-revision", "other=" ++ newRev] "other"
  for args in [#["--build-timeout", "1"], #["--suggest", "--build-timeout=600"]] do
    discard <| expectError project args "--build-timeout requires a trial"
  for args in [#["--demo"], #["--try-revision", "lib=main"], #["--unknown"], #["--project="],
      #["--build-timeout"], #["--build-timeout", "0"], #["--build-timeout=0.5"],
      #["--build-target"], #["--build-target="], #["--build-target=--help"], #["--build-target", "@dep/"],
      #["--build-target", "CoreLib:olean:extra"], #["--build-target", "./CoreLib.lean:olean:extra"],
      #["--build-target", "./CoreLib.toml"], #["--build-target", "@dep:"], #["--build-target", ":"],
      #["--build-target", ":.."], #["--build-target", "CoreLib:extra..dep"],
      #["--try-revision", "lib=" ++ oldRev, "--try-revision", "lib=" ++ newRev]] do
    discard <| expectError project args ""
  let positional ← cli #["4.36.0", "--", "--json"]
  expectCode positional 2
  check (positional.stdout.isEmpty && positional.stderr.contains "unexpected positional argument '--json'") positional.stderr
  IO.FS.removeFile (project / "lake-manifest.json")
  for config in ["lakefile.toml", "lakefile.lean"] do
    IO.FS.writeFile (project / config) ""
    let out ← invoke project #[]
    expectCode out 2
    check (out.stderr.contains "lake update" && out.stderr.contains "lake-manifest.json") out.stderr
    IO.FS.removeFile (project / config)

/-- Native stand-ins for curl and elan, launched by the CLI under test. -/
def fixture (mode : String) (args : List String) : IO UInt32 := do
  if args.head? == some "run" then
    if mode == "unavailable" then return 1
    if let ["run", _, "lake", "-d", _, "run", "inspect", output] := args then
      writeTrialOutputDirs output
      return 0
    if args.getLast? == some "build" then
      if mode == "timeout" then
        IO.println "build started"
        (← IO.getStdout).flush
        IO.sleep 5000
      else
        let lock ← readJson "lake-manifest.json"
        let pkg := (LakeCompat.Report.rows lock "packages")[0]!
        IO.println (LakeCompat.Report.string pkg "rev")
        return 1
    return 0
  if mode == "changed" then
    let some path ← IO.getEnv "LAKECOMPAT_CLI_LOCK" | throw (IO.userError "missing fixture lock")
    writeJson path (projectLock #[])
  if mode != "candidates" then
    (← IO.getStderr).putStr "404"
    return 22
  let url := args.getLast?.getD ""
  let response := if url.endsWith "/versions" then
      Json.mkObj [("data", .arr #[Json.mkObj [("revision", newRev), ("tag", "v2"),
        ("toolchain", "leanprover/lean4:v4.20.0")]])]
    else if url.endsWith "/builds" then Json.mkObj [("data", .arr #[])]
    else Json.mkObj [("name", "lib"), ("sources", .arr #[Json.mkObj [("host", "github"), ("fullName", "example/lib")]])]
  IO.println response.compress
  (← IO.getStderr).putStr "200"
  return 0

private def suggestions : IO Unit := withProject fun project => do
  let demo ← cli #["4.36.0", "--demo"]
  expectCode demo 0
  check (demo.stdout.contains "upgradeDemo -> greeting" && !demo.stdout.contains "--suggest") demo.stdout
  writeJson (project / "lake-manifest.json") (projectLock #[dependency oldRev "https://gitlab.com/example/lib"])
  let out ← invoke project #[]
  expectCode out 0
  check (!out.stdout.contains "--suggest") out.stdout
  let unsupported ← jsonResult (← invoke project #["--suggest", "--json"]) 0
  check (((unsupported.getObjValD "suggestions").getObjValD "lib").getObjValD "status" == "unsupported") unsupported.compress

private def trials : IO Unit := withProject fun project => do
  let bin := project.parent.get! / "bin"
  IO.FS.createDir bin
  for tool in ["elan", "curl"] do
    discard <| IO.Process.run { cmd := "ln", args := #["-s", (← IO.appPath).toString, (bin / tool).toString] }
  let path := bin.toString ++ ":" ++ (← IO.getEnv "PATH").getD ""
  let env := fun mode => #[("PATH", some path), ("LAKECOMPAT_CLI_FIXTURE", some mode),
    ("LAKECOMPAT_CLI_LOCK", some (project / "lake-manifest.json").toString)]
  IO.FS.writeFile (project / "lakefile.toml") ""
  for (extra, rev) in [(#[], newRev), (#["--try-revision", "lib=" ++ oldRev], oldRev)] do
    let r ← jsonResult (← invoke project (#["--try-suggested", "--json"] ++ extra) (env "candidates")) 1
    let verification := r.getObjValD "verification"
    check (r.getObjValD "auto_selected" == true && r.getObjValD "trial_revisions" == Json.mkObj [("lib", rev)] &&
      verification.getObjValD "status" == "failed" &&
      (LakeCompat.Report.string verification "output").contains rev) r.compress
  let quoted ← jsonResult (← invoke project #["--try-revision", "«lib»=" ++ oldRev, "--json"] (env "candidates")) 1
  check (quoted.getObjValD "trial_revisions" == Json.mkObj [("lib", oldRev)]) quoted.compress
  let unavailable ← invoke project #["--try-suggested"] (env "unavailable")
  expectCode unavailable 2
  check (unavailable.stdout.contains "trial uses the current lock" && unavailable.stdout.contains "Build unavailable")
    unavailable.stdout
  let strict ← jsonResult (← invoke project #["--verify", "--strict", "--json"] (env "unavailable")) 2
  check (!(LakeCompat.Report.rows strict "findings").isEmpty &&
    (strict.getObjValD "verification").getObjValD "status" == "unavailable") strict.compress
  let timed ← jsonResult (← invoke project #["--verify", "--json", "--build-timeout=1"] (env "timeout")) 2
  let failed := timed.getObjValD "verification"
  check (failed.getObjValD "status" == "inconclusive" && (LakeCompat.Report.string failed "message").contains "1-second" &&
    (LakeCompat.Report.string failed "output").contains "build started") timed.compress
  let changed ← jsonResult (← invoke project #["--try-suggested", "--json"] (env "changed")) 2
  let verification := changed.getObjValD "verification"
  check ((LakeCompat.Report.rows changed "packages")[0]!.getObjValD "revision" == oldRev &&
    verification.getObjValD "status" == "inconclusive" &&
    (LakeCompat.Report.string verification "message").contains "manifest changed") changed.compress

private def namedTargets : IO Unit := withTempDir fun project => do
  let dep := project / "dep"
  IO.FS.createDir dep
  IO.FS.writeFile (project / "lakefile.toml") <|
    "name = \"app\"\n[[lean_lib]]\nname = \"CoreLib\"\n[[require]]\nname = \"dep\"\npath = \"dep\"\n"
  IO.FS.writeFile (dep / "lakefile.toml") "name = \"dep\"\ndefaultTargets = [\"Extra\"]\n[[lean_lib]]\nname = \"Extra\"\n"
  writeJson (dep / "lake-manifest.json") (projectLock #[] "dep")
  -- Lake ignores a leftover Git subdirectory on a path entry.
  let entry := Json.mkObj [("name", "dep"), ("type", "path"), ("dir", "dep"),
    ("configFile", "lakefile.toml"), ("subDir", "../unused"), ("inherited", false)]
  writeJson (project / "lake-manifest.json") (projectLock #[entry])
  let toolchain := (include_str "../lean-toolchain").trimAscii.copy
  IO.FS.writeFile (project / "lean-toolchain") toolchain
  let targets := #["CoreLib.lean:olean", "dep/Extra.lean:olean", "CoreLib/Sub.lean:olean", ":extraDep", "@dep:extraDep"]
  let build := fun (specs : Array String) =>
    #[toolchain, "--project", project.toString] ++ specs.map ("--build-target=" ++ ·)
  let args := build targets
  IO.FS.createDir (project / "CoreLib")
  let files := [project / "CoreLib.lean", dep / "Extra.lean", project / "CoreLib/Sub.lean"]
  for file in files do IO.FS.writeFile file "example : Nat := 1\n"
  for file in files do
    IO.FS.writeFile file "example : Nat := false\n"
    let failed ← cli args
    expectCode failed 1
    check (failed.stdout.contains "CoreLib.lean:olean, dep/Extra.lean:olean" && failed.stdout.contains file.fileName.get!)
      failed.stdout
    IO.FS.writeFile file "example : Nat := 1\n"
  let passed ← jsonResult (← cli (args.push "--json")) 0
  check (passed.getObjValD "build_targets" == toJson targets &&
    (passed.getObjValD "verification").getObjValD "status" == "passed") passed.compress
  let defaults ← jsonResult (← cli ((build #["@«dep»"]).push "--json")) 0
  check (defaults.getObjValD "build_targets" == toJson ["@«dep»"]) defaults.compress
  IO.FS.writeFile (dep / "Extra.lean") "example : Nat := false\n"
  let broken ← cli (build #["@dep"])
  expectCode broken 1
  check (broken.stdout.contains "Extra.lean") broken.stdout
  IO.FS.writeFile (dep / "Extra.lean") "example : Nat := 1\n"
  IO.FS.writeFile (dep / "lean-toolchain") "leanprover/lean4:v4.34.1\n"
  let named := build #["CoreLib", "@dep/+Extra:olean", "./CoreLib/Sub.lean:olean"]
  let strict ← jsonResult (← cli (named ++ #["--strict", "--json"])) 1
  check ((strict.getObjValD "verification").getObjValD "status" == "passed" &&
    (LakeCompat.Report.rows strict "findings").any (fun f => f.getObjValD "kind" == "toolchain_mismatch")) strict.compress
  -- A filename that is only a declaration in a dependency still needs a package qualifier.
  IO.FS.writeFile (dep / "lakefile.toml")
    "name = \"dep\"\n[[lean_lib]]\nname = \"OnlyHere.lean\"\n"
  IO.FS.createDir (dep / "OnlyHere")
  IO.FS.writeFile (dep / "OnlyHere/lean.lean") "example : True := trivial\n"
  expectCode (← cli (build #["@dep/OnlyHere.lean"])) 0
  let missingFile ← cli (build #["dep/OnlyHere.lean"])
  expectCode missingFile 1
  check (missingFile.stdout.contains "dep/OnlyHere.lean") missingFile.stdout
  let noDefaults ← jsonResult (← cli ((build #["CoreLib", "@dep"]).push "--json")) 2
  let checkResult := noDefaults.getObjValD "verification"
  check (checkResult.getObjValD "status" == "inconclusive" &&
    (LakeCompat.Report.string checkResult "message").contains "dep has no default build targets") noDefaults.compress
  let absent ← cli (build #["OnlyHere.lean"])
  expectCode absent 1
  check (absent.stdout.contains "OnlyHere.lean" && absent.stdout.contains "package 'app'") absent.stdout
  IO.FS.writeFile (dep / "lakefile.toml") "name = \"dep\"\nextraDepTargets = [\"MissingInput\"]\n"
  let facet ← cli (build #["@dep:extraDep"])
  expectCode facet 1
  check (facet.stdout.contains "MissingInput") facet.stdout
  check (!(← (project / ".lake").pathExists)) "named-target build wrote to the source project"

private def testDriver : IO Unit := withTempDir fun project => do
  let toolchain := (include_str "../lean-toolchain").trimAscii.copy
  let args := #[toolchain, "--project", project.toString, "--test", "--json"]
  let lock := projectLock #[]
  writeJson (project / "lake-manifest.json") lock
  IO.FS.writeFile (project / "lean-toolchain") toolchain
  let config := project / "lakefile.toml"
  IO.FS.writeFile config "name = \"app\"\n"
  let missing ← jsonResult (← cli args) 2
  let result := missing.getObjValD "verification"
  check (result.getObjValD "status" == "inconclusive" && result.getObjValD "test_exit_code" == .null &&
    (LakeCompat.Report.string result "message").contains "test driver") missing.compress
  IO.FS.writeFile config <| "name = \"app\"\ntestDriver = \"tests\"\ntestDriverArgs = [\"configured\"]\n" ++
    "[[lean_exe]]\nname = \"tests\"\nroot = \"Tests\"\n"
  IO.FS.writeFile (project / "Tests.lean") <|
    "def main (args : List String) : IO UInt32 := do\n  let mode ← IO.FS.readFile \"mode\"\n" ++
    "  IO.println (\"driver \" ++ mode)\n  IO.println (reprStr args)\n" ++
    "  if mode == \"change-lock\" then IO.FS.writeFile \"lake-manifest.json\" \"{}\"\n" ++
    "  return if mode == \"fail\" then 7 else 0\n"
  for (mode, code, status, testCode) in [("pass", 0, "passed", 0), ("fail", 1, "failed", 7),
                                          ("change-lock", 2, "inconclusive", 0)] do
    IO.FS.writeFile (project / "mode") mode
    let driverArgs := if mode == "pass" then #["--filter", "case one", "", "--help", "--strict", "--"] else #[]
    let trialArgs := if !driverArgs.isEmpty then args.push "--" ++ driverArgs
                     else if mode == "fail" then args ++ #["--build-target", "./Tests.lean:olean"] else args
    let report ← jsonResult (← cli trialArgs) code.toUInt32
    let result := report.getObjValD "verification"
    check (report.getObjValD "test_requested" == true && result.getObjValD "status" == .str status &&
      result.getObjValD "test_exit_code" == toJson testCode &&
      (LakeCompat.Report.string result "output").contains ("driver " ++ mode) &&
      (LakeCompat.Report.string result "output").contains (reprStr ("configured" :: driverArgs.toList))) report.compress
    let recordedArgs := if driverArgs.isEmpty then Json.null else toJson driverArgs
    check (report.getObjValD "test_args" == recordedArgs) report.compress
  let buildFailure ← jsonResult (← cli (args ++ #["--build-target", "Missing"])) 1
  let result := buildFailure.getObjValD "verification"
  check (result.getObjValD "status" == "failed" && result.getObjValD "test_exit_code" == .null) result.compress
  check ((← readJson (project / "lake-manifest.json")) == lock && !(← (project / ".lake").pathExists))
    "test-driver trial wrote to the source project"

def run : IO Unit := do
  let version := (include_str "../VERSION").trimAscii.copy
  let output ← cli #["--version"] #[("PATH", some "")]
  expectCode output 0
  check (output.stdout == s!"lake-compat {version}\n" && output.stderr.isEmpty) output.stdout
  scan
  inputErrors
  suggestions
  unless Platform.isWindows do
    trials
    namedTargets
    testDriver

end LeanTests.Cli
