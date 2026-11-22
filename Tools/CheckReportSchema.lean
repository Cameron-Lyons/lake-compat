import LeanTests.JsonSchema
import LeanTests.Support
import LakeCompat.Report
import Tools.Process

namespace Tools.CheckReportSchema
open Lean System LeanTests.Support

private def target := (include_str "../lean-toolchain").trimAscii.copy

private def checkReport (rule : LeanTests.JsonSchema.Rule) (cli : FilePath) (args : Array String)
    (code : UInt32 := 0) (status : String := "")
    (env : Array (String × Option String) := #[]) : IO Json := do
  let result ← Tools.Process.run {cmd := cli.toString, args := #["--json"] ++ args, env} 90000
  check (result.exitCode == code)
    s!"{args}: expected exit {code}, got {result.exitCode}\n{result.stdout}{result.stderr}"
  let report ← IO.ofExcept (Json.parse result.stdout)
  IO.ofExcept <| (LeanTests.JsonSchema.validate rule report).mapError fun message => s!"{args}: {message}"
  unless status.isEmpty do
    check ((report.getObjValD "verification").getObjValD "status" == .str status)
      s!"{args}: expected verification status {status}, got {report.compress}"
  return report

private def checkFixtures (rule : LeanTests.JsonSchema.Rule) (cli : FilePath) : IO Unit := withTempDir fun work => do
  let project := work / "project"
  IO.FS.createDir project
  IO.FS.writeFile (project / "lakefile.toml")
    "name = \"app\"\ntestDriver = \"Checks\"\n[[lean_lib]]\nname = \"Checks\"\n"
  IO.FS.writeFile (project / "Checks.lean") "example : 1 + 1 = 2 := rfl\n"
  writeJson (project / "lake-manifest.json") (Json.mkObj [("name", "app"), ("version", "1.2.0")])
  let args := #[target, "--project", project.toString]
  let tested ← checkReport rule cli (args.push "--test") (status := "passed")
  check (tested.getObjValD "test_requested" == true &&
    (tested.getObjValD "verification").getObjValD "test_exit_code" == 0)
    "The trial must report the successful Lake test driver"
  -- Library test drivers reject arguments; the report must still record their invocation.
  let driverArgs := #["--filter", "case one"]
  let failed ← checkReport rule cli (args ++ #["--test", "--"] ++ driverArgs) 1 "failed"
  let testCode := ((failed.getObjValD "verification").getObjValD "test_exit_code").getNat?.toOption
  check (failed.getObjValD "test_args" == toJson driverArgs && testCode.any (· != 0))
    "The trial must report arguments and the failing Lake test exit code"

  IO.FS.writeFile (project / "lakefile.toml") "name = \"app\"\n"
  let dependency := Json.mkObj [("name", "lib"), ("type", "git"),
    ("url", "https://github.com/example/lib"), ("rev", .str (String.ofList (List.replicate 40 '1'))),
    ("inherited", false)]
  let manifest := Json.mkObj [("name", "app"), ("version", "1.2.0"),
    ("packagesDir", ".lake/packages"), ("packages", .arr #[dependency])]
  writeJson (project / "lake-manifest.json") manifest
  -- Reuse the native test driver's deterministic Reservoir and Lake subprocess fixtures.
  let fixture ← IO.FS.realPath (← binary "lake-compat-tests")
  for name in ["curl", "elan"] do
    let linked ← Tools.Process.run {cmd := "ln", args := #["-s", fixture.toString, (work / name).toString]}
    check (linked.exitCode == 0) s!"Cannot create the {name} fixture: {linked.stderr}"
  let searchPath := work.toString ++ ":" ++ (← IO.getEnv "PATH").getD "/usr/bin:/bin"
  let env := #[("PATH", some searchPath), ("LAKECOMPAT_CLI_FIXTURE", some "candidates")]
  let args := #["4.20.0", "--project", project.toString]
  let trial ← checkReport rule cli (args.push "--try-suggested") 1 "failed" env
  let suggestion := (trial.getObjValD "suggestions").getObjValD "lib"
  let candidates := LakeCompat.Report.rows suggestion "candidates"
  check (suggestion.getObjValD "status" == "found" && !candidates.isEmpty)
    "The automatic trial must exercise a nonempty candidate report"
  let revision := candidates[0]!.getObjValD "revision"
  check (trial.getObjValD "auto_selected" == true &&
    trial.getObjValD "trial_revisions" == Json.mkObj [("lib", revision)])
    "The automatic trial must report its selected revision"
  IO.FS.createDirAll (project / ".lake")
  writeJson (project / ".lake/package-overrides.json")
    (Json.mkObj [("schemaVersion", "1.3.0"), ("packages", .arr #[dependency])])
  discard <| checkReport rule cli (args.push "--verify") 2 "inconclusive"

def run (cli : FilePath) : IO Unit := do
  if Platform.isWindows then throw <| IO.userError "Report integration checks require a POSIX host."
  LeanTests.JsonSchema.runTests
  let rule ← IO.ofExcept LeanTests.JsonSchema.reportRule
  let demo := #[target, "--demo"]
  discard <| checkReport rule cli demo
  let strict ← checkReport rule cli (demo.push "--strict") 1
  check (!(LakeCompat.Report.rows strict "packages").isEmpty && !(LakeCompat.Report.rows strict "findings").isEmpty)
    "The strict demo must exercise package and finding schemas"
  let invalid ← checkReport rule cli #[] 2
  check ((invalid.getObjVal? "error").isOk) "The missing target must produce an error report"
  let passed ← checkReport rule cli (demo ++ #["--build-target", "upgradeDemo"]) (status := "passed")
  check (passed.getObjValD "build_targets" == toJson #["upgradeDemo"])
    "The passing trial must record its named build target"
  discard <| checkReport rule cli (demo.push "--verify") 2 "unavailable" #[("PATH", some "")]
  checkFixtures rule cli
  IO.println "CLI reports validated against schema/report-v1.json."

end Tools.CheckReportSchema

def main (args : List String) : IO UInt32 := do
  let usage := "Usage: lake exe check-report-schema [CLI-PATH]"
  if args == ["--help"] || args == ["-h"] then
    IO.println usage
    return 0
  unless args.length <= 1 do
    IO.eprintln usage
    return 2
  try
    let cli ← match args with
      | [] => LeanTests.Support.binary "lake-compat"
      | [path] => pure (System.FilePath.mk path)
      | _ => throw <| IO.userError usage
    Tools.CheckReportSchema.run (← IO.FS.realPath cli)
    return 0
  catch e =>
    IO.eprintln s!"check-report-schema: {e}"
    return 2
