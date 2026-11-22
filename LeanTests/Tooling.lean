import Tools.Process
import LeanTests.Support
import LakeCompat.Report

namespace LeanTests.Tooling
open Lean System LeanTests.Support

/-- Native subprocess fixtures for full-output collection and process-group deadlines. -/
def fixture (args : List String) : IO UInt32 := do
  match args with
  | ["output"] =>
    (← IO.getStdout).putStr (String.ofList (List.replicate 200000 'x'))
    (← IO.getStderr).putStr (String.ofList (List.replicate 200000 'y'))
    return 7
  | ["linger", marker] =>
    discard <| IO.Process.spawn {
      cmd := (← IO.appPath).toString
      args := #["process-fixture", "marker", marker] }
    return 0
  | ["marker", marker] =>
    IO.sleep 600
    IO.FS.writeFile marker "escaped timeout"
    return 0
  | _ => throw <| IO.userError "invalid process fixture arguments"

private def processes : IO Unit := withTempDir fun root => do
  let command : IO.Process.SpawnArgs := {
    cmd := (← IO.appPath).toString, cwd := root
    args := #["process-fixture", "output"] }
  let (output, elapsed) ← Tools.Process.timedRun command 5000
  check (output.exitCode == 7 && elapsed > 0) "process result lost exit code or duration"
  check (output.stdout == String.ofList (List.replicate 200000 'x')) "truncated or combined stdout"
  check (output.stderr == String.ofList (List.replicate 200000 'y')) "truncated or combined stderr"
  let oversized ← (Tools.Process.run command 5000 1024).toBaseIO
  match oversized with
  | .ok _ => throw <| IO.userError "accepted output over the byte limit"
  | .error e => check (e.toString.contains "exceeded 1024 bytes") e.toString
  if Platform.isWindows then return
  let marker := root / "marker"
  let timedOut ← (Tools.Process.run {command with
    args := #["process-fixture", "linger", marker.toString]} 100).toBaseIO
  match timedOut with
  | .ok _ => throw <| IO.userError "a descendant retained output pipes beyond the deadline"
  | .error e => check (e.toString.contains "100-millisecond timeout") e.toString
  IO.sleep 700
  check (!(← marker.pathExists)) "timed-out descendant escaped process-group cleanup"

private def benchmark : IO Unit := withTempDir fun root => do
  if Platform.isWindows then return
  let exe ← binary "benchmark-scans"
  let invoke := fun args => Tools.Process.run {cmd := exe.toString, args, cwd := root}
  let output ← invoke #["--parents", "2", "--children", "3", "--runs", "2"]
  check (output.exitCode == 0) s!"benchmark failed: {output.stderr}"
  let report ← IO.ofExcept (Json.parse output.stdout)
  for (key, expected) in [("packages", 5), ("dependency_edges", 6), ("runs", 2)] do
    check (report.getObjValD key == toJson expected) s!"incorrect benchmark {key}: {report.compress}"
  let rows := LakeCompat.Report.rows report "results"
  check (rows.size == 1) "benchmark did not measure its sibling CLI"
  check (LakeCompat.Report.string rows[0]! "executable" == (← binary "lake-compat").toString)
    "benchmark depends on the caller's working directory"
  for args in [#["--parents", "0"], #["--runs", "0"], #["--children", "nope"],
      #["--unknown"], #["--runs"]] do
    let invalid ← invoke args
    check (invalid.exitCode == 2 && !invalid.stderr.isEmpty) s!"invalid benchmark arguments accepted: {args}"
  check ((← invoke #["--help"]).exitCode == 0) "benchmark help failed"

def run : IO Unit := do
  processes
  benchmark

end LeanTests.Tooling
