import Lean.Data.Json.Parser
import Lean.Data.Json.Printer
import Lean.Data.Json.FromToJson.Basic
import Init.System.Uri
import Std.Internal.UV.System
import Tools.Process

open Lean System

namespace Tools.BenchmarkScans

private def target := (include_str "../lean-toolchain").trimAscii.copy

private def usage := "Usage: lake exe benchmark-scans [EXECUTABLE ...] [OPTIONS]
Measure offline scans of a shared Git dependency graph, excluding fixture setup.

  --parents N   Number of local packages (default 128)
  --children N  Number of shared Git dependencies (default 128)
  --runs N      Measured scans per executable, after warming up (default 5)
  --            Treat remaining arguments as executable paths
  -h, --help    Show this help

Without EXECUTABLE, use the lake-compat binary beside this tool.
Every executable must produce the same complete report without findings."

private structure Options where
  parents : Nat := 128
  children : Nat := 128
  runs : Nat := 5
  executables : Array FilePath := #[]

private def setOption (opts : Options) (name value : String) : Except String Options := do
  let some number := value.toNat?.filter (· > 0)
    | throw s!"{name} expects a positive integer"
  match name with
  | "--parents" => return {opts with parents := number}
  | "--children" => return {opts with children := number}
  | "--runs" => return {opts with runs := number}
  | _ => throw s!"unknown option '{name}'"

private def parseOptions (args : List String) : Except String Options := do
  let mut opts : Options := {}
  let mut positional := false
  let mut pending : Option String := none
  for arg in args do
    if let some name := pending then
      opts ← setOption opts name arg
      pending := none
    else if positional then
      opts := {opts with executables := opts.executables.push arg}
    else if arg == "--" then positional := true
    else if ["--parents", "--children", "--runs"].contains arg then pending := some arg
    else if arg.startsWith "-" then
      let some separator := arg.find? '=' | throw s!"unknown option '{arg}'"
      opts ← setOption opts (arg.sliceTo separator).copy (arg.sliceFrom separator.next!).copy
    else opts := {opts with executables := opts.executables.push arg}
  if let some name := pending then throw s!"{name} requires a value"
  return opts

private abbrev Environment := Array (String × Option String)

private def cleanGitEnvironment : IO Environment := do
  let vars ← Std.Internal.UV.System.osEnviron
  let removed := vars.filterMap fun (key, _) =>
    if key.startsWith "GIT_" then some (key, none) else none
  return removed ++ #[("GIT_CONFIG_NOSYSTEM", some "1"), ("GIT_CONFIG_GLOBAL", some "/dev/null")]

private def manifest (path : FilePath) (name : String) (packages : Array Json) : IO Unit := do
  IO.FS.createDirAll path
  let value := Json.mkObj [("version", "1.3.0"), ("name", name), ("packages", .arr packages)]
  IO.FS.writeFile (path / "lake-manifest.json") value.compress

private def fixture (root : FilePath) (opts : Options) (env : Environment) : IO FilePath := do
  let project := root / "project"
  let remote := root / "shared repo"
  for index in [:opts.children] do
    manifest (remote / s!"lib{index}") s!"lib{index}" #[]
  IO.FS.writeFile (remote / "lean-toolchain") (target ++ "\n")
  let git := fun args => do
    let output ← Tools.Process.run {cmd := "git", cwd := remote, env, args :=
      #["-c", "user.name=Benchmark", "-c", "user.email=benchmark@example.invalid",
        "-c", "commit.gpgSign=false", "-c", "core.hooksPath=/dev/null"] ++ args}
    unless output.exitCode == 0 do throw <| IO.userError s!"Git fixture setup failed: {output.stderr}"
    return output.stdout.trimAscii.copy
  discard <| git #["init", "-q", "--object-format=sha1"]
  discard <| git #["add", "."]
  discard <| git #["commit", "-qm", "dependency fixture"]
  let revision ← git #["rev-parse", "HEAD"]
  let dependencies := (Array.range opts.children).map fun index => Json.mkObj [
    ("name", s!"lib{index}"), ("type", "git"), ("url", remote.toString), ("rev", revision),
    ("subDir", s!"lib{index}"), ("inherited", true)]
  let requested := dependencies.map fun dep =>
    (dep.setObjVal! "url" (Uri.pathToUri remote)).setObjVal! "inherited" false
  let mut parents := #[]
  for index in [:opts.parents] do
    let name := s!"parent{index}"
    manifest (project / name) name requested
    parents := parents.push (Json.mkObj [("name", name), ("type", "path"), ("dir", name), ("inherited", false)])
  manifest project "benchmark" (parents ++ dependencies)
  IO.FS.writeFile (project / "lean-toolchain") (target ++ "\n")
  let cache := project / ".lake/packages"
  IO.FS.createDirAll cache
  for index in [:opts.children] do
    let output ← Tools.Process.run {cmd := "ln", args := #["-s", remote.toString, (cache / s!"lib{index}").toString]}
    unless output.exitCode == 0 do throw <| IO.userError s!"Cannot link fixture checkout: {output.stderr}"
  return project

private def scan (executable project : FilePath) (env : Environment) : IO (Json × Nat) := do
  let (output, elapsed) ← Tools.Process.timedRun {
    cmd := executable.toString, args := #[target, "--project", project.toString, "--json", "--strict"], env}
  unless output.exitCode == 0 do
    throw <| IO.userError s!"{executable}: scan exited {output.exitCode}\n{output.stderr}\n{output.stdout}"
  let report ← IO.ofExcept <| Json.parse output.stdout
  return (report, elapsed)

private def seconds (nanos : Nat) : Json := .num ⟨Int.ofNat nanos, 9⟩

private def run (opts : Options) : IO Unit := do
  if Platform.isWindows then throw <| IO.userError "The scan benchmark requires a POSIX host."
  let paths ← if opts.executables.isEmpty then pure #[(← IO.appDir) / "lake-compat"] else pure opts.executables
  let executables ← paths.mapM IO.FS.realPath
  let env ← cleanGitEnvironment
  IO.FS.withTempDir fun temp => do
    let root ← IO.FS.realPath temp
    let project ← fixture root opts env
    let mut samples : Array (Array Nat) := Array.replicate executables.size #[]
    let mut reference : Option Json := none
    -- Warm every executable, then interleave measured runs to reduce ordering bias.
    for iteration in [:opts.runs + 1] do
      for index in [:executables.size] do
        let executable := executables[index]!
        let (report, elapsed) ← scan executable project env
        let packages ← IO.ofExcept ((report.getObjValD "packages").getArr?)
        let findings ← IO.ofExcept ((report.getObjValD "findings").getArr?)
        unless findings.isEmpty && packages.size == opts.parents + opts.children do
          throw <| IO.userError s!"{executable}: benchmark must scan the whole graph without findings"
        if let some expected := reference then
          unless report == expected do throw <| IO.userError "Benchmark executables must produce identical reports"
        else reference := some report
        if iteration > 0 then samples := samples.modify index (·.push elapsed)
    let results := (executables.zip samples).map fun (executable, times) => Id.run do
      let ordered := times.qsort (· < ·)
      let middle := ordered.size / 2
      let median := if ordered.size % 2 == 0 then
          (ordered[middle - 1]! + ordered[middle]!) / 2 else ordered[middle]!
      return Json.mkObj [("executable", executable.toString),
        ("median_seconds", seconds median), ("min_seconds", seconds ordered[0]!)]
    IO.println (Json.mkObj [("packages", toJson (opts.parents + opts.children)),
      ("dependency_edges", toJson (opts.parents * opts.children)), ("runs", toJson opts.runs),
      ("results", .arr results)]).pretty

def main (args : List String) : IO UInt32 := do
  if (args.takeWhile (· != "--")).any (fun arg => arg == "--help" || arg == "-h") then
    IO.println usage
    return 0
  try
    run (← IO.ofExcept (parseOptions args))
    return 0
  catch e =>
    IO.eprintln s!"benchmark-scans: {e}"
    return 2

end Tools.BenchmarkScans

def main := Tools.BenchmarkScans.main
