import LeanTests.Support
import LakeCompat.Report

open Lean System

namespace LeanTests.Graph
open LeanTests.Support

def target := "leanprover/lean4:v4.25.0"
def older := "leanprover/lean4:v4.24.0"
def packagesDir (project : FilePath) := project / ".lake" / "packages"

abbrev Fields := List (String × Json)

def gitDep (name rev : String) (inherited := false) (fields : Fields := []) : Json :=
  Json.mkObj (([("type", "git"), ("name", name), ("url", s!"https://github.com/example/{name}.git"),
    ("rev", rev), ("inherited", inherited)] : Fields) ++ fields)

def localDep (name dir : String) (inherited := false) (fields : Fields := []) : Json :=
  Json.mkObj (([("type", "path"), ("name", name), ("dir", dir), ("inherited", inherited)] : Fields) ++ fields)

def manifest (dir : FilePath) (packages : Array Json := #[]) (fields : Fields := [])
    (file := "lake-manifest.json") : IO Unit := do
  IO.FS.createDirAll dir
  let obj := Json.mkObj (([("version", "1.3.0"), ("name", dir.fileName.getD "demo"),
    ("packages", .arr packages)] : Fields) ++ fields)
  writeJson (dir / file) obj

def pin (dir : FilePath) (value := target) : IO Unit := do
  IO.FS.createDirAll dir
  IO.FS.writeFile (dir / "lean-toolchain") (value ++ "\n")

def runCase (name : String) (test : FilePath → IO Unit) : IO Unit := do
  try
    withTempDir fun temp => do
      let project := temp / "demo"
      pin project "4.24.0"
      test project
  catch e => throw <| IO.userError s!"graph/{name}: {e}"

def commitMetadata (repo : FilePath) : IO String := do
  discard <| git repo #["init", "-q"]
  discard <| git repo #["add", "."]
  discard <| git repo #["commit", "-qm", "package metadata"]
  git repo #["rev-parse", "HEAD"]

def link (path target : FilePath) : IO Unit := do
  if Platform.isWindows then
    discard <| IO.Process.run {
      cmd := "powershell.exe",
      args := #["-NoProfile", "-NonInteractive", "-Command",
        "$ErrorActionPreference = 'Stop'; New-Item -ItemType SymbolicLink -Path $env:TEST_LINK -Target $env:TEST_TARGET"],
      env := #[("TEST_LINK", some path.toString), ("TEST_TARGET", some target.toString)]}
  else
    discard <| IO.Process.run {cmd := "ln", args := #["-s", target.toString, path.toString]}

def reportArgs (project : FilePath) := #[target, "--project", project.toString, "--json"]

def report (project : FilePath) (code : UInt32 := 0) : IO Json := do
  let output ← cli (reportArgs project)
  check (output.exitCode == code) s!"exit {output.exitCode}: {output.stdout}{output.stderr}"
  IO.ofExcept (Json.parse output.stdout)

def findings (report : Json) := LakeCompat.Report.rows report "findings"
def packages (report : Json) := LakeCompat.Report.rows report "packages"
def str (row : Json) (key : String) := LakeCompat.Report.string row key

def pkg (report : Json) (name : String) : IO Json := do
  let some p := (packages report).find? (fun p => str p "name" == name)
    | throw <| IO.userError s!"missing package {name}: {report.compress}"
  return p

def expect [BEq α] [ToJson α] (actual expected : α) : IO Unit :=
  unless actual == expected do
    throw <| IO.userError s!"expected {(toJson expected).compress}, got {(toJson actual).compress}"

def expectField (row : Json) (field : String) (expected : Json) : IO Unit := do
  unless (row.getObjVal? field).toOption == some expected do
    throw <| IO.userError s!"expected {field} = {expected.compress}, got {row.compress}"

def expectKinds (report : Json) (kinds : Array String) : IO Unit :=
  expect ((findings report).map (str · "kind")) kinds

def onlyFinding (report : Json) : IO Json := do
  let #[f] := findings report | throw <| IO.userError s!"expected one finding: {report.compress}"
  return f

def chain (names : List String) := toJson names

end LeanTests.Graph
