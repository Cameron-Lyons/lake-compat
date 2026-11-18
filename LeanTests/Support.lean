import Lean.Data.Json.Parser
import Lean.Data.Json.Printer

namespace LeanTests.Support
open Lean System

def check (condition : Bool) (message : String) : IO Unit :=
  unless condition do throw (IO.userError message)

def withTempDir (f : FilePath → IO α) : IO α :=
  IO.FS.withTempDir fun dir => do f (← IO.FS.realPath dir)

/-- Lake builds the CLI and fixture executables beside the test driver. -/
def binary (name : String) : IO FilePath := do
  return (← IO.appDir) / (if Platform.isWindows then name ++ ".exe" else name)

def cli (args : Array String) (env : Array (String × Option String) := #[])
    (cwd : Option FilePath := none) : IO IO.Process.Output := do
  IO.Process.output { cmd := (← binary "lake-compat").toString, args, env, cwd }

def git (repo : FilePath) (args : Array String) : IO String := do
  let output ← IO.Process.run { cmd := "git", cwd := repo, args :=
    #["-c", "user.name=Fixture", "-c", "user.email=fixture@example.invalid",
      "-c", "commit.gpgsign=false", "-c", "core.hooksPath=/dev/null"] ++ args }
  return output.trimAscii.copy

def writeJson (path : FilePath) (value : Json) : IO Unit :=
  IO.FS.writeFile path (value.compress ++ "\n")

def readJson (path : FilePath) : IO Json := do
  IO.ofExcept (Json.parse (← IO.FS.readFile path))

end LeanTests.Support
