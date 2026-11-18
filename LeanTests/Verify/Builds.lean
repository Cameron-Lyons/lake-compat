import LeanTests.Verify.Fixture

namespace LeanTests.Verify
open Lean System LeanTests.Support

private def appConfig := "name = \"app\"\ndefaultTargets = [\"Main\"]\n"
private def mainLibrary := "[[lean_lib]]\nname = \"Main\"\n"

private def implicitEmpty (target : String) : IO Unit := withProject fun project => do
  writeJson (project / "lake-manifest.json") (Json.mkObj [("name", "app"), ("version", "1.1.0")])
  write (project / "lakefile.toml") (appConfig ++ mainLibrary)
  let result ← trial project (target := target)
  expect result "passed"
  check (result.getObjValD "checked_revisions" == Json.mkObj []) result.compress

private def customCache (target : String) : IO Unit := withProject fun project => do
  let root := project.parent.get!
  let repo := root / "greeting"
  write (repo / "base/lakefile.toml") "name = \"base\"\n[[lean_lib]]\nname = \"Base\"\n"
  write (repo / "base/Base.lean") "def baseGreeting := \"hello\"\n"
  write (repo / "lakefile.toml") ("name = \"greeting\"\n[[lean_lib]]\nname = \"Greeting\"\n" ++
    "[[require]]\nname = \"base\"\npath = \"base\"\n")
  write (repo / "Greeting.lean") "import Base\ndef greeting := baseGreeting\n"
  discard <| git repo #["init", "-q"]
  discard <| git repo #["add", "."]
  discard <| git repo #["commit", "-qm", "greeting"]
  let revision ← git repo #["rev-parse", "HEAD"]
  write (project / "vendor/local/lakefile.toml") "name = \"local\"\n[[lean_lib]]\nname = \"Local\"\n"
  write (project / "vendor/local/Local.lean") "def localGreeting := \"local\"\n"
  write (project / "vendor/message.txt") "source data"
  link "vendor" (project / "vendor-link")
  write (project / "lakefile.toml") (appConfig ++ "packagesDir = \"vendor\"\n" ++
    s!"[[require]]\nname = \"greeting\"\ngit = \"../greeting\"\nrev = \"{revision}\"\n" ++
    "[[require]]\nname = \"local\"\npath = \"vendor/local\"\n" ++ mainLibrary)
  write (project / "Main.lean") ("import Greeting\nimport Local\nexample : String := greeting ++ localGreeting\n" ++
    "example : (include_str \"vendor/message.txt\") = \"source data\" := rfl\n")
  let lock := project / "lake-manifest.json"
  let entries := #[gitEntry "greeting" "../greeting" revision,
    pathEntry "base" "vendor-link/greeting/base" (inherited := true), pathEntry "local" "vendor/local"]
  writeJson lock ((manifest entries).setObjVal! "packagesDir" "vendor")
  let original ← IO.FS.readBinFile lock
  let checkout := project / "vendor/greeting"
  for existing in [false, true] do
    if existing then
      discard <| git project #["clone", "-q", repo.toString, checkout.toString]
      write (checkout / "Greeting.lean") "local edits\n"
      link root.toString (checkout / "scratch")
    let result ← trial project (target := target)
    expect result "passed"
    check (result.getObjValD "checked_revisions" == Json.mkObj [("greeting", .str revision)]) result.compress
    check ((← IO.FS.readBinFile lock) == original) "original relative Git URL or lock changed"
    if existing then
      check ((← IO.FS.readFile (checkout / "Greeting.lean")) == "local edits\n") "changed working checkout"
    else
      check (!(← checkout.pathExists)) "created a checkout in the user's package cache"
  -- Keep the lock spelling while the checkout and cache use another case on disk.
  if ← (project / "VENDOR/GREETING").pathExists then
    IO.FS.rename checkout (project / "vendor/Greeting")
    IO.FS.rename (project / "vendor") (project / "Vendor")
    let alternate := entries.set! 1 (pathEntry "base" "VENDOR/GREETING/base" (inherited := true))
    writeJson lock ((manifest alternate).setObjVal! "packagesDir" "vendor")
    expect (← trial project (target := target)) "passed"
    check ((← IO.FS.readFile (checkout / "Greeting.lean")) == "local edits\n") "changed mixed-case checkout"

private def copiedCache (target : String) : IO Unit := withProject fun project => do
  let source := project / "source"
  write (source / "lakefile.toml") ("name = \"copied\"\n[[require]]\nname = \"nested\"\npath = \"nested\"\n" ++
    "[[lean_lib]]\nname = \"Copied\"\n")
  write (source / "Copied.lean") "import Nested\ndef message := nestedMessage\n"
  write (source / "nested/lakefile.toml") "name = \"nested\"\n[[lean_lib]]\nname = \"Nested\"\n"
  write (source / "nested/Nested.lean") "def nestedMessage := \"current\"\n"
  let cached := project / "vendor/copied"
  IO.FS.createDirAll (project / "vendor")
  discard <| IO.Process.run {cmd := "cp", args := #["-R", source.toString, cached.toString]}
  write (cached / "nested/Nested.lean") "def nestedMessage := \"stale\"\n"
  write (project / "lakefile.toml") (appConfig ++ "packagesDir = \"vendor\"\n" ++
    "[[require]]\nname = \"copied\"\npath = \"source\"\ncopy = true\n" ++ mainLibrary)
  let lock := manifest #[pathEntry "copied" "source" (copy := true), pathEntry "nested" "vendor/copied/nested" (inherited := true)]
  writeJson (project / "lake-manifest.json") (lock.setObjVal! "packagesDir" "vendor")
  write (project / "Main.lean") "import Copied\nexample : message = \"current\" := rfl\n"
  expect (← trial project (target := target)) "passed"
  let checkCache := do
    check ((← IO.FS.readFile (cached / "nested/Nested.lean")).contains "stale") "modified original copied package"
  checkCache
  writeJson (project / "lake-manifest.json") lock
  write (project / "Main.lean") "import Copied\nexample : message = \"stale\" := rfl\n"
  let result ← trial project (target := target)
  expect result "failed"
  check ((field result "output").contains "Main.lean") result.compress
  checkCache

private def implicitExternalCache (target : String) : IO Unit := withProject fun project => do
  let external := project.parent.get! / "external packages"
  write (project / "source/lakefile.toml") "name = \"dep\"\n"
  writeJson (project / "lake-manifest.json") (manifest #[pathEntry "dep" "source" (copy := true)])
  IO.FS.removeFile (project / "lakefile.toml")
  let directory := (Json.str external.toString).compress
  let toml := s!"name = \"app\"\npackagesDir = {directory}\n" ++
    "[[require]]\nname = \"dep\"\npath = \"source\"\ncopy = true\n"
  let lean := s!"import Lake\nopen Lake DSL\npackage app where\n  packagesDir := {directory}\nrequire dep from \"source\"\n"
  for (file, contents) in [("lakefile.toml", toml), ("lakefile.lean", lean)] do
    write (project / file) contents
    expect (← trial project (target := target)) "inconclusive" "project-relative"
    check (!(← external.pathExists)) "materialized an external cache"
    IO.FS.removeFile (project / file)

def realBuilds : IO Unit := do
  let target := (include_str "../../lean-toolchain").trimAscii.copy
  let installed ← (IO.Process.output {cmd := "elan", args := #["run", target, "lake", "--version"]}).toBaseIO
  unless installed.toOption.any (·.exitCode == 0) do
    IO.println s!"Skipping real build verification fixtures; elan toolchain {target} is unavailable."
    return
  implicitEmpty target
  customCache target
  copiedCache target
  implicitExternalCache target

end LeanTests.Verify
