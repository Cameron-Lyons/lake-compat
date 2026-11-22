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

private def mixedGitFormats (target : String) : IO Unit := withProject fun project => do
  let mut entries := #[]
  let mut expected := Json.mkObj []
  let mut config := appConfig ++ "packagesDir = \"vendor\"\n" ++ mainLibrary
  for (name, moduleName, format) in [("modern", "Modern", "sha256"), ("classic", "Classic", "sha1")] do
    let repo := project.parent.get! / name
    let init ← IO.Process.output {cmd := "git", args := #["init", "-q", "--object-format=" ++ format, repo.toString]}
    if init.exitCode != 0 then
      if name == "modern" then
        IO.println s!"Skipping mixed Git object formats: {init.stderr}"
        return
      throw <| IO.userError init.stderr
    write (repo / "lakefile.toml") s!"name = \"{name}\"\n[[lean_lib]]\nname = \"{moduleName}\"\n"
    write (repo / (moduleName ++ ".lean")) s!"def {name} := 1\n"
    discard <| git repo #["add", "."]
    discard <| git repo #["commit", "-qm", name]
    let rev ← git repo #["rev-parse", "HEAD"]
    let url := "../" ++ name
    entries := entries.push (gitEntry name url rev)
    expected := expected.setObjVal! name (.str rev)
    config := config ++ s!"[[require]]\nname = \"{name}\"\ngit = \"{url}\"\nrev = \"{rev}\"\n"
  write (project / "lakefile.toml") config
  write (project / "Main.lean") "import Modern\nimport Classic\nexample : modern + classic = 2 := rfl\n"
  writeJson (project / "lake-manifest.json") ((manifest entries).setObjVal! "packagesDir" "vendor")
  let configDir := project.parent.get! / "config"
  write (configDir / "git/config") "[init]\n\tdefaultObjectFormat = sha256\n"
  let output ← cli #[target, "--project", project.toString, "--verify", "--json"]
    #[("XDG_CONFIG_HOME", some configDir.toString)]
  check (output.exitCode == 0) (output.stdout ++ output.stderr)
  let result := (← IO.ofExcept (Json.parse output.stdout)).getObjValD "verification"
  expect result "passed"
  check (result.getObjValD "checked_revisions" == expected && !(← (project / "vendor").pathExists)) result.compress

private def staleModuleCache (target : String) : IO Unit := withProject fun project => do
  write (project / "lakefile.toml") (appConfig ++ "buildDir = \"out dir\"\nleanLibDir = \"objects\"\n" ++ mainLibrary)
  let objects := project / "out dir/objects"
  write (objects / "Notes.trace") "source data"
  write (objects / "Notes.olean") "fixture"
  IO.FS.writeBinFile (objects / "Binary.trace") ⟨#[255, 0, 253]⟩
  write (objects / "Binary.olean") "fixture"
  write (project / "message.txt") "original"
  let source := "example : (include_str \"message.txt\") = \"original\" := rfl\n" ++
    "example : (include_str \"out dir/objects/Notes.trace\") = \"source data\" := rfl\n"
  write (project / "Main.lean") source
  discard <| IO.Process.run {cmd := "elan", args := #["run", target, "lake", "build"], cwd := project}
  let trace ← IO.FS.readBinFile (objects / "Main.trace")
  let olean ← IO.FS.readBinFile (objects / "Main.olean")
  -- Module-shaped outputs alone do not make a source fixture a valid build trace.
  let imitation := (← readJson (objects / "Main.trace")).setObjVal! "log" "fixture"
  writeJson (objects / "Example.trace") imitation
  -- A copy of genuine Lake metadata outside the active output directory can also be source data.
  IO.FS.createDir (project / "fixtures")
  IO.FS.writeBinFile (project / "fixtures/Main.trace") trace
  IO.FS.writeBinFile (project / "fixtures/Main.olean") olean
  write (project / "message.txt") "changed"
  let failed ← trial project (target := target)
  expect failed "failed"
  check ((field failed "output").contains "Main.lean") failed.compress
  check ((← IO.FS.readBinFile (objects / "Main.trace")) == trace &&
    (← IO.FS.readBinFile (objects / "Main.olean")) == olean) "changed the original build cache"
  write (project / "message.txt") "original"
  write (project / "Main.lean") (source ++ "def savedTrace := include_str \"fixtures/Main.trace\"\n" ++
    "def exampleTrace := include_str \"out dir/objects/Example.trace\"\n")
  expect (← trial project (target := target)) "passed"
  discard <| IO.Process.run {cmd := "elan", args := #["run", target, "lake", "build", "+Main:ltar"], cwd := project}
  IO.FS.removeFile (objects / "Main.olean")
  -- Leave only the packed module as reusable output, including in the snapshot detector.
  for path in [objects / "Notes.olean", objects / "Binary.olean", project / "fixtures/Main.olean"] do
    IO.FS.removeFile path
  write (project / "message.txt") "changed"
  let archived ← trial project (target := target)
  expect archived "failed"
  check (!(← (objects / "Main.olean").pathExists)) "unpacked the module archive in the original project"
  write (project / "lakefile.toml") "invalid toml"
  expect (← trial project (target := target)) "failed"

private def moduleCacheWithColon (target : String) : IO Unit := withProject fun project => do
  write (project / "lakefile.toml") (appConfig ++ "buildDir = \"out:/cache\"\n" ++ mainLibrary)
  write (project / "message.txt") "original"
  write (project / "Main.lean") "example : (include_str \"message.txt\") = \"original\" := rfl\n"
  discard <| IO.Process.run {cmd := "elan", args := #["run", target, "lake", "build"], cwd := project}
  write (project / "message.txt") "changed"
  let result ← trial project (target := target)
  expect result "failed"
  check ((field result "output").contains "Main.lean") result.compress
  write (project / "message.txt") "original"
  expect (← trial project (target := target)) "passed"

private def externalOutputs (target : String) : IO Unit := withProject fun project => do
  let external := project.parent.get! / "external outputs"
  write (external / "keep") "original"
  write (project / "Main.lean") "def main : IO Unit := pure ()\n"
  write (project / "Core.lean") "def value := 5\n"
  let lock := project / "lake-manifest.json"
  let original ← IO.FS.readBinFile lock
  for key in ["buildDir", "leanLibDir", "nativeLibDir", "irDir", "binDir"] do
    let config := "name = \"app\"\ndefaultTargets = [\"app\", \"Core\"]\n" ++
      s!"{key} = {(Json.str external.toString).compress}\n" ++
      "[[lean_lib]]\nname = \"Core\"\ndefaultFacets = [\"static\"]\n" ++
      "[[lean_exe]]\nname = \"app\"\nroot = \"Main\"\n"
    write (project / "lakefile.toml") config
    expect (← trial project (target := target)) "inconclusive"
    check ((← external.readDir).size == 1 && (← IO.FS.readFile (external / "keep")) == "original")
      s!"Lake wrote to the external {key}"
  check (!(← (project / ".lake").pathExists) && (← IO.FS.readBinFile lock) == original)
    "external output trial changed the original project"

private def dependencyOutput (target : String) : IO Unit := withProject fun project => do
  let dep := project / "dep"
  let external := project.parent.get! / "dependency outputs"
  write (project / "lakefile.toml") (appConfig ++ mainLibrary ++
    "[[require]]\nname = \"dep\"\npath = \"dep\"\n")
  write (project / "Main.lean") "import Dep\nexample : message = 5 := rfl\n"
  write (dep / "lakefile.lean") ("import Lake\nopen Lake DSL\npackage dep where\n" ++
    "  buildDir := System.FilePath.mk (include_str \"output.txt\")\nlean_lib Dep\n")
  write (dep / "Dep.lean") "def message := 5\n"
  let entry := (pathEntry "dep" "dep").setObjVal! "configFile" "lakefile.lean"
  let lock := project / "lake-manifest.json"
  writeJson lock (manifest #[entry])
  let original ← IO.FS.readBinFile lock
  write (dep / "output.txt") external.toString
  expect (← trial project (target := target)) "inconclusive"
  check (!(← external.pathExists)) "Lake created the dependency's external output directory"
  write (dep / "output.txt") "in-tree outputs"
  expect (← trial project (target := target)) "passed"
  for path in [project / ".lake", dep / ".lake", dep / "in-tree outputs"] do
    check (!(← path.pathExists)) s!"Lake wrote to the original project at {path}"
  check ((← IO.FS.readBinFile lock) == original) "dependency output trial changed the original lock"

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
  -- A newly fetched revision can turn an in-tree output path into an external symlink.
  let external := root / "external output"
  IO.FS.createDir external
  link external.toString (repo / "outputs")
  let config := repo / "lakefile.toml"
  write config ("buildDir = \"outputs/future\"\n" ++ (← IO.FS.readFile config))
  discard <| git repo #["add", "outputs", "lakefile.toml"]
  discard <| git repo #["commit", "-qm", "linked output"]
  let candidate := Json.mkObj [("greeting", .str (← git repo #["rev-parse", "HEAD"]))]
  expect (← trial project (pins := candidate) (target := target)) "inconclusive" "outside the trial workspace"
  check (← external.readDir).isEmpty "build wrote through the locked Git symlink"
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
  mixedGitFormats target
  staleModuleCache target
  moduleCacheWithColon target
  externalOutputs target
  dependencyOutput target
  customCache target
  copiedCache target
  implicitExternalCache target

end LeanTests.Verify
