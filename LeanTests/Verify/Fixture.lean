import LakeCompat.Report
import LakeCompat.Verify
import LeanTests.Support

namespace LeanTests.Verify
open Lean System LeanTests.Support

def fakeTarget := "leanprover/lean4:v4.20.0"
def originalToolchain := "leanprover/lean4:v4.19.0\n"
def originalSource := "-- uncommitted work\n"

def manifest (entries : Array Json := #[]) : Json :=
  Json.mkObj [("name", "app"), ("packages", .arr entries), ("version", "1.1.0")]

def pathEntry (name dir : String) (copy := false) (inherited := false) : Json :=
  Json.mkObj [("name", .str name), ("type", "path"), ("dir", .str dir),
    ("copy", .bool copy), ("inherited", .bool inherited), ("configFile", "lakefile.toml")]

def gitEntry (name url revision : String) : Json :=
  Json.mkObj [("name", .str name), ("type", "git"), ("url", .str url), ("rev", .str revision),
    ("inputRev", .str revision), ("inherited", false), ("configFile", "lakefile.toml")]

def write (path : FilePath) (contents : String) : IO Unit := do
  if let some dir := path.parent then IO.FS.createDirAll dir
  IO.FS.writeFile path contents

def link (target : String) (path : FilePath) : IO Unit := do
  discard <| IO.Process.run {cmd := "ln", args := #["-s", target, path.toString]}

def withProject (test : FilePath → IO Unit) : IO Unit := withTempDir fun root => do
  let project := root / "src"
  write (project / "lean-toolchain") originalToolchain
  write (project / "lakefile.toml") "name = \"app\"\n"
  write (project / "Main.lean") originalSource
  writeJson (project / "lake-manifest.json") (manifest)
  IO.FS.createDir (root / "bin")
  link (← IO.appPath).toString (root / "bin" / "elan")
  test project

def field (report : Json) (key : String) := LakeCompat.Report.string report key

def expect (report : Json) (status : String) (message := "") : IO Unit := do
  check (field report "status" == status && (field report "message").contains message)
    s!"expected {status} {message}, got {report.compress}"

/-- Run each trial in a subprocess with its own PATH and a deadline for the entire verifier.
Write JSON through a file to keep bounded compiler diagnostics intact, including escaped newlines. -/
def trial (project : FilePath) (scenario := "pass") (pins := Json.mkObj []) (ms := 600000)
    (target := fakeTarget) : IO Json := do
  let root := project.parent.get!
  let result := root / "result.json"
  if ← result.pathExists then IO.FS.removeFile result
  let mut env : LakeCompat.Verify.Environment := #[("LEAN_SYSROOT", some "/incorrect/lean")]
  if target == fakeTarget then
    let inherited := (← IO.getEnv "PATH").getD ""
    let searchPath := if scenario == "no-elan" then (root / "empty-bin").toString
      else (root / "bin").toString ++ ":" ++ inherited
    env := env ++ #[("PATH", some searchPath), ("LAKECOMPAT_TEST_ELAN", some project.toString),
      ("LAKECOMPAT_TEST_SCENARIO", some scenario)]
  let args := #[(← IO.appPath).toString, "verify-fixture", project.toString, target, toString ms,
    pins.compress, result.toString]
  let child ← LakeCompat.Verify.runCommand args root env (max (ms + 5000) 20000)
  check (!child.timedOut && child.exitCode == 0) s!"verifier subprocess failed: {child.output}"
  readJson result

def verifyFixture (args : List String) : IO UInt32 := do
  let [project, target, timeout, pins, result] := args | throw <| IO.userError "invalid verification fixture arguments"
  let some ms := timeout.toNat? | throw <| IO.userError "invalid verification timeout"
  let fields ← IO.ofExcept <| Json.parse pins >>= Json.getObj?
  let revisions ← IO.ofExcept <| fields.toList.mapM fun (name, value) => return (name, ← value.getStr?)
  writeJson result (← LakeCompat.verifyBuild project target revisions.toArray ms)
  return 0

private def fakeBuild (scenario : String) (project : FilePath) : IO UInt32 := do
  check ((← IO.currentDir) != project) "build ran in the original project"
  check ((← IO.FS.readFile "lean-toolchain").trimAscii == fakeTarget.toSlice) "incorrect trial toolchain"
  check ((← IO.FS.readFile "Main.lean").contains "uncommitted work") "working source missing"
  for path in [".git", ".lake/build/stale.olean"] do
    check (!(← (FilePath.mk path).pathExists)) s!"copied excluded input {path}"
  let root := project.parent.get!
  match scenario with
  | "isolation" =>
    for i in [:24] do
      let script := s!"module-{i}/build.sh"
      check ((← IO.FS.readFile script) == s!"echo {i}\n") "changed script"
      discard <| IO.Process.run {cmd := "test", args := #["-x", script]}
    IO.FS.writeFile "Main.lean" "-- changed during build"
    IO.println "done"
  | "change-lock" => IO.FS.writeFile "lake-manifest.json" "{}"
  | "overrides" => write ".lake/package-overrides.json" "{}"
  | "tail" =>
    IO.println (String.ofList (List.replicate 24000 'a') ++ " ERROR at Main.lean:3")
    return 2
  | "binary-log" =>
    let out ← IO.getStdout
    out.write ⟨Array.replicate 18000 0xff⟩
    out.putStr " build failed"
    return 1
  | "diagnostic" =>
    IO.print (String.join (List.replicate 2000 "Resolved dependency\n"))
    IO.println "error: ./Main.lean:3:8: Type mismatch\n  expected Nat"
    IO.print (String.join (List.replicate 3000 "Built another target\n"))
    IO.println "error: build failed"
    return 1
  | "links" =>
    for (name, target) in [("shared", "lib-link/Shared.lean"), ("lib-link", "lib")] do
      let value ← IO.Process.run {cmd := "readlink", args := #[name]}
      check (value == target ++ "\n") s!"changed symlink {name}"
    check ((← IO.FS.readFile "shared") == "shared input") "file symlink contents"
    check ((← IO.FS.readFile "lib-link/main-link") == (← IO.FS.readFile "Main.lean")) "directory symlink contents"
  | "metadata-links" =>
    check ((← IO.FS.readFile "original-toolchain") == originalToolchain) "modified toolchain symlink target"
    check ((← IO.FS.readFile "original-lock") == (← IO.FS.readFile (project / "original-lock"))) "modified lock symlink target"
  | "cleanup" =>
    IO.FS.writeFile (root / "trial-path") (← IO.currentDir).toString
    link "missing" "dangling"
    IO.FS.createDirAll "generated/nested"
    discard <| IO.Process.run {cmd := "ln", args := #[(root / "keep/contents").toString, "generated/nested/hard-link"]}
    link (root / "keep").toString "generated/nested/external-link"
    discard <| IO.Process.run {cmd := "chmod", args := #["500", "generated/nested", "generated"]}
  | "timeout" =>
    -- A descendant ignores TERM and retains the output pipe after the fake build exits.
    discard <| IO.Process.spawn {cmd := "/bin/sh", args := #["-c", "trap '' TERM; echo 'compiler still running'; exec sleep 90"]}
  | "clone" | "check-revision" | "dirty" =>
    let fields := (LakeCompat.Report.rows (← readJson "lake-manifest.json") "packages")[0]!
    let repo := root / "dep"
    if scenario == "check-revision" then
      check (field fields "rev" == (← git repo #["rev-parse", "HEAD"])) "missing trial revision in lock"
    let checkout := ".lake/packages/dep-postgres"
    discard <| IO.Process.run {cmd := "git", args := #["clone", "-q", repo.toString, checkout]}
    if scenario == "dirty" then write (FilePath.mk checkout / "postgres/lakefile.lean") "-- changed"
  | "tagged" =>
    let entry := (LakeCompat.Report.rows (← readJson "lake-manifest.json") "packages")[0]!.getObjValD "git"
    let revision ← git (root / "sha256-dep") #["rev-parse", "HEAD"]
    check (field entry "rev" == revision && field entry "inputRev?" == revision) "tagged SHA-256 pins"
    check (!(entry.getObjVal? "inputRev").isOk && entry.getObjValD "extra" == Json.mkObj [("keep", true)])
      "tagged entry fields changed"
    discard <| IO.Process.run {cmd := "git", args := #["clone", "-q", field entry "url", ".lake/packages/dep"]}
  | "pass" | "check-empty" | "check-change" => pure ()
  | _ => throw <| IO.userError s!"unrecognized fake build {scenario}"
  return 0

/-- The test executable doubles as elan when invoked through the fixture's PATH. -/
def fakeElan (args : List String) : IO UInt32 := do
  let "run" :: target :: "lake" :: command :: rest := args | throw <| IO.userError s!"invalid elan args {args}"
  let project := FilePath.mk ((← IO.getEnv "LAKECOMPAT_TEST_ELAN").get!)
  let scenario := (← IO.getEnv "LAKECOMPAT_TEST_SCENARIO").get!
  check (target == fakeTarget && (← IO.getEnv "LEAN_SYSROOT").isNone) "unexpected toolchain or inherited LEAN_SYSROOT"
  match command, rest with
  | "--version", [] =>
    if scenario == "uninstalled" then
      IO.println "no such toolchain"
      return 1
    IO.println "Lake version for test toolchain"
  | "translate-config", ["toml", output] => IO.FS.writeFile output "name = \"app\"\n"
  | "build", [] => return ← fakeBuild scenario project
  | "check-build", [] =>
    if scenario == "check-empty" then return 1
    if scenario == "check-change" then IO.FS.writeFile "lake-manifest.json" "{}"
  | _, _ => throw <| IO.userError s!"unexpected Lake command {command} {rest}"
  return 0

end LeanTests.Verify
