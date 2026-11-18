import LeanTests.Verify.Builds

namespace LeanTests.Verify
open Lean System LeanTests.Support

private def isolation : IO Unit := withProject fun project => do
  IO.FS.createDir (project / ".git")
  write (project / ".lake/build/stale.olean") ""
  for i in [:24] do
    let script := project / s!"module-{i}/build.sh"
    write script s!"echo {i}\n"
    IO.setAccessRights script {user := {read := true, write := true, execution := true}}
  let lock := project / "lake-manifest.json"
  let original ← IO.FS.readBinFile lock
  let result ← trial project "isolation"
  expect result "passed"
  check ((field result "output").contains "done") result.compress
  check ((← IO.FS.readBinFile lock) == original && (← IO.FS.readFile (project / "lean-toolchain")) == originalToolchain &&
    (← IO.FS.readFile (project / "Main.lean")) == originalSource) "build changed the original project"

private def outcomes : IO Unit := withProject fun project => do
  expect (← trial project "change-lock") "inconclusive" "changed"
  expect (← trial project "check-change") "inconclusive" "changed"
  expect (← trial project "overrides") "inconclusive" "overrides"
  expect (← trial project "check-empty") "inconclusive"
  expect (← trial project "no-elan") "unavailable"
  let missing ← trial project "uninstalled"
  expect missing "unavailable"
  check ((field missing "output").contains "no such toolchain") missing.compress
  let tail ← trial project "tail"
  expect tail "failed"
  let output := field tail "output"
  check (output.utf8ByteSize ≤ 16 * 1024 && output.contains "ERROR at Main.lean:3") "incorrect log tail"
  let binary ← trial project "binary-log"
  expect binary "failed"
  let output := field binary "output"
  check (output.utf8ByteSize ≤ LakeCompat.Verify.logLimit && output.endsWith "build failed") "unbounded binary log"
  let diag ← trial project "diagnostic"
  expect diag "failed"
  let output := field diag "output"
  check (output.utf8ByteSize ≤ 16 * 1024 && output.contains "Main.lean:3:8: Type mismatch\n  expected Nat" &&
    output.endsWith "error: build failed") "missing compiler diagnostic or final summary"
  let timed ← trial project "timeout" (ms := 500)
  expect timed "inconclusive" "timeout"
  check ((field timed "output").contains "compiler still running") timed.compress

private def snapshotNames : IO Unit := withProject fun project => do
  let copy := project.parent.get! / "copy"
  for name in [".Git", ".Lake"] do write (project / name / "marker") "source"
  let folded ← (project / ".git").pathExists
  if !folded then
    for name in [".git", ".lake"] do write (project / name / "marker") "cache"
  LakeCompat.Verify.copySources project copy
  for name in [".git", ".lake"] do check (!(← (copy / name).pathExists)) s!"copied {name}"
  for name in [".Git", ".Lake"] do
    check ((← (copy / name / "marker").pathExists) == !folded) s!"wrong snapshot handling for {name}"

private def localPaths : IO Unit := withProject fun project => do
  let lock := project / "lake-manifest.json"
  for path in [project.parent.get!.toString, "../elsewhere", ".", ".git/objects"] do
    writeJson lock ((manifest).setObjVal! "packagesDir" (.str path))
    expect (← trial project) "inconclusive" "packagesDir"
  IO.FS.createDir (project / "sources")
  writeJson lock ((manifest #[pathEntry "dep" "sources" (copy := true)]).setObjVal! "packagesDir" "sources/vendor")
  expect (← trial project) "inconclusive" "own source directory"
  if ← (project / "SOURCES").pathExists then
    writeJson lock ((manifest #[pathEntry "dep" "sources" (copy := true)]).setObjVal! "packagesDir" "SOURCES/new/vendor")
    expect (← trial project) "inconclusive" "own source directory"
  writeJson lock (manifest #[pathEntry "dep" "../outside"])
  expect (← trial project) "inconclusive" "outside"
  writeJson lock (manifest)
  write (project / ".lake/package-overrides.json") "{}"
  expect (← trial project) "inconclusive" "overrides"

private def validLinks : IO Unit := withProject fun project => do
  write (project / "lib/Shared.lean") "shared input"
  link "lib" (project / "lib-link")
  link "lib-link/Shared.lean" (project / "shared")
  link "../Main.lean" (project / "lib/main-link")
  expect (← trial project "links") "passed"
  for (file, backup) in [("lean-toolchain", "original-toolchain"), ("lake-manifest.json", "original-lock")] do
    IO.FS.rename (project / file) (project / backup)
    link backup (project / file)
  expect (← trial project "metadata-links") "passed"
  for file in ["lean-toolchain", "lake-manifest.json"] do
    check ((← (project / file).symlinkMetadata).type == .symlink) s!"original {file} symlink replaced"

private def invalidLinks : IO Unit := withProject fun project => do
  let path := project / "lib-link"
  for target in [project.parent.get!.toString, "../src/Main.lean", "missing",
      ".git/config", ".lake/build/M.olean", "lib-link"] do
    link target path
    expect (← trial project) "inconclusive" "symlink"
    IO.FS.removeFile path
  link "middle" path
  link (project / "Main.lean").toString (project / "middle")
  expect (← trial project) "inconclusive" "symlink"
  IO.FS.removeFile (project / "middle")
  IO.FS.removeFile path
  discard <| IO.Process.run {cmd := "mkfifo", args := #[path.toString]}
  expect (← trial project) "inconclusive" "special file"
  IO.FS.removeFile path
  IO.FS.createDir (project / "a")
  IO.FS.createDir (project / "b")
  link "../b" (project / "a/to-b")
  link "../a" (project / "b/to-a")
  expect (← trial project) "inconclusive" "cycle"

private def cleanup : IO Unit := withProject fun project => do
  let root := project.parent.get!
  let external := root / "keep"
  let marker := external / "contents"
  write marker "keep"
  -- ls -ld's first field gives the POSIX permission bits on both BSD and GNU hosts.
  let permissions := fun (path : FilePath) => do
    let line ← IO.Process.run {cmd := "ls", args := #["-ld", path.toString]}
    return (line.takeWhile (!·.isWhitespace)).copy
  let dirMode ← permissions external
  let fileMode ← permissions marker
  expect (← trial project "cleanup") "passed"
  let workspace := FilePath.mk (← IO.FS.readFile (root / "trial-path"))
  check (!(← workspace.parent.get!.pathExists)) "trial directory survived cleanup"
  check ((← IO.FS.readFile marker) == "keep" && (← permissions external) == dirMode &&
    (← permissions marker) == fileMode) "cleanup changed external files or permissions"

private def badRevisions : IO Unit := withProject fun project => do
  let old := String.ofList (List.replicate 40 'a')
  expect (← trial project (pins := Json.mkObj [("missing", .str old)])) "inconclusive"
  writeJson (project / "lake-manifest.json") (manifest #[gitEntry "dep" "https://example.org/dep" old])
  expect (← trial project (pins := Json.mkObj [("dep", "main")])) "inconclusive" "40- or 64-character"

private def checkoutPins : IO Unit := withProject fun project => do
  let repo := project.parent.get! / "dep"
  for dir in ["core", "postgres"] do write (repo / dir / "lakefile.lean") "-- package"
  write (repo / "nested/inner/app/input") ""
  write (repo / "nested/child/lakefile.lean") "-- inherited package"
  link "nested/inner/app" (repo / "alias")
  discard <| git repo #["init", "-q"]
  discard <| git repo #["add", "."]
  discard <| git repo #["commit", "-qm", "dep"]
  let revision ← git repo #["rev-parse", "HEAD"]
  let old := String.ofList (List.replicate 40 'a')
  let dep := gitEntry "«dep-postgres»" repo.toString old
  let lock := fun subDir => manifest #[dep.setObjVal! "subDir" subDir,
    pathEntry "core" ".lake/packages/dep-postgres/postgres/../core",
    pathEntry "child" ".lake/packages/dep-postgres/alias/../../child" (inherited := true),
    pathEntry "local" "vendor/mirror/../local"]
  IO.FS.createDirAll (project / "vendor/mirror")
  IO.FS.createDir (project / "vendor/local")
  let file := project / "lake-manifest.json"
  writeJson file (lock "postgres")
  let pins := Json.mkObj [("dep-postgres", .str revision)]
  let result ← trial project "check-revision" pins
  expect result "passed"
  check (result.getObjValD "checked_revisions" == pins && (← readJson file) == lock "postgres") result.compress
  expect (← trial project "clone") "inconclusive" "locked revision"
  expect (← trial project "dirty" pins) "inconclusive" "tracked files"
  writeJson file (lock "core/lakefile.lean")
  expect (← trial project "clone" pins) "inconclusive" "package directory"

private def taggedSha256 : IO Unit := withProject fun project => do
  let repo := project.parent.get! / "sha256-dep"
  let init ← IO.Process.output {cmd := "git", args := #["init", "-q", "--object-format=sha256", repo.toString]}
  if init.exitCode != 0 then
    IO.println s!"Skipping SHA-256 verification fixture: {init.stderr}"
    return
  discard <| git repo #["commit", "--allow-empty", "-qm", "dep"]
  let revision ← git repo #["rev-parse", "HEAD"]
  check (revision.length == 64) "expected a SHA-256 repository"
  let fields := Json.mkObj [("name", "dep"), ("url", "../sha256-dep"), ("inherited", false),
    ("rev", .str (String.ofList (List.replicate 64 '0'))), ("inputRev?", "main"), ("subDir?", .null),
    ("extra", Json.mkObj [("keep", true)])]
  writeJson (project / "lake-manifest.json") (Json.mkObj [("version", toJson 6),
    ("packages", .arr #[Json.mkObj [("git", fields)]])])
  let result ← trial project "tagged" (Json.mkObj [("dep", .str revision.toUpper)])
  expect result "passed"
  check (result.getObjValD "checked_revisions" == Json.mkObj [("dep", .str revision)]) result.compress

def run : IO Unit := do
  if Platform.isWindows then return
  isolation
  snapshotNames
  outcomes
  localPaths
  validLinks
  invalidLinks
  cleanup
  badRevisions
  checkoutPins
  taggedSha256
  realBuilds

end LeanTests.Verify
