import LeanTests.Graph.Fixture
import Init.System.Uri

open Lean System LeanTests.Support

namespace LeanTests.Graph.Revisions

private def fakeRevision (c : Char) := String.ofList (List.replicate 40 c)

private def dirtyCheckout (project : FilePath) : IO Unit := do
  let repo := packagesDir project / "parent"
  pin repo "4.24.0"
  manifest repo #[gitDep "leaf" (fakeRevision 'b')]
  manifest project #[gitDep "parent" (← commitMetadata repo)]
  let original ← report project
  expectKinds original #["toolchain_mismatch", "unlocked_dependency"]
  pin repo
  manifest repo
  discard <| git repo #["add", "lean-toolchain"]
  expect (← report project) original
  IO.FS.removeFile (repo / "lean-toolchain")
  IO.FS.removeFile (repo / "lake-manifest.json")
  expect (← report project) original

private def archive (project : FilePath) : IO Unit := do
  let repo := packagesDir project / "dependency"
  pin repo
  manifest repo
  manifest project #[gitDep "dependency" (fakeRevision 'a')]
  let r ← report project
  expectKinds r #["unknown_checkout_revision"]
  expectField (← pkg r "dependency") "toolchain" target

private def archiveBoundary (project : FilePath) : IO Unit := do
  let repo := packagesDir project / "archive"
  pin repo
  let external := project / "external"
  pin external
  manifest external
  link (repo / "link") external
  for (subdir, kind) in [("absent", "missing_checkout"), ("link", "unknown_package_directory")] do
    manifest project #[gitDep "archive" (fakeRevision 'a') (fields := [("subDir", subdir)])]
    let r ← report project
    expectKinds r #[kind]
    expectField (← pkg r "archive") "toolchain" .null

private def archiveMetadata (project : FilePath) : IO Unit := do
  let repo := packagesDir project / "archive"
  let config := repo / "config"
  pin config
  manifest config
  link (repo / "lean-toolchain") "config/lean-toolchain"
  link (repo / "lake-manifest.json") "config/lake-manifest.json"
  manifest project #[gitDep "archive" (fakeRevision 'a')]
  let internal ← report project
  expectKinds internal #["unknown_checkout_revision"]
  expectField (← pkg internal "archive") "toolchain" target
  let other := project / "other"
  pin other
  manifest other #[localDep "unrelated" "."]
  for file in ["lean-toolchain", "lake-manifest.json"] do
    IO.FS.removeFile (repo / file)
    link (repo / file) (other / file)
  let external ← report project
  expectKinds external #["unknown_checkout_revision", "unknown_toolchain", "invalid_manifest"]
  expectField (← pkg external "archive") "toolchain" .null

private def archiveToCheckout (project : FilePath) : IO Unit := do
  let archive := packagesDir project / "archive"
  pin archive
  manifest archive
  let repo := packagesDir project / "locked"
  pin repo
  manifest repo
  for dir in ["before", "after"] do manifest (repo / dir)
  pin (repo / "before") older
  link (repo / "alias") "before"
  let rev ← commitMetadata repo
  IO.FS.removeFile (repo / "alias")
  link (repo / "alias") "after"
  link (archive / "next") "../locked/alias"
  manifest project #[gitDep "archive" (fakeRevision 'a'), gitDep "locked" rev,
    localDep "local" ".lake/packages/archive/next"]
  let r ← report project
  let localPkg ← pkg r "local"
  expectField localPkg "toolchain" older
  expectField localPkg "checkout" (repo / "before").toString
  expectKinds r #["unknown_checkout_revision", "unknown_checkout_revision", "toolchain_mismatch"]

private def incompleteCheckout (project : FilePath) : IO Unit := do
  manifest project
  let workspaceRev ← commitMetadata project
  let repo := packagesDir project / "dependency"
  manifest repo
  pin repo
  IO.FS.createDir (repo / ".git")
  manifest project #[gitDep "dependency" workspaceRev]
  let r ← report project
  expectKinds r #["unknown_checkout_revision"]
  expectField (← pkg r "dependency") "toolchain" target

private def localEdits (project : FilePath) : IO Unit := do
  let source := project / "local"
  manifest source
  pin source "4.24.0"
  discard <| commitMetadata source
  manifest project #[localDep "local" "local"]
  pin source
  expectKinds (← report project) #[]

private def localRemotes (project : FilePath) : IO Unit := do
  let parent := project / "parent"
  let remote := project / "remotes/leaf %20é"
  manifest (remote / "lib")
  pin remote
  link (remote / "alias") "lib"
  let revision ← commitMetadata remote
  let other := parent / "remotes/leaf"
  for checkout in [packagesDir project / "leaf", other] do
    IO.FS.createDirAll checkout.parent.get!
    discard <| git project #["clone", "-q", remote.toString, checkout.toString]
  let dependency := fun (url subDir : String) inherited =>
    gitDep "leaf" revision inherited [("url", url), ("subDir", subDir)]
  manifest parent #[dependency "./remotes/leaf %20é" "alias" false]
  for (url, kinds) in [(remote.toString, #[]), (other.toString, #["source_conflict"])] do
    manifest project #[localDep "parent" "parent", dependency url "lib" true]
    expectKinds (← report project) kinds
  let uri := Uri.pathToUri remote
  expect (← git project #["ls-remote", uri, "HEAD"]) (revision ++ "\tHEAD")
  manifest project #[localDep "parent" "parent", dependency remote.toString "lib" true]
  for (url, kinds) in [(uri, #[]), (uri.replace "file:///" "file://localhost/", #[]),
      (Uri.pathToUri other, #["source_conflict"]), (uri ++ "%FF", #["source_conflict"])] do
    manifest parent #[dependency url "alias" false]
    expectKinds (← report project) kinds

private def untrackedMetadata (project : FilePath) : IO Unit := do
  let repo := packagesDir project / "group"
  pin repo "4.24.0"
  IO.FS.writeFile (repo / ".gitignore") "core/lean-toolchain\n"
  manifest (repo / "front") #[localDep "core" "../core"] (file := "custom.json")
  manifest (repo / "core")
  manifest project #[
    gitDep "group" (← commitMetadata repo) (fields := [("subDir", "front"), ("manifestFile", "custom.json")]),
    localDep "core" ".lake/packages/group/front/../core" true]
  pin (repo / "core")
  pin (repo / "front")
  let r ← report project
  expectKinds r #["toolchain_mismatch", "toolchain_mismatch"]
  expectField (findings r)[0]! "path" (chain ["demo", "group", "core"])
  expectField (findings r)[1]! "path" (chain ["demo", "group"])
  for p in packages r do expectField p "toolchain_source" "repository"

private def linkedWorktree (project : FilePath) : IO Unit := do
  let source := project / "original"
  pin source "4.24.0"
  manifest source
  IO.FS.rename (source / "lean-toolchain") (source / "version")
  link (source / "lean-toolchain") "./version"
  let revision ← commitMetadata source
  IO.FS.createDirAll (packagesDir project)
  let repo := packagesDir project / "dependency"
  discard <| git source #["worktree", "add", "--quiet", "--detach", repo.toString, revision]
  manifest project #[gitDep "dependency" revision]
  IO.FS.writeFile (repo / "version") target
  check ((← (repo / ".git").metadata).type == .file) "expected a linked Git worktree"
  let r ← report project
  expectKinds r #["toolchain_mismatch"]
  expectField (← pkg r "dependency") "toolchain" older

private def sha256Tree (project : FilePath) : IO Unit := do
  let repo := packagesDir project / "library"
  IO.FS.createDirAll repo
  let init ← IO.Process.output {cmd := "git", args := #["init", "-q", "--object-format=sha256", repo.toString]}
  if init.exitCode != 0 then
    IO.println s!"Skipping SHA-256 Git fixture: {init.stderr.trimAscii}"
    return
  let lib := repo / "library"
  let file := if Platform.isWindows then "locked manifest.json" else "locked\nmanifest.json"
  manifest (lib / "config") (file := file)
  pin repo
  link (repo / "alias") "./library"
  discard <| git repo #["add", "."]
  discard <| git repo #["commit", "-qm", "package"]
  let revision ← git repo #["rev-parse", "HEAD"]
  manifest project #[gitDep "library" revision (fields := [("subDir", "alias"), ("manifestFile", "./config/" ++ file)])]
  let r ← report project
  expectKinds r #[]
  let p ← pkg r "library"
  expectField p "checkout" lib.toString
  expectField p "toolchain_source" "repository"

private def wrongCheckout (project : FilePath) : IO Unit := do
  manifest project #[gitDep "stale" (fakeRevision '1')]
  let repo := packagesDir project / "stale"
  pin repo
  manifest repo #[gitDep "imaginary" "xx"]
  discard <| git repo #["init", "-q"]
  discard <| git repo #["commit", "--allow-empty", "-qm", "sample"]
  let r ← report project
  expectKinds r #["checkout_revision_mismatch"]
  expectField (← pkg r "stale") "toolchain" .null

private def unbornHead (project : FilePath) : IO Unit := do
  let repo := packagesDir project / "lib"
  manifest repo
  pin repo "4.24.0"
  manifest project #[gitDep "lib" (← commitMetadata repo)]
  discard <| git repo #["symbolic-ref", "HEAD", "refs/heads/unborn"]
  pin repo
  let r ← report project
  expectField (← pkg r "lib") "toolchain" older
  expectKinds r #["unknown_checkout_revision", "toolchain_mismatch"]

private def legacyCatFile (project : FilePath) : IO Unit := do
  if Platform.isWindows then return
  let repo := packagesDir project / "dep"
  manifest repo
  pin repo "4.24.0"
  let locked ← commitMetadata repo
  pin repo
  discard <| git repo #["commit", "-qam", "newer pin"]
  manifest project #[gitDep "dep" locked]
  let tools := project / "bin"
  let shim := tools / "git"
  IO.FS.createDir tools
  IO.FS.writeFile shim <|
    "#!/bin/sh\nfor arg do\n  if [ \"$arg\" = --batch-command ]; then exit 129; fi\ndone\nexec \"$TEST_REAL_GIT\" \"$@\"\n"
  IO.setAccessRights shim {user := {read := true, write := true, execution := true}}
  let gitPath ← IO.Process.run {cmd := "/bin/sh", args := #["-c", "command -v git"]}
  let path := tools.toString ++ ":" ++ (← IO.getEnv "PATH").getD ""
  let out ← cli (reportArgs project) #[("TEST_REAL_GIT", some gitPath.trimAscii.copy), ("PATH", some path)]
  expect out.exitCode.toNat 0
  let r ← IO.ofExcept (Json.parse out.stdout)
  expectKinds r #["checkout_revision_mismatch", "toolchain_mismatch"]
  expectField (← pkg r "dep") "toolchain" older

def run : IO Unit := do
  runCase "dirty and deleted locked metadata" dirtyCheckout
  runCase "archive provenance" archive
  runCase "archive boundary" archiveBoundary
  runCase "archive metadata symlink boundaries" archiveMetadata
  runCase "path from an archive into a locked checkout" archiveToCheckout
  runCase "incomplete checkout in another repository" incompleteCheckout
  runCase "working tree of path dependency" localEdits
  runCase "workspace-relative and absolute Git remotes" localRemotes
  runCase "ignored and untracked committed package metadata" untrackedMetadata
  runCase "linked Git worktree" linkedWorktree
  runCase "SHA-256 tree with custom manifest name" sha256Tree
  runCase "wrong checkout" wrongCheckout
  runCase "unborn HEAD" unbornHead
  runCase "Git without batch-command support" legacyCatFile

end LeanTests.Graph.Revisions
