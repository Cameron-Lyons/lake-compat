import LeanTests.Graph.Fixture

open Lean System LeanTests.Support

namespace LeanTests.Graph.Paths

private def physicalAlias (project : FilePath) : IO Unit := do
  let repo := project / "src" / "mono"
  manifest repo #[localDep "child" "child"]
  pin repo
  manifest (repo / "child")
  pin (repo / "child") "4.24.0"
  let revision ← commitMetadata repo
  IO.FS.createDirAll (packagesDir project)
  link (packagesDir project / "mono") repo
  pin (repo / "child")
  for source in ["src/mono/child", ".lake/packages/mono/child"] do
    manifest project #[gitDep "mono" revision, localDep "child" source true]
    let r ← report project
    let f ← onlyFinding r
    expectField f "kind" "toolchain_mismatch"
    expectField f "path" (chain ["demo", "mono", "child"])
    expectField (← pkg r "child") "checkout" (repo / "child").toString

private def sharedCheckout (project : FilePath) : IO Unit := do
  let repo := project / "shared"
  manifest repo
  manifest (repo / "child")
  pin repo "4.24.0"
  let first ← commitMetadata repo
  pin repo
  discard <| git repo #["commit", "-qam", "update toolchain"]
  let second ← git repo #["rev-parse", "HEAD"]
  IO.FS.createDirAll (packagesDir project)
  for name in ["older", "newer"] do link (packagesDir project / name) repo
  let olderDir := if ← (packagesDir project / "OLDER").isDir then ".LAKE/Packages/OLDER/child"
                  else ".lake/packages/older/child"
  manifest project #[gitDep "older" first, gitDep "newer" second,
    localDep "ambiguous" "shared/child", localDep "explicit" olderDir]
  let r ← report project
  expectField (← pkg r "older") "toolchain" older
  expectField (← pkg r "explicit") "toolchain" older
  expectField (← pkg r "newer") "toolchain" target
  expectField (← pkg r "ambiguous") "toolchain" .null
  check ((findings r).any fun f => str f "package" == "ambiguous" && str f "kind" == "unknown_package_directory")
    r.compress

private def checkoutSpelling (project : FilePath) : IO Unit := do
  let repo := packagesDir project / "ParÉnt"
  manifest repo
  pin repo
  manifest (repo / "child")
  pin (repo / "child") "4.24.0"
  let revision ← commitMetadata repo
  pin (repo / "child")
  let source := ".LAKE/Packages/parént/child"
  let alias := project / source
  let insensitive ← alias.isDir
  unless insensitive do
    -- A differently spelled directory can designate independent working sources.
    manifest alias
    pin alias
  manifest project #[gitDep "ParÉnt" revision, localDep "child" source]
  let r ← report project
  expectField (← pkg r "child") "toolchain" (if insensitive then older else target)

private def relativeSourceAtRevision (project : FilePath) : IO Unit := do
  let repo := project / "shared"
  manifest repo #[localDep "child" "child"]
  manifest (repo / "child")
  pin repo
  let first ← commitMetadata repo
  pin (repo / "child") "4.24.0"
  discard <| git repo #["add", "."]
  discard <| git repo #["commit", "-qm", "change the child toolchain"]
  let second ← git repo #["rev-parse", "HEAD"]
  IO.FS.createDirAll (packagesDir project)
  for name in ["older", "newer"] do link (packagesDir project / name) repo
  manifest project #[gitDep "older" first, gitDep "newer" second,
    localDep "child" ".lake/packages/older/child" true]
  let r ← report project
  -- Both parents request the same physical directory, each at its own locked commit.
  let edges := (findings r).filter fun f => !(str f "parent").isEmpty
  expect (edges.map fun f => (str f "parent", str f "kind")) #[("newer", "source_conflict")]
  let message := str edges[0]! "message"
  for rev in [first, second] do check (message.contains rev) message

private def untraversablePrefix (project : FilePath) : IO Unit := do
  manifest (project / "child")
  pin (project / "child")
  IO.FS.writeFile (project / "plain-file") "file"
  let repo := packagesDir project / "lib"
  manifest repo
  pin repo
  let other := gitDep "lib" (← commitMetadata repo)
  for name in ["absent", "plain-file"] do
    for gitDeps in [#[], #[other]] do
      manifest project (#[localDep "child" (name ++ "/../child")] ++ gitDeps)
      let r ← report project
      let f ← onlyFinding r
      expectField f "package" "child"
      check (["unknown_package_directory", "missing_checkout"].contains (str f "kind")) f.compress
      expectField (← pkg r "child") "toolchain" .null

private def externalCommittedLinks (project : FilePath) : IO Unit := do
  let repo := packagesDir project / "library"
  manifest repo
  link (repo / "lean-toolchain") "../../../lean-toolchain"
  link (repo / "external") "../../.."
  manifest project #[gitDep "library" (← commitMetadata repo), localDep "alias" ".lake/packages/library/external"]
  let r ← report project
  expect ((findings r).map fun f => (str f "package", str f "kind"))
    #[("alias", "unknown_package_directory"), ("library", "unknown_toolchain")]
  for p in packages r do expectField p "toolchain" .null

private def lockedDestination (project : FilePath) : IO Unit := do
  let repo := packagesDir project / "mono"
  pin repo
  for dir in ["original", "edited"] do manifest (repo / dir / "app")
  pin (repo / "original") "4.24.0"
  link (repo / "alias") "original/app"
  manifest project #[gitDep "mono" (← commitMetadata repo) (fields := [("subDir", "alias")])]
  IO.FS.removeFile (repo / "alias")
  link (repo / "alias") "edited/app"
  let r ← report project
  expectKinds r #["toolchain_mismatch"]
  expectField (← pkg r "mono") "toolchain" older

private def monorepoParent (project : FilePath) : IO Unit := do
  let repo := packagesDir project / "mono"
  pin repo
  manifest repo #[localDep "child" "alias/../../child"]
  for path in ["nested/inner/app", "nested/child", "other/inner/app", "other/child"] do manifest (repo / path)
  pin (repo / "nested" / "child") "4.24.0"
  link (repo / "alias") "nested/inner/app"
  let revision ← commitMetadata repo
  IO.FS.removeFile (repo / "alias")
  link (repo / "alias") "other/inner/app"
  link (project / "vendor") ".lake/packages"
  for source in ["./.lake/packages/mono/alias/../../child", "vendor/mono/alias/../../child",
      "vendor/mono/nested/child", "vendor/mono/other/child"] do
    manifest project #[gitDep "mono" revision, localDep "child" source true]
    let r ← report project
    let (child, kind) := if source.endsWith "/other/child" then ("other/child", "source_conflict")
                         else ("nested/child", "toolchain_mismatch")
    expectField (← pkg r "child") "checkout" (repo / child).normalize.toString
    expectKinds r #[kind]

private def workspaceTargets (project : FilePath) : IO Unit := do
  let deps := packagesDir project
  let repo := deps / "mono"
  pin repo
  manifest repo #[localDep "child" "original/child"]
  for path in ["original/app", "original/child", "edited/app", "edited/child"] do manifest (repo / path)
  pin (repo / "original/child") "4.24.0"
  link (repo / "alias") "original/app"
  let revision ← commitMetadata repo
  IO.FS.removeFile (repo / "alias")
  link (repo / "alias") "edited/app"
  IO.FS.createDir (deps / "staging")
  link (project / "staging") ".lake/packages/staging"
  let vendor := project / "vendor"
  let cases : List (FilePath × String) := [
    (repo / "alias", "vendor/../child"), ("staging/../mono/alias/../child", "vendor")]
  for (destination, source) in cases do
    link vendor destination
    manifest project #[gitDep "mono" revision, localDep "child" source true]
    let r ← report project
    expectField (← pkg r "child") "checkout" (repo / "original" / "child").toString
    expectKinds r #["toolchain_mismatch"]
    IO.FS.removeFile vendor

private def cycleDeadline (project : FilePath) : IO Unit := do
  let repo := packagesDir project / "mono"
  pin repo
  manifest repo
  link (packagesDir project / "loop") "mono/../loop"
  manifest project #[gitDep "mono" (← commitMetadata repo), localDep "loop" ".lake/packages/loop"]
  let child ← IO.Process.spawn {
    cmd := (← binary "lake-compat").toString, args := reportArgs project, stdout := .piped }
  let output ← IO.asTask child.stdout.readToEnd .dedicated
  let deadline := (← IO.monoMsNow) + 10000
  let mut status ← child.tryWait
  while status.isNone do
    if (← IO.monoMsNow) >= deadline then
      child.kill
      discard child.wait
      throw <| IO.userError "scan exceeded ten seconds"
    IO.sleep 10
    status ← child.tryWait
  expect (status.get!).toNat 0
  let r ← IO.ofExcept (Json.parse (← IO.ofExcept output.get))
  let f ← onlyFinding r
  expectField f "package" "loop"
  expectField f "kind" "unknown_package_directory"

private def crossRepository (project : FilePath) : IO Unit := do
  let deps := packagesDir project
  let first := deps / "first"
  let second := deps / "second"
  pin first
  manifest first #[localDep "child" "../second/child"]
  pin second
  manifest second
  pin (second / "child") "4.24.0"
  manifest (second / "child")
  manifest project #[gitDep "first" (← commitMetadata first), gitDep "second" (← commitMetadata second),
    localDep "child" ".lake/packages/first/../second/child" true]
  pin (second / "child")
  let r ← report project
  expectKinds r #["toolchain_mismatch"]
  expectField (← pkg r "child") "checkout" (second / "child").toString

private def staleCommit (project : FilePath) : IO Unit := do
  let repo := packagesDir project / "mono"
  pin repo
  manifest repo #[localDep "child" "alias"]
  manifest (repo / "old")
  pin (repo / "old") "4.24.0"
  link (repo / "alias") "old"
  let revision ← commitMetadata repo
  manifest repo
  IO.FS.rename (repo / "old") (repo / "new")
  IO.FS.removeFile (repo / "alias")
  link (repo / "alias") "new"
  discard <| git repo #["add", "-A"]
  discard <| git repo #["commit", "-qm", "move the child and drop its requirement"]
  let head ← git repo #["rev-parse", "HEAD"]
  manifest project #[gitDep "mono" revision.toUpper, localDep "child" ".lake/packages/mono/../mono/old" true]
  let r ← report project
  expect (packages r).size 2
  let child ← pkg r "child"
  expectField child "path" (chain ["demo", "mono", "child"])
  expectField child "toolchain" older
  expectField (← pkg r "mono") "toolchain" target
  expect ((findings r).map fun f => (str f "kind", str f "package")) #[("checkout_revision_mismatch", "child"),
    ("toolchain_mismatch", "child"), ("checkout_revision_mismatch", "mono")]
  check ((str (findings r)[0]! "message").contains head) "checkout mismatch must identify the current commit"
  expect (← git repo #["rev-parse", "HEAD"]) head

def run : IO Unit := do
  runCase "physical and symlinked Git checkout" physicalAlias
  runCase "ambiguous shared checkout" sharedCheckout
  runCase "case-sensitive and case-insensitive checkout spellings" checkoutSpelling
  runCase "relative requirements at shared checkout revisions" relativeSourceAtRevision
  runCase "nontraversable parent component" untraversablePrefix
  runCase "external committed links" externalCommittedLinks
  runCase "pin inherited at committed link destination" lockedDestination
  runCase "parent after committed link" monorepoParent
  runCase "workspace link targets inside a locked repository" workspaceTargets
  runCase "workspace link cycle across a Git boundary" cycleDeadline
  runCase "crossing locked repository boundaries" crossRepository
  runCase "stale checkout with locally available locked commit" staleCommit

end LeanTests.Graph.Paths
