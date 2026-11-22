import LeanTests.Graph.Fixture

open Lean System LeanTests.Support

namespace LeanTests.Graph.Manifests

private def shortestChains (project : FilePath) : IO Unit := do
  let deps := packagesDir project
  for name in ["a", "b", "shared", "leaf"] do pin (deps / name)
  manifest (deps / "leaf")
  let leaf ← commitMetadata (deps / "leaf")
  manifest (deps / "shared") #[gitDep "leaf" leaf] [("fixedToolchain", true)]
  pin (deps / "shared") "4.24.1"
  let shared ← commitMetadata (deps / "shared")
  manifest (deps / "a") #[gitDep "shared" "cccc", gitDep "leaf" "stale" true]
  manifest (deps / "b") #[gitDep "shared" shared]
  manifest project #[gitDep "a" (← commitMetadata (deps / "a")), gitDep "b" (← commitMetadata (deps / "b")),
    gitDep "shared" shared true, gitDep "leaf" leaf true]
  let r ← report project
  expectField r "root_toolchain" older
  expectKinds r #["revision_conflict", "toolchain_mismatch"]
  let conflict := (findings r)[0]!
  expectField conflict "path" (chain ["demo", "a", "shared"])
  expectField conflict "requested_revision" "cccc"
  expectField conflict "selected_revision" shared
  expectField (← pkg r "leaf") "path" (chain ["demo", "a", "shared", "leaf"])

private def remoteSpellings (project : FilePath) : IO Unit := do
  let parent := project / "parent"
  let dep := packagesDir project / "leaf"
  manifest dep
  pin dep
  let rev ← commitMetadata dep
  let rootLock := gitDep "leaf" rev true
  manifest project #[localDep "parent" "parent", rootLock]
  pin parent
  for url in ["https://github.com/Example/Leaf/", "ssh://git@github.com/example/leaf.git",
      "git@github.com:example/leaf.git", "https://github.com/another/leaf.git"] do
    manifest parent #[gitDep "leaf" rev (fields := [("url", url)])]
    let r ← report project
    if url.contains "another" then
      let f ← onlyFinding r
      expectField f "kind" "source_conflict"
      expectField f "path" (chain ["demo", "parent", "leaf"])
      expectField f "requested_revision" rev
      expectField f "selected_revision" rev
      for remote in [url, str rootLock "url"] do
        check ((str f "message").contains remote) s!"missing {remote} in {f.compress}"
    else expectKinds r #[]

private def customManifest (project : FilePath) : IO Unit := do
  let vendor := project / "vendor" / "mono"
  let lib := vendor / "child"
  manifest lib #[localDep "local" "../../../local"] (file := "custom.json")
  pin vendor
  manifest project #[
    gitDep "mono" (← commitMetadata vendor) (fields := [("subDir", "child"), ("manifestFile", "custom.json")]),
    localDep "local" "local" true] [("packagesDir", "vendor")]
  manifest (project / "local")
  let r ← report project
  expectKinds r #[]
  let mono ← pkg r "mono"
  expectField mono "checkout" lib.toString
  expectField mono "toolchain_source" "repository"
  let child ← pkg r "local"
  expectField child "toolchain_source" "workspace"
  expectField child "path" (chain ["demo", "mono", "local"])

private def gitSubdirectories (project : FilePath) : IO Unit := do
  let repo := packagesDir project / "child"
  pin repo
  manifest repo
  manifest (repo / "lib")
  manifest (repo / "nested")
  manifest (repo / "nested/app")
  link (repo / "alias") "nested/app"
  let rev ← commitMetadata repo
  IO.FS.removeFile (repo / "alias")
  link (repo / "alias") "lib"
  let cases : List (Json × String × Array String) := [
    (.null, ".", #[]), ("lib/", "./lib//.", #[]), ("lib", ".", #["source_conflict"]),
    ("nested/app", "alias", #[]), ("nested", "alias/..", #[]), (.null, "lib/..", #[]),
    ("lib", "alias", #["source_conflict"]), (.null, "absent/..", #["source_conflict"])]
  for (selected, requested, expected) in cases do
    manifest (project / "parent") #[gitDep "child" rev (fields := [("subDir", requested)])]
    manifest project #[localDep "parent" "parent", gitDep "child" rev true [("subDir", selected)]]
    expectKinds (← report project) expected

private def externalPin (project : FilePath) : IO Unit := do
  let parent := project.parent.get!
  manifest project #[localDep "external" "../external"]
  pin parent
  manifest (parent / "external")
  expectKinds (← report project) #["unknown_toolchain"]

private def enclosingToolchain (workspace : FilePath) : IO Unit := do
  let project := workspace / "apps/nested"
  let localPkg := localDep "local" "src/local"
  let shared := localDep "shared" "../../libs/shared"
  manifest (project / "src/local")
  manifest (workspace / "libs/shared")
  manifest project #[localPkg, shared]
  let inherited ← report project
  expectField inherited "root_toolchain" older
  expectKinds inherited #[]
  for name in ["local", "shared"] do
    let entry ← pkg inherited name
    expectField entry "toolchain" older
    expectField entry "toolchain_source" "workspace"
  pin (workspace / "libs") target
  let checkout := packagesDir project / "locked"
  manifest checkout
  manifest project #[localPkg, shared, gitDep "locked" (← commitMetadata checkout)]
  let overridden ← report project
  expectField (← pkg overridden "shared") "toolchain_source" "ancestor"
  expectField (← pkg overridden "shared") "toolchain" target
  let missing ← onlyFinding overridden
  expectField missing "package" "locked"
  expectField missing "kind" "unknown_toolchain"
  pin project target
  let nearer ← report project
  expectField nearer "root_toolchain" target
  expectField (← pkg nearer "local") "toolchain" target

private def missingAncestry (project : FilePath) : IO Unit := do
  let orphan := packagesDir project / "orphan"
  pin orphan "4.20.0"
  manifest orphan
  manifest project #[gitDep "parent" "p", gitDep "orphan" (← commitMetadata orphan) true]
  let r ← report project
  let actual := Json.mkObj ((findings r).toList.map fun f => (str f "kind", f.getObjValD "path"))
  expect actual (Json.mkObj [("missing_checkout", chain ["demo", "parent"]),
    ("toolchain_mismatch", chain []), ("unknown_path", chain [])])

private def missingLocks (project : FilePath) : IO Unit := do
  manifest project #[localDep "a" "a", localDep "b" "b" (fields := [("manifestFile", .null)])]
  for name in ["a", "b"] do pin (project / name)
  expectKinds (← report project) #["missing_manifest", "missing_manifest"]

private def nullDefaults (project : FilePath) : IO Unit := do
  manifest project #[localDep "parent" "parent" (fields := [
    ("manifestFile", .null), ("copy", .null), ("scope", .null)]), localDep "leaf" "leaf" true]
  manifest (project / "parent") #[localDep "leaf" "../leaf"] [("fixedToolchain", .null)]
  manifest (project / "leaf") (fields := [("packages", .null)])
  let r ← report project
  expectKinds r #[]
  let parent ← pkg r "parent"
  expectField parent "fixed_toolchain" false
  expectField parent "scope" ""
  expectField (← pkg r "leaf") "path" (chain ["demo", "parent", "leaf"])

private def orphanEdges (project : FilePath) : IO Unit := do
  manifest project #[localDep "orphan" "orphan" true, localDep "leaf" "leaf"]
  manifest (project / "orphan") #[localDep "leaf" "../other", gitDep "absent" "bb"]
  manifest (project / "leaf")
  let edges := (findings (← report project)).filter (fun f => str f "parent" == "orphan")
  expect (edges.map fun f => (str f "kind", str f "package"))
    #[("unlocked_dependency", "absent"), ("source_conflict", "leaf")]
  for f in edges do expectField f "path" (chain [])
  let output ← cli #[target, "--project", project.toString]
  expect output.exitCode.toNat 0
  check (output.stdout.contains "orphan -> leaf (path to orphan unknown)") output.stdout

private def invalidNestedLock (project : FilePath) : IO Unit := do
  manifest project #[localDep "a" "a"]
  pin (project / "a")
  manifest (project / "a") #[Json.mkObj [("name", "b"), ("type", "path"), ("dir", ".")]]
  IO.FS.createDirAll (project / ".lake")
  let file := project / ".lake" / "package-overrides.json"
  let overrides := Json.mkObj [("schemaVersion", "1.3.0"), ("packages", .arr #[])]
  writeJson file overrides
  expectKinds (← report project) #["invalid_manifest"]
  for value in [overrides.setObjVal! "packages" (.arr #[localDep "a" "elsewhere"]), Json.mkObj []] do
    writeJson file value
    expectKinds (← report project) #["active_overrides", "invalid_manifest"]

private def inheritedEntries (project : FilePath) : IO Unit := do
  manifest project #[localDep "parent" "parent"]
  let parent := project / "parent"
  let direct := localDep "library" "library"
  let inherited := localDep "library" "library" true
  manifest parent #[inherited]
  expectKinds (← report project) #[]
  for entries in [#[inherited, direct], #[direct, inherited], #[inherited, inherited],
      #[inherited.setObjVal! "dir" (.bool false)]] do
    manifest parent entries
    expectKinds (← report project) #["invalid_manifest"]

private def legacyManifest (project : FilePath) : IO Unit := do
  let repo := project / "lake-packages" / "my-lib"
  let lib := repo / "sub"
  manifest lib (fields := [("version", 6)])
  pin lib
  let dep := Json.mkObj [("name", "«my-lib»"), ("url", "https://github.com/example/my-lib"),
    ("rev", ← commitMetadata repo), ("inputRev?", "main"), ("subDir?", "sub"), ("inherited", false),
    ("opts", Json.mkObj [])]
  manifest project #[Json.mkObj [("git", dep)]] [("packagesDir", "lake-packages"), ("version", 5)]
  let r ← report project
  expectKinds r #[]
  expectField (← pkg r "my-lib") "input_rev" "main"

private def versionFormats (project : FilePath) : IO Unit := do
  for version in ["0.5.0", "0.6.0-preview", "0.7.0", "1.3.0-rc1"] do
    manifest project (fields := [("version", version)])
    expectKinds (← report project) #[]
  for version in ["0.4.99", "1.3.0-", "1.3.0-pre release", "2.0.0"] do
    manifest project (fields := [("version", version)])
    let err := str (← report project 2) "error"
    check (err.contains "unsupported Lake manifest version") err

private def quotedName (project : FilePath) : IO Unit := do
  let repo := packagesDir project / "a«b"
  manifest repo (fields := [("name", "«a«b»")])
  pin repo
  manifest project #[gitDep "«a«b»" (← commitMetadata repo)]
  let r ← report project
  expectKinds r #[]
  expectField (packages r)[0]! "name" "a«b"

private def copiedCycle (project : FilePath) : IO Unit := do
  let a := packagesDir project / "a"
  let b := project / "src" / "b"
  manifest project #[localDep "a" "src/a" (fields := [("copy", true)]), localDep "b" "src/b" true]
  manifest a #[localDep "b" "../../../src/b"]
  manifest b #[localDep "a" "../a"]
  pin a
  pin b
  let r ← report project
  expectKinds r #[]
  expectField (← pkg r "a") "checkout" a.toString

private def absentRequirement (project : FilePath) : IO Unit := do
  manifest project #[localDep "a" "a"]
  pin (project / "a")
  manifest (project / "a") #[gitDep "b" "bb"]
  let f ← onlyFinding (← report project)
  expectField f "kind" "unlocked_dependency"
  expectField f "path" (chain ["demo", "a", "b"])

private def invalidRoot (project : FilePath) : IO Unit := do
  let checkError := do
    let msg := str (← report project 2) "error"
    check (msg.contains "lake-manifest.json") msg
  checkError
  for entry in [Json.mkObj [("type", "path"), ("dir", "x")], gitDep "a" "aa" (fields := [("scope", .arr #[])]),
      localDep "a" "a" (fields := [("inherited", .null)]),
      gitDep "../a" "aa", Json.mkObj [("name", "a"), ("type", "git"), ("inherited", "false")]] do
    manifest project #[entry]
    checkError

def run : IO Unit := do
  runCase "shortest chains and revision conflicts" shortestChains
  runCase "equivalent Git remotes" remoteSpellings
  runCase "local dependencies and custom Git manifest" customManifest
  runCase "equivalent Git subdirectories" gitSubdirectories
  runCase "external package toolchain boundary" externalPin
  runCase "nested project inherits its enclosing toolchain" enclosingToolchain
  runCase "unreachable dependency" missingAncestry
  runCase "missing child locks" missingLocks
  runCase "null Lake manifest defaults" nullDefaults
  runCase "unreachable parent edges" orphanEdges
  runCase "invalid nested lock and active overrides" invalidNestedLock
  runCase "validation of inherited manifest entries" inheritedEntries
  runCase "legacy tagged manifest" legacyManifest
  runCase "semantic manifest versions" versionFormats
  runCase "escaped package name" quotedName
  runCase "copied path and dependency cycle" copiedCycle
  runCase "absent dependency" absentRequirement
  runCase "invalid root manifest" invalidRoot

end LeanTests.Graph.Manifests
