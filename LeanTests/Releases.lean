import LakeCompat.Releases

open Lean LakeCompat.Releases

namespace LeanTests.Releases

private def target := "leanprover/lean4:v4.35.0"
private def older := "leanprover/lean4:v4.34.0"
private def repo := "leanprover-community/mathlib4"
private def url := "https://github.com/" ++ repo
private def endpoint := "https://reservoir.lean-lang.org/api/v1/packages/leanprover-community/mathlib"

private def rev (c : Char) (length := 40) : String := String.ofList (List.replicate length c)

private def version (revision : String) (tag : Option String) (tc := target) (date := "2026-10-01") : Json :=
  Json.mkObj [("revision", revision), ("tag", toJson tag), ("toolchain", tc), ("date", date)]

private def build (revision : String) (extra : List (String × Json) := [])
    (passed := true) (tc := target) : Json :=
  Json.mkObj (([("revision", revision), ("toolchain", tc), ("built", passed)] : List (String × Json)) ++ extra)

private def metadata (name := "mathlib") (fullName := repo) (subDir : Option String := none) : Json :=
  let source := Json.mkObj [("host", "github"), ("fullName", fullName), ("subDir", toJson subDir)]
  Json.mkObj [("name", name), ("sources", .arr #[source])]

private def dataRows (rows : Array Json) : Json := Json.mkObj [("data", .arr rows)]

/-- Match complete URLs so an unexpected host, path or query fails the test. -/
private def fixture (base : String) (packageInfo : Json) (versions builds : Array Json) : Fetch := fun request =>
  if request == base then pure packageInfo
  else if request == base ++ "/versions" then pure (dataRows versions)
  else if request == base ++ "/builds" then pure (dataRows builds)
  else throw (.http 418)

private def check (ok : Bool) (label : String) : IO Unit :=
  unless ok do throw (IO.userError ("Reservoir: " ++ label))

private def status (json : Json) := json.getObjValD "status"
private def rows (json : Json) := ((json.getObjValD "candidates").getArr?).toOption.getD #[]

private def testRanking : IO Unit := do
  let versions := #[
    version (rev 'a') (some "stale") target "2025", version (rev 'a') (some "v4.35"),
    version (rev 'a') (some "ignore\nthis") target "2027",
    version (rev 'b') (some "v0.1") older, version (rev 'c') (some "v0.2"),
    version (rev 'd') (some "v0.3"), version (rev 'e') (some "v0.4") older,
    version (rev 'f') none, version (rev '0') none, version "short" none]
  let builds := #[
    build (rev 'A') [("requiredUpdate", false), ("tested", true), ("url", "https://example.org/build")],
    build (rev 'b') [("requiredUpdate", true)],
    build (rev 'd') [("runAt", "2026-10-01")], build (rev 'd') [("runAt", "2026-11-01")] false,
    build (rev 'e') [] true older, build (rev '0') [("tested", false)]]
  let report ← suggestVersions (url ++ ".git") target (some "mathlib")
    (fetch := fixture endpoint (metadata) versions builds)
  check (status report == "found") report.compress
  let candidates := rows report
  check (candidates.map (·.getObjValD "revision") == #['a','b','c','f'].map (fun c => Json.str (rev c)))
    "successful builds, clean locks, tags and exact declarations must rank in order"
  check (candidates[0]! == Json.mkObj [
    ("revision", rev 'a'), ("tag", "v4.35"), ("evidence", "built"), ("toolchain", target),
    ("tested", true), ("required_update", false), ("build_url", "https://example.org/build")])
    "tested build evidence lost its tag, URL or required-update flag"
  check (candidates[1]!.getObjValD "required_update" == .bool true) "updated builds must remain labelled"
  check (candidates[2]!.getObjValD "evidence" == "declared" &&
    (candidates[2]!.getObjValD "tested").isNull) "declared-only evidence must not imply tests passed"
  check ((candidates[3]!.getObjValD "tag").isNull) "an untagged revision must keep a null tag"

private def testTruncation : IO Unit := do
  let builds := #[
    build (rev '1') [("requiredUpdate", false)], build (rev '2') [("requiredUpdate", true)],
    build (rev '3'), build (rev '4'), build (rev '5'), build (rev '6'), build (rev '7' 64)]
  let base := "https://reservoir.lean-lang.org/api/v1/packages/leanprover-community/mathlib4"
  let report ← suggestVersions ("git@github.com:" ++ repo ++ ".git") target
    (fetch := fixture base (metadata) #[] builds)
  let candidates := rows report
  check (candidates.map (·.getObjValD "revision") ==
    [rev '1', rev '7' 64, rev '6', rev '5', rev '4'].toArray.map Json.str)
    "limit five by update requirement and deterministic full-SHA tie breaker"
  check (report.getObjValD "message" == "Showing 5 of 7 candidate revisions.") "count every eligible commit"

private def testToolchainIdentity : IO Unit := do
  let versions := #[version (rev 'a') none "4.35.0", version (rev 'b') none "v4.35.0",
    version (rev 'c') none "other/lean4:v4.35.0", version (rev 'd') none "4.35.0-rc1"]
  let builds := #[build (rev 'a') [] false "leanprover/lean4:4.35.0",
    build (rev 'e') [] true "4.35.0"]
  let report ← suggestVersions url "v4.35.0" (some "mathlib")
    (fetch := fixture endpoint metadata versions builds)
  let candidates := rows report
  check (candidates.map (·.getObjValD "revision") == #[rev 'e', rev 'b'].map Json.str)
    "equivalent toolchain spellings must match declarations and both successful and failed builds"
  check (candidates.all (fun row => row.getObjValD "toolchain" == target))
    "toolchain identity in candidates must use the canonical target"

private def testTiedEvidence : IO Unit := do
  let versions := #[version (rev 'a') (some "alpha"), version (rev 'a') (some "beta"),
                    version (rev 'b') none]
  let builds := #[build (rev 'a'), build (rev 'a') [("tested", true)],
                  build (rev 'b'), build (rev 'b') [] false].map
    (·.setObjVal! "runAt" "2026-11-01T12:00:00Z")
  let lookup := fun vs bs => suggestVersions url target (some "mathlib")
    (fetch := fixture endpoint (metadata) vs bs)
  let report ← lookup versions builds
  check (report == (← lookup versions.reverse builds.reverse))
    "equally recent evidence must rank independently of registry response order"
  let candidates := rows report
  check (candidates.size == 1 && candidates[0]!.getObjValD "revision" == rev 'a' &&
    candidates[0]!.getObjValD "tested" == .bool true && candidates[0]!.getObjValD "tag" == "beta")
    "a simultaneous failure disqualifies its revision, and a confirmed test is preferred"

private def testIdentity : IO Unit := do
  let calls ← IO.mkRef #[]
  let metaOnly (m : Json) : Fetch := fun req => do
    calls.modify (·.push req)
    return m
  for repoUrl in ["https://github.com.evil.test/name/repo", "file:///tmp/repo",
      "https://token@github.com/name/repo", "https://github.com/name/..",
      "https://github.com:444/name/repo"] do
    let report ← suggestVersions repoUrl target (fetch := metaOnly (metadata))
    check (status report == "unsupported") repoUrl
  for invalid in ["..", "a/b", "\nname"] do
    let report ← suggestVersions url target (scope := some invalid) (fetch := metaOnly (metadata))
    check (status report == "unsupported") "invalid scope"
  check (← calls.get).isEmpty "an unrecognized source must never reach the registry"

  for packageInfo in [metadata (fullName := "other/mathlib4"), metadata (subDir := some "other"),
               metadata (name := "other"), Json.mkObj [("name", "mathlib"), ("sources", .arr #[])]] do
    calls.set #[]
    let report ← suggestVersions url target (some "mathlib") (fetch := metaOnly packageInfo)
    check (status report == "unsupported" && (← calls.get) == #[endpoint])
      "mismatched package, repository or directory must stop before fetching revisions"

  let checker := "leanprover/lean4checker"
  let checkerEndpoint := "https://reservoir.lean-lang.org/api/v1/packages/leanprover/Lean4Checker"
  let checkerFetch := fixture checkerEndpoint (metadata "lean4checker" checker) #[] #[build (rev 'a')]
  let report ← suggestVersions ("https://github.com/" ++ checker) target (some "Lean4Checker") (fetch := checkerFetch)
  check (status report == "found" && (rows report).map (·.getObjValD "revision") == #[Json.str (rev 'a')])
    "Lake lock names can differ in case from the same Reservoir package"

  let base := "https://reservoir.lean-lang.org/api/v1/packages/my%20scope/my%2Bpkg"
  let fetch := fixture base (metadata "my+pkg" repo.toUpper (some "src/library")) #[] #[]
  for baseUrl in ["ssh://git@github.com/", "ssh://git@github.com:22/", "https://github.com:443/"] do
    let report ← suggestVersions (baseUrl ++ repo ++ "/") target
      (some "my+pkg") (some "my scope") (some "./src//library/") fetch
    check (status report == "no_match") "encoded package path, default port and normalized subdirectory"

private def testFailures : IO Unit := do
  for (err, detail) in [(RegistryError.http 404, "absent"), (.http 503, "503"),
      (.invalid, "invalid"), (.transport, "reach")] do
    let report ← suggestVersions url target (fetch := fun _ => throw err)
    let message := (report.getObjValD "message").getStr?.toOption.getD ""
    check (status report == "unavailable" && message.contains detail && (rows report).isEmpty)
      "unexpected registry error classification"
  for bad in [Json.null, Json.mkObj [("sources", "oops")]] do
    let report ← suggestVersions url target (fetch := fun _ => pure bad)
    check (status report == "unavailable") "malformed metadata must not look like an empty index"
  let badName ← suggestVersions url target (some "mathlib")
    (fetch := fun _ => pure ((metadata).setObjVal! "name" .null))
  check (status badName == "unavailable") "invalid package metadata must not look like a different package"
  for bad in [Json.mkObj [], dataRows #[.null], dataRows #[((build (rev 'c')).setObjVal! "runAt" 7)],
      dataRows #[Json.mkObj [("revision", rev 'c'), ("toolchain", target)]],
      dataRows #[((build (rev 'c')).setObjVal! "built" "false")]] do
    let report ← suggestVersions url target (some "mathlib") (fetch := fun req =>
      pure (if req == endpoint then metadata else if req.endsWith "/versions" then dataRows #[] else bad))
    check (status report == "unavailable") "malformed records must not produce no_match"

private def testUnreportedBuilds : IO Unit := do
  let unreported := fun revision => (build revision [("runAt", "2026-11-02")]).setObjVal! "built" .null
  let versions := #[version (rev 'a') none, version (rev 'c') none]
  let builds := #[build (rev 'b'), build (rev 'c') [] false,
    unreported (rev 'a'), unreported (rev 'b'), unreported (rev 'c'), unreported (rev 'd')]
  let report ← suggestVersions url target (some "mathlib") (fetch := fixture endpoint metadata versions builds)
  check (status report == "found" && (report.getObjValD "unavailable_history").isNull) report.compress
  let candidates := rows report
  check (candidates.map (·.getObjValD "revision") == #[rev 'b', rev 'a'].map Json.str &&
    candidates.map (·.getObjValD "evidence") == #["built", "declared"].map Json.str)
    "unreported outcomes must preserve recorded successes, failures and declarations"

private def testRegistryResponses : IO Unit := do
  let lookup := fun (pkg versions builds : Json) =>
    suggestVersions url target (some "mathlib") (fetch := fun req =>
      pure (if req == endpoint then pkg else if req.endsWith "/versions" then versions else builds))
  let versions := #[version (rev 'a') (some "v1")]
  let builds := #[build (rev 'b')]
  let expected ← lookup metadata (dataRows versions) (dataRows builds)
  let alternative ← lookup (Json.mkObj [("data", metadata)]) (.arr versions) (.arr builds)
  check (status alternative == "found" && alternative == expected)
    "bare and wrapped registry payloads must return the same candidates"
  let absent := Json.mkObj [("error", Json.mkObj [("status", 404), ("message", "package absent")])]
  let report ← lookup metadata (.arr versions) absent
  check (status report == "found" && report.getObjValD "unavailable_history" == "builds" &&
    (rows report)[0]!.getObjValD "evidence" == "declared")
    "an API error envelope must preserve independent version evidence"

private def testPartialHistory : IO Unit := do
  let lookup := fun (versions builds : Except RegistryError Json) =>
    suggestVersions url target (some "mathlib") (fetch := fun req =>
      if req == endpoint then pure metadata
      else if req == endpoint ++ "/versions" then ofExcept versions
      else if req == endpoint ++ "/builds" then ofExcept builds
      else throw (.http 418))
  let versionRows := dataRows #[version (rev 'a') (some "v1")]
  let buildRows := dataRows #[build (rev 'b') [("requiredUpdate", true)]]
  let checkPartial := fun report evidence missing => do
    check (status report == "found" && (rows report).size == 1) report.compress
    check ((rows report)[0]!.getObjValD "evidence" == evidence) "incorrect partial evidence"
    check (report.getObjValD "unavailable_history" == missing)
      "partial results must identify the missing history"
  checkPartial (← lookup (.error (.http 503)) (.ok buildRows)) "built" "versions"
  checkPartial (← lookup (.ok versionRows) (.error .transport)) "declared" "builds"
  let empty ← lookup (.ok (dataRows #[])) (.error .transport)
  check (status empty == "unavailable") "empty partial data cannot establish no_match"
  let malformed ← lookup (.ok versionRows) (.ok (dataRows #[build (rev 'a') [] false, Json.null]))
  check (status malformed == "unavailable") "invalid build data cannot promote a failed revision"

def run : IO Unit := do
  testRanking
  testTruncation
  testToolchainIdentity
  testTiedEvidence
  testIdentity
  testFailures
  testUnreportedBuilds
  testRegistryResponses
  testPartialHistory

end LeanTests.Releases
