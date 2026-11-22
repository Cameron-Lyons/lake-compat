import LakeCompat.Report
import LakeCompat.Verify.Process
import Tools.Process

open Lean System

namespace Tools.ReleaseCompat

private def target := (include_str "../lean-toolchain").trimAscii.copy
private def olderTarget := "leanprover/lean4:v4.34.1"
private def cliRepository := "https://github.com/leanprover/lean4-cli"
private def baseRevision := "e92c9f15fdfacc8536f31cfb3b7ad26c3c8cd204"
private def candidateRevision := "c24fe6078c630e89b489bffe59b3e20ac30d12c8"
private def upgradeRevision := "50810f72af7618bee62fe9800dc26e806c188703"

private def usage := "Usage: lake exe release-compat CLI [REPOSITORY REVISION]\n\
Check the packaged CLI against pinned public projects and Reservoir.\n\
Requires Git, network access, and Lean 4.34.1 plus the project toolchain in elan.\n\
With REPOSITORY and REVISION, also test installing this package from Git."

private structure Context where
  cli : FilePath
  work : FilePath
  env : LakeCompat.Verify.Environment

private def ensure (condition : Bool) (message : String) : IO Unit :=
  unless condition do throw <| IO.userError message

private def command (ctx : Context) (label cmd : String) (args : Array String)
    (cwd : Option FilePath := none) (timeoutMs := 120000) : IO String := do
  let output ← try
    Process.run {cmd, args, cwd := some (cwd.getD ctx.work), env := ctx.env} timeoutMs
  catch e => throw <| IO.userError s!"{label}: {e}"
  unless output.exitCode == 0 do
    let diagnostic := LakeCompat.Verify.boundedLogTail (output.stdout ++ "\n" ++ output.stderr)
    throw <| IO.userError s!"{label} exited with code {output.exitCode}.\n{diagnostic.trimAscii}"
  return output.stdout

private def checkedReport (label contents : String) : IO Json := do
  let report ← IO.ofExcept ((Json.parse contents).mapError (s!"{label}: {·}"))
  ensure (report.getObjValD "schema_version" == toJson (1 : Nat))
    s!"{label}: expected schema_version 1."
  return report

private def report (ctx : Context) (label : String) (args : Array String) : IO Json := do
  checkedReport label (← command ctx label ctx.cli.toString (args.push "--json") (timeoutMs := 900000))

private def checkout (ctx : Context) (url revision : String) (dir : FilePath) : IO Unit := do
  discard <| command ctx "Initialize a release fixture" "git" #["init", "-q", "--", dir.toString]
  discard <| command ctx "Fetch a pinned release fixture" "git"
    #["-C", dir.toString, "fetch", "-q", "--depth=1", "--", url, revision]
  discard <| command ctx "Check out a pinned release fixture" "git"
    #["-C", dir.toString, "checkout", "-q", "--detach", "FETCH_HEAD"]

private def readJson (path : FilePath) : IO Json := do
  IO.ofExcept ((Json.parse (← IO.FS.readFile path)).mapError (s!"{path}: {·}"))

private def stringField (json : Json) (field : String) : IO String :=
  IO.ofExcept ((json.getObjValD field).getStr?.mapError (s!"Expected string field {field}: {·}"))

private def demo (ctx : Context) : IO Unit := do
  IO.println "Verify the bundled project on Lean 4.34.1"
  let result ← report ctx "Bundled project verification" #[olderTarget, "--demo", "--verify"]
  ensure (LakeCompat.Report.string (result.getObjValD "verification") "status" == "passed")
    s!"The bundled project did not pass verification: {result.compress}"

private def realWorld (ctx : Context) : IO Unit := do
  IO.println "Trace the RealWorld transitive JSON pin conflict"
  let project := ctx.work / "realworld"
  checkout ctx "https://github.com/saviorand/lean-realworld-example-app"
    "483d47e1d3c797098ac55cbf23a95072f7a6b6e5" project
  let lock ← readJson (project / "lake-manifest.json")
  let packages ← IO.ofExcept ((lock.getObjValD "packages").getArr?)
  for name in ["jose", "json"] do
    let some pkg := packages.find? (fun pkg => LakeCompat.Report.string pkg "name" == name)
      | throw <| IO.userError s!"The pinned RealWorld lock does not contain {name}."
    checkout ctx (← stringField pkg "url") (← stringField pkg "rev") (project / ".lake/packages" / name)
  let result ← report ctx "RealWorld dependency scan" #[target, "--project", project.toString]
  ensure ((LakeCompat.Report.rows result "findings").any fun finding =>
    LakeCompat.Report.string finding "kind" == "revision_conflict" &&
    finding.getObjValD "path" == toJson ["realworld", "jose", "json"] &&
    finding.getObjValD "requested_revision" != finding.getObjValD "selected_revision")
    s!"The scan did not report the expected RealWorld revision conflict: {result.compress}"

/-- JSON strings are valid TOML basic strings for repository URLs and revisions. -/
private def dependencyConfig (name repository revision : String) : String :=
  "[[require]]\nname = " ++ (toJson name).compress ++ "\ngit = " ++
    (toJson repository).compress ++ "\nrev = " ++ (toJson revision).compress ++ "\n"

private def upgrade (ctx : Context) : IO Unit := do
  IO.println s!"Look up Lean 4.35.0 candidates for lean4-cli and verify an upgrade to {target}"
  let project := ctx.work / "consumer"
  IO.FS.createDir project
  IO.FS.writeFile (project / "lakefile.toml") <|
    "name = \"consumer\"\ndefaultTargets = [\"App\"]\n[[lean_lib]]\nname = \"App\"\n" ++
    dependencyConfig "Cli" cliRepository baseRevision
  IO.FS.writeFile (project / "lean-toolchain") (olderTarget ++ "\n")
  IO.FS.writeFile (project / "App.lean") "import Cli\n"
  discard <| command ctx "Resolve the original consumer lock" "elan"
    #["run", olderTarget, "lake", "--keep-toolchain", "update"] (some project) 600000
  let lockFile := project / "lake-manifest.json"
  let originalLock ← IO.FS.readFile lockFile
  let suggested ← report ctx "Live Reservoir lookup" #["4.35.0", "--project", project.toString, "--suggest"]
  let suggestion := (suggested.getObjValD "suggestions").getObjValD "Cli"
  ensure (LakeCompat.Report.string suggestion "status" == "found" &&
    (LakeCompat.Report.rows suggestion "candidates").any fun candidate =>
      LakeCompat.Report.string candidate "revision" == candidateRevision)
    s!"Reservoir did not return the expected Lean 4.35.0 candidate: {suggestion.compress}"
  let verified ← report ctx "Git-backed upgrade trial" #[target, "--project", project.toString,
    "--try-revision", "Cli=" ++ upgradeRevision, "--build-target", "App"]
  let pins := Json.mkObj [("Cli", .str upgradeRevision)]
  let verification := verified.getObjValD "verification"
  ensure (LakeCompat.Report.string verification "status" == "passed" &&
    verified.getObjValD "trial_revisions" == pins && verification.getObjValD "checked_revisions" == pins)
    s!"The upgrade trial did not verify the expected Git revision: {verified.compress}"
  ensure ((← IO.FS.readFile lockFile) == originalLock) "The upgrade trial changed the original consumer lock."
  let original ← readJson lockFile
  ensure ((LakeCompat.Report.rows original "packages").any fun pkg =>
    LakeCompat.Report.string pkg "name" == "Cli" && LakeCompat.Report.string pkg "rev" == baseRevision)
    "The consumer no longer selects its original lean4-cli revision."

private def downstream (ctx : Context) (repository revision : String) : IO Unit := do
  IO.println "Install the candidate from Git as a downstream Lake dependency"
  let project := ctx.work / "install"
  IO.FS.createDir project
  IO.FS.writeFile (project / "lean-toolchain") (target ++ "\n")
  IO.FS.writeFile (project / "lakefile.toml") <|
    "name = \"downstream\"\n" ++ dependencyConfig "lake-compat" repository revision
  let lake := #["run", target, "lake"]
  discard <| command ctx "Install the candidate from Git" "elan"
    (lake ++ #["--keep-toolchain", "update", "lake-compat"]) (some project) 600000
  let installed ← command ctx "Run the installed CLI" "elan"
    (lake ++ #["exe", "lake-compat", "--version"]) (some project) 600000
  let packaged ← command ctx "Read the packaged CLI version" ctx.cli.toString #["--version"]
  ensure (installed == packaged) s!"Installed and packaged CLI versions differ: {repr installed} versus {repr packaged}."
  let contents ← command ctx "Scan from the downstream Lake project" "elan"
    (lake ++ #["exe", "lake-compat", target, "--json"]) (some project) 600000
  let result ← checkedReport "Downstream dependency scan" contents
  ensure (LakeCompat.Report.string result "project" == "downstream" &&
    (LakeCompat.Report.rows result "packages").any fun pkg => LakeCompat.Report.string pkg "name" == "lake-compat")
    s!"The installed CLI did not scan the downstream project: {result.compress}"

def run (args : List String) : IO UInt32 := do
  if args == ["--help"] || args == ["-h"] then
    IO.println usage
    return 0
  let (cli, install) ← match args with
    | [cli] => pure (cli, none)
    | [cli, repository, revision] => pure (cli, some (repository, revision))
    | _ => IO.eprintln usage; return 2
  try
    ensure (!Platform.isWindows) "Release compatibility checks require a POSIX host."
    let cli ← IO.FS.realPath cli
    ensure ((← cli.metadata).type == .file) s!"Expected an executable file at {cli}."
    IO.FS.withTempDir fun path => do
      let work ← IO.FS.realPath path
      let ctx := { cli, work, env := ← LakeCompat.Verify.trialEnvironment target work : Context }
      demo ctx
      realWorld ctx
      upgrade ctx
      if let some (repository, revision) := install then downstream ctx repository revision
    IO.println "Release compatibility checks passed."
    return 0
  catch e =>
    IO.eprintln s!"release-compat: {e}"
    return 1

end Tools.ReleaseCompat

def main (args : List String) : IO UInt32 := Tools.ReleaseCompat.run args
