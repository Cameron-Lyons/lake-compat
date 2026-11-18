import LakeCompat.Report
import LeanTests.Support

namespace LeanTests.Distribution
open Lean System LakeCompat LeanTests.Support

/-- Stand in for tar failing after it has started to write its output. -/
def failTar (args : List String) : IO UInt32 := do
  let "-czhf" :: path :: _ := args | throw <| IO.userError "unexpected tar arguments"
  IO.FS.writeFile path "partial"
  IO.eprintln "fixture tar failure"
  return 1

private def archiveTest (root temp : FilePath) : IO Unit := do
  let out := temp / "release archives"
  let archive := out / s!"lake-compat-{Platform.target}.tar.gz"
  let packageTool ← binary "package-release"
  let pack := fun (env : Array (String × Option String)) => IO.Process.output {
    cmd := packageTool.toString, args := #["release archives"], cwd := temp, env }
  let result ← pack #[]
  check (result.exitCode == 0 && result.stdout.trimAscii == archive.toString.toSlice)
    s!"package-release failed: {result.stderr} {result.stdout}"
  let exe := if Platform.isWindows then "lake-compat.exe" else "lake-compat"
  let members ← IO.Process.run { cmd := "tar", args := #["-tzf", archive.toString] }
  check ((members.lines.map (·.trimAscii.copy)).toList == [exe, "LICENSE"]) s!"unexpected archive: {members}"
  let extracted := temp / "unpacked"
  IO.FS.createDir extracted
  discard <| IO.Process.run { cmd := "tar", args := #["-xzf", archive.toString, "-C", extracted.toString] }
  check ((← IO.FS.readBinFile (extracted / "LICENSE")) == (← IO.FS.readBinFile (root / "LICENSE")))
    "release archive license differs from the source"
  let standalone := temp / exe
  IO.FS.rename (extracted / exe) standalone
  let empty := temp / "empty"
  IO.FS.createDir empty
  let report ← IO.Process.run {
    cmd := standalone.toString, args := #["4.36.0", "--demo", "--json"], cwd := empty
    env := #[("PATH", some ""), ("LD_LIBRARY_PATH", none), ("DYLD_LIBRARY_PATH", none)] }
  let report ← IO.ofExcept (Json.parse report)
  let findings := Report.rows report "findings"
  check (Report.string report "project" == "upgradeDemo" && findings.size == 1 &&
    Report.string findings[0]! "kind" == "toolchain_mismatch" &&
    findings[0]!.getObjValD "path" == toJson ["upgradeDemo", "greeting"])
    "standalone executable failed to locate the bundled demo"
  check (← empty.readDir).isEmpty "standalone scan left files in the caller's directory"

  if !Platform.isWindows then
    let fake := temp / "fake tar"
    IO.FS.createDir fake
    discard <| IO.Process.run { cmd := "ln", args := #["-s", (← IO.appPath).toString, (fake / "tar").toString] }
    let path := fake.toString ++ ":" ++ (← IO.getEnv "PATH").getD ""
    let failed ← pack #[("PATH", some path), ("LAKECOMPAT_TAR_FIXTURE", some "1")]
    check (failed.exitCode != 0 && failed.stderr.contains "fixture tar failure") "expected a failed tar invocation"
    check ((← out.readDir).size == 1) "failed release left its staging directory behind"
    let membersAfter ← IO.Process.run { cmd := "tar", args := #["-tzf", archive.toString] }
    check (membersAfter == members) "failed release replaced the previous archive"

private def downstreamTest (root temp : FilePath) : IO Unit := do
  let project := temp / "consumer"
  IO.FS.createDir project
  IO.FS.writeFile (project / "lean-toolchain") (← IO.FS.readFile (root / "lean-toolchain"))
  IO.FS.writeFile (project / "lakefile.toml") <|
    "name = \"consumer\"\n[[require]]\nname = \"lake-compat\"\npath = " ++ (toJson root.toString).compress ++ "\n"
  discard <| IO.Process.run { cmd := "lake", args := #["--keep-toolchain", "update"], cwd := project }
  let output ← IO.Process.run { cmd := "lake", args := #["exe", "lake-compat", "4.36.0", "--json"], cwd := project }
  let report ← IO.ofExcept (Json.parse output)
  check (Report.string report "project" == "consumer" &&
    (Report.rows report "packages").map (Report.string · "name") == #["lake-compat"])
    "downstream Lake executable did not scan its caller"

def run : IO Unit := withTempDir fun temp => do
  -- The test driver is built in .lake/build/bin, even when run from another project.
  let root ← IO.FS.realPath ((← IO.appDir) / "../../..")
  archiveTest root temp
  downstreamTest root temp

end LeanTests.Distribution
