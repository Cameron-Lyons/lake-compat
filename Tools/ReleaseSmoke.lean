import LeanTests.JsonSchema
import LakeCompat.Report
import Tools.Process

namespace Tools.ReleaseSmoke
open Lean System

private def target := (include_str "../lean-toolchain").trimAscii.copy

private def ensure (condition : Bool) (message : String) : IO Unit :=
  unless condition do throw <| IO.userError message

/-- Check an archive on a host without Lean, using only this native checker and tar. -/
private def checkArchive (archive : FilePath) (version : String) : IO Unit := do
  let archive ← IO.FS.realPath archive
  let members ← Process.run {cmd := "tar", args := #["-tzf", archive.toString]}
  ensure (members.exitCode == 0) s!"Cannot read the release archive: {members.stderr}"
  ensure ((members.stdout.lines.map (·.trimAscii.copy)).toList == ["lake-compat", "LICENSE"])
    "Unexpected release archive members."
  IO.FS.withTempDir fun temp => do
    let root ← IO.FS.realPath temp
    let unpacked ← Process.run {cmd := "tar", args := #["-xzf", archive.toString, "-C", root.toString]}
    ensure (unpacked.exitCode == 0) s!"Cannot extract the release archive: {unpacked.stderr}"
    ensure (!(← IO.FS.readFile (root / "LICENSE")).trimAscii.isEmpty) "The release license is empty."
    let cwd := root / "empty"
    IO.FS.createDir cwd
    let invoke := fun args => Process.run {
      cmd := (root / "lake-compat").toString, args, cwd
      env := #[("PATH", some ""), ("LD_LIBRARY_PATH", none), ("DYLD_LIBRARY_PATH", none)] }
    let actual ← invoke #["--version"]
    ensure (actual.exitCode == 0 && actual.stdout.trimAscii == s!"lake-compat {version}".toSlice)
      s!"Unexpected release version: {actual.stdout}{actual.stderr}"
    let rule ← IO.ofExcept LeanTests.JsonSchema.reportRule
    for strict in [false, true] do
      let args := #[target, "--demo", "--json"] ++ if strict then #["--strict"] else #[]
      let output ← invoke args
      ensure (output.exitCode == if strict then 1 else 0)
        s!"Standalone demo exited {output.exitCode}: {output.stdout}{output.stderr}"
      let report ← IO.ofExcept (Json.parse output.stdout)
      IO.ofExcept (LeanTests.JsonSchema.validate rule report)
      ensure (LakeCompat.Report.string report "project" == "upgradeDemo" &&
        (LakeCompat.Report.rows report "findings").any fun finding =>
          LakeCompat.Report.string finding "kind" == "toolchain_mismatch" &&
          finding.getObjValD "path" == toJson ["upgradeDemo", "greeting"])
        "The standalone demo did not report its expected dependency path."
    ensure (← cwd.readDir).isEmpty "The standalone CLI left files in its caller's directory."

def run (args : List String) : IO UInt32 := do
  let usage := "Usage: release-smoke ARCHIVE VERSION"
  if args == ["--help"] || args == ["-h"] then
    IO.println usage
    return 0
  let [archive, version] := args | do IO.eprintln usage; return 2
  try
    ensure (!version.isEmpty) "Expected the archive's release version."
    checkArchive archive version
    IO.println "Standalone release smoke checks passed."
    return 0
  catch e =>
    IO.eprintln s!"release-smoke: {e}"
    return 1

end Tools.ReleaseSmoke

def main := Tools.ReleaseSmoke.run
