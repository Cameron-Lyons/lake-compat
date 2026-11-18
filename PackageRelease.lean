import Init.Data.ByteArray.Extra
import Lean.Data.Json.Parser

open Lean System

private def buildInputs : IO (FilePath × FilePath) := do
  let mut env := #[]
  if Platform.isOSX then
    -- Lake's default is 99.0. Stamp a distributable binary with a real minimum.
    let version ← match (← IO.getEnv "MACOSX_DEPLOYMENT_TARGET").filter (!·.isEmpty) with
      | some version => pure version
      | none => do
        let version ← IO.Process.run {
          cmd := "/usr/bin/sw_vers", args := #["-productVersion"], env := #[("SYSTEM_VERSION_COMPAT", some "0")] }
        pure version.trimAscii.copy
    env := #[("MACOSX_DEPLOYMENT_TARGET", some version)]
  -- Lake runs this tool from .lake/build/bin but preserves the caller's working directory.
  let checkout ← IO.FS.realPath ((← IO.appDir) / "../../..")
  let output ← IO.Process.run {
    cmd := "lake", args := #["--json", "query", "@lake-compat/lake-compat", "@lake-compat/releaseLicense"],
    cwd := checkout, env }
  let [binary, license] := output.trimAscii.toString.splitOn "\n"
    | throw <| IO.userError "Lake did not return the release executable and license paths."
  let path := fun s => IO.ofExcept (do return FilePath.mk (← (← Json.parse s).getStr?))
  return (← path binary, ← path license)

private def packageRelease (output : FilePath) : IO FilePath := do
  let (binary, license) ← buildInputs
  IO.FS.createDirAll output
  let output ← IO.FS.realPath output
  let archive := output / s!"lake-compat-{Platform.target}.tar.gz"
  let nonce := (← IO.getRandomBytes 8).toUInt64LE!
  let staging := output / s!".package-release-{nonce}"
  IO.FS.createDir staging
  try
    let staged := staging / "release.tar.gz"
    let some binDir := binary.parent | throw <| IO.userError s!"Invalid executable path: {binary}"
    let some licenseDir := license.parent | throw <| IO.userError s!"Invalid license path: {license}"
    discard <| IO.Process.run {
      cmd := "tar", args := #["-czhf", staged.toString, "-C", binDir.toString, binary.fileName.get!,
        "-C", licenseDir.toString, "LICENSE"],
      env := #[("COPYFILE_DISABLE", some "1")] }
    IO.FS.rename staged archive
    return archive
  finally IO.FS.removeDirAll staging

def main (args : List String) : IO UInt32 := do
  let usage := "Usage: lake exe package-release [OUTPUT-DIR]"
  match args with
  | ["--help"] | ["-h"] => IO.println usage; return 0
  | [] | [_] =>
    try
      IO.println (← packageRelease (args.head?.getD "dist"))
      return 0
    catch e => IO.eprintln s!"package-release: {e}"; return 1
  | _ => IO.eprintln usage; return 2
