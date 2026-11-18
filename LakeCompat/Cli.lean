import LakeCompat.Concurrent
import LakeCompat.Demo
import LakeCompat.Graph
import LakeCompat.Options
import LakeCompat.Releases
import LakeCompat.Report
import LakeCompat.Verify
import Std.Data.HashSet

namespace LakeCompat
open Lean System

private def regularFile (path : FilePath) : IO Bool := do
  try return (← path.metadata).type == .file
  catch e => if e matches .noFileOrDirectory .. then return false else throw e

private def checkProject (project : FilePath) : IO Unit := do
  let hint := " Set --project to the folder containing your lakefile.toml or lakefile.lean."
  unless ← project.isDir do
    let reason := if ← project.pathExists then s!"--project expects a directory, but {project} is a file."
                  else s!"Project directory {project} does not exist."
    throw <| IO.userError (reason ++ hint)
  if ← regularFile (project / "lake-manifest.json") then return
  if (← regularFile (project / "lakefile.toml")) || (← regularFile (project / "lakefile.lean")) then
    throw <| IO.userError s!"{project} has a Lake configuration but no lake-manifest.json. Run `lake update` in that directory to create the manifest, then retry."
  throw <| IO.userError (s!"No Lake configuration or lake-manifest.json found in {project}." ++ hint)

private abbrev Pins := Std.TreeMap String String

private def explicitPins (packages : Array Json) (revisions : Array (String × String)) : IO Pins := do
  if revisions.isEmpty then return {}
  let git := packages.foldl (init := ({} : Std.HashSet String)) fun names pkg =>
    if Report.string pkg "type" == "git" then names.insert (Report.string pkg "name") else names
  let mut pins : Pins := {}
  for (name, sha) in revisions do
    unless git.contains name do throw <| IO.userError s!"'{name}' is not a locked Git package"
    if let some existing := pins.get? name then
      unless sha == existing do throw <| IO.userError s!"two different revisions supplied for '{name}'"
    pins := pins.insert name sha
  return pins

private def lookupSuggestions (report : Json) : IO Json := do
  let mut flagged : Std.HashSet String := {}
  for finding in Report.rows report "findings" do
    flagged := flagged.insert (Report.string finding "package")
    if let some parent := Report.string? finding "parent" then flagged := flagged.insert parent
  let packages := (Report.rows report "packages").filter fun pkg =>
    flagged.contains (Report.string pkg "name") && Report.string pkg "type" == "git"
  let target := Report.string report "target"
  let results ← mapConcurrent packages fun pkg => do
    let name := Report.string pkg "name"
    let result ← Releases.suggestVersions (Report.string pkg "url") target
      (some name) (Report.string? pkg "scope") (Report.string? pkg "sub_dir")
    return (name, result)
  return Json.mkObj results.toList

private def selectedPins (packages : Array Json) (suggestions : Json) (explicit : Pins) : Pins := Id.run do
  let mut pins := explicit
  for pkg in packages do
    let name := Report.string pkg "name"
    if pins.contains name then continue
    let candidates := Report.rows (suggestions.getObjValD name) "candidates"
    if let some candidate := candidates[0]? then
      let revision := Report.string candidate "revision"
      if (Report.string pkg "revision").toLower != revision.toLower then pins := pins.insert name revision
  return pins

private def displayDemo (root : FilePath) (report : Json) : Json :=
  let base := root.toString ++ String.singleton FilePath.pathSeparator
  let packages := (Report.rows report "packages").map fun pkg =>
    let relative := (Report.string pkg "checkout").dropPrefix base
    pkg.setObjVal! "checkout" relative.toString
  (report.setObjVal! "packages" (.arr packages)).setObjVal! "project_source" "bundled_demo"

private def scan (opts : Options) (project : FilePath) : IO Json := do
  checkProject project
  let mut report ← analyze project opts.target
  let packages := Report.rows report "packages"
  let mut pins ← explicitPins packages opts.revisions
  if opts.suggest || opts.trySuggested then
    let suggestions ← lookupSuggestions report
    report := report.setObjVal! "suggestions" suggestions
    if opts.trySuggested then
      pins := selectedPins packages suggestions pins
      report := report.setObjVal! "auto_selected" true
  if opts.verify || opts.trySuggested || !pins.isEmpty then
    let result ← verifyBuild project (Report.string report "target") pins.toArray
    report := (report.setObjVal! "verification" result).setObjVal! "trial_revisions"
      (Json.mkObj (pins.toList.map fun (k, v) => (k, .str v)))
  return if opts.demo then displayDemo project report else report

private def reportError (json : Bool) (message : String) : IO UInt32 := do
  if json then IO.println (Json.mkObj [("error", .str message)]).compress
  else IO.eprintln s!"lake-compat: {message}"
  return 2

def run (args : List String) : IO UInt32 := do
  let flags := args.takeWhile (· != "--")
  if flags.any (fun arg => arg == "--help" || arg == "-h") then
    IO.println usage
    return 0
  let opts ← match parseOptions args with
    | .ok opts => pure opts
    | .error message => return ← reportError (flags.contains "--json") message
  try
    let report ← if opts.demo then withDemo (scan opts) else scan opts (opts.project.getD ".")
    IO.println (if opts.json then report.compress else Report.render report)
    return Report.exitCode report opts.strict
  catch e => reportError opts.json e.toString

end LakeCompat
