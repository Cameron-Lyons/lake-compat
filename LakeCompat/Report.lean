import Lean.Data.Json.Basic

namespace LakeCompat.Report
open Lean

def string? (obj : Json) (key : String) : Option String :=
  (obj.getObjValD key).getStr?.toOption

def string (obj : Json) (key : String) (fallback := "") : String :=
  (string? obj key).getD fallback

def rows (obj : Json) (key : String) : Array Json :=
  (obj.getObjValD key).getArr?.toOption.getD #[]

def exitCode (report : Json) (strict : Bool) : UInt32 :=
  match string (report.getObjValD "verification") "status" with
  | "inconclusive" | "unavailable" => 2
  | "failed" => 1
  | _ => if strict && !(rows report "findings").isEmpty then 1 else 0

private def candidateLine (candidate : Json) (locked : Option String) : String := Id.run do
  let sha := string candidate "revision"
  let tag := (string? candidate "tag").map (· ++ "  ") |>.getD ""
  let mut evidence := "declares this toolchain, build unconfirmed"
  if string candidate "evidence" == "built" then
    evidence := "Reservoir build passed"
    if candidate.getObjValD "tested" == .bool true then
      evidence := evidence ++ " and tests passed"
    if candidate.getObjValD "required_update" == .bool true then
      evidence := evidence ++ " after updating dependencies"
  if locked.any (fun rev => rev.toLower == sha.toLower) then evidence := evidence ++ ", already locked"
  return s!"    {tag}{sha}  ({evidence})"

private def count (n : Nat) (noun : String) := s!"{n} {noun}{if n == 1 then "" else "s"}"

def render (report : Json) : String := Id.run do
  let packages := rows report "packages"
  let findings := rows report "findings"
  let header := s!"{string report "project"}  |  {string report "root_toolchain" "unknown"} -> {string report "target"}"
  let mut lines := #[header, s!"{count packages.size "locked package"}, {count findings.size "metadata finding"}"]
  for finding in findings do
    let path := (rows finding "path").filterMap (·.getStr?.toOption)
    let chain := if path.isEmpty then string finding "package" ++ " (parent unknown)"
                 else " -> ".intercalate path.toList
    lines := lines.push s!"  [{(string finding "kind").replace "_" " "}] {chain}"
    lines := lines.push ("    " ++ string finding "message")
  if findings.any (fun f => string f "kind" == "toolchain_mismatch") then
    lines := lines.push "A different declared toolchain can still compile on the target."

  let suggestions := report.getObjValD "suggestions"
  if let .obj entries := suggestions then
    lines := lines.push "\nReservoir candidates for flagged Git packages"
    if entries.isEmpty then lines := lines.push "  No flagged Git packages to look up."
    for pkg in packages do
      let name := string pkg "name"
      if let some suggestion := entries.get? name then
        lines := lines.push s!"  {name}: {string suggestion "message"}"
        for candidate in rows suggestion "candidates" do
          lines := lines.push (candidateLine candidate (string? pkg "revision"))

  if let .ok check := report.getObjVal? "verification" then
    let status := string check "status"
    lines := lines.push s!"\nBuild {status}: {string check "message"}"
    if let .obj pins := report.getObjValD "trial_revisions" then
      if pins.isEmpty && report.getObjValD "auto_selected" == .bool true then
        lines := lines.push "  No alternate revision was selected. This trial uses the current lock."
      for (name, rev) in pins do
        lines := lines.push s!"  Trial pin: {name}={rev.getStr?.toOption.getD ""}"
    if let some output := string? check "output" then
      if !output.isEmpty && status != "passed" then lines := lines.push output
  else
    lines := lines.push "\nBuild not run. Use --verify to test the locked set on this toolchain."
    if !findings.isEmpty && suggestions.isNull then
      lines := lines.push "Use --suggest to look up alternative dependency revisions."
  return "\n".intercalate lines.toList

end LakeCompat.Report
