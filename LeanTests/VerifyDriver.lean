import LakeCompat.Verify

open Lean

/-- Small process boundary for the POSIX integration fixtures. -/
def main (args : List String) : IO Unit := do
  let [project, target, timeout, pins] := args | throw <| IO.userError "expected project, target, milliseconds, pins"
  let some ms := timeout.toNat? | throw <| IO.userError "expected timeout in milliseconds"
  let entries ← IO.ofExcept <| (do
    let obj ← (← Json.parse pins).getObj?
    obj.toList.mapM fun (name, value) => return (name, ← value.getStr?))
  IO.println (← LakeCompat.verifyBuild project target entries.toArray ms).compress
