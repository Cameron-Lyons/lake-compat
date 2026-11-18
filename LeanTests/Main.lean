import LakeCompat.Cli
import LeanTests.Concurrent
import LeanTests.Releases

open LakeCompat Lean

private def check (condition : Bool) (message : String) : IO Unit :=
  unless condition do throw (IO.userError message)

private def toolchains : IO Unit := do
  let stable := "leanprover/lean4:v4.25.0"
  for (input, expected) in [("4.25.0", stable), ("v4.25.0", stable),
      ("leanprover/lean4:4.25.0", stable), (stable, stable),
      ("nightly-2026-10-11", "leanprover/lean4:nightly-2026-10-11"),
      ("v4.22.0-rc3", "leanprover/lean4:v4.22.0-rc3"),
      ("fork/lean4:4.23.0", "fork/lean4:4.23.0"), ("local-dev", "local-dev")] do
    check ((normalizeToolchain input).toOption == some expected) s!"normalizing '{input}'"
  for input in ["", "   \n", "a b", "owner/repo:"] do
    check ((normalizeToolchain input).toOption.isNone) s!"invalid toolchain '{input}'"

def main : IO Unit := do
  toolchains
  LeanTests.Concurrent.run
  LeanTests.Releases.run
  withDemo fun project => do
    let report ← analyze project "4.36.0"
    let findings := Report.rows report "findings"
    check (findings.size == 1 && Report.string findings[0]! "kind" == "toolchain_mismatch")
      "expected the bundled greeting toolchain mismatch"
    check ((Report.render report).contains "upgradeDemo -> greeting") "missing dependency path"
    check (Report.exitCode report true == 1 && Report.exitCode report false == 0) "strict exit code"
  IO.println "Lean tests passed"
