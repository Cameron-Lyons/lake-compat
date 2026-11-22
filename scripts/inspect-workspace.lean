import Lake
import Lake.Load.Workspace

open Lake DSL Lean

package «lake-compat-inspector»

/-- This script runs under the selected toolchain, from the copied project's directory. -/
script inspect (args) do
  let [output] := args | error "expected an output file"
  let config : LoadConfig := {
    lakeEnv := (← getWorkspace).lakeEnv
    wsDir := ← IO.currentDir
    updateToolchain := false
  }
  let some ws ← (loadWorkspace config).toBaseIO | do
    IO.FS.writeFile output "null"
    return 1
  let packages := ws.packages.map fun pkg => Json.mkObj [
    ("name", toJson pkg.prettyName), ("hasDefaultTargets", toJson (!pkg.defaultTargets.isEmpty)),
    ("buildDir", toJson pkg.buildDir),
    ("leanLibDir", toJson pkg.leanLibDir), ("nativeLibDir", toJson pkg.staticLibDir),
    ("irDir", toJson pkg.irDir), ("binDir", toJson pkg.binDir)]
  IO.FS.writeFile output (Json.arr packages).compress
  return 0
