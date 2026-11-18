import Init.System.IO

namespace LakeCompat

/-- Bundle the fixture into the executable so the demo also works outside a Lake checkout. -/
private def demoFiles : List (System.FilePath × String) := [
  ("lean-toolchain", include_str "../examples/upgrade-demo/lean-toolchain"),
  ("lakefile.toml", include_str "../examples/upgrade-demo/lakefile.toml"),
  ("lake-manifest.json", include_str "../examples/upgrade-demo/lake-manifest.json"),
  ("Main.lean", include_str "../examples/upgrade-demo/Main.lean"),
  ("greeting/lean-toolchain", include_str "../examples/upgrade-demo/greeting/lean-toolchain"),
  ("greeting/lakefile.toml", include_str "../examples/upgrade-demo/greeting/lakefile.toml"),
  ("greeting/lake-manifest.json", include_str "../examples/upgrade-demo/greeting/lake-manifest.json"),
  ("greeting/Greeting.lean", include_str "../examples/upgrade-demo/greeting/Greeting.lean")]

def withDemo (run : System.FilePath → IO α) : IO α :=
  IO.FS.withTempDir fun temp => do
    let root := (← IO.FS.realPath temp) / "upgrade-demo"
    IO.FS.createDirAll (root / "greeting")
    for (name, contents) in demoFiles do
      IO.FS.writeFile (root / name) contents
    run root

end LakeCompat
