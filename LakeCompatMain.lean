import Init.System.IO

def main (args : List String) : IO UInt32 := do
  -- Lake places this package's executable in its own .lake/build/bin, including
  -- when a downstream project runs it. Keep that project's working directory.
  let exe ← IO.appPath
  let some pkg := exe.parent >>= (·.parent) >>= (·.parent) >>= (·.parent)
    | throw <| IO.userError "lake-compat could not locate its Lake checkout"
  unless ← (pkg / "lake_compat" / "cli.py").pathExists do
    throw <| IO.userError s!"lake-compat could not find its Python sources in {pkg}"
  -- Isolated mode keeps the calling project's modules and PYTHONPATH out of
  -- the import path. Pass the checkout and CLI arguments as separate arguments.
  let entry := "import sys; sys.path.insert(0, sys.argv.pop(1)); from lake_compat.__main__ import run; run()"
  let child ← IO.Process.spawn {
    cmd := "python3"
    args := #["-I", "-c", entry, pkg.toString] ++ args.toArray
  }
  child.wait
