import Init.System.FilePath
import LakeCompat.Source

namespace LakeCompat

def usage : String := "Usage: lake-compat TARGET [--project DIR | --demo] [OPTIONS]
Explain which locked dependencies need attention for a Lean upgrade.

  TARGET                   Lean toolchain, for example 4.36.0
  --project DIR            Read lake-manifest.json here (default: current directory)
  --demo                   Scan the bundled example project
  --suggest                Look up candidate revisions on Reservoir
  --verify                 Build the locked project with the installed target
  --try-suggested          Build a trial with each package's top candidate
  --try-revision NAME=SHA  Try a full Git commit; repeat to combine changes
  --json                   Emit a machine-readable report
  --strict                 Exit 1 if the original lock has metadata findings
  -h, --help               Show this help"

structure Options where
  target : String := ""
  project : Option System.FilePath := none
  demo : Bool := false
  suggest : Bool := false
  verify : Bool := false
  trySuggested : Bool := false
  revisions : Array (String × String) := #[]
  json : Bool := false
  strict : Bool := false
  deriving Inhabited

private def parsePin (value : String) : Except String (String × String) := do
  let expected := "--try-revision expects PACKAGE=SHA with a full 40- or 64-character Git commit"
  let some separator := value.revFind? '=' | throw expected
  let name := (value.sliceTo separator).copy
  let sha := (value.sliceFrom separator.next!).copy
  unless !name.isEmpty && Source.isFullRevision sha do throw expected
  return (name, sha.toLower)

private def positional (opts : Options) (value : String) : Except String Options := do
  unless opts.target.isEmpty do throw s!"unexpected positional argument '{value}'"
  unless !value.isEmpty do throw "expected a non-empty target Lean toolchain"
  return {opts with target := value}

def parseOptions (args : List String) : Except String Options := do
  let mut opts : Options := {}
  let mut positionalOnly := false
  let mut pending : Option String := none
  for arg in args do
    if positionalOnly then
      opts ← positional opts arg
    else if let some flag := pending then
      if arg.startsWith "-" then throw s!"{flag} requires a value"
      if flag == "--project" then opts := {opts with project := some arg}
      else opts := {opts with revisions := opts.revisions.push (← parsePin arg)}
      pending := none
    else
      match arg with
      | "--" => positionalOnly := true
      | "--project" | "--try-revision" => pending := some arg
      | "--demo" => opts := {opts with demo := true}
      | "--suggest" => opts := {opts with suggest := true}
      | "--verify" => opts := {opts with verify := true}
      | "--try-suggested" => opts := {opts with trySuggested := true}
      | "--json" => opts := {opts with json := true}
      | "--strict" => opts := {opts with strict := true}
      | _ =>
        if let some value := arg.dropPrefix? "--project=" then
          opts := {opts with project := some value.toString}
        else if let some value := arg.dropPrefix? "--try-revision=" then
          opts := {opts with revisions := opts.revisions.push (← parsePin value.toString)}
        else if arg.startsWith "-" then throw s!"unknown option '{arg}'"
        else opts ← positional opts arg
  if let some flag := pending then throw s!"{flag} requires a value"
  if opts.target.isEmpty then throw "expected a target Lean toolchain, for example 4.36.0"
  if opts.demo && opts.project.isSome then throw "--project and --demo are mutually exclusive"
  return opts

end LakeCompat
