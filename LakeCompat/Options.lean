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
  --build-timeout SECONDS  Time allowed for lake build (default: 600 seconds)
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
  buildTimeoutSeconds : Option Nat := none
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

private def optionValue (opts : Options) (flag value : String) : Except String Options := do
  match flag with
  | "--project" =>
    if value.isEmpty then throw "--project requires a directory"
    return {opts with project := some value}
  | "--try-revision" => return {opts with revisions := opts.revisions.push (← parsePin value)}
  | "--build-timeout" =>
    let some seconds := value.toNat?.filter (· > 0)
      | throw "--build-timeout expects a positive integer in seconds"
    return {opts with buildTimeoutSeconds := some seconds}
  | _ => throw s!"unknown option '{flag}'"

def parseOptions (args : List String) : Except String Options := do
  let mut opts : Options := {}
  let mut positionalOnly := false
  let mut pending : Option String := none
  for arg in args do
    if positionalOnly then
      opts ← positional opts arg
    else if let some flag := pending then
      if arg.startsWith "-" then throw s!"{flag} requires a value"
      opts ← optionValue opts flag arg
      pending := none
    else
      match arg with
      | "--" => positionalOnly := true
      | "--project" | "--try-revision" | "--build-timeout" => pending := some arg
      | "--demo" => opts := {opts with demo := true}
      | "--suggest" => opts := {opts with suggest := true}
      | "--verify" => opts := {opts with verify := true}
      | "--try-suggested" => opts := {opts with trySuggested := true}
      | "--json" => opts := {opts with json := true}
      | "--strict" => opts := {opts with strict := true}
      | _ =>
        if arg.startsWith "-" then
          let some sep := arg.find? '=' | throw s!"unknown option '{arg}'"
          opts ← optionValue opts (arg.sliceTo sep).copy (arg.sliceFrom sep.next!).copy
        else opts ← positional opts arg
  if let some flag := pending then throw s!"{flag} requires a value"
  if opts.target.isEmpty then throw "expected a target Lean toolchain, for example 4.36.0"
  if opts.demo && opts.project.isSome then throw "--project and --demo are mutually exclusive"
  return opts

end LakeCompat
