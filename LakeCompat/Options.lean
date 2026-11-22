import Init.System.FilePath
import Lean.Data.Name
import LakeCompat.Source

namespace LakeCompat

def usage : String := "Usage: lake-compat TARGET [--project DIR | --demo] [OPTIONS]
Explain which locked dependencies need attention for a Lean upgrade.

  TARGET                   Lean toolchain, for example 4.36.0
  --project DIR            Read lake-manifest.json here (default: current directory)
  --demo                   Scan the bundled example project
  --suggest                Look up candidate revisions on Reservoir
  --verify                 Build the project's default targets on this toolchain
  --test                   Run the project's Lake test driver after a successful build
  -- ARGS...               With --test, pass remaining arguments to the test driver
  --build-target TARGET    Build a target, module source file, @package defaults, or facet (repeatable)
  --try-suggested          Build a trial with each package's top candidate
  --try-revision NAME=SHA  Try a full Git commit; repeat to combine changes
  --build-timeout SECONDS  Time allowed for each build or test step (default 600 seconds)
  --json                   Emit a machine-readable report
  --strict                 Exit 1 if the original lock has metadata findings
  --version                Show the CLI version
  -h, --help               Show this help"

structure Options where
  target : String := ""
  project : Option System.FilePath := none
  demo : Bool := false
  suggest : Bool := false
  verify : Bool := false
  testArgs? : Option (Array String) := none
  buildTargets : Array String := #[]
  trySuggested : Bool := false
  revisions : Array (String × String) := #[]
  buildTimeoutSeconds : Option Nat := none
  json : Bool := false
  strict : Bool := false
  deriving Inhabited

/-- A standalone Lake package selector requests all of that package's default targets. -/
def defaultPackage? (spec : String) : Option String := do
  let name ← spec.dropPrefix? "@"
  guard (!name.isEmpty && !name.contains '/' && !name.contains ':')
  return name.copy

/-- Pass relative module paths and package selectors to Lake. Qualify named targets so
a bare name selects a target in the root package. -/
def buildTargetSpec (value : String) : Except String String := do
  let invalid := "--build-target expects a Lake target such as MyLib, @package/MyLib, src/Main.lean:olean, or :extraDep"
  if value.trimAscii.isEmpty || value.startsWith "-" || value.any (· < ' ') then throw invalid
  if (defaultPackage? value).isSome then return value
  let (base, facet) ← match value.splitOn ":" with
    | [base] => pure (base, "")
    | [base, facet] => do
      -- Lake treats an unparseable facet as the anonymous (default) facet.
      if facet.contains '/' || (!facet.isEmpty && facet.toName.isAnonymous) then throw invalid
      pure (base, ":" ++ facet)
    | _ => throw invalid
  if base.endsWith ".lean" && !base.startsWith "@" && !base.startsWith "+" then
    if (System.FilePath.mk base).isAbsolute || base.startsWith "../" then throw invalid
    return value
  if base.startsWith "./" then throw invalid
  let (pkg, target) ← match base.splitOn "/" with
    | [name] => do
      if name.startsWith "@" then pure ((name.drop 1).copy, "") else pure ("", name)
    | [pkg, name] => pure ((pkg.dropPrefix "@").copy, name)
    | _ => throw invalid
  if target.isEmpty && (facet.isEmpty || facet == ":") then throw invalid
  return s!"@{pkg}/{target}{facet}"

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
  | "--build-target" =>
    discard <| buildTargetSpec value
    return {opts with verify := true, buildTargets := opts.buildTargets.push value}
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
      if !opts.target.isEmpty then
        if let some testArgs := opts.testArgs? then
          opts := {opts with testArgs? := some (testArgs.push arg)}
          continue
      opts ← positional opts arg
    else if let some flag := pending then
      if arg.startsWith "-" then throw s!"{flag} requires a value"
      opts ← optionValue opts flag arg
      pending := none
    else
      match arg with
      | "--" => positionalOnly := true
      | "--project" | "--try-revision" | "--build-target" | "--build-timeout" => pending := some arg
      | "--demo" => opts := {opts with demo := true}
      | "--suggest" => opts := {opts with suggest := true}
      | "--verify" => opts := {opts with verify := true}
      | "--test" => opts := {opts with verify := true, testArgs? := some #[]}
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
