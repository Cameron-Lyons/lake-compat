import Lean.Data.Json.FromToJson.Basic
import LakeCompat.Concurrent
import LakeCompat.GitTree
import LakeCompat.Manifest
import LakeCompat.Source
import Std.Data.HashMap

namespace LakeCompat
open Lean System
namespace Graph

private abbrev ReadText := FilePath → IO (Option String)
private abbrev Issue := String × String
private abbrev Repositories := Std.HashMap FilePath String

private def fileText? (file : FilePath) : IO (Option String) := do
  try return some (← IO.FS.readFile file)
  catch
    | .noFileOrDirectory .. => return none
    | e => throw e

private def pin? (dir : FilePath) (readText : ReadText) : IO (Option String) := do
  let some text ← readText (dir / "lean-toolchain") | return none
  return some (← IO.ofExcept (normalizeToolchain text))

private def packagePin (checkout boundary project : FilePath) (isGit : Bool) (readText : ReadText) :
    IO (Option (String × String)) := do
  let mut dir := checkout
  repeat
    if let some pin ← pin? dir readText then
      let source := if dir == checkout then "package" else if dir == project then "workspace"
                    else if isGit then "repository" else "ancestor"
      return some (pin, source)
    if dir == boundary || (GitTree.relativeTo? boundary dir).isNone then return none
    let some parent := dir.parent | return none
    dir := parent
  return none

private def appendComponent (p : FilePath) (s : String) : FilePath :=
  if s.isEmpty || s == "." then p else if s == ".." then p.parent.getD p
  else if p.toString.endsWith FilePath.pathSeparator.toString then ⟨p.toString ++ s⟩ else p / s

/-- Resolve the existing prefix as well when a checkout or a path pin has not been materialized. -/
private partial def resolvePath (p : FilePath) : IO FilePath := do
  try IO.FS.realPath p
  catch
    | .noFileOrDirectory .. =>
      let some parent := p.parent | return p
      let leaf := (p.components.getLast?).getD ""
      return appendComponent (← resolvePath parent) leaf
    | e => throw e

private def lexicalPath (p : FilePath) : FilePath := Id.run do
  let parts := p.components
  let root : FilePath := if p.toString.startsWith FilePath.pathSeparator.toString then
    ⟨FilePath.pathSeparator.toString⟩ else ⟨parts.head! ++ FilePath.pathSeparator.toString⟩
  return parts.drop 1 |>.foldl appendComponent root

private def repository? (repos : Repositories) (p : FilePath) : Option FilePath := Id.run do
  let mut dir := p
  repeat
    if repos.contains dir then return some dir
    let some parent := dir.parent | return none
    dir := parent
  return none

private structure Located where
  entry : LockedPackage
  checkout : FilePath
  deriving Inhabited

private def locate (project packages : FilePath) (repos : Repositories) (entry : LockedPackage) :
    IO (Located × Option FilePath) := do
  match entry.source with
  | .git _ _ subDir _ =>
    let repo := packages / entry.name
    return (⟨entry, subDir.elim repo (repo / ·)⟩, some repo)
  | .path dir copy =>
    let path := if copy then packages / entry.name else project / dir
    -- Detect monorepos before following working-directory links. Keep '..' components
    -- in a locked checkout because a preceding directory can be a committed symlink.
    let normalized := lexicalPath path
    if let some repo := repository? repos normalized then
      return (⟨entry, if (GitTree.relativeTo? repo path).isSome then path else normalized⟩, some repo)
    let checkout ← resolvePath path
    return (⟨entry, checkout⟩, repository? repos checkout)

private structure Observation extends Located where
  pin : Option (String × String) := none
  fixedToolchain : Option Bool := none
  edges : PackageMap := ∅
  issues : Array Issue := #[]
  deriving Inhabited

private def problem (p : Located) (kind message : String) : Observation :=
  { toLocated := p, issues := #[(kind, message)] }

private def readPackage (p : Located) (project : FilePath) (repo : Option FilePath)
    (readText : ReadText) (issues : Array Issue := #[]) : IO Observation := do
  let mut issues := issues
  let mut pin := none
  try
    pin ← packagePin p.checkout (repo.getD project) project repo.isSome readText
    if pin.isNone then
      issues := issues.push ("unknown_toolchain", "No lean-toolchain file was found for this package.")
  catch e =>
    issues := issues.push ("unknown_toolchain", s!"Cannot read the package toolchain: {e}")
  let obs : Observation := { toLocated := p, pin, issues }
  let some manifest := p.entry.manifestFile | return { obs with issues := issues.push (
    "missing_manifest", "The root lock records no package manifest to inspect for dependency pins.") }
  let file := p.checkout / manifest
  try
    let some contents ← readText file | return { obs with issues := issues.push (
      "missing_manifest", s!"Dependency pins are unknown because {manifest} is missing.") }
    let lock ← IO.ofExcept (Lock.parse file contents)
    return { obs with fixedToolchain := some lock.fixedToolchain, edges := lock.packages.filter fun _ e => !e.inherited }
  catch e => return { obs with issues := issues.push ("invalid_manifest", s!"Cannot inspect the package manifest: {e}") }

private def missing (p : Located) : Observation :=
  problem p "missing_checkout" s!"Package directory is missing: {p.checkout}"

private def observeGit (project repo : FilePath) (revision : String) (env : GitTree.Env)
    (group : Array Located) : IO (Array Observation) := do
  unless ← repo.isDir do return group.map missing
  let fallback := fun () => group.mapM fun p => readPackage p project (some repo) fileText? #[(
    "unknown_checkout_revision", s!"Could not verify Git revision {revision} for {repo}. The available metadata comes from the working directory.")]
  unless ← (repo / ".git").pathExists do return ← fallback ()
  try
    GitTree.withReader repo env fun git => do
      let some head ← git.head? | return ← fallback ()
      if head.toLower != revision.toLower then
        return group.map fun p => problem p "checkout_revision_mismatch"
          s!"Checkout is at {head}. The root manifest selects {revision}."
      group.mapM fun p => do
        let checkout ← (git.directory head p.checkout).toBaseIO
        match checkout with
        | .error e => return problem p "unknown_package_directory" (toString e)
        | .ok dir => readPackage { p with checkout := dir } project (some repo) (git.readText? head)
  catch _ => fallback ()

private abbrev Observations := Std.TreeMap String Observation

private def observe (project packages : FilePath) (lock : Lock) : IO Observations := do
  let mut repos : Repositories := ∅
  for (name, pkg) in lock.packages do
    if let .git _ rev .. := pkg.source then repos := repos.insert (packages / name) rev
  let mut groups : Std.HashMap FilePath (Array Located) := ∅
  let mut observations : Observations := ∅
  for (_, pkg) in lock.packages do
    let (p, repo) ← locate project packages repos pkg
    if let some repo := repo then groups := groups.alter repo fun group => some ((group.getD #[]).push p)
    else
      let obs ← if ← p.checkout.isDir then readPackage p project none fileText? else pure (missing p)
      observations := observations.insert pkg.name obs
  if groups.isEmpty then return observations
  let env ← GitTree.environment
  let batches ← mapConcurrent groups.toArray fun (repo, group) =>
    observeGit project repo repos[repo]! env group
  for batch in batches do
    for obs in batch do observations := observations.insert obs.entry.name obs
  return observations

/-- Breadth-first search with sorted neighbors for stable shortest paths. Each child
    shares its parent's reversed path until the final report is serialized. -/
private def shortestPaths (root : String) (observations : Observations) : Std.HashMap String (List String) := Id.run do
  let mut paths : Std.HashMap String (List String) := ∅
  let mut queue := #[]
  for (name, obs) in observations do
    unless obs.entry.inherited do
      paths := paths.insert name [name, root]
      queue := queue.push name
  let mut i := 0
  while h : i < queue.size do
    let parent := queue[i]
    let chain := paths[parent]!
    for (child, _) in observations[parent]!.edges do
      if observations.contains child && !paths.contains child then
        paths := paths.insert child (child :: chain)
        queue := queue.push child
    i := i + 1
  return paths

private def sameRemote (a b : String) : Bool :=
  a == b || match Source.githubRepo? a, Source.githubRepo? b with
    | some (ao, ar), some (bo, br) => ao.toLower == bo.toLower && ar.toLower == br.toLower
    | _, _ => false

private def conflict (parent : Observation) (wanted selected : LockedPackage) (project : FilePath) :
    IO (Option Issue) := do
  let name := parent.entry.name
  match wanted.source, selected.source with
  | .git url rev sub _, .git rootUrl rootRev rootSub _ =>
    if rev.toLower != rootRev.toLower then
      return some ("revision_conflict", s!"{name} locks {rev}. The root selects {rootRev}.")
    if !sameRemote url rootUrl then
      return some ("source_conflict", s!"{name} locks repository {url}. The root locks {rootUrl}.")
    if sub.getD "" != rootSub.getD "" then
      return some ("source_conflict", s!"{name} and the root select different package subdirectories at the same revision.")
  | .path dir _, .path rootDir _ =>
    let requested ← resolvePath (parent.checkout / dir)
    let actual ← resolvePath (project / rootDir)
    if requested != actual then return some ("source_conflict", s!"{name} locks path {requested}. The root locks {actual}.")
  | .path .., .git .. => return some ("source_conflict", s!"{name} locks a path source. The root locks a git source.")
  | .git .., .path .. => return some ("source_conflict", s!"{name} locks a git source. The root locks a path source.")
  return none

private def inventory (p : Observation) (path : List String) : Json :=
  let (kind, url, subDir, inputRev) := match p.entry.source with
    | .git url _ sub input => ("git", some url, sub, input)
    | .path .. => ("path", none, none, none)
  Json.mkObj [
    ("name", p.entry.name), ("toolchain", toJson (p.pin.map Prod.fst)),
    ("toolchain_source", toJson (p.pin.map Prod.snd)), ("revision", toJson p.entry.source.revision?),
    ("url", toJson url), ("scope", p.entry.scope), ("sub_dir", toJson subDir), ("input_rev", toJson inputRev),
    ("checkout", p.checkout.toString), ("direct", toJson (!p.entry.inherited)), ("type", kind),
    ("fixed_toolchain", toJson p.fixedToolchain), ("path", toJson path), ("path_known", toJson (!path.isEmpty))]

private def finding (name : String) (path : List String) (issue : Issue) : Json :=
  Json.mkObj [("kind", issue.1), ("package", name), ("path", toJson path), ("message", issue.2)]

end Graph
open Graph

/-- Inspect the local dependency graph at the revisions selected by the root lock. -/
def analyze (project : FilePath) (target : String) : IO Json := do
  let project ← IO.FS.realPath project
  let target ← IO.ofExcept (normalizeToolchain target)
  let file := project / manifestName
  let lock ← IO.ofExcept (Lock.parse file (← IO.FS.readFile file))
  let dirname := project.fileName.getD "[anonymous]"
  let root ← IO.ofExcept (packageName (lock.name.getD dirname))
  let root := if root == "[anonymous]" then dirname else root
  let lakeDir := project / lock.lakeDir
  let packagesDir ← resolvePath (lock.packagesDir.elim (lakeDir / "packages") (project / ·))
  let rootPin ← pin? project fileText?
  let observations ← observe project packagesDir lock
  let paths := shortestPaths root observations
  let mut packages := #[]
  let mut findings := #[]
  let overrides := lakeDir / "package-overrides.json"
  if (← overrides.metadata.toBaseIO).toOption.any (·.type == .file) then
    findings := findings.push (finding root [root] ("active_overrides",
      s!"{overrides} can replace packages from the root lock at build time."))
  for (name, obs) in observations do
    let path := (paths.getD name []).reverse
    packages := packages.push (inventory obs path)
    let mut issues := obs.issues
    if path.isEmpty then issues := issues.push ("unknown_path",
      "The root lock lists this dependency, but its parent chain is unavailable.")
    if let some (pin, source) := obs.pin then
      if pin != target && source != "workspace" then
        let verb := if source == "package" then "Declares" else "Inherits"
        let note := if obs.fixedToolchain == some true then " Its manifest sets fixedToolchain." else ""
        issues := issues.push ("toolchain_mismatch", s!"{verb} {pin}. The target is {target}.{note}")
    for issue in issues do findings := findings.push (finding name path issue)
    unless path.isEmpty do
      for (child, wanted) in obs.edges do
        let selected := lock.packages[child]?
        let issue ← match selected with
          | some selected => conflict obs wanted selected project
          | none => pure <| some ("unlocked_dependency", s!"{name} locks {child}, which is absent from the root manifest.")
        if let some issue := issue then
          let detail := Json.mkObj [("parent", name), ("requested_revision", toJson wanted.source.revision?),
                                    ("selected_revision", toJson (selected.bind (·.source.revision?)))]
          findings := findings.push ((finding child (path ++ [child]) issue).mergeObj detail)
  return Json.mkObj [("project", root), ("target", target), ("root_toolchain", toJson rootPin),
                     ("packages", .arr packages), ("findings", .arr findings)]

end LakeCompat
