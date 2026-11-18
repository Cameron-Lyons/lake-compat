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

private structure Checkout where
  repo : FilePath
  revision : String
  deriving BEq, Hashable, Inhabited

private abbrev Repositories := Std.HashMap FilePath (Except String Checkout)

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
  try
    -- Some realpath implementations simplify "file/.." even though it is not traversable.
    if p.components.contains ".." then discard p.metadata
    IO.FS.realPath p
  catch
    | .noFileOrDirectory .. =>
      let some parent := p.parent | return p
      let leaf := (p.components.getLast?).getD ""
      -- Keep '..' after an unmaterialized component. The OS cannot traverse that prefix.
      return (← resolvePath parent) / leaf
    | e => throw e

private structure Located where
  entry : LockedPackage
  checkout : FilePath
  deriving Inhabited

private structure DirectoryQuery (α : Type) where
  key : α
  path : FilePath
  gitRepo : Option FilePath := none
  workspaceLinks : Nat := 0
  issues : Array Issue := #[]

private inductive WorkspaceComponent where
  | path (resolved : FilePath)
  | symlink (target : FilePath)

private abbrev PrefixCache := IO.Ref (Std.HashMap FilePath WorkspaceComponent)

private def workspaceLinkTarget (p : FilePath) : IO FilePath := do
  if Platform.isWindows then
    -- Pass the literal path as data, including names with quotes or PowerShell metacharacters.
    let command := "[Console]::OutputEncoding = [Text.UTF8Encoding]::new($false)\n" ++
      "$item = Get-Item -LiteralPath $env:LAKE_COMPAT_LINK_PATH -Force -ErrorAction Stop\n" ++
      "$targets = @($item.Target)\n" ++
      "if ($targets.Count -ne 1 -or !$targets[0]) { throw 'Cannot read link target' }\n" ++
      "[Console]::Write([string]$targets[0])"
    let target ← IO.Process.run {
      cmd := "powershell.exe",
      args := #["-NoProfile", "-NonInteractive", "-Command", command],
      env := #[("LAKE_COMPAT_LINK_PATH", some p.toString)] }
    return ⟨target⟩
  let raw ← IO.Process.run {cmd := "readlink", args := #[p.toString]}
  return ⟨(raw.dropSuffix "\n").copy⟩

private def workspaceComponent (cache : PrefixCache) (p : FilePath) : IO WorkspaceComponent := do
  if let some cached := (← cache.get)[p]? then return cached
  let metadata ← try some <$> p.symlinkMetadata catch
    | .noFileOrDirectory .. => pure none
    | e => throw e
  let component ← if metadata.any (·.type == .symlink) then
      pure (.symlink (← workspaceLinkTarget p))
    else if Platform.isWindows then pure (.path (← resolvePath p))
    else pure (.path p)
  cache.modify (·.insert p component)
  return component

/-- Split an absolute path at its root, preserving Windows drive letters and UNC shares. -/
private def rootParts (p : FilePath) : FilePath × List String :=
  let parts := p.components
  if Platform.isWindows then
    match parts with
    | "" :: "" :: host :: share :: rest => (⟨s!"\\\\{host}\\{share}\\"⟩, rest)
    | drive :: rest => (⟨drive ++ "\\"⟩, rest)
    | [] => (⟨"\\"⟩, [])
  else (⟨"/"⟩, parts.drop 1)

private def enterCheckout (repos : Repositories) (alias : FilePath) (q : DirectoryQuery α) :
    IO (DirectoryQuery α × Option Checkout) := do
  let checkout ← IO.ofExcept repos[alias]!
  let some suffix := GitTree.relativeTo? alias q.path
    | throw <| IO.userError s!"{q.path} leaves the locked repository {alias}"
  let path := if suffix.isEmpty then checkout.repo else checkout.repo / suffix
  return ({q with path}, some checkout)

/-- Resolve workspace prefixes until we reach a locked checkout. Its remaining components,
    including any '..' after a committed symlink, are interpreted by the Git reader. -/
private def locatePath (repos : Repositories) (cache : PrefixCache) (q : DirectoryQuery α) :
    IO (DirectoryQuery α × Option Checkout) := do
  if let some repo := q.gitRepo then return ← enterCheckout repos repo q
  let p := q.path
  if repos.isEmpty then return ({q with path := ← resolvePath p}, none)
  let (root, parts) := rootParts p
  let mut dir := root
  let mut rest := parts
  let mut links := q.workspaceLinks
  while let part :: tail := rest do
    rest := tail
    if part.isEmpty || part == "." then continue
    if part == ".." then
      unless ← dir.isDir do throw <| IO.userError s!"Expected a directory before '..' at {dir}"
    let next := appendComponent dir part
    if repos.contains next then dir := next
    else
      match ← workspaceComponent cache next with
      | .path resolved => dir := resolved
      | .symlink target =>
        if links >= 40 then throw <| IO.userError s!"Too many workspace symlinks at {next}"
        links := links + 1
        if target.isAbsolute then
          let (root, parts) := rootParts target
          -- A single leading separator on Windows denotes the root of the containing drive.
          dir := if Platform.isWindows && root.toString == "\\" then (rootParts dir).1 else root
          rest := parts ++ rest
        else rest := target.components ++ rest
        continue
    let mut ancestor := dir
    repeat
      if repos.contains ancestor then
        let path := tail.foldl (fun path (part : String) => path / part) dir
        return ← enterCheckout repos ancestor {q with path, workspaceLinks := links}
      -- Windows realPath can resolve an entire link target inside a repository at once.
      if dir == next then break
      let some parent := ancestor.parent | break
      ancestor := parent
  return ({q with path := dir, workspaceLinks := links}, none)

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

private structure Directory where
  path : FilePath
  repo : Option FilePath := none
  readText : ReadText := fileText?
  committed : Bool := false
  issues : Array Issue := #[]

private abbrev Inspect (α β : Type) := DirectoryQuery α → Except Issue Directory → IO β
private abbrev BatchResult (α β : Type) := Sum (DirectoryQuery α) β

private def inspectGit (repo : FilePath) (revision : String) (env : GitTree.Env)
    (group : Array (DirectoryQuery α)) (inspect : Inspect α β) : IO (Array (BatchResult α β)) := do
  let finish := fun (q : DirectoryQuery α) result => Sum.inr <$> inspect q result
  unless ← repo.isDir do
    return ← group.mapM fun q => finish q (.error ("missing_checkout", s!"Package directory is missing: {q.path}"))
  let fallback := fun () => do
    let boundary ← IO.FS.realPath repo
    group.mapM fun q => do
      let .ok path ← (resolvePath q.path).toBaseIO
        | return ← finish q (.error ("unknown_package_directory", s!"Cannot resolve {q.path}"))
      if (GitTree.relativeTo? boundary path).isNone && q.gitRepo.isSome then
        return ← finish q (.error ("unknown_package_directory", s!"Package directory leaves the repository: {q.path}"))
      finish q (.ok { path, repo := some boundary, issues := #[("unknown_checkout_revision",
        s!"Could not verify Git revision {revision} for {repo}. The available metadata comes from the working directory.")] })
  unless ← (repo / ".git").pathExists do return ← fallback ()
  let batch ← (GitTree.withReader repo env fun git => do
    let head ← git.head?
    let commit := revision.toLower
    let mut group := group
    if head != some commit then
      let issue := match head with
        | some hash => ("checkout_revision_mismatch", s!"Checkout is at {hash}. The root manifest selects {revision}.")
        | none => ("unknown_checkout_revision", s!"Could not verify the checkout's HEAD in {repo}.")
      let .ok true ← (git.hasCommit commit).toBaseIO | do
        if head.isNone then return none
        return some (← group.mapM fun q => finish q (.error issue))
      group := group.map fun q => if q.issues.contains issue then q else { q with issues := q.issues.push issue }
    let results ← group.mapM fun q => do
      match ← (git.directoryOrExit commit q.path).toBaseIO with
      | .error e => finish q (.error ("unknown_package_directory", toString e))
      | .ok (.inside path) =>
        finish q (.ok { path, repo := some repo, readText := git.readText? commit, committed := true })
      | .ok (.outside remaining) =>
        if q.gitRepo.isSome then
          return ← finish q (.error ("unknown_package_directory", s!"{q.path} leaves the locked repository"))
        return .inl {q with path := remaining}
    return some results).toBaseIO
  if let .ok (some results) := batch then return results
  fallback ()

/-- Group both package inspections and source path comparisons by repository so each batch reads
    one locked Git tree. A path that crosses a repository boundary is routed to its new owner. -/
private def inspectDirectories (repos : Repositories) (env : GitTree.Env)
    (queries : Array (DirectoryQuery α)) (inspect : Inspect α β) : IO (Array β) := do
  let cache ← IO.mkRef ∅
  let mut pending := queries
  let mut results := #[]
  while !pending.isEmpty do
    let mut groups : Std.HashMap Checkout (Array (DirectoryQuery α)) := ∅
    for q in pending do
      let located ← (locatePath repos cache q).toBaseIO
      let (q, repo) ← match located with
        | .ok location => pure location
        | .error e =>
          results := results.push (← inspect q (.error ("unknown_package_directory", toString e)))
          continue
      if let some repo := repo then
        groups := groups.alter repo fun group => some ((group.getD #[]).push q)
      else
        results := results.push (← inspect q (.ok { path := q.path }))
    pending := #[]
    let batches ← mapConcurrent groups.toArray fun (checkout, group) =>
      inspectGit checkout.repo checkout.revision env group inspect
    for batch in batches do
      for result in batch do
        match result with
        | .inl query => pending := pending.push query
        | .inr value => results := results.push value
  return results

private abbrev Observations := Std.TreeMap String Observation
private abbrev SourcePaths := Std.HashMap FilePath (Except Issue FilePath)

/-- A cache entry may link to a working tree that local path dependencies also name directly.
    Both spellings must use the locked commit. An explicit cache name disambiguates different
    revisions of one working tree; a physical path alone cannot. -/
private def repositories (packagesDir : FilePath) (lock : Lock) : IO Repositories := do
  let mut physical : Repositories := ∅
  let mut entries : Repositories := ∅
  for (name, pkg) in lock.packages do
    if let .git _ rev .. := pkg.source then
      let entry := packagesDir / name
      let repo := (← (IO.FS.realPath entry).toBaseIO).toOption.getD entry
      let checkout := {repo, revision := rev.toLower : Checkout}
      entries := entries.insert entry (.ok checkout)
      if entry != repo then
        physical := physical.alter repo fun existing => some <| match existing with
          | none => .ok checkout
          | some (.ok prior) => if prior == checkout then .ok checkout else
              .error s!"Multiple revisions are locked for the checkout at {repo}."
          | some (.error e) => .error e
  return if physical.isEmpty then entries else physical.insertMany entries

private def observe (project packages : FilePath) (lock : Lock) (repos : Repositories)
    (env : GitTree.Env) : IO (Observations × SourcePaths) := do
  let mut queries := #[]
  for (name, entry) in lock.packages do
    match entry.source with
    | .git _ _ subDir _ =>
      let repo := packages / name
      queries := queries.push {key := entry, path := subDir.elim repo (repo / ·), gitRepo := some repo}
    | .path dir copy =>
      queries := queries.push {key := entry, path := if copy then packages / name else project / dir}
  let observations ← inspectDirectories repos env queries fun q result => do
    let obs ← match result with
      | .error (kind, message) => pure (problem ⟨q.key, q.path⟩ kind message)
      | .ok dir => do
        let p := ⟨q.key, dir.path⟩
        let available ← if dir.committed then pure true else dir.path.isDir
        if available then readPackage p project dir.repo dir.readText dir.issues else pure (missing p)
    return ({obs with issues := q.issues ++ obs.issues}, result.map (·.path))
  let mut packages := ∅
  let mut sources := ∅
  for (obs, path) in observations do
    packages := packages.insert obs.entry.name obs
    if let .path dir false := obs.entry.source then sources := sources.insert (project / dir) path
  return (packages, sources)

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

private def sourcePaths (project : FilePath) (repos : Repositories) (env : GitTree.Env)
    (observations : Observations) (known : SourcePaths) : IO SourcePaths := do
  let mut requested : Std.HashSet FilePath := ∅
  for (_, parent) in observations do
    for (child, wanted) in parent.edges do
      let some selected := observations[child]? | continue
      if let .path dir _ := wanted.source then
        if let .path rootDir _ := selected.entry.source then
          let left := parent.checkout / dir
          let right := project / rootDir
          if left != right then
            unless known.contains left do requested := requested.insert left
            unless known.contains right do requested := requested.insert right
  let resolved ← inspectDirectories repos env (requested.toArray.map fun path => {key := path, path})
    fun q result => pure (q.key, result.map (·.path))
  return resolved.foldl (init := known) fun m (key, path) => m.insert key path

private def conflict (parent : Observation) (wanted selected : LockedPackage) (project : FilePath)
    (sources : SourcePaths) : Option Issue := Id.run do
  let name := parent.entry.name
  match wanted.source, selected.source with
  | .git url rev sub _, .git rootUrl rootRev rootSub _ =>
    if rev.toLower != rootRev.toLower then
      return some ("revision_conflict", s!"{name} locks {rev}. The root selects {rootRev}.")
    if !sameRemote url rootUrl then
      return some ("source_conflict", s!"{name} locks repository {url}. The root locks {rootUrl}.")
    if Source.directoryKey (sub.getD "") != Source.directoryKey (rootSub.getD "") then
      return some ("source_conflict", s!"{name} and the root select different package subdirectories at the same revision.")
  | .path dir _, .path rootDir _ =>
    let requested := parent.checkout / dir
    let actual := project / rootDir
    if requested == actual then return none
    match sources[requested]!, sources[actual]! with
    | .ok requested, .ok actual =>
      if requested != actual then return some ("source_conflict", s!"{name} locks path {requested}. The root locks {actual}.")
    | .error (_, message), _ | _, .error (_, message) => return some ("unknown_path_source", message)
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

/-- Inspect the local dependency graph and return the exact manifest used for the report. -/
def analyzeWithManifest (project : FilePath) (target : String) : IO (Json × Json) := do
  let project ← IO.FS.realPath project
  let target ← IO.ofExcept (normalizeToolchain target)
  let file := project / manifestName
  let raw ← IO.ofExcept ((Json.parse (← IO.FS.readFile file)).mapError (s!"{file}: {·}"))
  let lock ← IO.ofExcept (Lock.ofJson file raw)
  let dirname := project.fileName.getD "[anonymous]"
  let root := lock.name.getD dirname
  let root := if root == "[anonymous]" then dirname else root
  let lakeDir := project / lock.lakeDir
  let packagesDir ← resolvePath (lock.packagesDir.elim (lakeDir / "packages") (project / ·))
  let rootPin ← pin? project fileText?
  let repos ← repositories packagesDir lock
  let env ← if repos.isEmpty then pure #[] else GitTree.environment
  let (observations, known) ← observe project packagesDir lock repos env
  let paths := shortestPaths root observations
  let sources ← sourcePaths project repos env observations known
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
    for (child, wanted) in obs.edges do
      let selected := lock.packages[child]?
      let issue := match selected with
        | some selected => conflict obs wanted selected project sources
        | none => some ("unlocked_dependency", s!"{name} locks {child}, which is absent from the root manifest.")
      if let some issue := issue then
        let detail := Json.mkObj [("parent", name), ("requested_revision", toJson wanted.source.revision?),
                                  ("selected_revision", toJson (selected.bind (·.source.revision?)))]
        let chain := if path.isEmpty then [] else path ++ [child]
        findings := findings.push ((finding child chain issue).mergeObj detail)
  let report := Json.mkObj [("project", root), ("target", target), ("root_toolchain", toJson rootPin),
                            ("packages", .arr packages), ("findings", .arr findings)]
  return (report, raw)

/-- Inspect the local dependency graph at the revisions selected by the root lock. -/
def analyze (project : FilePath) (target : String) : IO Json := do
  return (← analyzeWithManifest project target).1

end LakeCompat
