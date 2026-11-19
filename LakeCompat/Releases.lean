import Lake.Util.Reservoir
import Std.Data.HashMap
import Std.Http.Data.URI.Encoding
import LakeCompat.Manifest
import LakeCompat.Source

open Lean

namespace LakeCompat.Releases

inductive RegistryError where
  | http (code : Nat)
  | transport
  | invalid
  deriving BEq, Repr

private def RegistryError.message : RegistryError → String
  | .http 404 => "Package or release metadata is absent from the Reservoir index."
  | .http code => s!"Reservoir returned HTTP {code}."
  | .invalid => "Reservoir returned incomplete, oversized, or invalid metadata."
  | .transport => "Could not reach Reservoir or finish reading its response."

/-- Fetch package metadata first. Independent version and build requests may run concurrently. -/
abbrev Fetch := String → EIO RegistryError Json

private def registry := "https://reservoir.lean-lang.org/api/v1/packages"
private def byteLimit := 8 * 1024 * 1024
private def candidateLimit := 5

private def readBody (h : IO.FS.Handle) : IO (Option ByteArray) := do
  let mut body := ByteArray.empty
  repeat
    let chunk ← h.read 65536
    if chunk.isEmpty then return some body
    if body.size + chunk.size > byteLimit then return none
    body := body ++ chunk
  return some body

/-- curl supplies HTTPS. The byte limit is enforced while reading, even for chunked responses. -/
private def fetchJson (url : String) : EIO RegistryError Json := do
  let (body?, status, exitCode) ← EIO.adapt (fun _ => .transport) do
    let child ← IO.Process.spawn {
      cmd := "curl"
      args := #["--disable", "--silent", "--fail", "--compressed", "--max-time", "8", "--max-filesize",
                toString byteLimit, "--header", "Accept: application/json", "--user-agent",
                "lake-compat", "--write-out", "%{stderr}%{http_code}", "--", url]
      stdin := .null, stdout := .piped, stderr := .piped
    }
    try
      let body? ← readBody child.stdout
      if body?.isNone then child.kill
      let status ← child.stderr.readToEnd
      return (body?, status, ← child.wait)
    catch error =>
      discard child.kill.toBaseIO
      discard child.wait.toBaseIO
      throw error
  let some body := body? | throw .invalid
  if exitCode == 63 then throw .invalid -- curl's maximum file size
  if exitCode != 0 && exitCode != 22 then throw .transport
  let some code := status.toNat? | throw .transport
  unless code == 200 do throw (.http code)
  let some raw := String.fromUTF8? body | throw .invalid
  let .ok json := Json.parse raw | throw .invalid
  return json

private def printable (value : String) : Bool :=
  !value.isEmpty && value.all fun c =>
    c >= ' ' && !(c >= '\x7f' && c <= '\u009f') && !(c >= '\u2028' && c <= '\u202e')
      && !(c >= '\u2066' && c <= '\u2069')

private def identifier (s : String) : Bool :=
  printable s && s != "." && s != ".." && !s.contains '/'

private def encode (s : String) : String :=
  toString (Std.Http.URI.EncodedString.encode (r := Std.Http.Internal.Char.isUnreserved) s)

private def stringField (row : Json) (name : String) : String :=
  ((row.getObjValD name).getStr?).toOption.getD ""

private def matchesTarget (row : Json) (target : String) : Bool :=
  let declared := stringField row "toolchain"
  declared == target || (normalizeToolchain declared).toOption == some target

private def matchesSource (sources : Array Json) (repo subDir : String) : Bool :=
  let repo := repo.toLower
  let dir := Source.directoryKey subDir
  sources.any fun source => Id.run do
    if stringField source "host" != "github" then return false
    if (stringField source "fullName").toLower != repo then return false
    match source.getObjValD "subDir" with
    | .null => return dir == (false, [])
    | .str path => return Source.directoryKey path == dir
    | _ => return false

private def payload (fetch : Fetch) (url : String) : EIO RegistryError Json := do
  match (fromJson? (← fetch url) : Except String (Lake.ReservoirResp Json)) with
  | .ok (.data value) => return value
  | .ok (.error status _) => throw (.http status)
  | .error _ => throw .invalid

private def records (response : Json) : EIO RegistryError (Array Json) := do
  let .arr rows := response | throw .invalid
  unless rows.all (fun row => row matches .obj _) do throw .invalid
  return rows

private structure Build where
  runAt : String
  passed : Bool
  tested : Option Bool
  requiredUpdate : Option Bool
  url : Option String
  deriving Inhabited

private structure Evidence where
  build : Option Build := none
  declared : Bool := false
  date : String := ""
  /-- Date of the newest usable tag may be older than the newest version record. -/
  tag : Option (String × String) := none
  deriving Inhabited

private abbrev Index := Std.HashMap String Evidence

private def revision? (row : Json) : Option String := do
  let rev ← (row.getObjValD "revision").getStr?.toOption
  guard (Source.isFullRevision rev)
  return rev.toLower

private def optionalField [FromJson α] (row : Json) (key : String) : Except RegistryError (Option α) :=
  (fromJson? (row.getObjValD key)).mapError fun _ => .invalid

attribute [local instance] lexOrd

private def indexVersions (versions : Array Json) (target : String) : Except RegistryError Index := do
  let mut index : Index := {}
  for row in versions do
    let some rev := revision? row | continue
    let evidence := index.getD rev {}
    let date := (← optionalField row "date").getD ""
    let label := stringField row "tag"
    let tag := if printable label && compare evidence.tag (some (date, label)) == .lt
      then some (date, label) else evidence.tag
    index := index.insert rev { evidence with
      declared := evidence.declared || matchesTarget row target
      date := max evidence.date date
      tag }
  return index

/-- A failure wins a tie in run times. Among equally recent successes, keep the
    strongest build evidence and use the URL as a stable final tie breaker. -/
private def Build.priority (b : Build) :=
  (b.runAt, !b.passed, b.requiredUpdate == some false, b.requiredUpdate.isNone,
    b.tested == some true, b.url)

private def indexBuilds (initial : Index) (builds : Array Json) (target : String) : Except RegistryError Index := do
  let mut index := initial
  for row in builds do
    unless matchesTarget row target do continue
    let some rev := revision? row | continue
    let .bool built := row.getObjValD "built" | throw .invalid
    let tested : Option Bool ← optionalField row "tested"
    let build : Build := {
      runAt := (← optionalField row "runAt").getD ""
      tested
      passed := built && tested != some false
      requiredUpdate := ← optionalField row "requiredUpdate"
      url := ← optionalField row "url"
    }
    let evidence := index.getD rev {}
    if evidence.build.any (fun prev => compare prev.priority build.priority != .lt) then continue
    index := index.insert rev { evidence with build := some build }
  return index

private def priority (rev : String) (e : Evidence) :=
  let update := e.build.bind (·.requiredUpdate)
  let date := if e.date.isEmpty then (e.build.map (·.runAt)).getD "" else e.date
  (e.build.isSome, update == some false, update.isNone, e.tag.isSome, e.declared, date, rev)

private def candidate (target rev : String) (e : Evidence) : Json :=
  Json.mkObj [
    ("revision", rev), ("tag", toJson (e.tag.map Prod.snd)), ("toolchain", target),
    ("evidence", if e.build.isSome then "built" else "declared"),
    ("required_update", toJson (e.build.bind (·.requiredUpdate))),
    ("tested", toJson (e.build.bind (·.tested))), ("build_url", toJson (e.build.bind (·.url)))]

private def result (status message : String) (candidates : List Json := []) : Json :=
  Json.mkObj [("status", status), ("message", message), ("candidates", .arr candidates.toArray)]

private def ranked (index : Index) (target : String) (missing : String := "") : Json := Id.run do
  let mut best := []
  let mut count := 0
  for (rev, evidence) in index do
    unless evidence.build.elim evidence.declared (·.passed) do continue
    count := count + 1
    let item := (priority rev evidence, rev, evidence)
    best := ([item].merge best fun a b => compare a.1 b.1 == .gt).take candidateLimit
  if best.isEmpty then
    unless missing.isEmpty do return result "unavailable" missing
    return result "no_match" "No successful build or eligible revision declaring this target in Reservoir."
  let summary := s!"Showing {best.length} of {count} candidate revisions."
  return result "found" (if missing.isEmpty then summary else summary ++ " " ++ missing)
    (best.map fun (_, rev, evidence) => candidate target rev evidence)

private def lookup (fetch : Fetch) (endpoint target repo : String) (package subDir : Option String) :
    EIO RegistryError Json := do
  let metadata ← payload fetch endpoint
  unless metadata matches .obj _ do throw .invalid
  if let some expected := package then
    let .str name := metadata.getObjValD "name" | throw .invalid
    if name.isEmpty then throw .invalid
    -- Reservoir indexes package identifiers case-insensitively.
    if name.toLower != expected.toLower then
      return result "unsupported" "Registry package name differs from the requested name."
  let .arr sources := metadata.getObjValD "sources" | throw .invalid
  unless matchesSource sources repo (subDir.getD ".") do
    return result "unsupported" "Registry source differs in repository or package directory."
  -- Both histories describe the source we just verified and can be retrieved independently.
  let fetchRows := fun suffix => do records (← payload fetch (endpoint ++ suffix))
  let buildsTask ← EIO.asTask (fetchRows "/builds") .dedicated
  let versions ← (do ofExcept (indexVersions (← fetchRows "/versions") target)).toBaseIO
  let declarations := versions.toOption.getD {}
  let builds := (← IO.wait buildsTask).bind (indexBuilds declarations · target)
  match versions, builds with
  | .ok _, .ok index => return ranked index target
  | .error err, .ok index =>
    let report := ranked index target s!"Version history unavailable. {err.message}"
    return report.setObjVal! "unavailable_history" "versions"
  | .ok index, .error err =>
    -- Malformed build records may include a known failure for an otherwise eligible declaration.
    let usable := if err == .invalid then {} else index
    let report := ranked usable target s!"Build history unavailable. {err.message}"
    return report.setObjVal! "unavailable_history" "builds"
  | .error err, .error _ => throw err

private def registrySource (url : String) (package scope : Option String) : Except String (String × String) := do
  let some (owner, repo) := Source.githubRepo? url
    | throw "Reservoir lookup requires an identifiable GitHub repository."
  let pkg := (package.filter (!·.isEmpty)).getD repo
  let org := (scope.filter (!·.isEmpty)).getD owner
  unless identifier pkg && identifier org do throw "Invalid Reservoir package identifier."
  return (s!"{registry}/{encode org}/{encode pkg}", owner ++ "/" ++ repo)

def supportsLookup (url : String) (package scope : Option String) : Bool :=
  (registrySource url package scope).isOk

/-- Look up one Git package and rank revisions with evidence for the exact target toolchain. -/
def suggestVersions (url target : String) (package scope subDir : Option String := none)
    (fetch : Fetch := fetchJson) : IO Json := do
  let .ok target := normalizeToolchain target | return result "unsupported" "Invalid target toolchain."
  let (endpoint, repo) ← match registrySource url package scope with
    | .ok source => pure source
    | .error message => return result "unsupported" message
  match ← (lookup fetch endpoint target repo (package.filter (!·.isEmpty)) subDir).toBaseIO with
  | .ok value => return value
  | .error err => return result "unavailable" err.message

end LakeCompat.Releases
