import Lean.Data.Json.FromToJson.Basic
import Lean.Data.Json.Parser
import Std.Data.HashMap
import Std.Http.Data.URI.Encoding
import LakeCompat.Source

open Lean

namespace LakeCompat.Releases

inductive RegistryError where
  | http (code : Nat)
  | transport
  | invalid
  deriving BEq, Repr

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
      args := #["--disable", "--silent", "--fail", "--max-time", "8", "--max-filesize",
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

-- Reservoir uses POSIX paths regardless of the client's operating system.
private def directory (value : String) : Bool × List String :=
  (value.startsWith "/", (value.splitOn "/").filter (fun p => !p.isEmpty && p != "."))

private def matchesSource (sources : Array Json) (repo subDir : String) : Bool :=
  sources.any fun source => Id.run do
    if stringField source "host" != "github" then return false
    if (stringField source "fullName").toLower != repo.toLower then return false
    match source.getObjValD "subDir" with
    | .null => return directory "." == directory subDir
    | .str path => return directory path == directory subDir
    | _ => return false

private def records (response : Json) : EIO RegistryError (Array Json) := do
  let .arr rows := response.getObjValD "data" | throw .invalid
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

private def indexEvidence (versions builds : Array Json) (target : String) : Except RegistryError Index := do
  let mut index : Index := {}
  for row in builds do
    if stringField row "toolchain" != target then continue
    let some rev := revision? row | continue
    let runAt := (← optionalField row "runAt").getD ""
    let evidence := index.getD rev {}
    -- A later failed run disqualifies even a revision which declares the target.
    if evidence.build.any (·.runAt >= runAt) then continue
    let tested : Option Bool ← optionalField row "tested"
    let build := {
      runAt, tested
      passed := row.getObjValD "built" == .bool true && tested != some false
      requiredUpdate := ← optionalField row "requiredUpdate"
      url := ← optionalField row "url"
    }
    index := index.insert rev { evidence with build := some build }
  for row in versions do
    let some rev := revision? row | continue
    let evidence := index.getD rev {}
    let date := (← optionalField row "date").getD ""
    let label := stringField row "tag"
    let tag := if printable label && !evidence.tag.any (·.1 >= date) then some (date, label) else evidence.tag
    index := index.insert rev { evidence with
      declared := evidence.declared || stringField row "toolchain" == target
      date := max evidence.date date
      tag }
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

attribute [local instance] lexOrd

private def ranked (index : Index) (target : String) : Json := Id.run do
  let mut best := []
  let mut count := 0
  for (rev, evidence) in index do
    unless evidence.build.elim evidence.declared (·.passed) do continue
    count := count + 1
    let item := (priority rev evidence, rev, evidence)
    best := ([item].merge best fun a b => compare a.1 b.1 == .gt).take candidateLimit
  if best.isEmpty then
    return result "no_match" "No successful build or eligible revision declaring this target in Reservoir."
  return result "found" s!"Showing {best.length} of {count} candidate revisions."
    (best.map fun (_, rev, evidence) => candidate target rev evidence)

private def lookup (fetch : Fetch) (endpoint target repo : String) (package subDir : Option String) :
    EIO RegistryError Json := do
  let metadata ← fetch endpoint
  unless metadata matches .obj _ do throw .invalid
  if package.any (fun name => name != stringField metadata "name") then
    return result "unsupported" "Registry package name differs from the requested name."
  let .arr sources := metadata.getObjValD "sources" | throw .invalid
  unless matchesSource sources repo (subDir.getD ".") do
    return result "unsupported" "Registry source differs in repository or package directory."
  let versions ← records (← fetch (endpoint ++ "/versions"))
  let builds ← records (← fetch (endpoint ++ "/builds"))
  let index ← ofExcept (indexEvidence versions builds target)
  return ranked index target

/-- Look up one Git package and rank revisions with evidence for the exact target toolchain. -/
def suggestVersions (url target : String) (package scope subDir : Option String := none)
    (fetch : Fetch := fetchJson) : IO Json := do
  let some (owner, repo) := Source.githubRepo? url
    | return result "unsupported" "Reservoir lookup requires an identifiable GitHub repository."
  let pkg := (package.filter (!·.isEmpty)).getD repo
  let org := (scope.filter (!·.isEmpty)).getD owner
  unless identifier pkg && identifier org do
    return result "unsupported" "Invalid Reservoir package identifier."
  let endpoint := s!"{registry}/{encode org}/{encode pkg}"
  match ← (lookup fetch endpoint target (owner ++ "/" ++ repo) (package.filter (!·.isEmpty)) subDir).toBaseIO with
  | .ok value => return value
  | .error err =>
    let message := match err with
      | .http 404 => "Package or release metadata is absent from the Reservoir index."
      | .http code => s!"Reservoir returned HTTP {code}."
      | .invalid => "Reservoir returned incomplete, oversized, or invalid metadata."
      | .transport => "Could not reach Reservoir or finish reading its response."
    return result "unavailable" message

end LakeCompat.Releases
