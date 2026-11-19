namespace LakeCompat.Source

/-- A full Git object ID in SHA-1 or SHA-256 format. -/
def isFullRevision (s : String) : Bool :=
  (s.utf8ByteSize == 40 || s.utf8ByteSize == 64) && s.all Char.isHexDigit

/-- Compare Git package subdirectories as POSIX paths, preserving symlink-sensitive `..`. -/
def directoryKey (value : String) : Bool × List String :=
  (value.startsWith "/", (value.splitOn "/").filter (fun p => !p.isEmpty && p != "."))

/-- Git interprets a colon before the first slash as a URL, remote helper, or SCP-style
address. Absolute filesystem paths include drive-qualified paths on Windows. -/
def localGitPath? (url : String) : Option System.FilePath :=
  let path : System.FilePath := url
  if path.isAbsolute || !(url.takeWhile (· != '/')).contains ':' then some path else none

/-- Repository identity for the standard GitHub HTTPS, SSH, and SCP-style remotes. -/
def githubRepo? (url : String) : Option (String × String) := do
  let lower := url.toLower
  let start ← ["https://github.com/", "https://github.com:443/", "ssh://git@github.com/",
    "ssh://git@github.com:22/", "git@github.com:"].find? (fun p => lower.startsWith p)
  let path := ((url.drop start.length).dropSuffix "/").copy
  let [owner, name] := path.splitOn "/" | none
  guard (!owner.isEmpty && owner.all (fun c => c.isAlphanum || c == '-'))
  guard (name.all (fun (c : Char) => c.isAlphanum || "_.-".any (· == c)))
  let repo := (name.dropSuffix ".git").copy
  guard (!repo.isEmpty && repo != "." && repo != "..")
  return (owner, repo)

end LakeCompat.Source
