namespace LakeCompat.Source

/-- A full Git object ID in SHA-1 or SHA-256 format. -/
def isFullRevision (s : String) : Bool :=
  (s.utf8ByteSize == 40 || s.utf8ByteSize == 64) && s.all Char.isHexDigit

/-- Repository identity for the standard GitHub HTTPS, SSH, and SCP-style remotes. -/
def githubRepo? (url : String) : Option (String × String) := do
  let start ← ["https://github.com/", "ssh://git@github.com/", "git@github.com:"].find?
    (fun p => url.toLower.startsWith p)
  let path := ((url.drop start.length).dropSuffix "/").copy
  let [owner, name] := path.splitOn "/" | none
  guard (!owner.isEmpty && owner.all (fun c => c.isAlphanum || c == '-'))
  guard (name.all (fun (c : Char) => c.isAlphanum || "_.-".any (· == c)))
  let repo := (name.dropSuffix ".git").copy
  guard (!repo.isEmpty && repo != "." && repo != "..")
  return (owner, repo)

end LakeCompat.Source
