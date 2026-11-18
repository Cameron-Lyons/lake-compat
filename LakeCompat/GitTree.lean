import Std.Internal.UV.System
import Std.Data.HashMap
import Init.Data.String

namespace LakeCompat.GitTree
open System

abbrev Env := Array (String × Option String)

/-- Metadata reads use the selected checkout independently of the caller's Git environment. -/
def environment : IO Env := do
  let variables ← Std.Internal.UV.System.osEnviron
  let clean := variables.filterMap fun (k, _) =>
    if k.startsWith "GIT_" then some (k, none) else none
  return clean ++ #[("GIT_NO_REPLACE_OBJECTS", some "1"), ("GIT_NO_LAZY_FETCH", some "1"),
                    ("GIT_ALLOW_PROTOCOL", some "")]

def relativeTo? (root path : FilePath) : Option String :=
  if root == path then some ""
  else
    let sep := FilePath.pathSeparator.toString
    let start := if root.toString.endsWith sep then root.toString else root.toString ++ sep
    (path.toString.dropPrefix? start).map (·.copy)

private structure Entry where
  mode : String
  oid : ByteArray

private abbrev Tree := Std.HashMap String Entry

structure Reader where
  repo : FilePath
  env : Env
  input : IO.FS.Handle
  output : IO.FS.Handle
  private trees : IO.Ref (Std.HashMap String Tree)

/-- Share one cat-file process and the parsed directory trees across a checkout. -/
def withReader (repo : FilePath) (env : Env) (use : Reader → IO α) : IO α := do
  let trees ← IO.mkRef ∅
  let child ← IO.Process.spawn {
    cmd := "git", args := #["--git-dir=.git", "cat-file", "--batch", "--follow-symlinks"],
    cwd := repo, env, stdin := .piped, stdout := .piped, stderr := .null }
  let (input, child) ← child.takeStdin
  try
    let value ← use ⟨repo, env, input, child.stdout, trees⟩
    discard child.wait
    return value
  catch e =>
    try child.kill catch _ => pure ()
    discard child.wait
    throw e

private def readBytes (out : IO.FS.Handle) (count : Nat) : IO ByteArray := do
  let mut data := ByteArray.empty
  while data.size < count do
    let chunk ← out.read (min (count - data.size) 65536).toUSize
    if chunk.isEmpty then throw <| IO.userError "unexpected end of Git object"
    data := data ++ chunk
  if (← out.getLine) != "\n" then throw <| IO.userError "invalid Git object delimiter"
  return data

private structure Object where
  hash : String
  type : String
  data : ByteArray

private inductive Record where
  | found (object : Object)
  | missing
  | unresolved

private def Reader.lookup (git : Reader) (spec : String) : IO Record := do
  if spec.contains '\n' then throw <| IO.userError "invalid newline in Git object query"
  git.input.putStr (spec ++ "\n")
  git.input.flush
  let header := ((← git.output.getLine).dropSuffix "\n").copy
  if header == spec ++ " missing" then return .missing
  match header.splitOn " " with
  | [hash, type, size] =>
    let some n := size.toNat? | throw <| IO.userError "invalid Git object length"
    return .found ⟨hash, type, ← readBytes git.output n⟩
  | [kind, size] =>
    if ["symlink", "dangling", "loop", "notdir"].contains kind then
      if let some n := size.toNat? then
        discard <| readBytes git.output n
        return .unresolved
    throw <| IO.userError s!"Git could not read {spec}"
  | _ => throw <| IO.userError s!"Git could not read {spec}"

def Reader.head? (git : Reader) : IO (Option String) := do
  let .found obj ← git.lookup "HEAD" | return none
  return if obj.type == "commit" then some obj.hash else none

private def Object.text (obj : Object) (name : String) : IO String := do
  unless obj.type == "blob" do throw <| IO.userError s!"{name} does not resolve to a file"
  let some text := String.fromUTF8? obj.data | throw <| IO.userError s!"invalid UTF-8 in {name}"
  return text

private def Reader.blob (git : Reader) (hash : String) : IO String := do
  let .found obj ← git.lookup hash | throw <| IO.userError s!"missing Git blob {hash}"
  obj.text hash

/-- A raw Git tree repeats mode, space, filename, NUL, and a binary object ID.
Only filenames representable in a Lake manifest need to be indexed. -/
private def Reader.tree (git : Reader) (spec : String) : IO Tree := do
  if let some tree := (← git.trees.get)[spec]? then return tree
  let .found obj ← git.lookup spec | throw <| IO.userError s!"missing Git tree {spec}"
  unless obj.type == "tree" do throw <| IO.userError s!"expected a Git tree at {spec}"
  let invalid := IO.userError s!"invalid Git tree {obj.hash}"
  let hashBytes := obj.hash.utf8ByteSize / 2
  let mut entries : Tree := ∅
  let mut offset := 0
  while offset < obj.data.size do
    let some space := obj.data.findIdx? (· == 32) offset | throw invalid
    let some nul := obj.data.findIdx? (· == 0) (space + 1) | throw invalid
    let next := nul + 1 + hashBytes
    if next > obj.data.size then throw invalid
    if let some name := String.fromUTF8? (obj.data.extract (space + 1) nul) then
      let some mode := String.fromUTF8? (obj.data.extract offset space) | throw invalid
      entries := entries.insert name ⟨mode, obj.data.extract (nul + 1) next⟩
    offset := next
  git.trees.modify (·.insert spec entries)
  return entries

private def Entry.hash (entry : Entry) : String :=
  entry.oid.foldl (init := "") fun s byte =>
    (s.push (byte.toNat / 16).digitChar).push (byte.toNat % 16).digitChar

private inductive Node where
  | dir (path : FilePath)
  | file (hash : String)
  | outside (remaining : FilePath)

/-- Walk committed entries component by component so that a link is resolved before a
following '..'. The ancestor stack also locates the toolchain inherited by a subpackage. -/
private def Reader.resolve? (git : Reader) (head : String) (path : FilePath) : IO (Option Node) := do
  let some relative := relativeTo? git.repo path | throw <| IO.userError s!"{path} leaves the locked repository"
  let mut pending := [(FilePath.mk relative).components]
  let mut dirs := [(git.repo, head ++ "^{tree}")]
  let mut links := 0
  repeat
    let (dir, tree) := dirs.head!
    let parts :: suffixes := pending | return some (.dir dir)
    let name :: rest := parts | do
      pending := suffixes
      continue
    pending := rest :: suffixes
    if name.isEmpty || name == "." then continue
    if name == ".." then
      if dir == git.repo then
        unless suffixes.isEmpty do throw <| IO.userError s!"symlink in {path} leaves the locked repository"
        return some (.outside (rest.foldl (fun p (part : String) => p / part) (dir.parent.getD dir)))
      dirs := dirs.tail!
      continue
    let some entry := (← git.tree tree)[name]? | do
      unless suffixes.isEmpty do throw <| IO.userError s!"{path} does not resolve inside the locked Git tree"
      return none
    match entry.mode with
    | "40000" => dirs := (dir / name, entry.hash) :: dirs
    | "120000" =>
      if links >= 40 then throw <| IO.userError s!"too many symlinks in {path}"
      let target : FilePath := ← git.blob entry.hash
      if target.isAbsolute then throw <| IO.userError s!"{dir / name} leaves the locked repository"
      if target.toString.isEmpty || target.toString.contains (Char.ofNat 0) then
        throw <| IO.userError s!"invalid Git symlink at {dir / name}"
      pending := target.components :: pending
      links := links + 1
    | "100644" | "100755" =>
      unless pending.all List.isEmpty do throw <| IO.userError s!"{dir / name} is not a directory"
      return some (.file entry.hash)
    | _ => throw <| IO.userError s!"unsupported Git entry at {dir / name}"
  return none

def Reader.readText? (git : Reader) (head : String) (file : FilePath) : IO (Option String) := do
  let some relative := relativeTo? git.repo file | throw <| IO.userError s!"{file} leaves the locked repository"
  -- Let Git read a normal file directly without indexing all of its siblings. Its symlink
  -- walker can report dangling for valid './' targets. Resolve those with the locked trees.
  let parts := (FilePath.mk relative).components
  unless relative.contains '\n' || parts.contains "." || parts.contains ".." do
    match ← git.lookup (head ++ ":" ++ "/".intercalate parts) with
    | .found obj => return some (← obj.text file.toString)
    | .missing => return none
    | .unresolved => pure ()
  match ← git.resolve? head file with
  | none => return none
  | some (.file hash) => some <$> git.blob hash
  | some (.dir _) => throw <| IO.userError s!"{file} does not resolve to a file"
  | some (.outside _) => throw <| IO.userError s!"{file} leaves the locked repository"

inductive DirectoryLocation where
  | inside (path : FilePath)
  | outside (remaining : FilePath)

/-- Return the remaining filesystem path when a local dependency climbs out of the checkout. -/
def Reader.directoryOrExit (git : Reader) (head : String) (path : FilePath) :
    IO DirectoryLocation := do
  match ← git.resolve? head path with
  | some (.dir dir) => return .inside dir
  | some (.outside rest) => return .outside rest
  | _ => throw <| IO.userError s!"package directory is absent at {head}: {path}"

def Reader.directory (git : Reader) (head : String) (checkout : FilePath) : IO FilePath := do
  let .inside dir ← git.directoryOrExit head checkout
    | throw <| IO.userError s!"{checkout} leaves the locked repository"
  return dir

end LakeCompat.GitTree
