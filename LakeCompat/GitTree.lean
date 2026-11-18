import Std.Internal.UV.System
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

structure Reader where
  repo : FilePath
  env : Env
  input : IO.FS.Handle
  output : IO.FS.Handle

/-- Reuse one cat-file process across the commit and all package metadata in a checkout. -/
def withReader (repo : FilePath) (env : Env) (use : Reader → IO α) : IO α := do
  let child ← IO.Process.spawn {
    cmd := "git", args := #["--git-dir=.git", "cat-file", "--batch", "--follow-symlinks"],
    cwd := repo, env, stdin := .piped, stdout := .piped, stderr := .null }
  let (input, child) ← child.takeStdin
  try
    let value ← use ⟨repo, env, input, child.stdout⟩
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

private def Reader.lookup (git : Reader) (spec : String) : IO (Option Object) := do
  if spec.contains '\n' then throw <| IO.userError "cannot address a Git metadata path containing a newline"
  git.input.putStr (spec ++ "\n")
  git.input.flush
  let header := ((← git.output.getLine).dropSuffix "\n").copy
  if header == spec ++ " missing" then return none
  match header.splitOn " " with
  | [hash, type, size] =>
    let some n := size.toNat? | throw <| IO.userError "invalid Git object length"
    return some ⟨hash, type, ← readBytes git.output n⟩
  | [_, size] =>
    -- Git reports dangling, cyclic and out-of-tree links with a length-prefixed record.
    if let some n := size.toNat? then discard <| readBytes git.output n
    throw <| IO.userError s!"{spec} does not resolve inside the locked Git tree"
  | _ => throw <| IO.userError s!"Git could not read {spec}"

def Reader.head? (git : Reader) : IO (Option String) := do
  let some obj ← git.lookup "HEAD" | return none
  return if obj.type == "commit" then some obj.hash else none

private def Reader.blob? (git : Reader) (spec : String) : IO (Option String) := do
  let some obj ← git.lookup spec | return none
  unless obj.type == "blob" do throw <| IO.userError s!"{spec} does not resolve to a file"
  let some text := String.fromUTF8? obj.data | throw <| IO.userError s!"invalid UTF-8 in {spec}"
  return some text

def Reader.readText? (git : Reader) (head : String) (file : FilePath) : IO (Option String) := do
  let some relative := relativeTo? git.repo file | throw <| IO.userError s!"{file} leaves the locked repository"
  let path := if Platform.isWindows then relative.replace "\\" "/" else relative
  git.blob? (head ++ ":" ++ path)

/-- Resolve directory links against the locked tree, including '..' following a link. -/
def Reader.directory (git : Reader) (head : String) (checkout : FilePath) : IO FilePath := do
  let some relative := relativeTo? git.repo checkout | throw <| IO.userError "package path leaves the locked repository"
  let mut parts := (FilePath.mk relative).components
  let mut directory := git.repo
  let mut links := 0
  repeat
    let name :: rest := parts | break
    parts := rest
    if name.isEmpty || name == "." then continue
    if name == ".." then
      if directory == git.repo then throw <| IO.userError "package path leaves the locked repository"
      directory := directory.parent.getD git.repo
      continue
    let location := directory / name
    let path := (relativeTo? git.repo location).getD ""
    let result ← IO.Process.output {
      cmd := "git", cwd := git.repo, env := git.env,
      args := #["--git-dir=.git", "--literal-pathspecs", "ls-tree", "-z", head, "--", path] }
    let header := (result.stdout.takeWhile (· != '\t')).copy
    let [mode, _, hash] := header.splitOn " "
      | throw <| IO.userError s!"package directory is absent at {head}: {location}"
    if result.exitCode != 0 then throw <| IO.userError s!"Git could not resolve {location}"
    if mode == "040000" then directory := location
    else if mode == "120000" && links < 40 then
      let some target ← git.blob? hash | throw <| IO.userError s!"missing package link {location}"
      let target := FilePath.mk target
      if target.isAbsolute then throw <| IO.userError s!"package link {location} leaves the locked tree"
      parts := target.components ++ parts
      links := links + 1
    else throw <| IO.userError s!"cannot resolve package directory {location} at {head}"
  return directory

end LakeCompat.GitTree
