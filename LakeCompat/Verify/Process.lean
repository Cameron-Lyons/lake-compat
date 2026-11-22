import Init.Data.String.Lossy
import Init.Data.String.FindPos
import Std.Internal.UV.System
import Std.Sync.Mutex

open System

namespace LakeCompat.Verify

def logLimit := 16 * 1024

structure CommandResult where
  exitCode : UInt32
  output : String
  timedOut : Bool

private structure Excerpt where
  tail : ByteArray := .empty
  diagnostic : ByteArray := .empty

/-- A byte cutoff can fall inside a valid UTF-8 character. Keep its leading byte
at the cut so decoding still distinguishes valid characters from malformed output. -/
private def charStart (bytes : ByteArray) (cut : Nat) : Nat := Id.run do
  unless bytes[cut]?.any (·.isUTF8ContinuationByte) do return cut
  for i in [cut - 3:cut] do
    if let some c := bytes.utf8DecodeChar? i then
      if cut < i + c.utf8Size then return i
  return cut

/-- Find the first compiler diagnostic about to scroll out of the tail. -/
private def scrollingDiagnostic (bytes : ByteArray) (overflow : Nat) : ByteArray := Id.run do
  let mut start := 0
  let mut markerStart := true
  for i in [:min bytes.size (overflow + 512)] do
    let c := Char.ofUInt8 bytes[i]!
    if markerStart then
      if c.toLower == 'e' || c.toLower == 'f' then
        let word := (String.fromUTF8Lossy (bytes.extract i (i + 12))).toLower
        if word.startsWith "error:" || word.startsWith "fatal:" || word.startsWith "fatal error:" then
          let offset := max start (i - 512)
          if offset < overflow then
            return bytes.extract (charStart bytes offset) (charStart bytes (offset + logLimit / 4))
    markerStart := c == '\n' || c == ':' || (markerStart && (c == ' ' || c == '\t'))
    if c == '\n' then start := i + 1
  return .empty

private def Excerpt.append (log : Excerpt) (block : ByteArray) : Excerpt := Id.run do
  let bytes := log.tail ++ block
  let overflow := bytes.size - logLimit
  if overflow == 0 then return { log with tail := bytes }
  let diagnostic := if log.diagnostic.isEmpty then scrollingDiagnostic bytes overflow else log.diagnostic
  return { tail := bytes.extract (charStart bytes overflow) bytes.size, diagnostic }

/-- Keep complete characters within the report's UTF-8 byte budget. Decoding an invalid
byte into the replacement character can increase the number of bytes in a build log. -/
def boundedLogTail (message : String) (limit := logLimit) : String := Id.run do
  if message.utf8ByteSize ≤ limit then return message
  let start := message.posGE ⟨message.utf8ByteSize - limit⟩ (Nat.sub_le ..)
  return (message.sliceFrom start).copy

private def Excerpt.text (log : Excerpt) : String := Id.run do
  let tail := String.fromUTF8Lossy log.tail
  -- Replacement characters can push a diagnostic out of the rendered output
  -- even when all of the original bytes fitted in the raw tail.
  let diagnostic := if log.diagnostic.isEmpty && tail.utf8ByteSize > logLimit then
      scrollingDiagnostic tail.toUTF8 (tail.utf8ByteSize - logLimit)
    else log.diagnostic
  if diagnostic.isEmpty then return (boundedLogTail tail).trimAscii.copy
  let diagnostic := String.fromUTF8Lossy diagnostic
  let omitted := "\n[... output omitted ...]\n"
  let excerpt := diagnostic ++ omitted ++ boundedLogTail tail (logLimit - diagnostic.utf8ByteSize - omitted.utf8ByteSize)
  return excerpt.trimAscii.copy

private def readOutput (pipe : IO.FS.Handle) (log : Std.Mutex Excerpt) : IO Unit := do
  repeat
    let block ← pipe.read 8192
    if block.isEmpty then break
    log.atomically <| modify (·.append block)

abbrev Environment := Array (String × Option String)

def trialEnvironment (target : String) (parent : FilePath) : IO Environment := do
  let vars ← Std.Internal.UV.System.osEnviron
  let removed : Environment := vars.filterMap fun (name, _) =>
    if ["LEAN_", "LAKE_", "GIT_"].any (name.startsWith ·) || name == "LEAN" || name == "LAKE" then
      some (name, none) else none
  let added : Array (String × String) := #[("ELAN_TOOLCHAIN", target), ("GIT_TERMINAL_PROMPT", "0"),
    ("GIT_CEILING_DIRECTORIES", parent.toString), ("GIT_DEFAULT_HASH", "sha1"), ("LAKE_NO_CACHE", "true"),
    ("LAKE_ARTIFACT_CACHE", "false"), ("LAKE_CACHE_DIR", (parent / "cache").toString)]
  return removed ++ added.map fun (k, v) => (k, some v)

/-- Drain one combined stream while watching the deadline, including when a descendant keeps the
pipe open after the command exits. Arguments are passed verbatim through the shell's `"$@"`. -/
def runCommand (args : Array String) (cwd : FilePath) (env : Environment) (timeoutMs : Nat) : IO CommandResult := do
  let deadline := (← IO.monoMsNow) + timeoutMs
  let child ← IO.Process.spawn {
    cmd := "/bin/sh"
    args := #["-c", "exec \"$@\" 2>&1", "sh"] ++ args
    cwd, env, stdin := .null, stdout := .piped, stderr := .null, setsid := true }
  let log ← Std.Mutex.new ({} : Excerpt)
  let reader ← IO.asTask (readOutput child.stdout log) .dedicated
  let killGroup : IO IO.Process.Output := IO.Process.output {cmd := "/bin/kill", args := #["-KILL", "--", s!"-{child.pid}"]}
  let code ← try
    let mut delay := 1
    let mut code : Option UInt32 := none
    repeat
      if ← IO.hasFinished reader then
        IO.ofExcept reader.get
        code ← child.tryWait
        if code.isSome then
          break
      let now ← IO.monoMsNow
      if now >= deadline then break
      IO.sleep (min delay (deadline - now)).toUInt32
      delay := min (2 * delay) 32
    pure code
  catch e =>
    discard killGroup
    discard child.wait
    throw e
  -- Child.kill sends SIGTERM, which build scripts and their descendants may ignore.
  discard killGroup
  if code.isNone then
    discard child.wait
    -- Give the reader a chance to collect the last pipe buffer after SIGKILL. An escaped process
    -- can still hold the pipe open, so do not block the timeout result on EOF.
    for _ in [:20] do
      if ← IO.hasFinished reader then break
      IO.sleep 5
  return {exitCode := code.getD 1, output := (← log.atomically get).text, timedOut := code.isNone}

end LakeCompat.Verify
