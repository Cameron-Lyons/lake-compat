import Init.Data.String.Lossy

namespace Tools.Process
open System

private def readOutput (pipe : IO.FS.Handle) (byteLimit : Nat) : IO String := do
  let mut bytes := ByteArray.empty
  repeat
    let block ← pipe.read 65536
    if block.isEmpty then return String.fromUTF8Lossy bytes
    if bytes.size + block.size > byteLimit then
      throw <| IO.userError s!"Command output exceeded {byteLimit} bytes."
    bytes := bytes ++ block
  return String.fromUTF8Lossy bytes

/-- Run a command directly, retaining separate complete output streams within a byte
budget. The duration is recorded by the completion worker, independent of polling.
Timeouts include descendants that retain an output pipe after the command exits. -/
def timedRun (args : IO.Process.SpawnArgs) (timeoutMs : Nat := 120000)
    (byteLimit : Nat := 64 * 1024 * 1024) : IO (IO.Process.Output × Nat) := do
  let started ← IO.monoNanosNow
  let child ← IO.Process.spawn {args with stdin := .null, stdout := .piped, stderr := .piped, setsid := true}
  let stdout ← IO.asTask (readOutput child.stdout byteLimit) .dedicated
  let stderr ← IO.asTask (readOutput child.stderr byteLimit) .dedicated
  let completed ← IO.asTask (prio := .dedicated) do
    let exitCode ← child.wait
    let out ← IO.ofExcept (← IO.wait stdout)
    let err ← IO.ofExcept (← IO.wait stderr)
    let elapsed := (← IO.monoNanosNow) - started
    return ({exitCode, stdout := out, stderr := err : IO.Process.Output}, elapsed)
  let terminate : IO Unit := do
    if Platform.isWindows then child.kill
    else discard <| IO.Process.output {cmd := "/bin/kill", args := #["-KILL", "--", s!"-{child.pid}"]}
  try
    let mut delay := 1
    repeat
      if ← IO.hasFinished completed then return ← IO.ofExcept completed.get
      -- A reader can stop before the command exits, for example on excessive output.
      -- Surface that error promptly instead of leaving the child blocked on its pipe.
      if ← IO.hasFinished stdout then discard <| IO.ofExcept stdout.get
      if ← IO.hasFinished stderr then discard <| IO.ofExcept stderr.get
      let elapsed := (← IO.monoNanosNow) - started
      if elapsed >= timeoutMs * 1000000 then
        throw <| IO.userError s!"Command {args.cmd} exceeded its {timeoutMs}-millisecond timeout."
      IO.sleep (min delay ((timeoutMs * 1000000 - elapsed + 999999) / 1000000)).toUInt32
      delay := min (2 * delay) 16
    throw <| IO.userError "Command monitor ended unexpectedly."
  finally
    discard terminate.toBaseIO
    -- Reap the child and finish the readers after terminating its process group.
    -- An escaped descendant may retain a pipe, so cleanup must also remain bounded.
    for _ in [:20] do
      if ← IO.hasFinished completed then break
      IO.sleep 5

/-- Full-output command execution with a deadline and process-group cleanup. -/
def run (args : IO.Process.SpawnArgs) (timeoutMs : Nat := 120000)
    (byteLimit : Nat := 64 * 1024 * 1024) : IO IO.Process.Output := do
  return (← timedRun args timeoutMs byteLimit).1

end Tools.Process
