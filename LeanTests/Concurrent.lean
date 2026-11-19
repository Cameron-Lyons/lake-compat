import LakeCompat.Concurrent

namespace LeanTests.Concurrent

def run : IO Unit := do
  let values := Array.range 8
  let results ← LakeCompat.mapConcurrent values fun n => do
    if n == 0 then IO.sleep 10
    return n + 1
  unless results == values.map (· + 1) do throw (IO.userError "worker results lost input order")

  let started ← IO.Promise.new
  let finished ← IO.mkRef false
  let result ← (LakeCompat.mapConcurrent (β := Unit) #[0, 1] fun i => do
    if i == 0 then
      let _ ← IO.wait started.result!
      IO.sleep 10
      finished.set true
      throw (IO.userError "first input failed")
    started.resolve ()
    throw (IO.userError "second input failed")).toBaseIO
  let .error failure := result | throw (IO.userError "missing worker failure")
  unless failure.toString == "first input failed" && (← finished.get) do
    throw (IO.userError "failed lookups must join their siblings and report the earliest input failure")

  let siblingStarted ← IO.Promise.new
  let calls ← Std.Mutex.new 0
  let stopped ← (LakeCompat.mapConcurrent (Array.range 16) (limit := 2) fun i => do
    calls.atomically (modify (· + 1))
    if i == 0 then
      let _ ← IO.wait siblingStarted.result!
      throw (IO.userError "first input failed")
    if i == 1 then siblingStarted.resolve ()
    IO.sleep 10).toBaseIO
  unless stopped matches .error _ do throw (IO.userError "missing worker failure")
  unless (← calls.atomically get) < 16 do
    throw (IO.userError "queued lookups continued after the scan failed")

end LeanTests.Concurrent
