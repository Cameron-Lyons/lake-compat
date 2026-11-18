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
  let result ← (LakeCompat.mapConcurrent #[0, 1] fun i => do
    if i == 0 then
      let _ ← IO.wait started.result!
      throw (IO.userError "lookup failed")
    started.resolve ()
    IO.sleep 10
    finished.set true).toBaseIO
  unless !result.isOk && (← finished.get) do
    throw (IO.userError "a failed lookup returned before its sibling completed")

end LeanTests.Concurrent
