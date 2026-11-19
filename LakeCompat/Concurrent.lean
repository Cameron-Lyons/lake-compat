import Std.Sync.Mutex

namespace LakeCompat

/-- Run independent I/O with bounded parallelism. Limits below two run sequentially.
    After a failure, stop taking new work, join the active workers and report the
    failure with the earliest input index. -/
def mapConcurrent (values : Array α) (f : α → IO β) (limit := 4) : IO (Array β) := do
  let count := min limit values.size
  if count ≤ 1 then return ← values.mapM f
  let next ← Std.Mutex.new 0
  let worker : BaseIO (Except (Nat × IO.Error) (Array (Nat × β))) := do
    let mut batch := #[]
    repeat
      let index ← next.atomically (modifyGet fun i => (i, i + 1))
      let some value := values[index]? | break
      match ← (f value).toBaseIO with
      | .ok output => batch := batch.push (index, output)
      | .error e =>
        next.atomically (set values.size)
        return .error (index, e)
    return .ok batch
  let tasks ← (Array.range count).mapM fun _ => BaseIO.asTask worker (prio := .dedicated)
  let mut results := Array.replicate values.size none
  let mut firstError := none
  for task in tasks do
    match ← IO.wait task with
    | .ok batch =>
      for (index, value) in batch do results := results.set! index (some value)
    | .error failure =>
      if firstError.all (fun previous => failure.1 < previous.1) then firstError := some failure
  if let some (_, e) := firstError then throw e
  return results.filterMap id

end LakeCompat
