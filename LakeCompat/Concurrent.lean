import Std.Sync.Mutex

namespace LakeCompat

/-- Run independent I/O with at most `limit` workers. Preserve input order and
    join every worker, including when an action fails. -/
def mapConcurrent (values : Array α) (f : α → IO β) (limit := 4) : IO (Array β) := do
  let count := min limit values.size
  if count ≤ 1 then return ← values.mapM f
  let next ← Std.Mutex.new 0
  let worker := do
    let mut batch := #[]
    repeat
      let index ← next.atomically (modifyGet fun i => (i, i + 1))
      let some value := values[index]? | break
      batch := batch.push (index, ← f value)
    return batch
  let tasks ← (Array.range count).mapM fun _ => IO.asTask worker (prio := .dedicated)
  let mut results := Array.replicate values.size none
  let mut firstError := none
  for task in tasks do
    match task.get with
    | .ok batch =>
      for (index, value) in batch do results := results.set! index (some value)
    | .error e => firstError := firstError <|> some e
  if let some e := firstError then throw e
  return results.filterMap id

end LakeCompat
