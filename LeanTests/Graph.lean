import LeanTests.Graph.Manifests
import LeanTests.Graph.Paths
import LeanTests.Graph.Revisions

namespace LeanTests.Graph

def run : IO Unit := do
  Manifests.run
  Revisions.run
  Paths.run

end LeanTests.Graph
