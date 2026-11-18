# lake-compat

Find the dependency that needs attention before upgrading a Lean 4 project. Given a target toolchain, `lake-compat` traces conflicting pins back to the package that requested them. It can look up alternative revisions and build your project's default Lake targets with them. Use `--json` for the full package inventory and dependency paths.

## Build and run

Build the native executable with the toolchain in `lean-toolchain` (Lean 4.36.0). The executable embeds its demo and can run outside the source checkout.

```sh
lake build
lake exe lake-compat 4.36.0 --demo
.lake/build/bin/lake-compat 4.36.0 --project /path/to/your/project
```

The demo reports `upgradeDemo -> greeting`. The dependency declares Lean 4.35.0, one release behind the target. Copy the executable to a directory on your path to run `lake-compat` directly. Metadata scans work offline. Git is used to read locked commits. A standalone executable built on a compatible platform can scan projects using other Lean toolchains.

To use the command as a Lake dependency in a project on the supported build toolchain, add the following to its `lakefile.toml`. Pin `rev` to the commit you want to use.

```toml
[[require]]
name = "lake-compat"
git = "https://github.com/Cameron-Lyons/lake-compat"
```

From the project's root, run `lake --keep-toolchain update lake-compat`, then `lake exe lake-compat 4.36.0`. Without `--project`, the command uses the current directory.

## Read a scan

The scan reads the project's `lake-manifest.json`, including legacy versions 5–7 and current 1.x manifests. For a Git dependency, it checks the checkout's revision and reads its toolchain and manifest from the locked commit. Local path dependencies use the files in your working tree. Missing checkouts and unverifiable Git revisions appear as findings, including in `--strict` mode. On a fresh checkout, an ordinary `lake build` on the project's current toolchain materializes its dependencies.

For example, in the Lean RealWorld application, `realworld -> jose -> json` identifies the dependency that selected a different JSON revision. The diagnostic includes both full commits. A mismatch is a reason to test the combination. The application may already build with that override. [RealWorld dependency configuration](https://github.com/saviorand/lean-realworld-example-app/tree/483d47e1d3c797098ac55cbf23a95072f7a6b6e5)

## Try an upgrade

| Command inside a Lean project | Result |
| --- | --- |
| `lake-compat 4.36.0 --suggest` | Look up alternative revisions for flagged Git packages and the parents involved in conflicts. |
| `lake-compat 4.36.0 --verify` | Build the current locked revisions using the target toolchain. |
| `lake-compat 4.36.0 --try-suggested` | Pick the top candidate for each package with a match and build that combination in a temporary copy. Packages without candidates keep their current pins. |
| `lake-compat 4.36.0 --try-revision "Cli=$CLI_COMMIT"` | Test a specific full Git SHA. Repeat the option to test several changes together. Explicit choices take precedence over automatic choices. |
| `lake-compat 4.36.0 --json --strict` | Produce a machine-readable metadata report suitable for CI. |

Suggestions use `curl` to query Reservoir for indexed GitHub repositories. The lookup checks the package's repository, subdirectory, and exact target toolchain. It prefers successful recorded builds and flags any that required a dependency update. It also lists revisions that declare the target when build evidence is unavailable. The report distinguishes an unavailable lookup from a lookup that found no candidates. [Reservoir](https://reservoir.lean-lang.org/)

Build trials require Git, a POSIX host, and the target already installed in elan. For example, `elan toolchain install leanprover/lean4:v4.36.0` makes that toolchain available. A trial copies the working source, including uncommitted project edits, into a temporary directory, sets the target and selected pins there, and runs the default Lake build. Lake can fetch dependencies and execute project and dependency build scripts with your permissions. Git dependencies come from the locked commits. The root snapshot omits `.git` and Lake caches, so a build step that needs the root repository's history can fail in the trial.

A successful trial checks that Lake retained the trial manifest and toolchain and that the resulting Git checkouts match every expected revision. With `--json`, `trial_revisions` lists requested changes and `verification.checked_revisions` records the full set of Git pins in a passing build. To adopt a passing trial, update `lean-toolchain` and the relevant requirements in your project's `lakefile.toml` or `lakefile.lean`, regenerate the manifest with Lake, and verify the resulting lock.

Automatic trials test one combination and hold the dependency graph fixed. If an upgrade adds a dependency, use Lake to resolve that changed graph on an upgrade branch. The verifier supports standard `.lake/packages` layouts, in-tree path dependencies, and relative symlinks to files or directories within the copied source. External path dependencies, package overrides, and links that cannot be reproduced in the snapshot yield an inconclusive result. The build timeout is 10 minutes and the source snapshot limit is 1 GiB.

## Exit codes

| Code | Meaning |
| --- | --- |
| 0 | Report completed, and any requested build passed. |
| 1 | The build failed, or `--strict` found metadata issues in the original lock. |
| 2 | Input was invalid, the build toolchain was unavailable, or verification was inconclusive. |

Package metadata findings always describe the project's original lock. A trial build has its own result. Project-specific tests can be run separately after adopting the pins.

## Python distributions

The wheel and zipapp bundle the same native executable, so each build targets its host platform. Both launchers support Python 3.9 or later. Installing a prebuilt wheel or running a zipapp requires no Lean installation for metadata scans.

```sh
python3 -m pip install .
lake-compat 4.36.0 --demo

python3 tools/build_zipapp.py
python3 dist/lake-compat.pyz 4.36.0 --demo
```

Both build commands require the source checkout or source distribution and its Lean toolchain.

## Development checks

CI runs the native and integration tests on Ubuntu with Python 3.9 and macOS with Python 3.15. Run the following commands from the source checkout in a Python virtual environment, with `actionlint` and `shellcheck` installed for workflow linting.

```sh
python -m pip install -r requirements-dev.txt
ruff check .
ruff format --check .
actionlint
lake --wfail lint --linters=.extra LakeCompat LakeCompat.Cli LakeCompatMain LeanTests.Main LeanTests.VerifyDriver
lake --wfail build LakeCompat lake-compat verify-test-driver
lake --wfail test
python -W error -m unittest discover -s tests -t . -v
lake exe lake-compat "$(cat lean-toolchain)" --demo --verify
```

To check the distribution, build into an empty directory. The build frontend creates a source distribution, then builds the wheel from its contents. The smoke test installs that wheel in a fresh environment and checks the demo's JSON report with an empty `PATH`.

```sh
artifacts=$(mktemp -d)
python -m build --outdir "$artifacts"
python tools/check_wheel.py "$artifacts"/*.whl
```

Lake-compat is distributed under the Apache License, Version 2.0. [License](LICENSE)
