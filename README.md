# lake-compat

Find the dependency that needs attention before upgrading a Lean 4 project. Given a target toolchain, `lake-compat` traces conflicting pins back to the package that requested them. It can look up alternative revisions and build your project's default Lake targets with them. Use `--json` for the full package inventory and dependency paths.

## Use with Lake

The Lake executable uses Python 3.9 or later from your path as `python3`. To install it in a Lean project, add the following dependency to the project's `lakefile.toml`.

```toml
[[require]]
name = "lake-compat"
git = "https://github.com/Cameron-Lyons/lake-compat"
rev = "v0.1.0"
```

From the project root, fetch the tagged release and scan the project's lock for a Lean 4.36.0 upgrade.

```sh
lake --keep-toolchain update lake-compat
lake exe lake-compat 4.36.0
```

To try the included sample, run `lake exe lake-compat 4.36.0 --demo`.

## Standalone download

Download the standalone file and run the bundled demo with Python 3.9 or later. It works offline without a Lean installation. [Download lake-compat.pyz](https://github.com/Cameron-Lyons/lake-compat/releases/download/v0.1.0/lake-compat.pyz)

```sh
python3 lake-compat.pyz 4.36.0 --demo
```

The report shows `upgradeDemo -> greeting`. The dependency declares Lean 4.35.0, one release behind the target. Add `--json` to inspect the package and its toolchain.

To scan your own project, put `lake-compat.pyz` beside its `lake-manifest.json` and run `python3 lake-compat.pyz 4.36.0` there. You can also use `--project` to select the project directory. An installed wheel provides the `lake-compat` command with the same arguments.

The scan reads the project's `lake-manifest.json`. For a Git dependency, it checks the checkout's revision and reads its toolchain and manifest from the locked commit. Local path dependencies use the files in your working tree. Missing checkouts and unverifiable Git revisions appear as findings, including in `--strict` mode. On a fresh checkout, an ordinary `lake build` on the project's current toolchain materializes its dependencies.

For example, in the Lean RealWorld application, `realworld -> jose -> json` identifies the dependency that selected a different JSON revision. The diagnostic includes both full commits. A mismatch is a reason to test the combination. The application may already build with that override. [RealWorld dependency configuration](https://github.com/saviorand/lean-realworld-example-app/tree/483d47e1d3c797098ac55cbf23a95072f7a6b6e5)

## Try an upgrade

| Command inside a Lean project | Result |
| --- | --- |
| `lake-compat 4.36.0 --suggest` | Look up alternative revisions for flagged Git packages and the parents involved in conflicts. |
| `lake-compat 4.36.0 --verify` | Build the current locked revisions using the target toolchain. |
| `lake-compat 4.36.0 --try-suggested` | Pick the top candidate for each package with a match and build that combination in a temporary copy. Packages without candidates keep their current pins. |
| `lake-compat 4.36.0 --try-revision "Cli=$CLI_COMMIT"` | Test a specific full Git SHA. Repeat the option to test several changes together. Explicit choices take precedence over automatic choices. |
| `lake-compat 4.36.0 --json --strict` | Produce a machine-readable metadata report suitable for CI. |

Suggestions come from Reservoir for indexed GitHub repositories. The lookup checks the package's repository, subdirectory, and exact target toolchain. It prefers successful recorded builds and flags any that required a dependency update. It also lists revisions that declare the target when build evidence is unavailable. The report distinguishes an unavailable lookup from a lookup that found no candidates. [Reservoir](https://reservoir.lean-lang.org/)

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

## From source

Run these commands from the root of the source checkout or unpacked source distribution.

```sh
python3 tools/build_zipapp.py
python3 dist/lake-compat.pyz 4.36.0 --demo
```

To install the console command into a Python environment, run `python3 -m pip install .` there. Run the test suite with `python3 -m unittest discover -v`.

Lake-compat is distributed under the Apache License, Version 2.0. [License](LICENSE)
