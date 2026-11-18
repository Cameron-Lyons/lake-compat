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

The scan reads the project's `lake-manifest.json`, including legacy versions 5–7 and current 1.x manifests. It reads each Git dependency's toolchain and manifest from its locked commit whenever that commit is present locally. If the checkout points at a different commit, the report also flags the mismatch. Local path dependencies use the working tree until their paths enter a locked Git checkout. Scans with Git dependencies use `readlink` on POSIX or Windows PowerShell to follow workspace symlinks in those paths. Missing checkouts and unverifiable Git revisions appear as findings, including in `--strict` mode. On a fresh checkout, an ordinary `lake build` on the project's current toolchain materializes its dependencies.

For example, in the Lean RealWorld application, `realworld -> jose -> json` identifies the dependency that selected a different JSON revision. The diagnostic includes both full commits. A mismatch is a reason to test the combination. The application may already build with that override. [RealWorld dependency configuration](https://github.com/saviorand/lean-realworld-example-app/tree/483d47e1d3c797098ac55cbf23a95072f7a6b6e5)

## Try an upgrade

| Command inside a Lean project | Result |
| --- | --- |
| `lake-compat 4.36.0 --suggest` | Look up alternative revisions for flagged Git packages and the parents involved in conflicts. |
| `lake-compat 4.36.0 --verify` | Build the current locked revisions using the target toolchain. |
| `lake-compat 4.36.0 --try-suggested` | Pick the top candidate for each package with a match and build that combination in a temporary copy. Packages without candidates keep their current pins. |
| `lake-compat 4.36.0 --try-revision "Cli=$CLI_COMMIT"` | Test a specific full Git SHA. Repeat the option to test several changes together. Explicit choices take precedence over automatic choices. |
| `lake-compat 4.36.0 --json --strict` | Produce a machine-readable metadata report suitable for CI. |

Suggestions use `curl` to query Reservoir for indexed GitHub repositories. The lookup checks the package's repository, subdirectory, and exact target toolchain. It prefers successful recorded builds and flags any that required a dependency update. It also lists revisions that declare the target when build evidence is unavailable. If only one history can be retrieved, the report labels each candidate with its available evidence. In JSON, `unavailable_history` identifies the missing `builds` or `versions` history. A lookup with incomplete history and no candidates is reported as unavailable. [Reservoir](https://reservoir.lean-lang.org/)

Build trials require Git, a POSIX host, and the target already installed in elan. For example, `elan toolchain install leanprover/lean4:v4.36.0` makes that toolchain available. A trial copies the working source, including uncommitted project edits, into a temporary directory, sets the target and selected pins there, and runs the default Lake build. Lake can fetch dependencies and execute project and dependency build scripts with your permissions. Git dependencies come from the locked commits. A relative Git URL such as `../greeting` is resolved from the original project so the trial can fetch the same local repository. The root snapshot omits `.git` and Lake caches, so a build step that needs the root repository's history can fail in the trial.

The trial starts from the lock used for the metadata scan. If that lock changes before the snapshot, verification reports an inconclusive result. A successful trial checks that Lake retained the trial manifest and toolchain and that the resulting Git checkouts match every expected revision. With `--json`, `trial_revisions` lists requested changes and `verification.checked_revisions` records the full set of Git pins in a passing build. To adopt a passing trial, update `lean-toolchain` and the relevant requirements in your project's `lakefile.toml` or `lakefile.lean`, regenerate the manifest with Lake, and verify the resulting lock.

Automatic trials test one combination and hold the dependency graph fixed. If an upgrade adds a dependency, use Lake to resolve that changed graph on an upgrade branch. The verifier supports the standard `.lake` metadata directory, in-tree package directories, in-tree path dependencies, and relative symlinks within the copied source. Lake materializes fresh checkouts in the trial's package directory. If you keep other source files alongside those checkouts in a directory such as `vendor/`, the trial copies them too. External path dependencies, package overrides, and links that cannot be reproduced in the snapshot yield an inconclusive result.

If a manifest omits `packagesDir`, an offline scan looks for checkouts in the default packages directory. For build trials with Git or copied dependencies, the target Lake must support `translate-config` to resolve the omitted directory in a temporary copy. The trial records the resulting project-relative path before fetching or copying packages.

The source snapshot limit is 1 GiB. The `lake build` step has a ten-minute timeout by default. Use `--build-timeout SECONDS` with any build trial to change it. For example, `lake-compat 4.36.0 --verify --build-timeout 1800` allows half an hour for compilation.

## Exit codes

| Code | Meaning |
| --- | --- |
| 0 | Report completed, and any requested build passed. |
| 1 | The build failed, or `--strict` found metadata issues in the original lock. |
| 2 | Input was invalid, the build toolchain was unavailable, or verification was inconclusive. |

Package metadata findings always describe the project's original lock. A trial build has its own result. Project-specific tests can be run separately after adopting the pins.

## Standalone distribution

Build a release archive for the host platform with Lake and `tar`. The command prints the archive path. The archive contains the native executable and its license. On macOS, the build uses `MACOSX_DEPLOYMENT_TARGET` when set and otherwise targets the host's macOS version.

```sh
lake exe package-release dist
```

Extract the archive on a compatible host, then run `./lake-compat 4.36.0 --demo` or place the executable on your path. The executable can scan metadata without a Lean installation. Build verification still uses the selected toolchain through elan.

## Development checks

CI runs the Lean tests on Ubuntu and macOS. The suite exercises the CLI, Git checkouts, isolated builds, and the packaged executable. Run these checks from the source checkout with Git, elan, and the pinned Lean toolchain installed. Workflow linting uses `actionlint` and `shellcheck`.

```sh
actionlint
lake --wfail lint --linters=.extra LakeCompat LakeCompat.Cli LakeCompatMain LeanTests.Main PackageRelease
lake --wfail build LakeCompat lake-compat package-release
lake --wfail test
lake exe lake-compat "$(cat lean-toolchain)" --demo --verify
```

Lake-compat is distributed under the Apache License, Version 2.0. [License](LICENSE)
