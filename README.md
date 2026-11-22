# lake-compat

Find the dependency that needs attention before upgrading a Lean 4 project. Given a target toolchain, `lake-compat` traces conflicting pins back to the package that requested them. It can look up alternative revisions and build your project's Lake targets with them. Use `--json` for the full package inventory and dependency paths.

## Install and run

Download the archive for your platform and `SHA256SUMS` from the same [GitHub release](https://github.com/Cameron-Lyons/lake-compat/releases). Verify the archive with `sha256sum -c SHA256SUMS --ignore-missing` on Linux. On macOS, run `shasum -a 256 <archive>` and compare its digest with the matching entry in `SHA256SUMS`. Extract it with `tar -xzf <archive>`, then run the bundled demo.

```sh
./lake-compat --version
./lake-compat 4.36.0 --demo
```

The native release replaces the `v0.1.0` Python wheel and `.pyz`. Use the standalone executable directly in scripts that used `python3 lake-compat.pyz`. Consumers of `--json` will see the new `schema_version` field.

Build the native executable with the toolchain in `lean-toolchain` (Lean 4.36.0). The executable embeds its demo and can run outside the source checkout.

```sh
lake build
lake exe lake-compat 4.36.0 --demo
.lake/build/bin/lake-compat 4.36.0 --project /path/to/your/project
```

The demo reports `upgradeDemo -> greeting`. The dependency declares Lean 4.35.0, one release behind the target. Copy the executable to a directory on your path to run `lake-compat` directly. Metadata scans work offline. Git is used to read locked commits. A standalone executable built on a compatible platform can scan projects using other Lean toolchains.

To use the command as a Lake dependency in a project on the supported build toolchain, add the following to its `lakefile.toml`.

```toml
[[require]]
name = "lake-compat"
git = "https://github.com/Cameron-Lyons/lake-compat"
rev = "v1.0.0"
```

From the project's root, run `lake --keep-toolchain update lake-compat`, then `lake exe lake-compat 4.36.0`. Without `--project`, the command uses the current directory.

## Read a scan

The scan reads the project's `lake-manifest.json`, including legacy versions 5–7 and current 1.x manifests. It reads each Git dependency's toolchain and manifest from its locked commit whenever that commit is present locally. If the checkout points at a different commit, the report also flags the mismatch. Local path dependencies use the working tree until their paths enter a locked Git checkout. Scans with Git dependencies use `readlink` on POSIX or Windows PowerShell to follow workspace symlinks in those paths. Missing checkouts and unverifiable Git revisions appear as findings, including in `--strict` mode. On a fresh checkout, an ordinary `lake build` on the project's current toolchain materializes its dependencies.

For a nested Lake project, the scan uses the nearest `lean-toolchain` in the project or its parent directories. Local packages under that directory inherit the same pin unless a closer file overrides it. Locked Git packages resolve their pins within their own commits.

For example, in the Lean RealWorld application, `realworld -> jose -> json` identifies the dependency that selected a different JSON revision. The diagnostic includes both full commits. A mismatch is a reason to test the combination. The application may already build with that override. [RealWorld dependency configuration](https://github.com/saviorand/lean-realworld-example-app/tree/483d47e1d3c797098ac55cbf23a95072f7a6b6e5)

## Try an upgrade

| Command inside a Lean project | Result |
| --- | --- |
| `lake-compat 4.36.0 --suggest` | Look up alternative revisions for flagged Git packages and the parents involved in conflicts. |
| `lake-compat 4.36.0 --verify` | Build the current locked revisions using the target toolchain. |
| `lake-compat 4.36.0 --test` | Build the project and run its configured Lake test driver in the same trial. |
| `lake-compat 4.36.0 --build-target MyLib --build-target MyExe` | Build both named targets in one trial. This also works for projects with no default targets. |
| `lake-compat 4.36.0 --try-suggested` | Pick the top candidate for each package with a match and build that combination in a temporary copy. Packages without candidates keep their current pins. |
| `lake-compat 4.36.0 --try-revision "Cli=$CLI_COMMIT"` | Test a full Git commit ID. A trial can combine SHA-1 and SHA-256 packages. Repeat the option to test several changes together. Explicit choices take precedence over automatic choices. |
| `lake-compat 4.36.0 --json --strict` | Produce a machine-readable metadata report suitable for CI. |

Suggestions use `curl` to query Reservoir for indexed GitHub repositories. The lookup checks the package's repository, subdirectory, and exact target toolchain. It prefers successful recorded builds and flags any that required a dependency update. It also lists revisions that declare the target when build evidence is unavailable. If only one history can be retrieved, the report labels each candidate with its available evidence. In JSON, `unavailable_history` identifies the missing `builds` or `versions` history. A lookup with incomplete history and no candidates is reported as unavailable. [Reservoir](https://reservoir.lean-lang.org/)

Build trials require Git, a POSIX host, and the target already installed in elan. For example, `elan toolchain install leanprover/lean4:v4.36.0` makes that toolchain available. A trial copies the working source, including uncommitted project edits, into a temporary directory, sets the toolchain and selected pins there, and runs Lake. Without an explicit target, it builds the project's default targets.

Repeat `--build-target` to select targets for a trial. A bare name selects a target in the root package. Use `@package` to build a dependency’s default targets, `@package/Name` for one named target, or `@package/+Module:olean` for a module facet. If a selected package has no defaults, the trial reports an inconclusive result and asks for a named target. To select a module by source file, use a project-relative path such as `--build-target Main.lean:olean` or `--build-target src/Main.lean:olean`. Paths beginning with `./` also work. For a package facet, use `:facet` for the root or `@package:facet` for a dependency. You can combine these selections with `--try-revision` or `--try-suggested`. The JSON field `build_targets` records explicit selections.

Use `--test` to run `lake test` after the build. It also works with `--try-revision`, `--try-suggested`, and named targets. For an executable or script driver, put its arguments after `--`, for example `lake-compat 4.36.0 --test -- --filter "parser smoke"`. Lake appends those arguments to the project’s configured `testDriverArgs`. A library driver takes no arguments. A project with a test driver can use this mode even if it has no default build targets. A failing test driver fails the trial. If Lake cannot confirm a configured driver, the trial is inconclusive.

Lake can fetch dependencies and execute project and dependency build scripts with your permissions. Git dependencies come from the locked commits. A relative Git URL such as `../greeting` is resolved from the original project so the trial can fetch the same local repository. The root snapshot omits `.git` and `.lake`, so a build step that needs the root repository's history can fail in the trial. For custom or executable Lake configurations, the target Lake reports the packages’ build, library, binary, and intermediate directories before compilation. A directory outside the temporary project makes verification inconclusive. In-tree output paths can contain spaces or colons. When a custom directory contains compiled modules from the working copy, the verifier invalidates their Lake traces in the trial so Lake rebuilds the requested modules.

The trial starts from the lock used for the metadata scan. If that lock changes before the snapshot, verification reports an inconclusive result. A successful trial checks that Lake retained the trial manifest and toolchain and that the resulting Git checkouts match every expected revision. With `--json`, `trial_revisions` lists requested changes and `verification.checked_revisions` records the full set of Git pins in a passing build. To adopt a passing trial, update `lean-toolchain` and the relevant requirements in your project's `lakefile.toml` or `lakefile.lean`, regenerate the manifest with Lake, and verify the resulting lock.

Automatic trials test one combination and hold the dependency graph fixed. If an upgrade adds a dependency, use Lake to resolve that changed graph on an upgrade branch. The verifier supports the standard `.lake` metadata directory, in-tree package directories, in-tree path dependencies, and relative symlinks within the copied source. Lake materializes fresh checkouts in the trial's package directory. If you keep other source files alongside those checkouts in a directory such as `vendor/`, the trial copies them too. External path dependencies, active or unreadable package overrides, and links that cannot be reproduced in the snapshot yield an inconclusive result. A valid override manifest with an empty package list leaves the root lock in effect.

If a manifest omits `packagesDir`, an offline scan looks for checkouts in the default packages directory. For build trials with Git or copied dependencies, the target Lake must support `translate-config` to resolve the omitted directory in a temporary copy. The trial records the resulting project-relative path before fetching or copying packages.

The source snapshot limit is 1 GiB. The build and optional test step each have a ten-minute timeout by default. Use `--build-timeout SECONDS` with any build trial to change it. For example, `lake-compat 4.36.0 --verify --build-timeout 1800` allows half an hour for compilation. Supplying a build timeout without requesting a trial is an input error.

## Exit codes

| Code | Meaning |
| --- | --- |
| 0 | Report completed, and any requested build and tests passed. |
| 1 | A build or test failed, or `--strict` found metadata issues in the original lock. |
| 2 | Input was invalid, the build toolchain was unavailable, or verification was inconclusive. |

Package metadata findings always describe the project's original lock. A passing trial can therefore exit 1 under `--strict`. An unavailable or inconclusive trial exits 2 even when `--strict` also finds issues. Suggestions alone leave the exit code unchanged.

## JSON contract

`--json` emits one object on stdout with `schema_version: 1`. A scan includes `project`, `target`, `root_toolchain`, `packages`, and `findings`. Input and scan errors return an `error` string and exit 2. Suggestions appear when requested. Trials include `trial_revisions` and `verification`, whose status is `passed`, `failed`, `unavailable`, or `inconclusive`. A passed trial includes `checked_revisions`, even when the set of Git pins is empty. With `--test`, the report includes `test_requested: true`. When arguments follow `--`, `test_args` records them in order. Once `lake test` finishes, `verification.test_exit_code` records its exit code. [JSON Schema](schema/report-v1.json)

Use finding `kind` for automation. The current kinds are:

| Findings | Kinds |
| --- | --- |
| Toolchain | `toolchain_mismatch`, `unknown_toolchain` |
| Conflicting or incomplete lock | `revision_conflict`, `source_conflict`, `unlocked_dependency`, `active_overrides` |
| Checkout | `missing_checkout`, `checkout_revision_mismatch`, `unknown_checkout_revision`, `unknown_package_directory` |
| Package manifest or ancestry | `missing_manifest`, `invalid_manifest`, `unknown_path`, `unknown_path_source` |

The command-line flags, documented exit codes, and JSON field types are the 1.x compatibility contract. Consumers should tolerate extra JSON fields and new finding kinds. Human-readable messages can change. The Lean modules are implementation APIs and may change independently of the CLI contract.

## Supported environments

| Operation | Support for the native release |
| --- | --- |
| Install a standalone archive | Linux x86-64 on Ubuntu 24.04 and Apple silicon on macOS 15 |
| Build from source or use as a Lake dependency | Lean 4.36.0 |
| Scan metadata | Lake manifest versions 5–7 and 1.x. Lean is unnecessary at runtime. Git and a local checkout of locked commits are needed for Git metadata. |
| Look up suggestions | `curl` and Reservoir access. Available toolchains and revisions depend on the index. |
| Verify a build | Linux or macOS with Git and the target toolchain installed in elan. The release checks exercise Lean 4.34.1 and 4.36.0. |

Windows builds and Intel macOS binaries are outside the supported release matrix.

## Standalone distribution

Build a release archive for the host platform with Lake and `tar`. The command prints the archive path. The archive contains the native executable and its license. On macOS, the build uses `MACOSX_DEPLOYMENT_TARGET` when set and otherwise targets the host's macOS version.

```sh
lake exe package-release dist
```

Extract the archive on a compatible host, then run `./lake-compat 4.36.0 --demo` or place the executable on your path. The executable can scan metadata without a Lean installation. Build verification still uses the selected toolchain through elan.

## Development checks

The application, tests, benchmarks, report validation, and release compatibility tools are written in Lean. CI runs the Lean tests on Ubuntu and macOS. The suite exercises the CLI, Git checkouts, isolated builds, and the packaged executable. Run these checks from the source checkout with Git, elan, and the pinned Lean toolchain installed. Workflow linting uses `actionlint`.

```sh
actionlint
lake --wfail lint --linters=.extra \
  LakeCompat LakeCompat.Cli LakeCompatMain LeanTests.Main PackageRelease \
  Tools.Process Tools.BenchmarkScans Tools.CheckReportSchema Tools.ReleaseCompat Tools.ReleaseSmoke
lake --wfail build LakeCompat lake-compat package-release \
  benchmark-scans check-report-schema release-compat release-smoke
lake --wfail test
lake exe check-report-schema
lake exe lake-compat "$(cat lean-toolchain)" --demo --verify
```

`lake exe check-report-schema` validates real CLI reports against the published JSON Schema, including successful and failing trials, test drivers, revision suggestions, and input errors. Its Lean validator supports the vocabulary used by this contract and rejects unsupported schema keywords or references. Validator regressions also run in `lake test`. Pass an executable path to check a packaged CLI instead of the local build. No Python packages are required.

To measure offline scan performance, run `lake exe benchmark-scans`. It creates a temporary workspace with 256 packages and 16,384 dependency edges sharing a local Git repository. Fixture setup is excluded from timing. The tool warms each executable, reports median and minimum scan times, and requires complete reports without findings. It needs Git on a POSIX host.

Pass saved and current executables to compare them on the same fixture; every report must be identical. Use `--parents`, `--children`, and `--runs` to vary the workload. This measures repeated local repository comparisons, rather than network requests or compilation.

```sh
lake exe benchmark-scans /tmp/lake-compat-before .lake/build/bin/lake-compat --runs 7
```

## Release process

Set the same version in `VERSION` and `lakefile.toml`, then run the **Native release** workflow manually to test packaging without publishing. It builds both archives, runs them on separate runners without a Lean installation step, and uses the Linux archive to check a pinned RealWorld conflict, a live Reservoir lookup, a Git-backed upgrade trial, and downstream installation from Git.

The workflow builds a native `release-smoke` checker alongside each archive. It validates the archive's license, version, demo report schema, and strict exit code with an empty executable search path. This checker is a CI artifact; published archives still contain only the application and license. Run it locally with `lake exe release-smoke ARCHIVE VERSION`.

Run those compatibility checks locally with `lake exe release-compat /absolute/path/to/lake-compat`. This needs network access and both Lean 4.34.1 and the project's toolchain installed in elan. Append a repository URL and revision to also exercise downstream Git installation. The tool creates temporary projects and checks that upgrade verification leaves the original lock unchanged.

The same workflow runs for a matching `v<VERSION>` tag and publishes the tested archives and checksums after all gates pass. A tag such as `v1.0.0-rc.1` creates a GitHub prerelease. Promote a candidate to a stable release after its checks pass and release-blocking issues are resolved.

Lake-compat is distributed under the Apache License, Version 2.0. [License](LICENSE)
