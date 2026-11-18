#!/usr/bin/env bash
set -euo pipefail

# Run with the packaged CLI and both verification toolchains installed in elan.
if [[ $# != 1 && $# != 3 ]]; then
  echo 'Usage: bash scripts/release-compat.sh ABSOLUTE_CLI_PATH [REPOSITORY REVISION]' >&2
  exit 2
fi
cli=$1
root=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")/.." && pwd)
target=$(cat "$root/lean-toolchain")
work=$(mktemp -d)
trap 'rm -rf -- "$work"' EXIT

checkout() {
  local url=$1 revision=$2 dest=$3
  git init -q "$dest"
  git -C "$dest" fetch -q --depth=1 "$url" "$revision"
  git -C "$dest" checkout -q --detach FETCH_HEAD
}

echo 'Verify the bundled project on Lean 4.34.1'
"$cli" 4.34.1 --demo --verify --json |
  jq -e '.schema_version == 1 and .verification.status == "passed"'

echo 'Trace the RealWorld transitive JSON pin conflict'
realworld=$work/realworld
checkout https://github.com/saviorand/lean-realworld-example-app \
  483d47e1d3c797098ac55cbf23a95072f7a6b6e5 "$realworld"
lock=$realworld/lake-manifest.json
for package in jose json; do
  url=$(jq -er --arg p "$package" '.packages[] | select(.name == $p) | .url' "$lock")
  revision=$(jq -er --arg p "$package" '.packages[] | select(.name == $p) | .rev' "$lock")
  checkout "$url" "$revision" "$realworld/.lake/packages/$package"
done
"$cli" "$target" --project "$realworld" --json |
  jq -e 'any(.findings[]; .kind == "revision_conflict" and
    .path == ["realworld", "jose", "json"] and
    .requested_revision != .selected_revision)'

echo 'Look up Lean 4.35.0 candidates for a locked lean4-cli and verify a 4.36.0 upgrade'
project=$work/consumer
mkdir -p "$project"
old=c24fe6078c630e89b489bffe59b3e20ac30d12c8
new=50810f72af7618bee62fe9800dc26e806c188703
# Start from an already locked 4.34.0 dependency for the live 4.35.0 lookup.
base=e92c9f15fdfacc8536f31cfb3b7ad26c3c8cd204
cli_repo=https://github.com/leanprover/lean4-cli
cat > "$project/lakefile.toml" <<TOML
name = "consumer"
defaultTargets = ["App"]
[[lean_lib]]
name = "App"
[[require]]
name = "Cli"
git = "$cli_repo"
rev = "$base"
TOML
printf 'leanprover/lean4:v4.34.1\n' > "$project/lean-toolchain"
printf 'import Cli\n' > "$project/App.lean"
(
  cd "$project"
  lake --keep-toolchain update
)
"$cli" 4.35.0 --project "$project" --suggest --json |
  jq -e --arg old "$old" '.suggestions.Cli.status == "found" and
    any(.suggestions.Cli.candidates[]; .revision == $old)'
"$cli" "$target" --project "$project" --try-revision "Cli=$new" --build-target App --json |
  jq -e --arg new "$new" '.verification.status == "passed" and
    .trial_revisions.Cli == $new and .verification.checked_revisions == {Cli: $new}'
jq -e --arg base "$base" 'any(.packages[]; .name == "Cli" and .rev == $base)' \
  "$project/lake-manifest.json"

if [[ $# == 3 ]]; then
  echo 'Install the candidate from Git as a downstream Lake dependency'
  install=$work/install
  mkdir -p "$install"
  printf '%s\n' "$target" > "$install/lean-toolchain"
  cat > "$install/lakefile.toml" <<TOML
name = "downstream"
[[require]]
name = "lake-compat"
git = "$2"
rev = "$3"
TOML
  (
    cd "$install"
    lake --keep-toolchain update lake-compat
    lake exe lake-compat --version > version.txt
    "$cli" --version | diff - version.txt
    lake exe lake-compat "$target" --json |
      jq -e '.project == "downstream" and any(.packages[]; .name == "lake-compat")'
  )
fi
