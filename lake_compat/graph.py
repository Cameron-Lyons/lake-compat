"""Inspect a Lake workspace using its selected lock and its materialized dependencies."""

import json
import os
import re
import subprocess
from collections import deque
from functools import partial
from pathlib import Path

MANIFEST = "lake-manifest.json"
_RELEASE = re.compile(r"v?(\d+\.\d+\.\d+(?:[-+][\w.+-]+)?)")
_DESCRIPTOR = re.compile(r"(?:[\w.-]+/[\w.-]+:)?[\w.+-]+")


def normalize_toolchain(value: str) -> str:
    """Expand official release/channel shorthand while preserving custom Elan names."""
    value = value.strip()
    if not _DESCRIPTOR.fullmatch(value):
        raise ValueError(f"invalid Lean toolchain {value!r}")
    origin, separator, version = value.rpartition(":")
    if separator and origin != "leanprover/lean4":
        return value
    release = _RELEASE.fullmatch(version)
    if release:
        version = "v" + release.group(1)
    elif not separator and version not in ("stable", "nightly", "beta") and not version.startswith("nightly-"):
        return value  # This may be a local toolchain linked with Elan.
    return "leanprover/lean4:" + version


def _read_text(file):
    return file.read_text(encoding="utf-8")


def _pin(directory, read_text=_read_text):
    try:
        return normalize_toolchain(read_text(directory / "lean-toolchain"))
    except FileNotFoundError:
        return None


def _package_pin(checkout, repo, project, read_text):
    """Look up Elan metadata without walking above the checkout or workspace."""
    boundary = repo if repo is not None else project
    directory = checkout
    while True:
        pin = _pin(directory, read_text)
        if pin is not None:
            if directory == checkout:
                return pin, "package"
            if directory == project:
                return pin, "workspace"
            return pin, "repository" if repo is not None else "ancestor"
        if boundary not in directory.parents:
            return None, None
        directory = directory.parent


def _field(entry, field, optional=False):
    value = entry.get(field)
    if optional and value is None:
        return None
    if not isinstance(value, str) or not value or "\x00" in value:
        raise ValueError(f"expected a non-empty string for {field}")
    return value


def package_name(value: str) -> str:
    """Return the directory spelling of a serialized Lean package name."""
    if not isinstance(value, str):
        raise ValueError("expected a Lean package name")
    name = value.replace("«", "").replace("»", "")
    if not name or name in (".", "..") or any(c in name for c in ("/", "\\", "\x00")):
        raise ValueError(f"invalid package name {value!r}")
    return name


def _manifest(file, read_text=_read_text):
    try:
        data = json.loads(read_text(file))
        if not isinstance(data, dict):
            raise ValueError("expected a Lake manifest object")
        version = data.get("version", data.get("schemaVersion"))
        legacy = type(version) is int and 5 <= version <= 7
        current = isinstance(version, str) and re.fullmatch(r"1\.\d+\.\d+(?:-.*)?", version)
        if not (legacy or current):
            raise ValueError(f"unsupported Lake manifest version {version!r}")
        raw_entries = data.get("packages", [])
        if not isinstance(raw_entries, list):
            raise ValueError("expected a packages array")
        entries = {}
        for raw in raw_entries:
            if not isinstance(raw, dict):
                raise ValueError("expected a package object")
            if len(raw) == 1 and next(iter(raw)) in ("git", "path"):
                # Versions 5 and 6 used tagged JSON and question-mark fields.
                kind, fields = next(iter(raw.items()))
                if not isinstance(fields, dict):
                    raise ValueError(f"invalid legacy {kind} entry")
                raw = dict(fields, type=kind, inputRev=fields.get("inputRev?"), subDir=fields.get("subDir?"))
            name = package_name(_field(raw, "name"))
            if name in entries:
                raise ValueError(f"duplicate package {name!r}")
            kind = raw.get("type")
            if kind not in ("git", "path"):
                raise ValueError(f"unknown source type for {name!r}: {kind!r}")
            if type(raw.get("inherited")) is not bool:
                raise ValueError(f"expected a boolean for {name}.inherited")
            if "copy" in raw and type(raw["copy"]) is not bool:
                raise ValueError(f"expected a boolean for {name}.copy")
            if "scope" in raw and not isinstance(raw["scope"], str):
                raise ValueError(f"expected a string for {name}.scope")
            for field in ("manifestFile", "subDir", "inputRev"):
                _field(raw, field, optional=True)
            if kind == "git":
                _field(raw, "rev")
                _field(raw, "url")
            else:
                _field(raw, "dir")
            entries[name] = raw
        for field in ("name", "lakeDir", "packagesDir"):
            _field(data, field, optional=True)
        if "fixedToolchain" in data and type(data["fixedToolchain"]) is not bool:
            raise ValueError("expected a boolean for fixedToolchain")
        return data, entries
    except ValueError as error:
        raise ValueError(f"{file}: {error}") from error


def _locations(project, packages_dir, name, entry, locked_repos):
    if entry["type"] == "git":
        repo = packages_dir / name
        return repo / (entry.get("subDir") or ""), repo
    checkout = packages_dir / name if entry.get("copy") else project / entry["dir"]
    # A path package in a locked monorepo shares the Git dependency's revision.
    # Check its lexical location before following any mutable worktree symlinks.
    normalized = Path(os.path.abspath(checkout))
    for parent in (normalized, *normalized.parents):
        if parent in locked_repos:
            return checkout if checkout.is_relative_to(parent) else normalized, parent
    location = checkout.resolve()
    repo = next((p for p in (location, *location.parents) if p in locked_repos), None)
    return location, repo


def _git(repo, args, input_bytes=None):
    env = {key: value for key, value in os.environ.items() if not key.startswith("GIT_")}
    env.update(GIT_NO_REPLACE_OBJECTS="1", GIT_NO_LAZY_FETCH="1", GIT_ALLOW_PROTOCOL="")
    return subprocess.run(["git", "-C", str(repo), *args], input=input_bytes, env=env,
                          stdout=subprocess.PIPE, stderr=subprocess.DEVNULL, check=False)


def _git_text(repo, revision, file):
    """Read one blob at the selected commit, following symlinks inside its tree."""
    relative = file.relative_to(repo)
    spec = f"{revision}:{relative.as_posix()}".encode("utf-8")
    if b"\n" in spec:
        raise ValueError(f"cannot address a Git metadata path containing a newline: {relative!r}")
    process = _git(repo, ["cat-file", "--batch", "--follow-symlinks"], spec + b"\n")
    header, _, body = process.stdout.partition(b"\n")
    if process.returncode:
        raise OSError(f"Git could not read {relative} at {revision}")
    if header == spec + b" missing":
        raise FileNotFoundError(file)
    fields = header.split()
    if len(fields) != 3 or fields[1] != b"blob":
        raise ValueError(f"{relative} does not resolve to a file inside the locked Git tree")
    return body[:-1].decode("utf-8")  # cat-file appends one record delimiter.


def _git_directory(repo, revision, checkout):
    """Resolve a package directory using only directory entries at the locked revision."""
    parts = deque(checkout.relative_to(repo).parts)
    directory = repo
    link_count = 0
    while parts:
        name = parts.popleft()
        if name == "..":
            if directory == repo:
                raise ValueError("package path leaves the locked repository")
            directory = directory.parent
            continue
        location = directory / name
        entry = _git(repo, ["--literal-pathspecs", "ls-tree", "-z", revision, "--", location.relative_to(repo).as_posix()])
        header = entry.stdout.partition(b"\t")[0].split()
        if entry.returncode or len(header) != 3:
            raise ValueError(f"package directory is absent at {revision}: {location}")
        if header[0] == b"040000":
            directory = location
            continue
        if header[0] != b"120000" or link_count >= 40:
            raise ValueError(f"cannot resolve package directory {location} at {revision}")
        target = _git(repo, ["cat-file", "blob", header[2].decode("ascii")])
        path = Path(os.fsdecode(target.stdout))
        if target.returncode or path.is_absolute():
            raise ValueError(f"package link {location} does not resolve inside the locked tree")
        parts.extendleft(reversed(path.parts))
        link_count += 1
    return directory


def _observe(project, packages_dir, name, entry, locked_repos):
    checkout, repo = _locations(project, packages_dir, name, entry, locked_repos)
    package = {
        "name": name, "toolchain": None, "toolchain_source": None, "revision": entry.get("rev"),
        "url": entry.get("url"), "scope": entry.get("scope") or "",
        "sub_dir": entry.get("subDir"), "input_rev": entry.get("inputRev"),
        "checkout": str(checkout), "direct": entry.get("inherited") is False,
        "type": entry["type"], "fixed_toolchain": None,
    }
    issues = []
    edges = {}

    if not (repo if repo is not None else checkout).is_dir():
        issues.append(("missing_checkout", f"Package directory is missing: {checkout}"))
        return package, edges, issues

    read_text = _read_text
    if repo is not None:
        revision = locked_repos[repo]
        head = None
        if (repo / ".git").exists():
            try:
                proc = _git(repo, ["rev-parse", "--verify", "HEAD"])
                if proc.returncode == 0:
                    head = proc.stdout.decode("ascii").strip()
            except OSError:
                pass
        if head is None:
            issues.append(("unknown_checkout_revision", f"Could not verify Git revision {revision} for {repo}. "
                          "The available metadata comes from the working directory."))
        elif head.lower() != revision.lower():
            message = f"Checkout is at {head}. The root manifest selects {revision}."
            issues.append(("checkout_revision_mismatch", message))
            return package, edges, issues
        else:
            try:
                checkout = _git_directory(repo, head, checkout)
            except (ValueError, OSError) as error:
                issues.append(("unknown_package_directory", str(error)))
                return package, edges, issues
            package["checkout"] = str(checkout)
            read_text = partial(_git_text, repo, head)

    try:
        package["toolchain"], package["toolchain_source"] = _package_pin(checkout, repo, project, read_text)
        if package["toolchain"] is None:
            issues.append(("unknown_toolchain", "No lean-toolchain file was found for this package."))
    except (OSError, ValueError) as error:
        issues.append(("unknown_toolchain", f"Cannot read the package toolchain: {error}"))

    manifest_file = entry.get("manifestFile", MANIFEST)
    if manifest_file is None:
        issues.append(("missing_manifest", "The root lock records no package manifest to inspect for dependency pins."))
        return package, edges, issues
    try:
        child, children = _manifest(checkout / manifest_file, read_text)
        package["fixed_toolchain"] = child.get("fixedToolchain", False)
        edges = {name: item for name, item in children.items() if item.get("inherited") is False}
    except FileNotFoundError:
        issues.append(("missing_manifest", f"Dependency pins are unknown because {manifest_file} is missing."))
    except (OSError, ValueError) as error:
        issues.append(("invalid_manifest", f"Cannot inspect the package manifest: {error}"))
    return package, edges, issues


def _paths(root, direct, edges):
    """Breadth-first from known direct entries. Preserve unknown ancestry as unknown."""
    paths = {name: [root, name] for name in sorted(direct)}
    queue = deque(paths)
    while queue:
        parent = queue.popleft()
        for child in sorted(edges[parent]):
            if child in edges and child not in paths:
                paths[child] = paths[parent] + [child]
                queue.append(child)
    return paths


def _conflict(parent, requested, selected, parent_dir, project):
    if requested["type"] != selected["type"]:
        return "source_conflict", (
            f"{parent} locks a {requested['type']} source. The root locks a {selected['type']} source."
        )
    if requested["type"] == "path":
        wanted = (parent_dir / requested["dir"]).resolve()
        actual = (project / selected["dir"]).resolve()
        if wanted != actual:
            return "source_conflict", f"{parent} locks path {wanted}. The root locks {actual}."
    else:
        if requested["rev"].lower() != selected["rev"].lower():
            return "revision_conflict", f"{parent} locks {requested['rev']}. The root selects {selected['rev']}."
        if (requested.get("subDir") or "") != (selected.get("subDir") or ""):
            return "source_conflict", f"{parent} and the root select different package subdirectories at the same revision."
    return None


def analyze(project: Path, target: str) -> dict:
    """Describe the local locked graph and its evidence for upgrading to target."""
    project = Path(project).resolve()
    target = normalize_toolchain(target)
    manifest, entries = _manifest(project / MANIFEST)
    root = package_name(manifest.get("name") or project.name)
    if root == "[anonymous]":
        root = project.name
    lake_dir = project / (manifest.get("lakeDir") or ".lake")
    packages_dir = project / manifest["packagesDir"] if manifest.get("packagesDir") else lake_dir / "packages"
    packages_dir = packages_dir.resolve()
    locked_repos = {packages_dir / name: entry["rev"] for name, entry in entries.items() if entry["type"] == "git"}
    root_toolchain = _pin(project)
    packages, edges, issues = {}, {}, {}
    for name, entry in sorted(entries.items()):
        packages[name], edges[name], issues[name] = _observe(project, packages_dir, name, entry, locked_repos)

    paths = _paths(root, [name for name, pkg in packages.items() if pkg["direct"]], edges)
    findings = []
    overrides = lake_dir / "package-overrides.json"
    if overrides.is_file():
        findings.append({"kind": "active_overrides", "package": root, "path": [root],
                         "message": f"{overrides} can replace packages from the root lock at build time."})
    for name, pkg in packages.items():
        path = paths.get(name, [])
        pkg.update(path=path, path_known=bool(path))
        if not path:
            issues[name].append(("unknown_path", "The root lock lists this dependency, but its parent chain is unavailable."))
        if pkg["toolchain"] and pkg["toolchain"] != target and pkg["toolchain_source"] != "workspace":
            note = " Its manifest sets fixedToolchain." if pkg["fixed_toolchain"] else ""
            pin = "Declares" if pkg["toolchain_source"] == "package" else "Inherits"
            issues[name].append(("toolchain_mismatch", f"{pin} {pkg['toolchain']}. The target is {target}.{note}"))
        for kind, message in issues[name]:
            findings.append({"kind": kind, "package": name, "path": path, "message": message})
        if not path:
            continue
        for child, requested in sorted(edges[name].items()):
            selected = entries.get(child)
            if selected is None:
                conflict = "unlocked_dependency", f"{name} locks {child}, which is absent from the root manifest."
            else:
                conflict = _conflict(name, requested, selected, Path(pkg["checkout"]), project)
            if conflict:
                kind, message = conflict
                findings.append({
                    "kind": kind, "package": child, "path": path + [child], "message": message,
                    "parent": name, "requested_revision": requested.get("rev"),
                    "selected_revision": selected.get("rev") if selected else None,
                })
    return {"project": root, "target": target, "root_toolchain": root_toolchain,
            "packages": list(packages.values()), "findings": findings}
