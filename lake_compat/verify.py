"""Opt-in compilation of a locked workspace using an installed elan toolchain."""

from graphlib import CycleError, TopologicalSorter
import json
import os
from pathlib import Path
import re
import selectors
import shutil
import signal
import stat
import subprocess
import tempfile
import time

from .graph import package_name


BUILD_TIMEOUT = 600
LOG_BYTES = 16 * 1024
SOURCE_BYTES = 1024 ** 3


class _LogExcerpt:
    """Keep the output tail and, if it would scroll away, the first compiler error."""

    _error = re.compile(rb"(?:^|:[ \t]+)(?:error|fatal(?: error)?):", re.IGNORECASE | re.MULTILINE)
    _omitted = b"\n[... output omitted ...]\n"

    def __init__(self):
        self.tail = bytearray()
        self.diagnostic = b""

    def append(self, block):
        self.tail.extend(block)
        overflow = len(self.tail) - LOG_BYTES
        if overflow <= 0:
            return
        if not self.diagnostic:
            match = self._error.search(self.tail)
            if match:
                line_start = self.tail.rfind(b"\n", 0, match.start()) + 1
                # Preserve the location prefix and the compiler's following explanation.
                start = max(line_start, match.start() - 512)
                if start < overflow:
                    self.diagnostic = bytes(self.tail[start:start + LOG_BYTES // 4])
        del self.tail[:overflow]

    def text(self):
        if self.diagnostic:
            remaining = LOG_BYTES - len(self.diagnostic) - len(self._omitted)
            data = self.diagnostic + self._omitted + self.tail[-remaining:]
        else:
            data = self.tail
        return data.decode("utf-8", errors="replace").strip()


def _run(args, directory, env, timeout):
    """Keep bounded diagnostics and terminate the process group on timeout."""
    deadline = time.monotonic() + timeout
    log = _LogExcerpt()
    with subprocess.Popen(
        args, cwd=directory, env=env, stdin=subprocess.DEVNULL,
        stdout=subprocess.PIPE, stderr=subprocess.STDOUT, start_new_session=True,
    ) as process:
        try:
            with selectors.DefaultSelector() as selector:
                selector.register(process.stdout, selectors.EVENT_READ)
                while True:
                    remaining = deadline - time.monotonic()
                    if remaining <= 0 or not selector.select(remaining):
                        raise subprocess.TimeoutExpired(args, timeout)
                    block = os.read(process.stdout.fileno(), 8192)
                    if not block:
                        break
                    log.append(block)
            code = process.wait(timeout=max(0, deadline - time.monotonic()))
        except subprocess.TimeoutExpired as error:
            error.output = log.text()
            raise
        finally:
            # Descendants can hold a pipe open even after the parent has exited.
            try:
                os.killpg(process.pid, signal.SIGKILL)
            except ProcessLookupError:
                pass
    return code, log.text()


def _relative(value, label):
    if not isinstance(value, str) or not value:
        raise ValueError(f"Invalid {label} in the manifest.")
    path = Path(value)
    normalized = Path(os.path.normpath(value))
    if path.is_absolute() or ".." in normalized.parts or "\x00" in value:
        raise ValueError(f"Cannot isolate {label} outside its package: {value!r}.")
    return path


def _load_trial(source, revisions):
    if not any((source / f"lakefile.{ext}").is_file() for ext in ("lean", "toml")):
        raise ValueError("Expected a lakefile.lean or lakefile.toml at the project root.")
    raw = (source / "lake-manifest.json").read_text(encoding="utf-8")
    manifest = json.loads(raw)
    if not isinstance(manifest, dict) or not isinstance(manifest.get("packages"), list):
        raise ValueError("Expected a Lake manifest with a packages array.")
    if (Path(manifest.get("lakeDir") or ".lake") != Path(".lake")
            or Path(manifest.get("packagesDir") or ".lake/packages") != Path(".lake/packages")):
        raise ValueError("Build verification currently supports the standard .lake/packages layout.")
    pins = {}
    local_paths = {}
    names = set()
    for entry in manifest["packages"]:
        if not isinstance(entry, dict):
            raise ValueError("Unsupported Lake manifest package format.")
        if not isinstance(entry.get("name"), str):
            raise ValueError("Expected a string package name in the manifest.")
        name = package_name(entry["name"])
        if name in names:
            raise ValueError(f"Duplicated package name: {name!r}.")
        names.add(name)
        for field in ("subDir", "configFile", "manifestFile"):
            if entry.get(field) is not None:
                _relative(entry[field], f"{name}.{field}")
        if entry.get("type") == "path":
            local_path = _relative(entry.get("dir"), f"{name}.dir")
            local_paths[name] = local_path
            continue
        if entry.get("type") != "git":
            raise ValueError(f"Unsupported manifest source for {name}; expected git or path.")
        if name in revisions:
            entry["rev"] = entry["inputRev"] = revisions[name]
        revision = entry.get("rev")
        if not isinstance(revision, str) or not re.fullmatch(r"[0-9a-fA-F]{40}", revision):
            raise ValueError(f"{name} needs a full 40-character Git commit in the trial manifest.")
        pins[name] = revision.lower()
    unknown = set(revisions).difference(pins)
    if unknown:
        raise ValueError(f"Trial revisions must name locked Git packages: {', '.join(sorted(unknown))}.")
    _check_local_paths(source, local_paths, pins, materialized=False)
    return manifest, pins, local_paths


def _check_local_paths(root, local_paths, pins, *, materialized):
    for name, relative in local_paths.items():
        parts = Path(os.path.normpath(relative)).parts
        boundary = root
        if ".git" in relative.parts:
            raise ValueError(f"{name} references a Git metadata directory.")
        if ".lake" in relative.parts:
            if len(parts) < 3 or parts[:2] != (".lake", "packages") or parts[2] not in pins:
                raise ValueError(f"{name} has a cache path outside the pinned Git checkouts.")
            boundary = root.joinpath(*parts[:3])
            if not materialized:
                # Lake fetches the containing Git package into the isolated copy.
                continue
        try:
            directory = (root / relative).resolve(strict=True)
        except (OSError, RuntimeError) as exc:
            raise ValueError(f"Cannot resolve path dependency {name} at {relative}: {exc}") from exc
        if not directory.is_relative_to(boundary) or not directory.is_dir():
            raise ValueError(f"Missing or external local path dependency {name} at {relative}.")


def _source_link_target(source, link):
    """Validate each step, including '..' after an intermediate directory link."""
    relative = link.relative_to(source)
    target = Path(os.readlink(link))
    if target.is_absolute():
        raise ValueError(f"Cannot isolate an absolute symlink: {relative}.")
    current = link.parent
    try:
        for component in target.parts:
            if component in {".git", ".lake"}:
                raise ValueError(f"Source symlink uses an excluded directory: {relative}.")
            current = (current / component).resolve(strict=True)
            if not current.is_relative_to(source):
                raise ValueError(f"Source symlink leaves the project: {relative}.")
            if {".git", ".lake"}.intersection(current.relative_to(source).parts):
                raise ValueError(f"Source symlink uses an excluded directory: {relative}.")
        link.stat()  # Also enforce the OS's directory and symlink traversal rules.
    except (OSError, RuntimeError) as exc:
        raise ValueError(f"Cannot resolve source symlink {relative}: {exc}") from exc
    return current


def _copy_sources(source, destination):
    if source == destination or source in destination.parents:
        raise ValueError("The temporary directory must be outside the project.")
    total = 0

    def ignore(directory, names):
        nonlocal total
        skipped = {".git", ".lake"}.intersection(names)
        for name in set(names).difference(skipped):
            path = Path(directory, name)
            info = path.lstat()
            if stat.S_ISREG(info.st_mode):
                total += info.st_size
                if total > SOURCE_BYTES:
                    raise ValueError("Project sources exceed the 1 GiB temporary-copy limit.")
            elif not (stat.S_ISDIR(info.st_mode) or stat.S_ISLNK(info.st_mode)):
                raise ValueError(f"Cannot isolate a special file: {path.relative_to(source)}.")
        return skipped

    shutil.copytree(source, destination, symlinks=True, ignore=ignore)
    # Check the snapshot itself so edits to a symlink during the copy cannot
    # introduce a link into the original tree. os.walk never descends links.
    directories = TopologicalSorter()
    for directory, subdirs, files in os.walk(destination, followlinks=False):
        directory = Path(directory)
        for name in subdirs:
            path = directory / name
            target = _source_link_target(destination, path) if path.is_symlink() else path
            directories.add(target, directory)
        for name in files:
            path = directory / name
            if path.is_symlink():
                _source_link_target(destination, path)
    try:
        directories.prepare()
    except CycleError as exc:
        path = exc.args[1][0].relative_to(destination)
        raise ValueError(f"Directory symlinks create a traversal cycle through {path}.") from exc


def _git_revisions(workspace, pins, env):
    checked = {}
    for name, expected in pins.items():
        repo = workspace / ".lake" / "packages" / name
        if not (repo / ".git").is_dir() or repo.resolve() != repo:
            raise ValueError(f"Lake did not materialize an isolated checkout of {name}.")
        code, head = _run(["git", "rev-parse", "--verify", "HEAD"], repo, env, 10)
        if code != 0 or head != expected:
            raise ValueError(f"The checkout for {name} does not match its locked revision {expected}.")
        code, _ = _run(["git", "diff", "--quiet", "--no-ext-diff", "HEAD", "--"], repo, env, 30)
        if code:
            raise ValueError(f"Could not confirm that the tracked files in {name} match its locked revision.")
        checked[name] = head
    return checked


def verify_build(project: Path, target: str, *, revisions=None) -> dict:
    """Build default targets in a fresh copy. Lake may fetch locked dependencies.

    Like any ordinary build, this executes trusted project and dependency
    code with the caller's permissions. The copy isolates ordinary Lake writes.
    """
    if os.name != "posix":
        return {"status": "unavailable", "message": "Build verification requires a POSIX host."}
    elan = shutil.which("elan")
    if not elan:
        return {"status": "unavailable", "message": "elan is required to select the installed target toolchain."}
    if not target or target.startswith("-") or any(c.isspace() for c in target):
        return {"status": "inconclusive", "message": "Invalid elan toolchain name."}
    result = {}
    try:
        source = Path(project).resolve(strict=True)
        overrides = source / ".lake" / "package-overrides.json"
        if overrides.exists() or overrides.is_symlink():
            raise ValueError("This workspace has package overrides; its root manifest alone cannot reproduce them.")
        with tempfile.TemporaryDirectory(prefix="lake-compat-") as temp:
            parent = Path(temp).resolve()
            env = {k: v for k, v in os.environ.items()
                   if not k.startswith(("LEAN_", "LAKE_", "GIT_")) and k not in ("LEAN", "LAKE")}
            env.update(ELAN_TOOLCHAIN=target, GIT_TERMINAL_PROMPT="0",
                       GIT_CEILING_DIRECTORIES=str(parent), LAKE_NO_CACHE="true",
                       LAKE_ARTIFACT_CACHE="false", LAKE_CACHE_DIR=str(parent / "cache"))
            lake = [elan, "run", target, "lake"]  # elan run never installs without --install.
            code, version = _run(lake + ["--version"], parent, env, 20)
            if code:
                return {"status": "unavailable", "message": f"Cannot run Lake with installed toolchain {target}.",
                        "output": version}
            workspace = parent / "project"
            _copy_sources(source, workspace)
            manifest, pins, local_paths = _load_trial(workspace, revisions or {})
            lock_file = workspace / "lake-manifest.json"
            toolchain_file = workspace / "lean-toolchain"
            # A trial changes these two inputs only. Replacing a symlink here
            # preserves the separate source file it originally pointed to.
            if toolchain_file.is_symlink():
                toolchain_file.unlink()
            if lock_file.is_symlink():
                lock_file.unlink()
            toolchain_file.write_text(target + "\n", encoding="utf-8")
            # Always write the snapshot taken above, including any requested trial pins.
            lock_file.write_text(json.dumps(manifest, indent=2) + "\n", encoding="utf-8")
            code, output = _run(lake + ["build"], workspace, env, BUILD_TIMEOUT)
            result = {"output": output, "lake_version": version}
            if code == 0:
                check_code, detail = _run(lake + ["check-build"], workspace, env, 30)
            if (json.loads(lock_file.read_text(encoding="utf-8")) != manifest
                    or toolchain_file.read_text(encoding="utf-8").strip() != target):
                return dict(result, status="inconclusive", message="The build changed its trial manifest or toolchain.")
            if code:
                hint = (" If the candidate needs different dependencies, resolve them with lake update "
                        "on your upgrade branch and rerun the check.") if revisions else ""
                return dict(result, status="failed", message=f"lake build exited with code {code}.{hint}")
            if check_code:
                return dict(result, status="inconclusive",
                            message="Lake could not confirm the project has default build targets.",
                            output=(output + "\n" + detail).strip()[-LOG_BYTES:])
            trial_overrides = workspace / ".lake" / "package-overrides.json"
            if trial_overrides.exists() or trial_overrides.is_symlink():
                raise ValueError("The build created package overrides; the trial manifest cannot confirm effective pins.")
            checked = _git_revisions(workspace, pins, env)
            _check_local_paths(workspace, local_paths, pins, materialized=True)
            return dict(result, status="passed", message=f"Default targets built on {target} with the trial pins.",
                        checked_revisions=checked)
    except subprocess.TimeoutExpired as error:
        if error.output:
            result["output"] = error.output
        return dict(result, status="inconclusive", message=f"Verification exceeded its {error.timeout}-second command timeout.")
    except (OSError, ValueError, TypeError, RecursionError) as error:
        return dict(result, status="inconclusive", message=str(error))
