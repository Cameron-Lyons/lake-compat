"""Look up revision-specific upgrade evidence in the public Reservoir index."""

import http.client
import json
import re
from pathlib import PurePosixPath
from urllib.error import HTTPError
from urllib.parse import quote
from urllib.request import Request, urlopen


REGISTRY = "https://reservoir.lean-lang.org/api/v1/packages"
MAX_BYTES = 8 * 1024 * 1024
MAX_CANDIDATES = 5
GITHUB_URL = re.compile(
    r"(?:https://github\.com/|ssh://git@github\.com/|git@github\.com:)"
    r"([a-z0-9-]+)/([a-z0-9_.-]+)/?", re.IGNORECASE | re.ASCII
)
REVISION = re.compile(r"(?:[0-9a-f]{40}|[0-9a-f]{64})")


def _read_json(url):
    request = Request(url, headers={"Accept": "application/json", "User-Agent": "lake-compat"})
    with urlopen(request, timeout=8) as response:
        body = response.read(MAX_BYTES + 1)
    if len(body) > MAX_BYTES:
        raise ValueError("Registry response exceeds the size limit")
    value = json.loads(body)
    if not isinstance(value, dict):
        raise ValueError("Expected a registry object")
    return value


def _records(url):
    rows = _read_json(url).get("data")
    if not isinstance(rows, list) or not all(isinstance(row, dict) for row in rows):
        raise ValueError("Expected registry records")
    return rows


def _github_repo(url):
    match = GITHUB_URL.fullmatch(url or "")
    if not match:
        return None
    owner, repo = match.groups()
    repo = repo.removesuffix(".git")
    if repo in ("", ".", ".."):
        return None
    return owner, repo


def _matches_source(metadata, full_name, sub_dir):
    sources = metadata.get("sources")
    if not isinstance(sources, list):
        raise ValueError("Missing package sources")
    for source in sources:
        if not isinstance(source, dict) or source.get("host") != "github":
            continue
        name = source.get("fullName")
        directory = source.get("subDir") or "."
        if (isinstance(name, str) and name.casefold() == full_name.casefold()
                and isinstance(directory, str)
                and PurePosixPath(directory) == PurePosixPath(sub_dir or ".")):
            return True
    return False


def _candidates(versions, builds, target):
    # A newer failed run supersedes an older success for the same commit.
    runs = {}
    for row in sorted(builds, key=lambda item: item.get("runAt") or "", reverse=True):
        rev = row.get("revision") or ""
        if row.get("toolchain") == target and REVISION.fullmatch(rev):
            runs.setdefault(rev, row)

    dates = {}
    tags = {}
    declared = set()
    for row in sorted(versions, key=lambda item: item.get("date") or "", reverse=True):
        rev = row.get("revision") or ""
        if not REVISION.fullmatch(rev):
            continue
        dates.setdefault(rev, row.get("date") or "")
        tag = row.get("tag")
        if isinstance(tag, str) and tag and tag.isprintable():
            tags.setdefault(rev, tag)
        if row.get("toolchain") == target:
            declared.add(rev)

    candidates = []
    for rev in runs.keys() | declared:
        run = runs.get(rev, {})
        if run and (run.get("built") is not True or run.get("tested") is False):
            continue
        candidates.append({
            "revision": rev,
            "tag": tags.get(rev),
            "toolchain": target,
            "evidence": "built" if run else "declared",
            "required_update": run.get("requiredUpdate"),
            "tested": run.get("tested"),
            "build_url": run.get("url"),
        })

    def priority(candidate):
        rev = candidate["revision"]
        return (
            candidate["evidence"] == "built",
            candidate["required_update"] is False,
            candidate["required_update"] is None,
            candidate["tag"] is not None,
            rev in declared,
            dates.get(rev) or runs.get(rev, {}).get("runAt") or "",
            rev,
        )

    return sorted(candidates, key=priority, reverse=True)


def suggest_versions(url: str, target: str, *, package=None, scope=None, sub_dir=None) -> dict:
    """Return indexed builds and revisions declaring the target for one GitHub package.

    The target is a canonical Lean toolchain identifier. The source URL is used
    only as an identifier: all HTTP requests go to Reservoir.
    """
    def empty(status, message):
        return {"status": status, "message": message, "candidates": []}

    github = _github_repo(url)
    if not github:
        return empty("unsupported", "Reservoir lookup requires an identifiable GitHub repository.")
    owner, repo = github
    parts = [scope or owner, package or repo]
    if any(part in (".", "..") or "/" in part or not part.isprintable() for part in parts):
        return empty("unsupported", "Invalid Reservoir package identifier.")
    endpoint = REGISTRY + "/" + "/".join(quote(part, safe="") for part in parts)

    try:
        metadata = _read_json(endpoint)
        if package and metadata.get("name") != package:
            return empty("unsupported", "Registry package name differs from the requested name.")
        if not _matches_source(metadata, f"{owner}/{repo}", sub_dir):
            return empty("unsupported", "Registry source differs in repository or package directory.")
        candidates = _candidates(_records(endpoint + "/versions"), _records(endpoint + "/builds"), target)
    except HTTPError as error:
        error.close()
        if error.code == 404:
            return empty("unavailable", "Package or release metadata is absent from the Reservoir index.")
        return empty("unavailable", f"Reservoir returned HTTP {error.code}.")
    except (OSError, http.client.HTTPException):
        return empty("unavailable", "Could not reach Reservoir or finish reading its response.")
    except (TypeError, ValueError, RecursionError):
        return empty("unavailable", "Reservoir returned incomplete, oversized, or invalid metadata.")

    if not candidates:
        return empty("no_match", "No successful build or eligible revision declaring this target in Reservoir.")
    return {
        "status": "found",
        "message": f"Showing {min(len(candidates), MAX_CANDIDATES)} of {len(candidates)} candidate revisions.",
        "candidates": candidates[:MAX_CANDIDATES],
    }
