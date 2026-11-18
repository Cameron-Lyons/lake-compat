"""Command line interface for inspecting and testing Lake dependency pins."""

import argparse
import json
import re
import sys
from concurrent.futures import ThreadPoolExecutor
from contextlib import nullcontext
from pathlib import Path

from .demo import demo_project
from .graph import MANIFEST, analyze
from .releases import suggest_versions
from .verify import verify_build


def _revision_pin(value):
    name, sep, sha = value.rpartition("=")
    if not name or not sep or not re.fullmatch(r"[0-9a-fA-F]{40}", sha):
        raise argparse.ArgumentTypeError("expected PACKAGE=FULL_40_CHARACTER_GIT_SHA")
    return name, sha.lower()


def _parser():
    parser = argparse.ArgumentParser(
        prog="lake-compat",
        description="Explain which locked dependencies need attention for a Lean upgrade.",
    )
    parser.add_argument("target", help="target Lean toolchain, e.g. 4.36.0")
    source = parser.add_mutually_exclusive_group()
    source.add_argument("--project", type=Path, metavar="DIR",
                        help="directory containing lake-manifest.json (default: current directory)")
    source.add_argument("--demo", action="store_true", help="scan the included example project")
    parser.add_argument("--suggest", action="store_true",
                        help="look up target-toolchain revisions for flagged Git packages on Reservoir")
    parser.add_argument("--verify", action="store_true",
                        help="run the project's default Lake build in a temporary copy using an installed target")
    parser.add_argument("--try-suggested", action="store_true",
                        help="look up candidates and build one trial using each package's top-ranked revision")
    parser.add_argument("--try-revision", action="append", type=_revision_pin, default=[],
                        dest="revisions", metavar="PACKAGE=SHA",
                        help="verify with this Git revision substituted in the copy; repeat to test a set")
    parser.add_argument("--json", action="store_true", help="emit a machine-readable report")
    parser.add_argument("--strict", action="store_true",
                        help="also exit 1 for metadata findings, even if the requested build passes")
    return parser


def _suggestions(report):
    flagged = {item["package"] for item in report["findings"]}
    flagged.update(item["parent"] for item in report["findings"] if "parent" in item)
    packages = [p for p in report["packages"] if p["name"] in flagged and p["url"]]

    def lookup(pkg):
        result = suggest_versions(pkg["url"], report["target"], package=pkg["name"],
                                  scope=pkg["scope"], sub_dir=pkg["sub_dir"])
        return pkg["name"], result

    with ThreadPoolExecutor(max_workers=4) as pool:
        return dict(pool.map(lookup, packages))


def _check_pins(report, pins):
    packages = {p["name"]: p for p in report["packages"]}
    result = {}
    for name, rev in pins:
        if name not in packages or packages[name]["type"] != "git":
            raise ValueError(f"{name!r} is not a locked Git package")
        if name in result and rev != result[name]:
            raise ValueError(f"two different revisions supplied for {name!r}")
        result[name] = rev
    return result


def _check_project(project):
    if (project / MANIFEST).is_file():
        return
    if not project.exists():
        reason = f"Project directory {project} does not exist."
    elif not project.is_dir():
        reason = f"--project expects a directory, but {project} is a file."
    elif any((project / name).is_file() for name in ("lakefile.toml", "lakefile.lean")):
        raise ValueError(f"{project} has a Lake configuration but no {MANIFEST}. "
                         "Run `lake update` in that directory to create the manifest, then retry.")
    else:
        reason = f"No Lake configuration or {MANIFEST} found in {project}."
    raise ValueError(f"{reason} Set --project to the folder containing your lakefile.toml or lakefile.lean.")


def _suggested_pins(report):
    selected = {}
    for package in report["packages"]:
        candidates = report["suggestions"].get(package["name"], {}).get("candidates", [])
        if candidates and candidates[0]["revision"] != package["revision"]:
            selected[package["name"]] = candidates[0]["revision"]
    return selected


def _candidate_line(candidate, locked):
    revision = candidate["revision"]
    label = (candidate["tag"] + "  ") if candidate["tag"] else ""
    if candidate["evidence"] == "built":
        evidence = "Reservoir build passed"
        if candidate["tested"] is True:
            evidence += " and tests passed"
        if candidate["required_update"] is True:
            evidence += " after updating dependencies"
    else:
        evidence = "declares this toolchain, build unconfirmed"
    if revision == locked:
        evidence += ", already locked"
    return f"    {label}{revision}  ({evidence})"


def _render(report):
    packages, findings = report["packages"], report["findings"]
    current = report["root_toolchain"] or "unknown"
    package_label = "package" if len(packages) == 1 else "packages"
    finding_label = "finding" if len(findings) == 1 else "findings"
    lines = [f"{report['project']}  |  {current} -> {report['target']}",
             f"{len(packages)} locked {package_label}, {len(findings)} metadata {finding_label}"]
    for finding in findings:
        chain = " -> ".join(finding["path"]) or (finding["package"] + " (parent unknown)")
        lines.append(f"  [{finding['kind'].replace('_', ' ')}] {chain}")
        lines.append("    " + finding["message"])
    if any(f["kind"] == "toolchain_mismatch" for f in findings):
        lines.append("A different declared toolchain can still compile on the target.")

    if "suggestions" in report:
        lines.extend(["", "Reservoir candidates for flagged Git packages"])
        if not report["suggestions"]:
            lines.append("  No flagged Git packages to look up.")
        locked = {p["name"]: p["revision"] for p in packages}
        for name, suggestion in report["suggestions"].items():
            lines.append(f"  {name}: {suggestion['message']}")
            lines.extend(_candidate_line(c, locked[name]) for c in suggestion["candidates"])

    if "verification" in report:
        check = report["verification"]
        lines.extend(["", f"Build {check['status']}: {check['message']}"])
        if report.get("auto_selected") and not report["trial_revisions"]:
            lines.append("  No alternate revision was selected. This trial uses the current lock.")
        for name, rev in report["trial_revisions"].items():
            lines.append(f"  Trial pin: {name}={rev}")
        if check.get("output") and check["status"] != "passed":
            lines.append(check["output"])
    else:
        lines.append("\nBuild not run. Use --verify to test the locked set on this toolchain.")
        if findings and "suggestions" not in report:
            lines.append("Use --suggest to look up alternative dependency revisions.")
    return "\n".join(lines)


def main(argv=None):
    args = _parser().parse_args(argv)
    try:
        source = demo_project() if args.demo else nullcontext(args.project or Path.cwd())
        with source as project:
            _check_project(project)
            report = analyze(project, args.target)
            pins = _check_pins(report, args.revisions)
            if args.suggest or args.try_suggested:
                report["suggestions"] = _suggestions(report)
            if args.try_suggested:
                pins = {**_suggested_pins(report), **pins}
                report["auto_selected"] = True
            if args.verify or args.try_suggested or pins:
                report["trial_revisions"] = pins
                report["verification"] = verify_build(project, report["target"], revisions=pins)
            if args.demo:
                for package in report["packages"]:
                    package["checkout"] = str(Path(package["checkout"]).relative_to(project.resolve()))
                report["project_source"] = "bundled_demo"
    except (OSError, ValueError, RuntimeError) as error:
        if args.json:
            print(json.dumps({"error": str(error)}))
        else:
            print(f"lake-compat: {error}", file=sys.stderr)
        return 2

    print(json.dumps(report, indent=2) if args.json else _render(report))
    status = report.get("verification", {}).get("status")
    if status in ("inconclusive", "unavailable"):
        return 2
    if status == "failed" or (args.strict and report["findings"]):
        return 1
    return 0
