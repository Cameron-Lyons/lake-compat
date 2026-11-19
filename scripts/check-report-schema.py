"""Validate reports from the built CLI against the public Draft 2020-12 schema."""

import argparse
import json
import os
import subprocess
import tempfile
from pathlib import Path

from jsonschema import Draft202012Validator


ROOT = Path(__file__).resolve().parents[1]
BIN = ROOT / ".lake/build/bin"


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("cli", type=Path, nargs="?", default=BIN / "lake-compat")
    cli = parser.parse_args().cli.resolve()
    schema = json.loads((ROOT / "schema/report-v1.json").read_text())
    Draft202012Validator.check_schema(schema)
    validator = Draft202012Validator(schema)

    def check(args, code=0, status=None, env=None):
        command = [str(cli), "--json", *args]
        proc = subprocess.run(command, capture_output=True, text=True, env=env, timeout=90)
        if proc.returncode != code:
            raise RuntimeError(
                f"{args}: expected exit {code}, got {proc.returncode}\n{proc.stdout}{proc.stderr}"
            )
        report = json.loads(proc.stdout)
        validator.validate(report)
        if status and report.get("verification", {}).get("status") != status:
            raise ValueError(f"{args}: expected verification status {status!r}, got {report}")
        return report

    target = (ROOT / "lean-toolchain").read_text().strip()
    demo = [target, "--demo"]
    check(demo)
    strict = check([*demo, "--strict"], 1)
    if not strict["packages"] or not strict["findings"]:
        raise ValueError("The strict demo must exercise package and finding schemas")
    if "error" not in check([], 2):
        raise ValueError("The missing target must produce an error report")
    passed = check([*demo, "--build-target", "upgradeDemo"], status="passed")
    if passed.get("build_targets") != ["upgradeDemo"]:
        raise ValueError("The passing trial must record its named build target")
    check([*demo, "--verify"], 2, "unavailable", env={**os.environ, "PATH": ""})

    with tempfile.TemporaryDirectory() as temp:
        work = Path(temp)
        project = work / "project"
        project.mkdir()
        (project / "lakefile.toml").write_text(
            'name = "app"\ntestDriver = "Checks"\n[[lean_lib]]\nname = "Checks"\n'
        )
        (project / "Checks.lean").write_text("example : 1 + 1 = 2 := rfl\n")
        (project / "lake-manifest.json").write_text(json.dumps({"name": "app", "version": "1.2.0"}))
        tested = check([target, "--project", str(project), "--test"], status="passed")
        if tested.get("test_requested") is not True or tested["verification"].get("test_exit_code") != 0:
            raise ValueError("The trial must report the successful Lake test driver")
        # Lake library test drivers reject arguments. The report still records the invocation.
        driver_args = ["--filter", "case one"]
        failed = check([target, "--project", str(project), "--test", "--", *driver_args], 1, "failed")
        if failed.get("test_args") != driver_args or not failed["verification"].get("test_exit_code"):
            raise ValueError("The trial must report the arguments and failing Lake test exit code")
        (project / "lakefile.toml").write_text('name = "app"\n')
        dependency = {
            "name": "lib",
            "type": "git",
            "url": "https://github.com/example/lib",
            "rev": "1" * 40,
            "inherited": False,
        }
        manifest = {
            "name": "app",
            "version": "1.2.0",
            "packagesDir": ".lake/packages",
            "packages": [dependency],
        }
        (project / "lake-manifest.json").write_text(json.dumps(manifest))
        # LeanTests.Cli supplies deterministic Reservoir responses and a failed build.
        fixture = (BIN / "lake-compat-tests").resolve(strict=True)
        for name in ("curl", "elan"):
            (work / name).symlink_to(fixture)
        env = {
            **os.environ,
            "PATH": os.pathsep.join([str(work), os.environ.get("PATH", os.defpath)]),
            "LAKECOMPAT_CLI_FIXTURE": "candidates",
        }
        args = ["4.20.0", "--project", str(project)]
        trial = check([*args, "--try-suggested"], 1, "failed", env)
        suggestion = trial["suggestions"]["lib"]
        if suggestion["status"] != "found" or not suggestion["candidates"]:
            raise ValueError("The automatic trial must exercise a nonempty candidate report")
        revision = suggestion["candidates"][0]["revision"]
        if trial.get("auto_selected") is not True or trial["trial_revisions"] != {"lib": revision}:
            raise ValueError("The automatic trial must report its selected revision")

        (project / ".lake").mkdir()
        (project / ".lake/package-overrides.json").write_text(
            json.dumps({"schemaVersion": "1.3.0", "packages": [dependency]})
        )
        check([*args, "--verify"], 2, "inconclusive")

    print("CLI reports validated against schema/report-v1.json.")


if __name__ == "__main__":
    main()
