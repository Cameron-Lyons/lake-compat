"""Install a built wheel and smoke-test its console command away from the checkout."""

import argparse
import json
import os
import subprocess
import sys
from pathlib import Path
from tempfile import TemporaryDirectory


def check_wheel(wheel):
    wheel = wheel.resolve(strict=True)
    _, python_tag, abi, platform = wheel.stem.rsplit("-", 3)
    if (python_tag, abi) != ("py3", "none") or "any" in platform.split("."):
        raise ValueError(f"Expected a py3-none wheel for the build platform, got {wheel.name}")

    with TemporaryDirectory() as temp:
        env = Path(temp) / "venv"
        subprocess.run([sys.executable, "-I", "-m", "venv", env], cwd=temp, check=True)
        scripts = env / ("Scripts" if os.name == "nt" else "bin")
        python = scripts / ("python.exe" if os.name == "nt" else "python")
        cli = scripts / ("lake-compat.exe" if os.name == "nt" else "lake-compat")
        subprocess.run(
            [python, "-I", "-m", "pip", "--isolated", "install", "--no-index", wheel],
            cwd=temp,
            check=True,
        )
        scan = subprocess.run(
            [cli, "4.36.0", "--demo", "--json"],
            cwd=temp,
            env={**os.environ, "PYTHONPATH": "", "PATH": ""},
            stdout=subprocess.PIPE,
            check=True,
        )
        report = json.loads(scan.stdout)
        findings = [(row["kind"], row["path"]) for row in report["findings"]]
        if report["project"] != "upgradeDemo" or findings != [
            ("toolchain_mismatch", ["upgradeDemo", "greeting"])
        ]:
            raise ValueError(f"Unexpected demo report: {report}")


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("wheel", type=Path)
    check_wheel(parser.parse_args().wheel)
