"""Run the same native executable that a Lake dependency or standalone user runs."""

import os
import subprocess
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
BINARY = ROOT / ".lake/build/bin" / ("lake-compat.exe" if os.name == "nt" else "lake-compat")


def build_cli():
    subprocess.run(["lake", "build", "lake-compat"], cwd=ROOT, check=True)


def cli(*args, check=False, **kwargs):
    return subprocess.run([BINARY, *args], capture_output=True, text=True, check=check, **kwargs)
