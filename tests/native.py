"""Run the same native executable that a Lake dependency or standalone user runs."""

import os
from pathlib import Path
import subprocess

ROOT = Path(__file__).resolve().parents[1]
BINARY = ROOT / ".lake/build/bin" / ("lake-compat.exe" if os.name == "nt" else "lake-compat")


def build_cli():
    subprocess.run(["lake", "build", "lake-compat"], cwd=ROOT, check=True)


def cli(*args, **kwargs):
    return subprocess.run([BINARY, *args], capture_output=True, text=True, **kwargs)
