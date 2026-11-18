"""Python console entry point for the native Lean executable."""

import os
import subprocess
import sys
from pathlib import Path

BINARY = "lake-compat.exe" if os.name == "nt" else "lake-compat"


def main(argv=None):
    package = Path(__file__).resolve().parent
    binary = package / "bin" / BINARY
    if not binary.is_file() and (package.parent / "LakeCompatMain.lean").is_file():
        # python -m lake_compat from a source checkout.
        subprocess.run(
            ["lake", "build", "lake-compat"], cwd=package.parent, check=True, stdout=sys.stderr
        )
        binary = package.parent / ".lake" / "build" / "bin" / BINARY
    return subprocess.call([binary, *(sys.argv[1:] if argv is None else argv)])
