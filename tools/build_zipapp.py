"""Package the host's native Lean executable in a standalone Python zipapp."""

import os
import subprocess
from pathlib import Path
from tempfile import TemporaryDirectory
from zipfile import ZIP_DEFLATED, ZipFile, ZipInfo


def build_native(root):
    subprocess.run(["lake", "build", "lake-compat"], cwd=root, check=True)
    name = "lake-compat.exe" if os.name == "nt" else "lake-compat"
    return root / ".lake" / "build" / "bin" / name


LAUNCHER = b"""\
from pathlib import Path
import os
import shutil
import subprocess
import sys
from tempfile import TemporaryDirectory
from zipfile import ZipFile

with TemporaryDirectory(prefix="lake-compat-") as temp:
    binary = Path(temp) / ("lake-compat.exe" if os.name == "nt" else "lake-compat")
    with ZipFile(sys.argv[0]) as archive, archive.open("native") as src, binary.open("wb") as dst:
        shutil.copyfileobj(src, dst)
    binary.chmod(0o700)
    raise SystemExit(subprocess.call([binary, *sys.argv[1:]]))
"""


def build_zipapp(root, output):
    binary = build_native(root)
    output.parent.mkdir(parents=True, exist_ok=True)
    with TemporaryDirectory(dir=output.parent) as staging:
        staged = Path(staging) / output.name
        staged.write_bytes(b"#!/usr/bin/env python3\n")
        with ZipFile(staged, "a") as archive:
            for name, data in [
                ("__main__.py", LAUNCHER),
                ("LICENSE", (root / "LICENSE").read_bytes()),
                ("native", binary.read_bytes()),
            ]:
                entry = ZipInfo(name)
                entry.create_system = 3
                entry.external_attr = 0o100644 << 16
                archive.writestr(entry, data, compress_type=ZIP_DEFLATED)
        staged.chmod(0o755)
        staged.replace(output)


if __name__ == "__main__":
    root = Path(__file__).resolve().parents[1]
    output = root / "dist/lake-compat.pyz"
    build_zipapp(root, output)
    print(output)
