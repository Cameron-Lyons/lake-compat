"""Package the host's native Lean executable in a standalone Python zipapp."""

import os
import shutil
import subprocess
from pathlib import Path
from tempfile import TemporaryDirectory
from zipfile import ZIP_DEFLATED, ZipFile, ZipInfo


def build_native(root):
    subprocess.run(["lake", "build", "lake-compat"], cwd=root, check=True)
    name = "lake-compat.exe" if os.name == "nt" else "lake-compat"
    return root / ".lake" / "build" / "bin" / name


def build_zipapp(root, output):
    binary = build_native(root)
    output.parent.mkdir(parents=True, exist_ok=True)
    with TemporaryDirectory(dir=output.parent) as staging:
        staged = Path(staging) / output.name
        staged.write_bytes(b"#!/usr/bin/env python3\n")
        with ZipFile(staged, "a") as archive:
            for name, path in [
                ("__main__.py", root / "lake_compat" / "cli.py"),
                ("LICENSE", root / "LICENSE"),
                ("native", binary),
            ]:
                entry = ZipInfo(name)
                entry.create_system = 3
                entry.external_attr = 0o100644 << 16
                entry.compress_type = ZIP_DEFLATED
                entry.file_size = path.stat().st_size
                with path.open("rb") as src, archive.open(entry, "w") as dst:
                    shutil.copyfileobj(src, dst)
        staged.chmod(0o755)
        staged.replace(output)


if __name__ == "__main__":
    root = Path(__file__).resolve().parents[1]
    output = root / "dist/lake-compat.pyz"
    build_zipapp(root, output)
    print(output)
