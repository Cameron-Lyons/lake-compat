"""Build the standalone CLI using Python's standard library."""

from pathlib import Path
from tempfile import TemporaryDirectory
from zipfile import ZipFile, ZipInfo


def build_zipapp(root, output):
    package = root / "lake_compat"
    example = package / "upgrade-demo"
    files = [root / "LICENSE", *package.glob("*.py")]
    for directory in (example, example / "greeting"):
        files.extend(file for file in directory.iterdir()
                     if file.name == "lean-toolchain" or file.suffix in (".lean", ".toml", ".json"))

    output.parent.mkdir(parents=True, exist_ok=True)
    with TemporaryDirectory(dir=output.parent) as staging:
        staged = Path(staging) / output.name
        staged.write_bytes(b"#!/usr/bin/env python3\n")
        with ZipFile(staged, "a") as archive:
            # Stored entries and fixed ZIP metadata make the bytes independent
            # of compression library, host platform and source timestamps.
            def add(name, data):
                entry = ZipInfo(name)
                entry.create_system = 3
                entry.external_attr = 0o100644 << 16
                archive.writestr(entry, data)

            add("__main__.py", b"from lake_compat.__main__ import run\nrun()\n")
            for file in sorted(files, key=lambda f: f.relative_to(root).as_posix()):
                add(file.relative_to(root).as_posix(), file.read_bytes())
        staged.chmod(0o755)
        staged.replace(output)


if __name__ == "__main__":
    root = Path(__file__).resolve().parents[1]
    output = root / "dist/lake-compat.pyz"
    build_zipapp(root, output)
    print(output)
