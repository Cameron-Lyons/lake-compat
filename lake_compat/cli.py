"""Python console entry point for the native Lean executable."""

import json
import os
import shutil
import subprocess
import sys
from itertools import takewhile
from pathlib import Path
from tempfile import TemporaryDirectory
from zipfile import BadZipFile, ZipFile
from zipimport import zipimporter

BINARY = "lake-compat.exe" if os.name == "nt" else "lake-compat"


def _run(argv):
    # The standalone archive embeds this module as its __main__.py.
    if __name__ == "__main__" and __spec__ and isinstance(__spec__.loader, zipimporter):
        with TemporaryDirectory(prefix="lake-compat-") as temp:
            binary = Path(temp) / BINARY
            with ZipFile(__spec__.loader.archive) as archive:
                if "native" not in archive.namelist():
                    raise BadZipFile("zipapp is missing its native executable")
                with archive.open("native") as src, binary.open("wb") as dst:
                    shutil.copyfileobj(src, dst)
            binary.chmod(0o700)
            return subprocess.call([binary, *argv])

    package = Path(__file__).resolve().parent
    binary = package / "bin" / BINARY
    if not binary.is_file() and (package.parent / "LakeCompatMain.lean").is_file():
        # python -m lake_compat from a source checkout.
        subprocess.run(
            ["lake", "build", "lake-compat"], cwd=package.parent, check=True, stdout=sys.stderr
        )
        binary = package.parent / ".lake" / "build" / "bin" / BINARY
    return subprocess.call([binary, *argv])


def main(argv=None):
    args = sys.argv[1:] if argv is None else list(argv)
    try:
        return _run(args)
    except (OSError, subprocess.CalledProcessError, BadZipFile) as error:
        message = f"Could not start lake-compat: {error}"
        if "--json" in takewhile(lambda arg: arg != "--", args):
            print(json.dumps({"error": message}))
        else:
            print(message, file=sys.stderr)
        return 2


if __name__ == "__main__":
    raise SystemExit(main())
