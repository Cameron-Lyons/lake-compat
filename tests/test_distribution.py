import json
from pathlib import Path
import subprocess
import sys
from tempfile import TemporaryDirectory
import unittest
from zipfile import ZipFile

from tools.build_zipapp import build_zipapp


class StandaloneDistribution(unittest.TestCase):
    def test_downloaded_archive_can_scan_demo_in_an_empty_directory(self):
        source = Path(__file__).resolve().parents[1]
        with TemporaryDirectory() as temp:
            download = Path(temp)
            archive = download / "lake-compat.pyz"
            build_zipapp(source, archive)
            # -I keeps the source checkout and its dependencies off the import path.
            command = [sys.executable, "-I", archive.name, "4.36.0", "--demo"]
            scan = subprocess.run(command + ["--json"], cwd=download, capture_output=True,
                                  text=True, check=True)
            report = json.loads(scan.stdout)
            self.assertEqual(report["project"], "upgradeDemo")
            self.assertEqual([(row["kind"], row["path"]) for row in report["findings"]],
                             [("toolchain_mismatch", ["upgradeDemo", "greeting"])])
            self.assertEqual(list(download.iterdir()), [archive])
            with ZipFile(archive) as content:
                self.assertFalse(any(name.startswith("examples/") for name in content.namelist()))
                self.assertEqual(content.read("LICENSE"), (source / "LICENSE").read_bytes())
