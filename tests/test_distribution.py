import json
import os
from pathlib import Path
import shutil
import subprocess
import sys
from tempfile import TemporaryDirectory
import unittest
from zipfile import ZipFile

from tests.native import BINARY, ROOT, build_cli
from tools.build_zipapp import build_zipapp


class StandaloneDistribution(unittest.TestCase):
    def test_downloaded_archive_can_scan_demo_in_an_empty_directory(self):
        with TemporaryDirectory() as temp:
            download = Path(temp)
            archive = download / "lake-compat.pyz"
            build_zipapp(ROOT, archive)
            # -I keeps the source checkout and its dependencies off the import path.
            command = [sys.executable, "-I", archive.name, "4.36.0", "--demo"]
            scan = subprocess.run(command + ["--json"], cwd=download, capture_output=True,
                                  text=True, check=True, env={**os.environ, "PATH": ""})
            report = json.loads(scan.stdout)
            self.assertEqual(report["project"], "upgradeDemo")
            self.assertEqual([(row["kind"], row["path"]) for row in report["findings"]],
                             [("toolchain_mismatch", ["upgradeDemo", "greeting"])])
            self.assertEqual(list(download.iterdir()), [archive])
            with ZipFile(archive) as content:
                self.assertFalse(any(name.startswith("examples/") for name in content.namelist()))
                self.assertEqual(content.read("LICENSE"), (ROOT / "LICENSE").read_bytes())

    def test_native_binary_runs_after_relocation_without_external_tools(self):
        build_cli()
        with TemporaryDirectory() as temp:
            binary = Path(temp) / BINARY.name
            shutil.copy2(BINARY, binary)
            scan = subprocess.run([binary, "4.36.0", "--demo", "--json"], cwd=temp,
                                  capture_output=True, env={**os.environ, "PATH": ""}, check=True)
            self.assertEqual(json.loads(scan.stdout)["packages"][0]["checkout"], "greeting")
            self.assertEqual(list(Path(temp).iterdir()), [binary])

    def test_downstream_lake_executable_scans_its_caller(self):
        with TemporaryDirectory() as temp:
            project = Path(temp)
            shutil.copy2(ROOT / "lean-toolchain", project / "lean-toolchain")
            (project / "lakefile.toml").write_text(
                'name = "consumer"\n[[require]]\nname = "lake-compat"\n'
                f'path = {json.dumps(str(ROOT))}\n')
            subprocess.run(["lake", "--keep-toolchain", "update"], cwd=project, check=True)
            scan = subprocess.run(["lake", "exe", "lake-compat", "4.36.0", "--json"],
                                  cwd=project, capture_output=True, check=True)
            report = json.loads(scan.stdout)
            self.assertEqual(report["project"], "consumer")
            self.assertEqual([p["name"] for p in report["packages"]], ["lake-compat"])
