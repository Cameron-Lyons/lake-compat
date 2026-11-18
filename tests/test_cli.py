import json
import os
import subprocess
import sys
import unittest
from pathlib import Path
from tempfile import TemporaryDirectory
from unittest.mock import patch

from tests.native import build_cli, cli

setUpModule = build_cli


class CliTests(unittest.TestCase):
    def setUp(self):
        temp = TemporaryDirectory()
        self.addCleanup(temp.cleanup)
        self.project = Path(temp.name)
        self.old, self.new = "1" * 40, "2" * 40
        package = self.project / ".lake/packages/lib"
        package.mkdir(parents=True)
        (package / "lean-toolchain").write_text("v4.19.0")
        entry = {
            "name": "lib",
            "type": "git",
            "url": "https://github.com/example/lib",
            "rev": self.old,
            "inherited": False,
        }
        for directory, name, entries in [(package, "lib", []), (self.project, "app", [entry])]:
            (directory / "lake-manifest.json").write_text(
                json.dumps({"name": name, "version": "1.2.0", "packages": entries})
            )
        (self.project / "lean-toolchain").write_text("v4.19.0")

    def call(self, *args):
        result = cli("4.20.0", "--project", self.project, *args)
        return result.returncode, result.stdout + result.stderr

    def test_default_is_offline_and_strict_is_opt_in(self):
        # An ordinary scan only needs the executable when there are no Git checkouts.
        with patch.dict(os.environ, PATH=""):
            code, output = self.call()
            self.assertEqual(code, 0)
            self.assertIn("app -> lib", output)
            self.assertIn("Build not run", output)
            code, raw = self.call("--json", "--strict")
            self.assertEqual(code, 1)
            self.assertIn("toolchain_mismatch", {f["kind"] for f in json.loads(raw)["findings"]})

    def test_strict_still_reports_old_locked_pin_after_local_edit(self):
        repo = self.project / ".lake/packages/lib"

        def git(*args):
            return subprocess.check_output(["git", "-C", str(repo), *args], text=True).strip()

        git("init", "--quiet")
        git("add", "lean-toolchain", "lake-manifest.json")
        git(
            "-c",
            "user.name=Fixture",
            "-c",
            "user.email=fixture@example.invalid",
            "-c",
            "commit.gpgSign=false",
            "-c",
            "core.hooksPath=/dev/null",
            "commit",
            "--quiet",
            "-m",
            "pin",
        )
        lock = self.project / "lake-manifest.json"
        manifest = json.loads(lock.read_text())
        manifest["packages"][0]["rev"] = git("rev-parse", "HEAD")
        lock.write_text(json.dumps(manifest))
        (repo / "lean-toolchain").write_text("v4.20.0")
        code, raw = self.call("--strict", "--json")
        self.assertEqual(code, 1)
        report = json.loads(raw)
        self.assertIn("toolchain_mismatch", {f["kind"] for f in report["findings"]})
        self.assertEqual(report["packages"][0]["toolchain"], "leanprover/lean4:v4.19.0")

    def test_unknown_trial_package_produces_json_input_error(self):
        code, raw = self.call("--json", "--try-revision", "other=" + self.new)
        self.assertEqual(code, 2)
        self.assertIn("other", json.loads(raw)["error"])

    def test_missing_directory_points_to_project_root(self):
        code, raw = self.call("--project", str(self.project / "missing"), "--json")
        self.assertEqual(code, 2)
        message = json.loads(raw)["error"]
        self.assertIn("does not exist", message)
        self.assertIn("lakefile.toml", message)
        self.assertNotIn("lake update", message)

    def test_project_flag_rejects_regular_files(self):
        code, raw = self.call("--project", str(self.project / "lean-toolchain"), "--json")
        self.assertEqual(code, 2)
        self.assertIn("--project expects a directory", json.loads(raw)["error"])

    def test_lake_project_without_manifest_explains_how_to_create_it(self):
        (self.project / "lake-manifest.json").unlink()
        for config in ("lakefile.toml", "lakefile.lean"):
            with self.subTest(config=config):
                config_path = self.project / config
                config_path.touch()
                code, output = self.call()
                self.assertEqual(code, 2)
                self.assertIn("lake update", output)
                self.assertIn("lake-manifest.json", output)
                config_path.unlink()

    def tool(self, name, body):
        directory = self.project / "bin"
        directory.mkdir(exist_ok=True)
        script = directory / name
        script.write_text(f"#!{sys.executable}\n" + body)
        script.chmod(0o755)
        return str(directory) + os.pathsep + os.environ["PATH"]

    def test_automatic_trial_uses_selected_set_and_manual_overrides(self):
        responses = {
            "lib": {"name": "lib", "sources": [{"host": "github", "fullName": "example/lib"}]},
            "versions": {
                "data": [
                    {"revision": self.new, "tag": "v2", "toolchain": "leanprover/lean4:v4.20.0"}
                ]
            },
            "builds": {"data": []},
        }
        path = self.tool(
            "curl",
            "import json, sys\n"
            f"print(json.dumps({responses!r}[sys.argv[-1].split('/')[-1]]))\n"
            "sys.stderr.write('200')",
        )
        # Capture what actually reached the manifest before stopping this fixture build.
        self.tool(
            "elan",
            "import json, pathlib, sys\n"
            "if sys.argv[-1] == 'build':\n"
            "    print(json.loads(pathlib.Path('lake-manifest.json').read_text())['packages'][0]['rev'])\n"
            "    sys.exit(1)",
        )
        (self.project / "lakefile.toml").touch()
        with patch.dict(os.environ, PATH=path):
            for extra, expected in [
                ((), self.new),
                (("--try-revision", "lib=" + self.old), self.old),
            ]:
                code, raw = self.call("--try-suggested", "--json", *extra)
                report = json.loads(raw)
                self.assertEqual(code, 1, report)
                self.assertTrue(report["auto_selected"])
                self.assertEqual(report["trial_revisions"], {"lib": expected})
                self.assertIn(expected, report["verification"]["output"])

    def test_no_suggestions_can_still_test_original_pins(self):
        self.tool("curl", "import sys\nsys.stderr.write('404')\nsys.exit(22)")
        path = self.tool("elan", "import sys\nsys.exit(1)")
        with patch.dict(os.environ, PATH=path):
            code, output = self.call("--try-suggested")
            self.assertEqual(code, 2)
            self.assertIn("trial uses the current lock", output)
            self.assertIn("Build unavailable", output)

    def test_parse_errors_are_machine_readable(self):
        for args in [
            ("--demo",),
            ("--try-revision", "lib=main"),
            ("--unknown",),
            ("--try-revision", "lib=" + self.old, "--try-revision", "lib=" + self.new),
        ]:
            code, output = self.call("--json", *args)
            self.assertEqual(code, 2)
            self.assertTrue(json.loads(output)["error"])

    def test_json_after_end_of_options_is_positional(self):
        result = cli("4.36.0", "--", "--json")
        self.assertEqual(result.returncode, 2)
        self.assertEqual(result.stdout, "")
        self.assertIn("unexpected positional argument '--json'", result.stderr)
