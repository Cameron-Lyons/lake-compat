import io
import json
import subprocess
from contextlib import redirect_stderr, redirect_stdout
from pathlib import Path
from tempfile import TemporaryDirectory
import unittest
from unittest.mock import patch

from lake_compat.cli import main


class CliTests(unittest.TestCase):
    def setUp(self):
        temp = TemporaryDirectory()
        self.addCleanup(temp.cleanup)
        self.project = Path(temp.name)
        self.old, self.new = "1" * 40, "2" * 40
        package = self.project / ".lake/packages/lib"
        package.mkdir(parents=True)
        (package / "lean-toolchain").write_text("v4.19.0")
        entry = {"name": "lib", "type": "git", "url": "https://github.com/example/lib",
                 "rev": self.old, "inherited": False}
        for directory, name, entries in [(package, "lib", []), (self.project, "app", [entry])]:
            (directory / "lake-manifest.json").write_text(json.dumps(
                {"name": name, "version": "1.2.0", "packages": entries}))
        (self.project / "lean-toolchain").write_text("v4.19.0")

    def call(self, *args):
        output = io.StringIO()
        with redirect_stdout(output), redirect_stderr(output):
            code = main(["4.20.0", "--project", str(self.project), *args])
        return code, output.getvalue()

    def test_default_is_offline_and_strict_is_opt_in(self):
        with patch("lake_compat.cli.suggest_versions") as lookup, patch("lake_compat.cli.verify_build") as build:
            code, output = self.call()
            self.assertEqual(code, 0)
            self.assertIn("app -> lib", output)
            self.assertIn("Build not run", output)
            code, raw = self.call("--json", "--strict")
            self.assertEqual(code, 1)
            self.assertIn("toolchain_mismatch", {f["kind"] for f in json.loads(raw)["findings"]})
            lookup.assert_not_called()
            build.assert_not_called()

    def test_strict_still_reports_old_locked_pin_after_local_edit(self):
        repo = self.project / ".lake/packages/lib"

        def git(*args):
            return subprocess.check_output(["git", "-C", str(repo), *args], text=True).strip()

        git("init", "--quiet")
        git("add", "lean-toolchain", "lake-manifest.json")
        git("-c", "user.name=Fixture", "-c", "user.email=fixture@example.invalid",
            "-c", "commit.gpgSign=false", "-c", "core.hooksPath=/dev/null", "commit", "--quiet", "-m", "pin")
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

    def test_automatic_trial_uses_selected_set_and_manual_overrides(self):
        candidate = {"revision": self.new, "tag": "v2", "evidence": "built", "required_update": False,
                     "tested": True, "toolchain": "leanprover/lean4:v4.20.0"}
        suggestions = {"status": "found", "message": "1 candidate", "candidates": [candidate]}
        passed = {"status": "passed", "message": "Built the trial"}
        with patch("lake_compat.cli.suggest_versions", return_value=suggestions) as lookup, \
                patch("lake_compat.cli.verify_build", return_value=passed) as build:
            code, raw = self.call("--try-suggested", "--json")
            self.assertEqual(code, 0)
            self.assertEqual(json.loads(raw)["trial_revisions"], {"lib": self.new})
            build.assert_called_with(self.project, candidate["toolchain"], revisions={"lib": self.new})
            self.assertEqual(lookup.call_args.kwargs["package"], "lib")
            code, _ = self.call("--try-suggested", "--try-revision", "lib=" + self.old)
            self.assertEqual(code, 0)
            build.assert_called_with(self.project, candidate["toolchain"], revisions={"lib": self.old})

    def test_no_suggestions_can_still_test_original_pins(self):
        lookup = {"status": "no_match", "message": "No matching build", "candidates": []}
        with patch("lake_compat.cli.suggest_versions", return_value=lookup), \
                patch("lake_compat.cli.verify_build", return_value={"status": "passed", "message": "Built"}):
            code, output = self.call("--try-suggested")
        self.assertEqual(code, 0)
        self.assertIn("trial uses the current lock", output)

    def test_build_failure_and_inconclusive_have_distinct_exit_codes(self):
        for status, expected in [("failed", 1), ("unavailable", 2), ("inconclusive", 2)]:
            with self.subTest(status=status), patch("lake_compat.cli.verify_build",
                    return_value={"status": status, "message": "details", "output": "Lake log"}):
                code, output = self.call("--verify")
                self.assertEqual(code, expected)
                self.assertIn("Lake log", output)
