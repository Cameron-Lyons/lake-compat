import json
import os
import shutil
import subprocess
import sys
import tempfile
import unittest
from pathlib import Path
from unittest.mock import patch

from tests.native import ROOT as REPO

DRIVER = REPO / ".lake/build/bin/verify-test-driver"
LOG_BYTES = 16 * 1024


def setUpModule():
    if os.name == "posix":
        subprocess.run(["lake", "build", "verify-test-driver"], cwd=REPO, check=True)


def verify_build(project, target, *, revisions=None, timeout_ms=600000):
    output = subprocess.check_output(
        [str(DRIVER), str(project), target, str(timeout_ms), json.dumps(revisions or {})],
        text=True,
        timeout=max(timeout_ms / 1000 + 5, 20),
    )
    return json.loads(output)


@unittest.skipUnless(os.name == "posix", "build verification uses POSIX process groups")
class VerifyBuildTests(unittest.TestCase):
    def setUp(self):
        temp = tempfile.TemporaryDirectory()
        self.addCleanup(temp.cleanup)
        self.root = Path(temp.name)
        self.project = self.root / "src"
        self.project.mkdir()
        self.target = "leanprover/lean4:v4.20.0"
        self.original_toolchain = "leanprover/lean4:v4.19.0\n"
        (self.project / "lean-toolchain").write_text(self.original_toolchain)
        (self.project / "lakefile.toml").write_text('name = "app"\n')
        (self.project / "Main.lean").write_text("-- uncommitted work\n")
        self.manifest = {"name": "app", "packages": [], "version": "1.1.0"}
        self.lock = self.project / "lake-manifest.json"
        self.save_manifest()
        self.bin = self.root / "bin"
        self.bin.mkdir()
        self.elan = self.bin / "elan"
        self.write_elan()
        path = str(self.bin) + os.pathsep + os.environ["PATH"]
        environment = patch.dict(os.environ, PATH=path, LEAN_SYSROOT="/incorrect/lean")
        environment.start()
        self.addCleanup(environment.stop)

    def save_manifest(self):
        for entry in self.manifest["packages"]:
            entry.setdefault("inherited", False)
        self.lock.write_text(json.dumps(self.manifest))

    def write_elan(self, build_body="pass", check_body="pass"):
        script = (
            f"#!{sys.executable}\n"
            "import json, os, pathlib, subprocess, sys, time\n"
            f"assert sys.argv[1:4] == ['run', {self.target!r}, 'lake']\n"
            "assert 'LEAN_SYSROOT' not in os.environ\n"
            "if sys.argv[4] == '--version':\n"
            "    print('Lake version for test toolchain')\n"
            "elif sys.argv[4] == 'translate-config':\n"
            "    pathlib.Path(sys.argv[-1]).write_text('name = \"app\"\\n')\n"
            "elif sys.argv[4] == 'build':\n"
            f"    assert pathlib.Path.cwd() != pathlib.Path({str(self.project)!r}).resolve()\n"
            f"    assert pathlib.Path('lean-toolchain').read_text().strip() == {self.target!r}\n"
            "    assert 'uncommitted work' in pathlib.Path('Main.lean').read_text()\n"
            "    assert not pathlib.Path('.git').exists()\n"
            "    assert not pathlib.Path('.lake/build/stale.olean').exists()\n"
            "    " + build_body.replace("\n", "\n    ") + "\n"
            "elif sys.argv[4] == 'check-build':\n"
            "    " + check_body.replace("\n", "\n    ") + "\n"
            "else:\n"
            "    sys.exit(5)\n"
        )
        self.elan.write_text(script)
        self.elan.chmod(0o755)

    def test_copy_and_isolation(self):
        (self.project / ".git").mkdir()
        cache = self.project / ".lake/build"
        cache.mkdir(parents=True)
        (cache / "stale.olean").touch()
        for index in range(24):
            directory = self.project / f"module-{index}"
            directory.mkdir()
            script = directory / "build.sh"
            script.write_text(f"echo {index}\n")
            script.chmod(0o755)
        self.write_elan(
            "for index in range(24):\n"
            "    script = pathlib.Path(f'module-{index}/build.sh')\n"
            "    assert script.read_text() == f'echo {index}\\n' and os.access(script, os.X_OK)\n"
            "pathlib.Path('Main.lean').write_text('-- changed during build')\nprint('done')"
        )
        original = self.lock.read_bytes()
        report = verify_build(self.project, self.target)
        self.assertEqual(report["status"], "passed", report)
        self.assertIn("done", report["output"])
        self.assertEqual(self.lock.read_bytes(), original)
        self.assertEqual((self.project / "lean-toolchain").read_text(), self.original_toolchain)
        self.assertIn("uncommitted work", (self.project / "Main.lean").read_text())

    def test_success_with_changed_lock_is_inconclusive(self):
        self.write_elan("pathlib.Path('lake-manifest.json').write_text('{}')")
        report = verify_build(self.project, self.target)
        self.assertEqual(report["status"], "inconclusive")
        self.assertIn("changed", report["message"])

    def use_installed_toolchain(self):
        self.elan.unlink()
        target = (REPO / "lean-toolchain").read_text().strip()
        if (
            not shutil.which("elan")
            or subprocess.run(
                ["elan", "run", target, "lake", "--version"], capture_output=True, check=False
            ).returncode
        ):
            self.skipTest("requires the project toolchain installed in elan")
        return target

    def test_build_with_implicit_empty_packages(self):
        target = self.use_installed_toolchain()
        self.lock.write_text(json.dumps({"name": "app", "version": "1.1.0"}))
        (self.project / "lakefile.toml").write_text(
            'name = "app"\ndefaultTargets = ["Main"]\n\n[[lean_lib]]\nname = "Main"\n'
        )
        result = verify_build(self.project, target)
        self.assertEqual(result["status"], "passed", result)
        self.assertEqual(result["checked_revisions"], {})

    def test_build_in_custom_package_cache(self):
        target = self.use_installed_toolchain()
        repo = self.root / "greeting"
        shutil.copytree(
            REPO / "examples/upgrade-demo/greeting", repo, ignore=shutil.ignore_patterns(".lake")
        )
        base = repo / "base"
        base.mkdir()
        (base / "lakefile.toml").write_text('name = "base"\n[[lean_lib]]\nname = "Base"\n')
        (base / "Base.lean").write_text('def baseGreeting := "hello"\n')
        with (repo / "lakefile.toml").open("a") as lakefile:
            lakefile.write('\n[[require]]\nname = "base"\npath = "base"\n')
        (repo / "Greeting.lean").write_text("import Base\ndef greeting := baseGreeting\n")
        subprocess.run(["git", "init", "-q", repo], check=True)
        subprocess.run(["git", "add", "."], cwd=repo, check=True)
        subprocess.run(
            [
                "git",
                "-c",
                "user.name=Test",
                "-c",
                "user.email=test@example.com",
                "commit",
                "-qm",
                "greeting",
            ],
            cwd=repo,
            check=True,
        )
        revision = subprocess.check_output(
            ["git", "rev-parse", "HEAD"], cwd=repo, text=True
        ).strip()
        cache = "vendor"
        local = self.project / cache / "local"
        local.mkdir(parents=True)
        (local / "lakefile.toml").write_text('name = "local"\n[[lean_lib]]\nname = "Local"\n')
        (local / "Local.lean").write_text('def localGreeting := "local"\n')
        (local.parent / "message.txt").write_text("source data")
        (self.project / "lakefile.toml").write_text(
            f'name = "app"\npackagesDir = "{cache}"\ndefaultTargets = ["Main"]\n'
            f'[[require]]\nname = "greeting"\ngit = {json.dumps(str(repo))}\nrev = "{revision}"\n'
            '[[require]]\nname = "local"\npath = "vendor/local"\n'
            '[[lean_lib]]\nname = "Main"\n'
        )
        (self.project / "Main.lean").write_text(
            "import Greeting\nimport Local\nexample : String := greeting ++ localGreeting\n"
            'example : (include_str "vendor/message.txt") = "source data" := rfl\n'
        )
        self.manifest["packagesDir"] = cache
        self.manifest["packages"] = [
            {
                "name": "greeting",
                "type": "git",
                "url": str(repo),
                "rev": revision,
                "inputRev": revision,
                "configFile": "lakefile.toml",
            },
            {
                "name": "base",
                "type": "path",
                "dir": cache + "/greeting/base",
                "inherited": True,
                "configFile": "lakefile.toml",
            },
            {"name": "local", "type": "path", "dir": "vendor/local", "configFile": "lakefile.toml"},
        ]
        self.save_manifest()
        checkout = self.project / cache / "greeting"
        for existing in (False, True):
            with self.subTest(existing=existing):
                if existing:
                    subprocess.run(["git", "clone", "-q", repo, checkout], check=True)
                    (checkout / "Greeting.lean").write_text("local edits\n")
                    (checkout / "scratch").symlink_to(self.root)
                result = verify_build(self.project, target)
                self.assertEqual(result["status"], "passed", result)
                self.assertEqual(result["checked_revisions"], {"greeting": revision})
                if existing:
                    self.assertEqual((checkout / "Greeting.lean").read_text(), "local edits\n")
                else:
                    self.assertFalse(checkout.exists())

    def test_custom_cache_refreshes_copied_packages_with_inherited_paths(self):
        target = self.use_installed_toolchain()
        source = self.project / "source"
        nested = source / "nested"
        nested.mkdir(parents=True)
        (source / "lakefile.toml").write_text(
            'name = "copied"\n[[require]]\nname = "nested"\npath = "nested"\n'
            '[[lean_lib]]\nname = "Copied"\n'
        )
        (source / "Copied.lean").write_text("import Nested\ndef message := nestedMessage\n")
        (nested / "lakefile.toml").write_text('name = "nested"\n[[lean_lib]]\nname = "Nested"\n')
        (nested / "Nested.lean").write_text('def nestedMessage := "current"\n')
        cached = self.project / "vendor/copied"
        shutil.copytree(source, cached)
        (cached / "nested/Nested.lean").write_text('def nestedMessage := "stale"\n')
        (self.project / "lakefile.toml").write_text(
            'name = "app"\npackagesDir = "vendor"\ndefaultTargets = ["Main"]\n'
            '[[require]]\nname = "copied"\npath = "source"\ncopy = true\n'
            '[[lean_lib]]\nname = "Main"\n'
        )
        self.manifest["packagesDir"] = "vendor"
        self.manifest["packages"] = [
            {
                "name": "copied",
                "type": "path",
                "dir": "source",
                "copy": True,
                "configFile": "lakefile.toml",
            },
            {
                "name": "nested",
                "type": "path",
                "dir": "vendor/copied/nested",
                "inherited": True,
                "configFile": "lakefile.toml",
            },
        ]
        self.save_manifest()
        main = self.project / "Main.lean"
        main.write_text('import Copied\nexample : message = "current" := rfl\n')
        result = verify_build(self.project, target)
        self.assertEqual(result["status"], "passed", result)
        self.assertIn("stale", (cached / "nested/Nested.lean").read_text())

        del self.manifest["packagesDir"]
        self.save_manifest()
        main.write_text('import Copied\nexample : message = "stale" := rfl\n')
        result = verify_build(self.project, target)
        self.assertEqual(result["status"], "failed", result)
        self.assertIn("Main.lean", result["output"])
        self.assertIn("stale", (cached / "nested/Nested.lean").read_text())

    def test_implicit_external_cache_is_rejected_before_materialization(self):
        target = self.use_installed_toolchain()
        external = self.root / "external packages"
        source = self.project / "source"
        source.mkdir()
        (source / "lakefile.toml").write_text('name = "dep"\n')
        self.manifest["packages"] = [
            {
                "name": "dep",
                "type": "path",
                "dir": "source",
                "copy": True,
                "configFile": "lakefile.toml",
            }
        ]
        self.save_manifest()
        (self.project / "lakefile.toml").unlink()
        directory = json.dumps(str(external))
        toml = (
            f'name = "app"\npackagesDir = {directory}\n'
            '[[require]]\nname = "dep"\npath = "source"\ncopy = true\n'
        )
        lean = f'import Lake\nopen Lake DSL\npackage app where\n  packagesDir := {directory}\nrequire dep from "source"\n'
        for filename, contents in [("lakefile.toml", toml), ("lakefile.lean", lean)]:
            with self.subTest(filename=filename):
                config = self.project / filename
                config.write_text(contents)
                result = verify_build(self.project, target)
                self.assertEqual(result["status"], "inconclusive", result)
                self.assertIn("project-relative", result["message"])
                self.assertFalse(external.exists())
                config.unlink()

    def test_package_cache_must_be_project_relative(self):
        for path in (str(self.root), "../elsewhere", ".", ".git/objects"):
            with self.subTest(path=path):
                self.manifest["packagesDir"] = path
                self.save_manifest()
                result = verify_build(self.project, self.target)
                self.assertEqual(result["status"], "inconclusive", result)
                self.assertIn("packagesDir", result["message"])

    def test_build_failure_includes_only_bounded_tail(self):
        self.write_elan("print('a' * 24000 + ' ERROR at Main.lean:3')\nsys.exit(2)")
        report = verify_build(self.project, self.target)
        self.assertEqual(report["status"], "failed", report)
        self.assertLessEqual(len(report["output"]), LOG_BYTES)
        self.assertIn("ERROR at Main.lean:3", report["output"])

    def test_build_failure_preserves_first_diagnostic_and_final_summary(self):
        self.write_elan(
            "print('Resolved dependency\\n' * 2000)\n"
            "print('error: ./Main.lean:3:8: Type mismatch\\n  expected Nat')\n"
            "print('Built another target\\n' * 3000)\n"
            "print('error: build failed')\nsys.exit(1)"
        )
        report = verify_build(self.project, self.target)
        self.assertEqual(report["status"], "failed", report)
        self.assertLessEqual(len(report["output"]), LOG_BYTES)
        self.assertIn("Main.lean:3:8: Type mismatch\n  expected Nat", report["output"])
        self.assertTrue(report["output"].endswith("error: build failed"))

    def test_excludes_empty_build(self):
        self.write_elan(check_body="sys.exit(1)")
        self.assertEqual(verify_build(self.project, self.target)["status"], "inconclusive")

    def test_changes_to_effective_build_graph_are_inconclusive(self):
        self.write_elan(check_body="pathlib.Path('lake-manifest.json').write_text('{}')")
        result = verify_build(self.project, self.target)
        self.assertIn("changed", result["message"])
        self.write_elan(
            "pathlib.Path('.lake').mkdir()\n"
            "pathlib.Path('.lake/package-overrides.json').write_text('{}')"
        )
        result = verify_build(self.project, self.target)
        self.assertEqual(result["status"], "inconclusive")
        self.assertIn("overrides", result["message"])

    def test_no_elan_or_installed_target(self):
        with patch.dict(os.environ, PATH=str(self.root / "empty-bin")):
            self.assertEqual(verify_build(self.project, self.target)["status"], "unavailable")
        self.elan.write_text(
            f"#!{sys.executable}\nimport sys\nprint('no such toolchain')\nsys.exit(1)\n"
        )
        result = verify_build(self.project, self.target)
        self.assertEqual(result["status"], "unavailable")
        self.assertIn("no such toolchain", result["output"])

    def test_symlinks_overrides_and_external_paths(self):
        link = self.project / "outside"
        link.symlink_to(self.root)
        self.assertIn("symlink", verify_build(self.project, self.target)["message"])
        link.unlink()
        overrides = self.project / ".lake/package-overrides.json"
        overrides.parent.mkdir()
        overrides.write_text("{}")
        self.assertIn("overrides", verify_build(self.project, self.target)["message"])
        overrides.unlink()
        self.manifest["packages"] = [{"name": "dep", "type": "path", "dir": "../outside"}]
        self.save_manifest()
        self.assertIn("outside", verify_build(self.project, self.target)["message"])

    def test_in_tree_file_and_directory_links_survive_copy(self):
        lib = self.project / "lib"
        lib.mkdir()
        (lib / "Shared.lean").write_text("shared input")
        (self.project / "lib-link").symlink_to("lib")
        (self.project / "shared").symlink_to("lib-link/Shared.lean")
        (lib / "main-link").symlink_to("../Main.lean")
        self.write_elan(
            "assert os.readlink('shared') == 'lib-link/Shared.lean'\n"
            "assert os.readlink('lib-link') == 'lib'\n"
            "assert pathlib.Path('shared').read_text() == 'shared input'\n"
            "assert pathlib.Path('lib-link/main-link').read_text() == pathlib.Path('Main.lean').read_text()"
        )
        self.assertEqual(verify_build(self.project, self.target)["status"], "passed")

    def test_trial_metadata_links_leave_their_targets_intact(self):
        (self.project / "lean-toolchain").rename(self.project / "original-toolchain")
        (self.project / "lean-toolchain").symlink_to("original-toolchain")
        self.lock.rename(self.project / "original-lock")
        self.lock.symlink_to("original-lock")
        self.write_elan(
            f"assert pathlib.Path('original-toolchain').read_text() == {self.original_toolchain!r}\n"
            f"assert pathlib.Path('original-lock').read_text() == {json.dumps(self.manifest)!r}"
        )
        self.assertEqual(verify_build(self.project, self.target)["status"], "passed")
        self.assertTrue(self.lock.is_symlink())
        self.assertTrue((self.project / "lean-toolchain").is_symlink())

    def test_invalid_link_targets_and_special_files(self):
        link = self.project / "lib-link"
        for target in (
            "../src/Main.lean",
            "missing",
            ".git/config",
            ".lake/build/M.olean",
            "lib-link",
        ):
            with self.subTest(target=target):
                link.symlink_to(target)
                result = verify_build(self.project, self.target)
                self.assertEqual(result["status"], "inconclusive", result)
                self.assertIn("symlink", result["message"])
                link.unlink()
        link.symlink_to("middle")
        (self.project / "middle").symlink_to(self.project / "Main.lean")
        self.assertIn("symlink", verify_build(self.project, self.target)["message"])
        (self.project / "middle").unlink()
        link.unlink()
        os.mkfifo(link)
        self.assertIn("special file", verify_build(self.project, self.target)["message"])

    def test_directory_links_cannot_introduce_recursive_traversal(self):
        (self.project / "a").mkdir()
        (self.project / "b").mkdir()
        (self.project / "a" / "to-b").symlink_to("../b")
        (self.project / "b" / "to-a").symlink_to("../a")
        result = verify_build(self.project, self.target)
        self.assertEqual(result["status"], "inconclusive")
        self.assertIn("cycle", result["message"])

    def test_cleanup_handles_build_created_links_and_read_only_directories(self):
        temp_path = self.root / "trial-path"
        external = self.root / "keep"
        external.mkdir()
        marker = external / "contents"
        marker.write_text("keep")
        original_modes = (external.stat().st_mode, marker.stat().st_mode)
        self.write_elan(
            f"pathlib.Path({str(temp_path)!r}).write_text(str(pathlib.Path.cwd()))\n"
            "pathlib.Path('dangling').symlink_to('missing')\n"
            "directory = pathlib.Path('generated/nested')\n"
            "directory.mkdir(parents=True)\n"
            f"os.link({str(marker)!r}, directory / 'hard-link')\n"
            f"(directory / 'external-link').symlink_to({str(external)!r})\n"
            "directory.chmod(0o500)\n"
            "directory.parent.chmod(0o500)"
        )
        result = verify_build(self.project, self.target)
        self.assertEqual(result["status"], "passed", result)
        self.assertFalse(Path(temp_path.read_text()).parent.exists())
        self.assertEqual(marker.read_text(), "keep")
        self.assertEqual((external.stat().st_mode, marker.stat().st_mode), original_modes)

    def test_timeout(self):
        self.write_elan(
            "child = 'import signal,time; signal.signal(signal.SIGTERM, signal.SIG_IGN); "
            'print("compiler still running", flush=True); time.sleep(90)\'\n'
            "subprocess.Popen([sys.executable, '-c', child])"
        )
        report = verify_build(self.project, self.target, timeout_ms=500)
        self.assertEqual(report["status"], "inconclusive")
        self.assertIn("timeout", report["message"])
        self.assertIn("compiler still running", report.get("output", ""))

    def test_verified_trial_revision_requires_correct_checkout(self):
        git_repo = self.root / "dep"
        git_repo.mkdir()

        def git(*args):
            return subprocess.check_output(["git", "-C", str(git_repo), *args], text=True).strip()

        git("init", "--quiet")
        for folder in ("core", "postgres"):
            (git_repo / folder).mkdir()
            (git_repo / folder / "lakefile.lean").write_text("-- package")
        (git_repo / "nested/inner/app").mkdir(parents=True)
        (git_repo / "nested/inner/app/input").touch()
        (git_repo / "nested/child").mkdir()
        (git_repo / "nested/child/lakefile.lean").write_text("-- inherited package")
        (git_repo / "alias").symlink_to("nested/inner/app")
        git("add", ".")
        git(
            "-c", "user.name=Test", "-c", "user.email=test@example.com", "commit", "-q", "-m", "dep"
        )
        revision = git("rev-parse", "HEAD")
        old_revision = "a" * 40
        self.manifest["packages"] = [
            {
                "name": "«dep-postgres»",
                "type": "git",
                "rev": old_revision,
                "url": str(git_repo),
                "subDir": "postgres",
            },
            {"name": "core", "type": "path", "dir": ".lake/packages/dep-postgres/postgres/../core"},
            {
                "name": "child",
                "type": "path",
                "dir": ".lake/packages/dep-postgres/alias/../../child",
                "inherited": True,
            },
            {"name": "local", "type": "path", "dir": "vendor/mirror/../local"},
        ]
        (self.project / "vendor/mirror").mkdir(parents=True)
        (self.project / "vendor/local").mkdir()
        self.save_manifest()
        self.write_elan(
            "lock = json.loads(pathlib.Path('lake-manifest.json').read_text())\n"
            f"assert lock['packages'][0]['rev'] == {revision!r}\n"
            f"subprocess.check_call(['git', 'clone', '--quiet', {str(git_repo)!r}, '.lake/packages/dep-postgres'])"
        )
        result = verify_build(self.project, self.target, revisions={"dep-postgres": revision})
        self.assertEqual(result["status"], "passed", result)
        self.assertEqual(result["checked_revisions"], {"dep-postgres": revision})
        self.assertEqual(json.loads(self.lock.read_text()), self.manifest)
        self.write_elan(
            f"subprocess.check_call(['git', 'clone', '--quiet', {str(git_repo)!r}, '.lake/packages/dep-postgres'])"
        )
        result = verify_build(self.project, self.target)
        self.assertEqual(result["status"], "inconclusive")
        self.assertIn("locked revision", result["message"])
        self.write_elan(
            f"subprocess.check_call(['git', 'clone', '--quiet', {str(git_repo)!r}, '.lake/packages/dep-postgres'])\n"
            "pathlib.Path('.lake/packages/dep-postgres/postgres/lakefile.lean').write_text('-- changed')"
        )
        result = verify_build(self.project, self.target, revisions={"dep-postgres": revision})
        self.assertEqual(result["status"], "inconclusive")
        self.assertIn("tracked files", result["message"])

    def test_unknown_and_invalid_trial_revisions(self):
        self.assertEqual(
            verify_build(self.project, self.target, revisions={"missing": "a" * 40})["status"],
            "inconclusive",
        )
        self.manifest["packages"] = [
            {"name": "dep", "type": "git", "url": "https://example.org/dep", "rev": "a" * 40}
        ]
        self.save_manifest()
        self.assertIn(
            "40- or 64-character",
            verify_build(self.project, self.target, revisions={"dep": "main"})["message"],
        )

    def test_sha256_trial_keeps_tagged_manifest_fields(self):
        repo = self.root / "sha256-dep"
        init = subprocess.run(
            ["git", "init", "-q", "--object-format=sha256", repo],
            capture_output=True,
            text=True,
            check=False,
        )
        if init.returncode:
            self.skipTest(f"Git lacks SHA-256 repositories: {init.stderr}")
        subprocess.run(
            [
                "git",
                "-C",
                repo,
                "-c",
                "user.name=Test",
                "-c",
                "user.email=test@example.com",
                "commit",
                "--allow-empty",
                "-qm",
                "dep",
            ],
            check=True,
        )
        revision = subprocess.check_output(
            ["git", "-C", repo, "rev-parse", "HEAD"], text=True
        ).strip()
        self.assertEqual(len(revision), 64)
        entry = {
            "git": {
                "name": "dep",
                "url": str(repo),
                "inherited": False,
                "rev": "0" * 64,
                "inputRev?": "main",
                "subDir?": None,
                "extra": {"keep": True},
            }
        }
        self.lock.write_text(json.dumps({"version": 6, "packages": [entry]}))
        self.write_elan(
            "lock = json.loads(pathlib.Path('lake-manifest.json').read_text())\n"
            "fields = lock['packages'][0]['git']\n"
            f"assert fields['rev'] == fields['inputRev?'] == {revision!r}\n"
            "assert 'inputRev' not in fields and fields['extra'] == {'keep': True}\n"
            f"subprocess.check_call(['git', 'clone', '-q', {str(repo)!r}, '.lake/packages/dep'])"
        )
        result = verify_build(self.project, self.target, revisions={"dep": revision.upper()})
        self.assertEqual(result["status"], "passed", result)
        self.assertEqual(result["checked_revisions"], {"dep": revision})
