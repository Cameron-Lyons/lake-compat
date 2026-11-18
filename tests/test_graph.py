import json
import shutil
import subprocess
import tempfile
import unittest
from pathlib import Path

from tests.native import build_cli, cli

setUpModule = build_cli


TARGET = "leanprover/lean4:v4.25.0"


def git_dep(name, rev, inherited=False, **kwargs):
    return dict(
        name=name,
        rev=rev,
        inherited=inherited,
        type="git",
        url=f"https://github.com/example/{name}.git",
        **kwargs,
    )


def local_dep(name, directory, inherited=False, **kwargs):
    return dict(name=name, dir=directory, inherited=inherited, type="path", **kwargs)


def manifest(directory, *packages, file="lake-manifest.json", **fields):
    directory.mkdir(parents=True, exist_ok=True)
    data = {"version": "1.3.0", "packages": list(packages), "name": directory.name}
    data.update(fields)
    (directory / file).write_text(json.dumps(data), encoding="utf-8")


def pin(directory, value=TARGET):
    directory.mkdir(parents=True, exist_ok=True)
    (directory / "lean-toolchain").write_text(value + "\n", encoding="utf-8")


def git(directory, *args):
    return subprocess.check_output(
        [
            "git",
            "-C",
            str(directory),
            "-c",
            "user.name=Test",
            "-c",
            "user.email=t@example.com",
            "-c",
            "commit.gpgsign=false",
            *args,
        ],
        text=True,
    ).strip()


def commit_metadata(directory):
    git(directory, "init", "-q")
    git(directory, "add", ".")
    git(directory, "commit", "-qm", "package metadata")
    return git(directory, "rev-parse", "HEAD")


class GraphTest(unittest.TestCase):
    def setUp(self):
        temp = tempfile.TemporaryDirectory()
        self.addCleanup(temp.cleanup)
        self.project = Path(temp.name).resolve() / "demo"
        self.packages = self.project / ".lake" / "packages"
        pin(self.project, "4.24.0")

    def report(self):
        result = cli(TARGET, "--project", self.project, "--json")
        self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
        return json.loads(result.stdout)

    def test_transitive_revision_conflicts_and_shortest_chains(self):
        for name in ("a", "b", "shared", "leaf"):
            pin(self.packages / name)
        manifest(self.packages / "leaf")
        leaf = commit_metadata(self.packages / "leaf")
        manifest(self.packages / "shared", git_dep("leaf", leaf), fixedToolchain=True)
        pin(self.packages / "shared", "4.24.1")
        shared = commit_metadata(self.packages / "shared")
        manifest(self.packages / "a", git_dep("shared", "cccc"), git_dep("leaf", "stale", True))
        manifest(self.packages / "b", git_dep("shared", shared))
        manifest(
            self.project,
            git_dep("a", commit_metadata(self.packages / "a")),
            git_dep("b", commit_metadata(self.packages / "b")),
            git_dep("shared", shared, True),
            git_dep("leaf", leaf, True),
        )

        result = self.report()
        self.assertEqual(result["root_toolchain"], "leanprover/lean4:v4.24.0")
        self.assertEqual(
            {f["kind"] for f in result["findings"]}, {"revision_conflict", "toolchain_mismatch"}
        )
        self.assertEqual(len(result["findings"]), 2)
        conflict = next(f for f in result["findings"] if f["kind"] == "revision_conflict")
        self.assertEqual(conflict["path"], ["demo", "a", "shared"])
        self.assertEqual(
            (conflict["requested_revision"], conflict["selected_revision"]), ("cccc", shared)
        )
        self.assertEqual(
            {p["name"]: p["path"] for p in result["packages"]}["leaf"],
            ["demo", "a", "shared", "leaf"],
        )

    def test_git_source_conflict_at_same_commit_and_equivalent_remote_spellings(self):
        parent = self.project / "parent"
        dependency = self.packages / "leaf"
        manifest(dependency)
        pin(dependency)
        revision = commit_metadata(dependency)
        root_lock = git_dep("leaf", revision, inherited=True)
        manifest(self.project, local_dep("parent", "parent"), root_lock)
        pin(parent)
        for url in (
            "https://github.com/Example/Leaf/",
            "ssh://git@github.com/example/leaf.git",
            "git@github.com:example/leaf.git",
            "https://github.com/another/leaf.git",
        ):
            with self.subTest(url=url):
                requested = dict(git_dep("leaf", revision), url=url)
                manifest(parent, requested)
                findings = self.report()["findings"]
                if "another" in url:
                    self.assertEqual(len(findings), 1)
                    finding = findings[0]
                    self.assertEqual(
                        (finding["kind"], finding["path"]),
                        ("source_conflict", ["demo", "parent", "leaf"]),
                    )
                    self.assertEqual(finding["requested_revision"], finding["selected_revision"])
                    self.assertIn(url, finding["message"])
                    self.assertIn(root_lock["url"], finding["message"])
                else:
                    self.assertEqual(findings, [])

    def test_local_paths_git_subdir_and_custom_manifest(self):
        vendor = self.project / "vendor" / "mono"
        lib = vendor / "child"
        local = self.project / "local"
        manifest(lib, local_dep("local", "../../../local"), file="custom.json")
        pin(vendor)
        manifest(
            self.project,
            git_dep("mono", commit_metadata(vendor), subDir="child", manifestFile="custom.json"),
            local_dep("local", "local", True),
            packagesDir="vendor",
        )
        manifest(local)
        report = self.report()
        self.assertEqual(report["findings"], [])
        by_name = {p["name"]: p for p in report["packages"]}
        self.assertEqual(by_name["mono"]["checkout"], str(lib))
        self.assertEqual(by_name["mono"]["toolchain_source"], "repository")
        self.assertEqual(by_name["local"]["toolchain_source"], "workspace")
        self.assertEqual(by_name["local"]["path"], ["demo", "mono", "local"])

    def test_external_path_does_not_borrow_unrelated_ancestor_pin(self):
        ext = self.project.parent / "external"
        manifest(self.project, local_dep("external", "../external"))
        pin(self.project.parent)
        manifest(ext)
        self.assertEqual([f["kind"] for f in self.report()["findings"]], ["unknown_toolchain"])

    def test_no_inferred_direct_edge_for_flat_transitives(self):
        pin(self.packages / "orphan", "4.20.0")
        manifest(self.packages / "orphan")
        manifest(
            self.project,
            git_dep("parent", "p"),
            git_dep("orphan", commit_metadata(self.packages / "orphan"), True),
        )
        result = self.report()
        findings = {f["kind"]: f["path"] for f in result["findings"]}
        self.assertEqual(
            findings,
            {"missing_checkout": ["demo", "parent"], "toolchain_mismatch": [], "unknown_path": []},
        )

    def test_missing_child_lock_cannot_hide_missing_evidence(self):
        manifest(self.project, local_dep("a", "a"), local_dep("b", "b", manifestFile=None))
        for name in ("a", "b"):
            pin(self.project / name)
        self.assertEqual(
            [f["kind"] for f in self.report()["findings"]], ["missing_manifest", "missing_manifest"]
        )

    def test_malformed_nested_metadata_and_overrides(self):
        manifest(self.project, local_dep("a", "a"))
        pin(self.project / "a")
        manifest(self.project / "a", {"name": "b", "type": "path", "dir": "."})
        (self.project / ".lake").mkdir()
        (self.project / ".lake" / "package-overrides.json").write_text('{"packages":[]}')
        report = self.report()
        self.assertEqual(
            [f["kind"] for f in report["findings"]], ["active_overrides", "invalid_manifest"]
        )

    def test_legacy_manifest_and_quoted_name(self):
        dep = {
            "name": "«my-lib»",
            "url": "https://github.com/example/my-lib",
            "rev": "aaa",
            "inputRev?": "main",
            "subDir?": "sub",
            "inherited": False,
            "opts": {},
        }
        pkg = self.project / "lake-packages" / "my-lib" / "sub"
        manifest(pkg, version=6)
        pin(pkg)
        dep["rev"] = commit_metadata(pkg.parent)
        manifest(self.project, {"git": dep}, packagesDir="lake-packages", version=5)
        report = self.report()
        self.assertEqual(report["findings"], [])
        self.assertEqual(report["packages"][0]["name"], "my-lib")
        self.assertEqual(report["packages"][0]["input_rev"], "main")

    def test_copy_uses_materialized_path_and_cycles_terminate(self):
        manifest(self.project, local_dep("a", "src/a", copy=True), local_dep("b", "src/b", True))
        a = self.packages / "a"
        b = self.project / "src" / "b"
        manifest(a, local_dep("b", "../../../src/b"))
        manifest(b, local_dep("a", "../a"))
        pin(a)
        pin(b)
        result = self.report()
        self.assertEqual(result["findings"], [])
        self.assertEqual(result["packages"][0]["checkout"], str(a))

    def test_absent_requirement_is_reported_on_the_correct_parent(self):
        manifest(self.project, local_dep("a", "a"))
        pin(self.project / "a")
        manifest(self.project / "a", git_dep("b", "bb"))
        (finding,) = self.report()["findings"]
        self.assertEqual(
            (finding["kind"], finding["path"]), ("unlocked_dependency", ["demo", "a", "b"])
        )

    def test_modified_and_deleted_metadata_cannot_mask_locked_pins(self):
        repo = self.packages / "parent"
        pin(repo, "4.24.0")
        manifest(repo, git_dep("leaf", "b" * 40))
        revision = commit_metadata(repo)
        manifest(self.project, git_dep("parent", revision))
        original = self.report()
        self.assertEqual(
            {f["kind"] for f in original["findings"]}, {"toolchain_mismatch", "unlocked_dependency"}
        )

        pin(repo, TARGET)
        manifest(repo)
        git(repo, "add", "lean-toolchain")
        self.assertEqual(self.report(), original)
        (repo / "lean-toolchain").unlink()
        (repo / "lake-manifest.json").unlink()
        self.assertEqual(self.report(), original)

    def test_archive_metadata_requires_a_provenance_finding(self):
        repo = self.packages / "dependency"
        pin(repo)
        manifest(repo)
        manifest(self.project, git_dep("dependency", "a" * 40))
        report = self.report()
        self.assertEqual([f["kind"] for f in report["findings"]], ["unknown_checkout_revision"])
        self.assertEqual(report["packages"][0]["toolchain"], TARGET)

    def test_incomplete_checkout_cannot_borrow_workspace_git_objects(self):
        manifest(self.project)
        workspace_rev = commit_metadata(self.project)
        repo = self.packages / "dependency"
        manifest(repo)
        pin(repo)
        (repo / ".git").mkdir()
        manifest(self.project, git_dep("dependency", workspace_rev))
        report = self.report()
        self.assertEqual([f["kind"] for f in report["findings"]], ["unknown_checkout_revision"])
        self.assertEqual(report["packages"][0]["toolchain"], TARGET)

    def test_local_path_dependency_uses_uncommitted_metadata(self):
        local = self.project / "local"
        manifest(local)
        pin(local, "4.24.0")
        commit_metadata(local)
        manifest(self.project, local_dep("local", "local"))
        pin(local, TARGET)
        self.assertEqual(self.report()["findings"], [])

    def test_untracked_metadata_and_ignored_subpackage_pin_use_locked_tree(self):
        repo = self.packages / "group"
        pin(repo, "4.24.0")
        (repo / ".gitignore").write_text("core/lean-toolchain\n")
        manifest(repo / "front", local_dep("core", "../core"), file="custom.json")
        manifest(repo / "core")
        base = commit_metadata(repo)
        manifest(
            self.project,
            git_dep("group", base, subDir="front", manifestFile="custom.json"),
            local_dep("core", ".lake/packages/group/front/../core", True),
        )
        pin(repo / "core", TARGET)  # ignored by Git
        pin(repo / "front", TARGET)  # untracked
        result = self.report()
        self.assertEqual(
            [(f["kind"], f["path"]) for f in result["findings"]],
            [
                ("toolchain_mismatch", ["demo", "group", "core"]),
                ("toolchain_mismatch", ["demo", "group"]),
            ],
        )
        self.assertTrue(all(p["toolchain_source"] == "repository" for p in result["packages"]))

    def test_worktree_and_committed_metadata_symlinks(self):
        source = self.project / "original"
        pin(source, "4.24.0")
        manifest(source)
        (source / "lean-toolchain").rename(source / "version")
        (source / "lean-toolchain").symlink_to("version")
        revision = commit_metadata(source)
        repo = self.packages / "dependency"
        repo.parent.mkdir(parents=True)
        git(source, "worktree", "add", "--quiet", "--detach", str(repo), revision)
        manifest(self.project, git_dep("dependency", revision))
        (repo / "version").write_text(TARGET)
        self.assertTrue((repo / ".git").is_file())
        report = self.report()
        self.assertEqual([f["kind"] for f in report["findings"]], ["toolchain_mismatch"])
        self.assertEqual(report["packages"][0]["toolchain"], "leanprover/lean4:v4.24.0")

    def test_symlinked_subdirectory_inherits_from_its_locked_destination(self):
        repo = self.packages / "mono"
        pin(repo, TARGET)
        for directory in ("original", "edited"):
            manifest(repo / directory / "app")
        pin(repo / "original", "4.24.0")
        (repo / "alias").symlink_to("original/app")
        revision = commit_metadata(repo)
        manifest(self.project, git_dep("mono", revision, subDir="alias"))
        (repo / "alias").unlink()
        (repo / "alias").symlink_to("edited/app")
        result = self.report()
        self.assertEqual([f["kind"] for f in result["findings"]], ["toolchain_mismatch"])
        self.assertEqual(result["packages"][0]["toolchain"], "leanprover/lean4:v4.24.0")

    @unittest.skipUnless(shutil.which("git"), "git needed to create a real checkout")
    def test_wrong_checkout_is_not_used_as_evidence(self):
        manifest(self.project, git_dep("stale", "1" * 40))
        repo = self.packages / "stale"
        pin(repo)
        manifest(repo, git_dep("imaginary", "xx"))
        subprocess.run(["git", "init", "-q", str(repo)], check=True)
        subprocess.run(
            [
                "git",
                "-C",
                str(repo),
                "-c",
                "user.name=Test",
                "-c",
                "user.email=t@example.com",
                "-c",
                "commit.gpgsign=false",
                "commit",
                "--allow-empty",
                "-qm",
                "sample",
            ],
            check=True,
        )
        result = self.report()
        self.assertEqual([f["kind"] for f in result["findings"]], ["checkout_revision_mismatch"])
        self.assertIsNone(result["packages"][0]["toolchain"])

    def test_invalid_root_input_has_actionable_file_context(self):
        result = cli(TARGET, "--project", self.project, "--json")
        self.assertEqual(result.returncode, 2)
        self.assertIn("lake-manifest.json", json.loads(result.stdout)["error"])
        for package in (
            {"type": "path", "dir": "x"},
            git_dep("a", "aa", scope=[]),
            git_dep("../a", "aa"),
            {"name": "a", "type": "git", "inherited": "false"},
        ):
            manifest(self.project, package)
            result = cli(TARGET, "--project", self.project, "--json")
            self.assertEqual(result.returncode, 2)
            self.assertIn("lake-manifest.json", json.loads(result.stdout)["error"])
