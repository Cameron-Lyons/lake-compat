import io
import unittest
from unittest.mock import patch
from urllib.error import HTTPError

from lake_compat import releases


TARGET = "leanprover/lean4:v4.35.0"
OLDER = "leanprover/lean4:v4.34.0"
REPO = "leanprover-community/mathlib4"
URL = "https://github.com/" + REPO


def version(revision, tag, toolchain=TARGET):
    return {"revision": revision, "tag": tag, "toolchain": toolchain, "date": "2026-10-01"}


def run(revision, success=True, toolchain=TARGET, **kwargs):
    return dict(revision=revision, toolchain=toolchain, built=success, **kwargs)


class ReleaseSuggestions(unittest.TestCase):
    def registry(self, versions, builds, source=None):
        metadata = {
            "name": "mathlib",
            "sources": [source or {"host": "github", "fullName": REPO}],
        }
        payloads = [metadata, {"data": versions}, {"data": builds}]
        return patch.object(releases, "_read_json", side_effect=payloads)

    def test_rank_builds_and_tag_declarations_by_exact_revision_and_toolchain(self):
        tested, updated, declared, failed, foreign, untagged = [c * 40 for c in "abcdef"]
        versions = [
            version(tested, "v4.35"), version(updated, "v0.1", OLDER),
            version(declared, "v0.2"), version(failed, "v0.3"),
            version(foreign, "v0.4", OLDER), version(untagged, None),
        ]
        builds = [
            run(tested, requiredUpdate=False, tested=True, url="https://example.org/build"),
            run(updated, requiredUpdate=True),
            run(failed, success=False, runAt="2026-11-01"),
            run(failed, runAt="2026-10-01"), run(foreign, toolchain=OLDER),
        ]
        with self.registry(versions, builds) as fetch:
            result = releases.suggest_versions(URL + ".git", TARGET, package="mathlib")
        self.assertEqual(fetch.call_args_list[0].args[0], releases.REGISTRY + "/leanprover-community/mathlib")
        self.assertEqual(result["status"], "found")
        self.assertEqual([row["revision"] for row in result["candidates"]], [tested, updated, declared, untagged])
        first, second, third, fourth = result["candidates"]
        self.assertEqual((first["tested"], first["build_url"]), (True, "https://example.org/build"))
        self.assertEqual((second["evidence"], second["required_update"]), ("built", True))
        self.assertEqual((third["evidence"], third["required_update"]), ("declared", None))
        self.assertEqual((fourth["evidence"], fourth["tag"]), ("declared", None))

    def test_untagged_builds_and_failed_tests(self):
        good, failed_tests = "1" * 40, "2" * 40
        with self.registry([version(failed_tests, "v3")], [run(good), run(failed_tests, tested=False)]):
            result = releases.suggest_versions("git@github.com:" + REPO + ".git", TARGET)
        self.assertEqual(len(result["candidates"]), 1)
        self.assertIsNone(result["candidates"][0]["tag"])
        self.assertEqual(result["candidates"][0]["revision"], good)

    def test_rejects_registry_mismatch(self):
        sources = [{"host": "github", "fullName": "other/mathlib4"},
                   {"host": "github", "fullName": REPO, "subDir": "other"}]
        for source in sources:
            with self.subTest(source=source), self.registry([], [], source) as fetch:
                result = releases.suggest_versions(URL, TARGET)
                self.assertEqual(result["status"], "unsupported")
                self.assertEqual(fetch.call_count, 1)
        with self.registry([], [], sources[1]):
            result = releases.suggest_versions(URL, TARGET, sub_dir="./other")
        self.assertEqual(result["status"], "no_match")
        with self.registry([], []) as fetch:
            result = releases.suggest_versions(URL, TARGET, package="unrelated")
        self.assertEqual((result["status"], fetch.call_count), ("unsupported", 1))

    def test_only_queries_registry_for_recognized_sources(self):
        urls = ["https://github.com.evil.test/name/repo", "file:///tmp/repo",
                "https://token@github.com/name/repo", "https://github.com/name/.."]
        with patch.object(releases, "_read_json") as fetch:
            for url in urls:
                self.assertEqual(releases.suggest_versions(url, TARGET)["status"], "unsupported")
            self.assertEqual(releases.suggest_versions(URL, TARGET, scope="..")["status"], "unsupported")
        fetch.assert_not_called()

    def test_remote_failures_do_not_imply_incompatibility(self):
        error = HTTPError(releases.REGISTRY, 404, "no metadata", {}, io.BytesIO())
        for problem in (error, TimeoutError(), ValueError("bad JSON")):
            with self.subTest(problem=problem), patch.object(releases, "_read_json", side_effect=problem):
                result = releases.suggest_versions(URL, TARGET)
                self.assertEqual((result["status"], result["candidates"]), ("unavailable", []))
        with patch.object(releases, "_read_json", side_effect=[{"sources": []}]):
            self.assertEqual(releases.suggest_versions(URL, TARGET)["status"], "unsupported")
