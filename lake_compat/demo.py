"""Provide the bundled Lake project for an offline first scan."""

from contextlib import contextmanager
from importlib import resources
from pathlib import Path
from tempfile import TemporaryDirectory


@contextmanager
def demo_project():
    """Yield the sample's root on disk for the duration of a scan or build."""
    sample = resources.files("lake_compat").joinpath("upgrade-demo")
    with TemporaryDirectory(prefix="lake-compat-demo-") as temp:
        project = Path(temp) / "upgrade-demo"
        packages = [(sample, project, "Main.lean"),
                    (sample.joinpath("greeting"), project / "greeting", "Greeting.lean")]
        for contents, destination, module in packages:
            destination.mkdir()
            for name in ("lean-toolchain", "lakefile.toml", "lake-manifest.json", module):
                (destination / name).write_bytes(contents.joinpath(name).read_bytes())
        yield project
