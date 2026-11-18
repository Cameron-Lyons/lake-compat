"""Bundle the Lake executable in a platform-specific, Python-ABI-independent wheel."""

from pathlib import Path
import platform
import shutil
import sys
from setuptools import setup
from setuptools.command.bdist_wheel import bdist_wheel
from setuptools.command.build_py import build_py

# PEP 517 runs setup.py without the source tree on the import path.
sys.path.insert(0, str(Path(__file__).resolve().parent))
from tools.build_zipapp import build_native


class BuildNative(build_py):
    def run(self):
        super().run()
        binary = build_native(Path(__file__).resolve().parent)
        destination = Path(self.build_lib) / "lake_compat" / "bin"
        destination.mkdir(parents=True, exist_ok=True)
        shutil.copy2(binary, destination / binary.name)


class NativeWheel(bdist_wheel):
    def finalize_options(self):
        super().finalize_options()
        self.root_is_pure = False

    def get_tag(self):
        _, _, plat = super().get_tag()
        if platform.system() == "Darwin":
            # A universal Python can build a single-architecture Lean executable.
            major = platform.mac_ver()[0].split(".")[0]
            plat = f"macosx_{major}_0_{platform.machine()}"
        return "py3", "none", plat


setup(cmdclass={"build_py": BuildNative, "bdist_wheel": NativeWheel})
