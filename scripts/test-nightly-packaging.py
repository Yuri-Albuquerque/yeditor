#!/usr/bin/env python3
# Copyright (c) 2026 Yuri Flores Albuquerque.
# SPDX-License-Identifier: FSL-1.1-MIT
"""Run packaging scripts against tiny payloads, without native GUI dependencies."""
import os
from pathlib import Path
import shutil
import subprocess
import struct
import tempfile
import unittest

REPO = Path(__file__).resolve().parents[1]


def executable(path, content):
    path.write_text(content)
    path.chmod(0o755)


class NightlyPackagingTests(unittest.TestCase):
    def test_appdir_uses_asdf_output_and_launches_without_appimage_runtime(self):
        with tempfile.TemporaryDirectory(prefix="yeditor packaging ") as tmp:
            root = Path(tmp)
            (root / "bin").mkdir()
            (root / "resources").mkdir()
            executable(root / "bin/lem",
                       '#!/bin/sh\nprintf "%s\\n" "$APPDIR" "$@"\n')
            (root / "bin/terminal.so").write_text("fixture")
            for name in ("yeditor.png", "yeditor.desktop"):
                shutil.copyfile(REPO / "resources" / name, root / "resources" / name)
            deploy = root / "linuxdeploy"
            image = root / "appimagetool"
            executable(deploy, "#!/bin/sh\nexit 0\n")
            executable(image, '#!/bin/sh\nset -eu\ntest -x "$1/AppRun"\ntouch "$2"\n')
            env = dict(os.environ, LINUXDEPLOY=str(deploy), APPIMAGETOOL=str(image))
            env.pop("APPDIR", None)
            subprocess.run(["bash", str(REPO / "docker/make_appdir.sh")],
                           cwd=root, env=env, check=True, capture_output=True, text=True)
            self.assertTrue((root / "Yeditor-x86_64.AppImage").is_file())
            png = (root / "AppDir/yeditor.png").read_bytes()
            self.assertEqual(struct.unpack(">II", png[16:24]), (256, 256))
            appdir = root / "AppDir"
            args = ["--help", "a file with spaces.py"]
            result = subprocess.run([str(appdir / "AppRun"), *args],
                                    cwd="/", env=env, check=True, capture_output=True, text=True)
            self.assertEqual(result.stdout.splitlines(), [str(appdir), *args])
            # Also cover the wrapper's fallback when APPDIR is absent.
            result = subprocess.run([str(appdir / "usr/bin/run-yeditor"), "--help"],
                                    cwd="/", env=env, check=True, capture_output=True, text=True)
            self.assertEqual(result.stdout.splitlines()[-1], "--help")

    def test_macos_readme_does_not_execute_documented_commands(self):
        script = (REPO / "scripts/macos-deploy.bash").read_text()
        start = script.index('README_PATH="bin/README.md"')
        end = script.index("rm -f yeditor-macos.zip", start)
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)
            (root / "bin").mkdir()
            marker = root / "unexpected-xattr"
            executable(root / "xattr",
                       '#!/bin/sh\ntouch "$YEDITOR_TEST_MARKER"\n')
            env = dict(os.environ, PATH=str(root) + os.pathsep + os.environ["PATH"],
                       YEDITOR_TEST_MARKER=str(marker))
            subprocess.run(["bash", "-e", "-c", script[start:end]], cwd=root, env=env, check=True)
            self.assertFalse(marker.exists(), "Writing the README must not run xattr")
            self.assertIn("Yeditor.app", (root / "bin/README.md").read_text())


if __name__ == "__main__":
    unittest.main()
