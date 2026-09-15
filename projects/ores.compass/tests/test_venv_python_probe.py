"""
Tests for the venv interpreter probe in compass.sh.

The wrapper must not assume a POSIX venv layout. The venv module writes
`python.exe` on Windows whatever interpreter name creates the environment,
so `Scripts/python3` never exists there; assuming it stopped Continuous
Windows with exit 127. These tests build fake venv layouts on disk and run
the probe function taken from the wrapper itself, so the name list under
test is the one that ships.

Run with:  python -m pytest projects/ores.compass/tests/test_venv_python_probe.py -v
"""

import re
import subprocess
from pathlib import Path

WRAPPER = Path(__file__).parent.parent / "compass.sh"


def _probe_function():
    text = WRAPPER.read_text(encoding="utf-8")
    match = re.search(r"^resolve_venv_python\(\) \{\n(.*?)^\}\n", text,
                      re.MULTILINE | re.DOTALL)
    assert match, "resolve_venv_python() not found in compass.sh"
    return "resolve_venv_python() {\n" + match.group(1) + "}\n"


def _resolve(tmp_path, names):
    venv_bin = tmp_path / "venv" / "Scripts"
    venv_bin.mkdir(parents=True)
    for name in names:
        (venv_bin / name).write_text("", encoding="utf-8")

    script = _probe_function() + f'VENV_BIN="{venv_bin}"\nresolve_venv_python\n'
    proc = subprocess.run(["bash", "-c", script], capture_output=True,
                          text=True)
    assert proc.returncode == 0, proc.stderr
    return proc.stdout.strip()


class TestResolveVenvPython:
    def test_posix_venv_selects_python3(self, tmp_path):
        assert _resolve(tmp_path, ["python3", "python"]) == \
            str(tmp_path / "venv" / "Scripts" / "python3")

    def test_windows_venv_selects_python_exe(self, tmp_path):
        # The layout CPython's venv module writes on win32, and the one
        # the wrapper used to skip over in favour of a missing python3.
        assert _resolve(tmp_path, ["python.exe"]) == \
            str(tmp_path / "venv" / "Scripts" / "python.exe")

    def test_windows_venv_prefers_python3_exe_when_present(self, tmp_path):
        assert _resolve(tmp_path, ["python3.exe", "python.exe"]) == \
            str(tmp_path / "venv" / "Scripts" / "python3.exe")

    def test_bare_python_name(self, tmp_path):
        assert _resolve(tmp_path, ["python"]) == \
            str(tmp_path / "venv" / "Scripts" / "python")

    def test_no_interpreter_falls_back_to_python3(self, tmp_path):
        # Nothing to pick: keep the POSIX name, so the failure names the
        # path the wrapper would have used without the probe.
        assert _resolve(tmp_path, []) == \
            str(tmp_path / "venv" / "Scripts" / "python3")
