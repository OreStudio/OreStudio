"""
Tests for the verbs compass forwards to compass-ng.

Run with:  python -m pytest projects/ores.compass/tests/test_compass_ng_verbs.py -v
"""

import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).parent.parent / "src"))

import compass  # noqa: E402


def _fake_compass_ng(directory, exit_code):
    calls = directory / "calls.txt"
    exe = directory / "compass-ng"
    exe.write_text(f'#!/bin/sh\nprintf "%s\\n" "$@" > "{calls}"\nexit {exit_code}\n')
    exe.chmod(0o755)
    return calls


def test_a_verb_runs_in_compass_ng_over_the_checkout(tmp_path, monkeypatch):
    calls = _fake_compass_ng(tmp_path, 3)
    monkeypatch.setenv("PATH", str(tmp_path))
    assert compass.cmd_compass_ng(["diff", "-j", "6"]) == 3
    assert calls.read_text().splitlines() == ["-C", str(compass.PROJECT_ROOT), "diff", "-j", "6"]


def test_a_missing_compass_ng_is_refused(tmp_path, monkeypatch, capsys):
    monkeypatch.setenv("PATH", str(tmp_path))
    assert compass.cmd_compass_ng(["generate"]) == 2
    assert "compass-ng is not on the PATH" in capsys.readouterr().err

