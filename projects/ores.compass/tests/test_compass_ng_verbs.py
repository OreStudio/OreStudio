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



def _pinned_root(tmp_path, committed="2026-11-18T01:30:12Z"):
    root = tmp_path / "root"
    (root / "build" / "scripts").mkdir(parents=True)
    (root / "build" / "scripts" / "compass_ng.pin").write_text(
        f"commit: 21ae16da5fcc55a2855606726c82770c3c08e080\ncommitted: {committed}\n")
    return root


def _installed(tmp_path, monkeypatch, committed):
    bin_dir = tmp_path / "bin"
    bin_dir.mkdir(parents=True)
    exe = bin_dir / "compass-ng"
    exe.write_text("#!/bin/sh\nprintf 'compass 0.0.1\\ncommit: af0f0bedb886 (af0f0be)\\n"
                   f"committed: {committed}\\n'\n")
    exe.chmod(0o755)
    monkeypatch.setenv("PATH", str(bin_dir))


def test_a_missing_compass_ng_is_reported(tmp_path, monkeypatch):
    monkeypatch.setenv("PATH", str(tmp_path))
    assert compass.compass_ng_problem(_pinned_root(tmp_path)) == "compass-ng is not installed"


def test_a_build_older_than_the_pin_is_reported(tmp_path, monkeypatch):
    _installed(tmp_path, monkeypatch, "2026-11-18T00:41:28Z")
    problem = compass.compass_ng_problem(_pinned_root(tmp_path))
    assert problem == "compass-ng af0f0be is older than the 21ae16d ORE Studio needs"


def test_the_pinned_build_or_a_newer_one_meets_the_pin(tmp_path, monkeypatch):
    _installed(tmp_path, monkeypatch, "2026-11-18T01:30:12Z")
    assert compass.compass_ng_problem(_pinned_root(tmp_path)) is None
    _installed(tmp_path / "newer", monkeypatch, "2026-12-01T00:00:00Z")
    assert compass.compass_ng_problem(_pinned_root(tmp_path / "newer")) is None
