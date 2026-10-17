"""
Tests for `compass test run` command construction.

Run with:  python -m pytest projects/ores.compass/tests/test_test_run.py -v
No live database or file system access required.
"""

import sys
from pathlib import Path

# Allow importing from the src directory without installing the package.
sys.path.insert(0, str(Path(__file__).parent.parent / "src"))

from compass import _test_run_command  # noqa: E402


def test_plain_form():
    cmd = _test_run_command("clang-debug", "", [])
    assert cmd == ["ctest", "--preset", "clang-debug"]


def test_pass_through_args():
    cmd = _test_run_command("clang-debug", "", ["-VV", "-R", "foo"])
    assert cmd == ["ctest", "--preset", "clang-debug", "-VV", "-R", "foo"]


def test_cdash_script_form():
    cmd = _test_run_command("clang-debug", "Experimental", [])
    assert cmd == [
        "ctest", "--preset", "clang-debug", "--script",
        "CTest.cmake,build_group=Experimental,preset=clang-debug",
    ]


def test_cdash_rejects_unknown_group():
    import pytest

    with pytest.raises(ValueError):
        _test_run_command("clang-debug", "Staging", [])


def test_run_holds_and_releases_the_build_lock(monkeypatch, tmp_path, capsys):
    """cmd_test remembers the holder, runs ctest, and closes the lock."""
    import compass

    captured = {}
    lock_state = {}

    class FakeLock:
        def remember(self):
            lock_state["remembered"] = True

        def close(self):
            lock_state["closed"] = True

    def fake_acquire():
        return FakeLock(), "test-slot", 1

    def fake_log_path(slot):
        return tmp_path / f"ores-build.log.{slot}"

    def fake_run_logged(cmd, cwd, log, env=None):
        captured["cmd"] = cmd
        return 0

    monkeypatch.setattr(compass, "_acquire_build_lock", fake_acquire)
    monkeypatch.setattr(compass, "_tr_read_preset", lambda: "test-preset")
    monkeypatch.setattr(compass, "_test_run_preflight", lambda: True)
    monkeypatch.setattr(compass, "_build_log_path", fake_log_path)
    monkeypatch.setattr(compass, "PROJECT_ROOT", tmp_path)
    monkeypatch.setattr(compass, "_run_logged", fake_run_logged)

    rc = compass.cmd_test(["run", "--preset", "test-preset"])
    assert rc == 0
    assert captured["cmd"] == ["ctest", "--preset", "test-preset"]
    assert lock_state == {"remembered": True, "closed": True}
    assert "Build lock" not in capsys.readouterr().out
