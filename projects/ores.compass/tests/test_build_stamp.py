"""
Tests for the start and finish times `compass build` prints and logs.

Run with:  python -m pytest projects/ores.compass/tests/test_build_stamp.py -v
"""

import re
import sys
from pathlib import Path

import pytest

sys.path.insert(0, str(Path(__file__).parent.parent / "src"))

import compass  # noqa: E402
import compass_claude  # noqa: E402

STAMP = r"\d{4}-\d{2}-\d{2} \d{2}:\d{2}:\d{2} \S+"


class _FakeLock:
    def announce(self):
        pass

    def close(self):
        pass


@pytest.fixture
def build(tmp_path, monkeypatch):
    """cmd_build with the lock and the cmake step faked; returns the log path."""
    (tmp_path / "build" / "output" / "p").mkdir(parents=True)
    log_path = tmp_path / "build.log"
    monkeypatch.setattr(compass, "PROJECT_ROOT", tmp_path)
    monkeypatch.setattr(compass, "_tr_read_preset", lambda: "p")
    monkeypatch.setattr(compass, "deps_log_repair_command", lambda *a: None)
    monkeypatch.setattr(compass, "_acquire_build_lock", lambda: (_FakeLock(), "a", 2))
    monkeypatch.setattr(compass, "_build_log_path", lambda *a, **k: log_path)
    monkeypatch.setattr(compass, "_check_publish_artifacts", lambda d: [])
    monkeypatch.setattr(compass_claude, "_has_user_systemd", lambda: False)

    def run(rc):
        monkeypatch.setattr(compass, "_run_logged", lambda *a, **k: rc)
        return compass.cmd_build([])

    return run, log_path


def test_a_build_states_when_it_started_and_when_it_succeeded(build, capsys):
    run, log_path = build
    assert run(0) == 0
    out = capsys.readouterr().out
    jobs, started, finished = (out.index("Parallel build jobs"),
                               out.index("Build started"), out.index("Build finished"))
    assert jobs < started < finished
    assert re.search(rf"Build started: {STAMP}", out)
    assert re.search(rf"Build finished \(succeeded, took [\d:]+\): {STAMP}", out)
    log = log_path.read_text()
    assert re.search(rf"Build started: {STAMP}", log)
    assert re.search(rf"Build finished \(succeeded, took [\d:]+\): {STAMP}", log)


def test_a_failed_build_states_its_exit_code_when_it_finished(build, capsys):
    run, log_path = build
    assert run(2) == 2
    assert re.search(rf"Build finished \(failed, exit 2, took [\d:]+\): {STAMP}",
                     capsys.readouterr().out)
    assert "failed, exit 2" in log_path.read_text()


def test_an_interrupted_build_states_when_it_stopped(build, monkeypatch, capsys):
    _, log_path = build

    def interrupt(*args, **kwargs):
        raise KeyboardInterrupt

    monkeypatch.setattr(compass, "_run_logged", interrupt)
    with pytest.raises(KeyboardInterrupt):
        compass.cmd_build([])
    assert re.search(rf"Build finished \(interrupted, took [\d:]+\): {STAMP}",
                     capsys.readouterr().out)
    assert "interrupted" in log_path.read_text()


def test_a_dry_run_states_no_times(build, capsys):
    run, _ = build
    assert compass.cmd_build(["--dry-run"]) == 0
    out = capsys.readouterr().out
    assert "Build started" not in out
    assert "Build finished" not in out
