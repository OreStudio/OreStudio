"""
Tests for the per-worktree build lock.

Run with:  python -m pytest projects/ores.compass/tests/test_worktree_build_lock.py -v
"""

import fcntl
import sys
from pathlib import Path

import pytest

sys.path.insert(0, str(Path(__file__).parent.parent / "src"))

import compass  # noqa: E402


@pytest.fixture
def worktree(tmp_path, monkeypatch):
    monkeypatch.setattr(compass, "PROJECT_ROOT", tmp_path)
    monkeypatch.setattr(compass, "_WORKTREE_BUILD_LOCK", None)
    yield tmp_path
    if compass._WORKTREE_BUILD_LOCK is not None:
        compass._WORKTREE_BUILD_LOCK.close()


def hold_lock(holder_line):
    """Hold the worktree lock through a separate open file, as another
    process of the same worktree would."""
    path = compass._worktree_build_lock_path()
    path.parent.mkdir(parents=True, exist_ok=True)
    held = open(path, "a+")
    fcntl.flock(held, fcntl.LOCK_EX | fcntl.LOCK_NB)
    held.seek(0)
    held.truncate()
    held.write(holder_line)
    held.flush()
    return held


def test_a_build_takes_the_worktree_lock(worktree):
    compass._acquire_worktree_build_lock()
    assert compass._WORKTREE_BUILD_LOCK is not None
    assert "pid=" in compass._worktree_build_lock_path().read_text()


def test_a_second_build_in_the_worktree_is_refused_and_names_the_first(worktree, capsys):
    held = hold_lock("pid=4242 | compass build | since 2026-10-04T21:49:01\n")
    try:
        with pytest.raises(SystemExit) as exit_info:
            compass._acquire_worktree_build_lock()
        assert exit_info.value.code == 1
        out = capsys.readouterr().out
        assert "already running in this worktree" in out
        assert "pid=4242" in out
    finally:
        held.close()


def test_the_lock_frees_when_the_first_build_ends(worktree):
    hold_lock("pid=4242 | compass build\n").close()
    compass._acquire_worktree_build_lock()
    assert compass._WORKTREE_BUILD_LOCK is not None


def test_a_second_call_in_the_same_process_keeps_its_lock(worktree):
    compass._acquire_worktree_build_lock()
    first = compass._WORKTREE_BUILD_LOCK
    compass._acquire_worktree_build_lock()
    assert compass._WORKTREE_BUILD_LOCK is first
