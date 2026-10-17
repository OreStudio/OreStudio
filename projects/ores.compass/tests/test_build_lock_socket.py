"""
Tests for the host-wide build lock, which is an abstract AF_UNIX socket.

Run with:  python -m pytest projects/ores.compass/tests/test_build_lock_socket.py -v

These tests exercise contention inside one network namespace. They cannot
prove contention across two bubblewrap sandboxes -- only a second
environment can demonstrate that -- but they do prove the mechanism needs
no filesystem and needs no writable /tmp.
"""

import builtins
import errno
import os
import sys
import threading
from pathlib import Path

import pytest

sys.path.insert(0, str(Path(__file__).parent.parent / "src"))

import compass  # noqa: E402


@pytest.fixture
def one_slot(monkeypatch):
    """A single-slot lock with the poll interval and side effects tamed."""
    monkeypatch.setattr(compass, "BUILD_LOCK_SLOTS", (("a", 3),))
    monkeypatch.setattr(compass, "BUILD_LOCK_POLL_INTERVAL", 0.05)
    monkeypatch.setattr(compass, "_acquire_worktree_build_lock", lambda: None)
    monkeypatch.setattr(compass, "_read_env_map",
                        lambda: {"ORES_ENV_NAME": "test-env"})
    monkeypatch.setattr(compass, "PROJECT_ROOT", Path("/nonexistent-worktree"))
    yield


def test_a_held_slot_makes_the_second_acquisition_poll_until_release(one_slot):
    first, slot_name, jobs = compass._acquire_build_lock()
    assert (slot_name, jobs) == ("a", 3)
    acquired = threading.Event()
    result = {}

    def contend():
        lock, name, _ = compass._acquire_build_lock()
        result["lock"] = lock
        result["name"] = name
        acquired.set()

    thread = threading.Thread(target=contend, daemon=True)
    thread.start()
    try:
        assert not acquired.wait(0.5), "second acquisition took a held slot"
        first.close()
        assert acquired.wait(5.0), "second acquisition never got the freed slot"
        assert result["name"] == "a"
    finally:
        first.close()
        if "lock" in result:
            result["lock"].close()
        thread.join(timeout=5.0)


def test_releasing_an_acquired_slot_releases_it(one_slot):
    first, _, _ = compass._acquire_build_lock()
    first.close()
    second, slot_name, _ = compass._acquire_build_lock()
    try:
        assert slot_name == "a"
    finally:
        second.close()


def test_two_different_slots_can_be_held_at_once(monkeypatch):
    monkeypatch.setattr(compass, "BUILD_LOCK_SLOTS", (("a", 3), ("b", 3)))
    monkeypatch.setattr(compass, "BUILD_LOCK_POLL_INTERVAL", 0.05)
    monkeypatch.setattr(compass, "_acquire_worktree_build_lock", lambda: None)
    monkeypatch.setattr(compass, "_read_env_map",
                        lambda: {"ORES_ENV_NAME": "test-env"})
    monkeypatch.setattr(compass, "PROJECT_ROOT", Path("/nonexistent-worktree"))
    first, first_name, _ = compass._acquire_build_lock()
    second, second_name, _ = compass._acquire_build_lock()
    try:
        assert {first_name, second_name} == {"a", "b"}
        assert compass._build_lock_slot_free("a") is False
        assert compass._build_lock_slot_free("b") is False
    finally:
        first.close()
        second.close()


def test_the_holder_answers_its_identity_and_stops_after_release(one_slot):
    lock, slot_name, jobs = compass._acquire_build_lock()
    try:
        holder = compass.build_lock_holder(slot_name)
        assert holder is not None
        assert f"pid={os.getpid()}" in holder
        assert "jobs=3" in holder
        assert compass._build_lock_slot_free(slot_name) is False
    finally:
        lock.close()
    assert compass.build_lock_holder(slot_name) is None
    assert compass._build_lock_slot_free(slot_name) is True


def test_the_mechanism_touches_no_filesystem_path(one_slot, monkeypatch):
    opened = []
    real_open = builtins.open
    real_os_open = os.open

    def spy_open(file, *args, **kwargs):
        opened.append(str(file))
        return real_open(file, *args, **kwargs)

    def spy_os_open(path, *args, **kwargs):
        opened.append(str(path))
        return real_os_open(path, *args, **kwargs)

    monkeypatch.setattr(builtins, "open", spy_open)
    monkeypatch.setattr(os, "open", spy_os_open)

    lock, slot_name, _ = compass._acquire_build_lock()
    try:
        assert compass.build_lock_holder(slot_name) is not None
        assert compass._build_lock_slot_free(slot_name) is False
        status = compass.build_lock_status()
        assert status == [(slot_name, 3, False, compass.build_lock_holder(slot_name))]
    finally:
        lock.close()
    assert compass._build_lock_slot_free(slot_name) is True
    assert opened == [], f"build lock opened filesystem paths: {opened}"


class _UnbindableSocket:
    """A socket whose bind always fails, to stand in for an unusable host."""

    def bind(self, name):
        raise OSError(errno.EACCES, "Permission denied")

    def close(self):
        pass


def test_an_unusable_socket_refuses_to_build_and_says_why(
        one_slot, monkeypatch, capsys):
    monkeypatch.setattr(compass.socket, "socket",
                        lambda *a, **k: _UnbindableSocket())
    with pytest.raises(SystemExit) as exit_info:
        compass._acquire_build_lock()
    assert exit_info.value.code == 1
    err = capsys.readouterr().err
    assert "Cannot acquire a build lock" in err
    assert "Refusing to build without the lock" in err
