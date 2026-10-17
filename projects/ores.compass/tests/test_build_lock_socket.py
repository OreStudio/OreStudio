"""
Tests for the host-wide build lock, which is an abstract AF_UNIX socket.

Run with:  python -m pytest projects/ores.compass/tests/test_build_lock_socket.py -v

These tests exercise contention inside one network namespace. They cannot
prove contention across two bubblewrap sandboxes -- only a second
environment can demonstrate that -- but they do prove the mechanism needs
no filesystem and no writable /tmp. Every run uses uniquely named slots, so
a test never collides with a live build holding 'a' or 'b'.
"""

import errno
import os
import sys
import threading
import uuid
from pathlib import Path

import pytest

sys.path.insert(0, str(Path(__file__).parent.parent / "src"))

import compass  # noqa: E402


@pytest.fixture
def lock_slots(monkeypatch, tmp_path):
    """Configure N uniquely named slots and tame the lock's side effects."""
    def configure(count):
        names = [f"test-{uuid.uuid4().hex}" for _ in range(count)]
        monkeypatch.setattr(compass, "BUILD_LOCK_SLOTS",
                            tuple((name, 3) for name in names))
        monkeypatch.setattr(compass, "BUILD_LOCK_POLL_INTERVAL", 0.05)
        monkeypatch.setattr(compass, "_acquire_worktree_build_lock",
                            lambda: None)
        monkeypatch.setattr(compass, "_read_env_map",
                            lambda: {"ORES_ENV_NAME": "test-env"})
        monkeypatch.setattr(compass, "PROJECT_ROOT", tmp_path)
        monkeypatch.delenv(compass._HOST_NETNS_ENV, raising=False)
        return names

    return configure


@pytest.fixture
def one_slot(lock_slots):
    return lock_slots(1)[0]


def test_a_held_slot_makes_the_second_acquisition_poll_until_release(one_slot):
    first, slot_name, jobs = compass._acquire_build_lock()
    assert (slot_name, jobs) == (one_slot, 3)
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
        assert result["name"] == one_slot
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
        assert slot_name == one_slot
    finally:
        second.close()


def test_two_different_slots_can_be_held_at_once(lock_slots):
    names = lock_slots(2)
    first, first_name, _ = compass._acquire_build_lock()
    second, second_name, _ = compass._acquire_build_lock()
    try:
        assert {first_name, second_name} == set(names)
        assert compass._build_lock_slot_free(first_name) is False
        assert compass._build_lock_slot_free(second_name) is False
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
        assert "netns=" in holder
        assert compass._build_lock_slot_free(slot_name) is False
    finally:
        lock.close()
    assert compass.build_lock_holder(slot_name) is None
    assert compass._build_lock_slot_free(slot_name) is True


def test_announce_names_the_slot_and_socket(one_slot, capsys):
    lock, slot_name, _ = compass._acquire_build_lock()
    try:
        lock.announce()
    finally:
        lock.close()
    out = capsys.readouterr().out
    assert f"slot '{slot_name}'" in out
    assert f"{compass._BUILD_LOCK_SOCKET_PREFIX}{slot_name}" in out
    assert "shared namespace unverified" in out


def test_status_reports_the_last_holder_of_a_free_slot(one_slot):
    lock, slot_name, _ = compass._acquire_build_lock()
    lock.announce()
    lock.close()
    _, _, free, holder = compass.build_lock_status()[0]
    assert free is True
    assert f"pid={os.getpid()}" in holder
    assert "netns=" in holder


def test_status_says_no_build_has_taken_an_unused_slot(one_slot):
    assert compass.build_lock_status() == [(one_slot, 3, True, None)]


def test_status_command_prints_the_last_holder(one_slot, capsys):
    lock, _, _ = compass._acquire_build_lock()
    lock.announce()
    lock.close()
    capsys.readouterr()
    assert compass.cmd_build(["--status"]) == 0
    out = capsys.readouterr().out
    assert "free. Last holder:" in out
    assert f"pid={os.getpid()}" in out


def test_status_command_says_no_build_has_taken_the_slot(one_slot, capsys):
    assert compass.cmd_build(["--status"]) == 0
    assert "no build has taken it yet" in capsys.readouterr().out


def test_the_mechanism_touches_no_filesystem_path(one_slot, monkeypatch):
    opened = []
    real_open = open
    real_os_open = os.open

    def spy_open(file, *args, **kwargs):
        opened.append(str(file))
        return real_open(file, *args, **kwargs)

    def spy_os_open(path, *args, **kwargs):
        opened.append(str(path))
        return real_os_open(path, *args, **kwargs)

    monkeypatch.setattr("builtins.open", spy_open)
    monkeypatch.setattr(os, "open", spy_os_open)

    lock, slot_name, _ = compass._acquire_build_lock()
    try:
        holder = compass.build_lock_holder(slot_name)
        assert holder is not None
        assert compass._build_lock_slot_free(slot_name) is False
        assert compass.build_lock_status() == [(slot_name, 3, False, holder)]
    finally:
        lock.close()
    assert compass._build_lock_slot_free(slot_name) is True
    assert opened == [], f"build lock opened filesystem paths: {opened}"


def test_a_published_host_network_namespace_that_matches_is_accepted(
        one_slot, monkeypatch, capsys):
    here = compass._network_namespace_id()
    assert here is not None, "this platform has no readable /proc/self/ns/net"
    monkeypatch.setenv(compass._HOST_NETNS_ENV, here)
    lock, name, _ = compass._acquire_build_lock()
    try:
        assert name == one_slot
        lock.announce()
    finally:
        lock.close()
    assert "shared namespace asserted" in capsys.readouterr().out


def test_a_published_host_network_namespace_that_differs_is_refused(
        one_slot, monkeypatch, capsys):
    here = compass._network_namespace_id()
    assert here is not None, "this platform has no readable /proc/self/ns/net"
    monkeypatch.setenv(compass._HOST_NETNS_ENV, str(int(here) + 1))
    with pytest.raises(SystemExit) as exit_info:
        compass._acquire_build_lock()
    assert exit_info.value.code == 1
    err = capsys.readouterr().err
    assert "network namespace" in err
    assert "share" in err


def test_without_a_published_reference_the_assertion_is_inactive(monkeypatch):
    monkeypatch.delenv(compass._HOST_NETNS_ENV, raising=False)
    assert compass._shared_network_namespace_failure() is None


class _UnbindableSocket:
    """A socket whose bind always fails, to stand in for an unusable host."""

    def bind(self, name):
        raise OSError(errno.EACCES, "Permission denied")

    def close(self):
        pass


class _AbstractNamespaceLessSocket:
    """An AF_UNIX socket with no abstract namespace, as on macOS or Windows."""

    def bind(self, name):
        raise OSError(errno.EINVAL, "Invalid argument")

    def close(self):
        pass


class _NonExclusiveSocket:
    """A socket whose binds never collide, so the name cannot hold a lock."""

    def bind(self, name):
        pass

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
    assert "Cannot use the build lock" in err
    assert "abstract AF_UNIX" in err
    assert "Refusing to continue without the lock" in err


def test_a_platform_without_the_abstract_namespace_is_refused_clearly(
        one_slot, monkeypatch, capsys):
    monkeypatch.setattr(compass.socket, "socket",
                        lambda *a, **k: _AbstractNamespaceLessSocket())
    with pytest.raises(SystemExit) as exit_info:
        compass._acquire_build_lock()
    assert exit_info.value.code == 1
    err = capsys.readouterr().err
    assert "the abstract AF_UNIX namespace is unavailable" in err
    assert "Invalid argument" in err


def test_a_namespace_that_does_not_keep_names_exclusive_is_refused(
        one_slot, monkeypatch, capsys):
    monkeypatch.setattr(compass.socket, "socket",
                        lambda *a, **k: _NonExclusiveSocket())
    with pytest.raises(SystemExit) as exit_info:
        compass._acquire_build_lock()
    assert exit_info.value.code == 1
    err = capsys.readouterr().err
    assert "the abstract AF_UNIX namespace is unavailable" in err
    assert "not exclusive" in err
