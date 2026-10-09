"""
Tests for nats_purge: stream selection, server start-up, and exit codes.

Run with:  python -m pytest projects/ores.compass/tests/test_nats_purge.py -v
No live NATS server and no systemd access required: the nats CLI, the
unit lookup and the port probe are all faked.
"""

import subprocess
import sys
from pathlib import Path

import pytest

# Allow importing from the src directory without installing the package.
sys.path.insert(0, str(Path(__file__).parent.parent / "src"))

import nats_purge


class FakeProc:
    """The CompletedProcess surface nats_purge reads."""

    def __init__(self, returncode=0, stdout="", stderr=""):
        self.returncode = returncode
        self.stdout = stdout
        self.stderr = stderr


@pytest.fixture
def project_root(tmp_path):
    (tmp_path / ".env").write_text(
        "ORES_CHECKOUT_LABEL=test_env\n"
        "ORES_NATS_URL=nats://localhost:20405\n"
        "ORES_NATS_PORT=20405\n"
        "ORES_NATS_SUBJECT_PREFIX=ores.dev.brave_hopper\n")
    return tmp_path


class TestPrefixDerivation:
    def test_dots_become_underscores(self):
        env = {"ORES_NATS_SUBJECT_PREFIX": "ores.dev.brave_hopper"}
        assert nats_purge._stream_prefix(env) == "ores_dev_brave_hopper"

    def test_a_single_segment_prefix_is_unchanged(self):
        assert nats_purge._stream_prefix(
            {"ORES_NATS_SUBJECT_PREFIX": "ores"}) == "ores"

    def test_stream_names_keep_only_the_prefixed_lines(self):
        text = ("ores_dev_brave_hopper_workflow\n"
                "elsewhere_workflow\n"
                "\n"
                "ores_dev_brave_hopper_compute_assignments\n")
        assert nats_purge._stream_names(text, "ores_dev_brave_hopper") == [
            "ores_dev_brave_hopper_workflow",
            "ores_dev_brave_hopper_compute_assignments",
        ]


class TestRunPurgesOnlyThisEnvironment:
    def test_only_prefixed_streams_are_purged(self, project_root, monkeypatch):
        listing = ("ores_dev_brave_hopper_workflow\n"
                   "another_env_workflow\n"
                   "ores_dev_brave_hopper_marketdata_ticks\n")
        monkeypatch.setattr(nats_purge, "_list_streams", lambda env: listing)
        monkeypatch.setattr(nats_purge, "_ensure_available",
                            lambda env, label: 0)

        purged = []

        def fake_run(argv, **kwargs):
            purged.append(argv)
            return FakeProc(returncode=0, stdout="{}")

        monkeypatch.setattr(subprocess, "run", fake_run)
        rc = nats_purge.run([], project_root)

        assert rc == 0
        purge_commands = [a for a in purged if "purge" in a]
        assert [a[a.index("purge") + 1] for a in purge_commands] == [
            "ores_dev_brave_hopper_workflow",
            "ores_dev_brave_hopper_marketdata_ticks",
        ]

    def test_no_matching_streams_is_success(self, project_root, monkeypatch):
        monkeypatch.setattr(nats_purge, "_list_streams",
                            lambda env: "another_env_workflow\n")
        monkeypatch.setattr(nats_purge, "_ensure_available",
                            lambda env, label: 0)
        monkeypatch.setattr(subprocess, "run",
                            lambda argv, **kwargs: FakeProc(returncode=0))
        assert nats_purge.run([], project_root) == 0

    def test_a_failed_purge_returns_non_zero(self, project_root, monkeypatch):
        monkeypatch.setattr(
            nats_purge, "_list_streams",
            lambda env: "ores_dev_brave_hopper_workflow\n")
        monkeypatch.setattr(nats_purge, "_ensure_available",
                            lambda env, label: 0)

        def fake_run(argv, **kwargs):
            if "purge" in argv:
                return FakeProc(returncode=1, stderr="purge refused")
            return FakeProc(returncode=0, stdout="{}")

        monkeypatch.setattr(subprocess, "run", fake_run)
        assert nats_purge.run([], project_root) == 1


class TestServerStartup:
    def test_the_server_is_started_when_the_first_listing_fails(
            self, project_root, monkeypatch):
        calls = []

        def fake_list(env):
            calls.append(1)
            if len(calls) == 1:
                return None
            return "ores_dev_brave_hopper_workflow\n"

        started = []
        monkeypatch.setattr(nats_purge, "_list_streams", fake_list)
        monkeypatch.setattr(nats_purge, "_start_server",
                            lambda label, port: started.append(label) or 0)
        monkeypatch.setattr(nats_purge, "_sleep", lambda seconds: None)
        monkeypatch.setattr(subprocess, "run",
                            lambda argv, **kwargs: FakeProc(returncode=0,
                                                            stdout="{}"))

        assert nats_purge.run([], project_root) == 0
        assert started == ["test_env"]
        assert len(calls) == 3

    def test_the_server_is_not_started_when_the_listing_succeeds(
            self, project_root, monkeypatch):
        monkeypatch.setattr(
            nats_purge, "_list_streams",
            lambda env: "ores_dev_brave_hopper_workflow\n")
        started = []
        monkeypatch.setattr(nats_purge, "_start_server",
                            lambda label, port: started.append(label) or 0)
        monkeypatch.setattr(subprocess, "run",
                            lambda argv, **kwargs: FakeProc(returncode=0,
                                                            stdout="{}"))

        assert nats_purge.run([], project_root) == 0
        assert started == []

    def test_a_server_that_stays_down_fails(self, project_root, monkeypatch):
        monkeypatch.setattr(nats_purge, "_list_streams", lambda env: None)
        monkeypatch.setattr(nats_purge, "_start_server",
                            lambda label, port: 1)
        assert nats_purge.run([], project_root) == 1
