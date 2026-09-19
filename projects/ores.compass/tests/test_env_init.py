"""
Tests for env_init.py's value precedence.

Run with:  python -m pytest projects/ores.compass/tests/test_env_init.py -v
No database, systemd or network access required.
"""

import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).parent.parent / "src"))

import env_init


class TestEnvValue:
    """The checkout's own .env is authoritative.

    An ORES_* exported into the ambient environment by another worktree's
    shell must not rewrite this checkout's identity: the database name and the
    NATS subject prefix decide which environment a command touches."""

    def test_the_checkout_env_wins_over_the_process_environment(
            self, monkeypatch):
        monkeypatch.setenv("ORES_DATABASE_NAME", "ores_dev_elsewhere")
        assert env_init._env_value(
            {"ORES_DATABASE_NAME": "ores_dev_here"},
            "ORES_DATABASE_NAME", "ores_dev_derived") == "ores_dev_here"

    def test_the_process_environment_is_the_fallback_for_a_fresh_checkout(
            self, monkeypatch):
        monkeypatch.setenv("ORES_DATABASE_NAME", "ores_dev_from_ci")
        assert env_init._env_value(
            {}, "ORES_DATABASE_NAME", "ores_dev_derived") == "ores_dev_from_ci"

    def test_the_derived_value_is_the_last_resort(self, monkeypatch):
        monkeypatch.delenv("ORES_DATABASE_NAME", raising=False)
        assert env_init._env_value(
            {}, "ORES_DATABASE_NAME", "ores_dev_derived") == "ores_dev_derived"

    def test_an_empty_existing_value_does_not_win(self, monkeypatch):
        monkeypatch.setenv("ORES_NATS_SUBJECT_PREFIX", "ores.dev.from_env")
        assert env_init._env_value(
            {"ORES_NATS_SUBJECT_PREFIX": ""},
            "ORES_NATS_SUBJECT_PREFIX", "ores.dev.derived") == "ores.dev.from_env"
