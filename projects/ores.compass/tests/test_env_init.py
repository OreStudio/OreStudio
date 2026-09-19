"""
Tests for env_init.py's value precedence.

Run with:  python -m pytest projects/ores.compass/tests/test_env_init.py -v
No database, systemd or network access required.
"""

import json
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


class TestWebEnvironmentBinding:
    """The ores.web BFF resolves its site configuration by id.

    It falls back to ORES_ENV_NAME, which is the checkout label and not
    necessarily a declared id, so .env must carry the id the BFF needs or the
    web service refuses to start."""

    @staticmethod
    def _declare(root: Path, environments: list) -> None:
        config_dir = root / "projects" / "ores.web" / "config"
        config_dir.mkdir(parents=True, exist_ok=True)
        (config_dir / "environments.json").write_text(
            json.dumps({"environments": environments}), encoding="utf-8")

    def test_the_local_instance_is_selected_when_the_label_is_not_an_id(
            self, tmp_path):
        self._declare(tmp_path, [
            {"id": "swift_curie_local", "host": "localhost", "port": 20205,
             "subjectPrefix": "ores.dev.swift_curie"},
            {"id": "swift_curie_newton", "host": "192.168.1.22", "port": 20205,
             "subjectPrefix": "ores.dev.swift_curie"},
        ])
        assert env_init._resolve_web_env_id(
            tmp_path, 20205, "ores.dev.swift_curie", "swift_curie"
        ) == "swift_curie_local"

    def test_the_label_wins_over_the_local_instance(self, tmp_path):
        self._declare(tmp_path, [
            {"id": "swift_curie", "host": "192.168.1.22", "port": 20205,
             "subjectPrefix": "ores.dev.swift_curie"},
            {"id": "swift_curie_local", "host": "localhost", "port": 20205,
             "subjectPrefix": "ores.dev.swift_curie"},
        ])
        assert env_init._resolve_web_env_id(
            tmp_path, 20205, "ores.dev.swift_curie", "swift_curie"
        ) == "swift_curie"

    def test_a_checkout_without_the_web_config_resolves_to_nothing(
            self, tmp_path):
        assert env_init._resolve_web_env_id(
            tmp_path, 20205, "ores.dev.swift_curie", "swift_curie") is None

    def test_an_ambiguous_match_resolves_to_nothing(self, tmp_path):
        self._declare(tmp_path, [
            {"id": "one", "host": "localhost", "port": 20205,
             "subjectPrefix": "ores.dev.swift_curie"},
            {"id": "two", "host": "localhost", "port": 20205,
             "subjectPrefix": "ores.dev.swift_curie"},
        ])
        assert env_init._resolve_web_env_id(
            tmp_path, 20205, "ores.dev.swift_curie", "swift_curie") is None
