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


class TestBusctlSetting:
    """The transport defaults to on, and an explicit flag always wins.

    compass drives the fleet with systemctl --user, which the user manager
    refuses from inside the DSH sandbox, so busctl is the transport that
    works there and the safe default. A host where plain systemctl reaches
    the manager turns it off with --no-use-busctl."""

    def test_a_fresh_checkout_defaults_to_on(self, monkeypatch):
        monkeypatch.delenv("ORES_USE_BUSCTL", raising=False)
        assert env_init._busctl_setting({}, None) == "1"

    def test_an_explicit_flag_turns_it_off_over_an_existing_on(
            self, monkeypatch):
        monkeypatch.delenv("ORES_USE_BUSCTL", raising=False)
        assert env_init._busctl_setting({"ORES_USE_BUSCTL": "1"}, False) == "0"

    def test_an_explicit_flag_turns_it_on_over_an_existing_off(
            self, monkeypatch):
        monkeypatch.delenv("ORES_USE_BUSCTL", raising=False)
        assert env_init._busctl_setting({"ORES_USE_BUSCTL": "0"}, True) == "1"

    def test_no_flag_keeps_the_checkout_choice(self, monkeypatch):
        monkeypatch.setenv("ORES_USE_BUSCTL", "1")
        assert env_init._busctl_setting({"ORES_USE_BUSCTL": "0"}, None) == "0"

    def test_the_process_environment_seeds_a_fresh_checkout(self, monkeypatch):
        monkeypatch.setenv("ORES_USE_BUSCTL", "0")
        assert env_init._busctl_setting({}, None) == "0"


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

    def test_a_provisioned_hyphenated_label_matches_its_underscored_id(
            self, tmp_path):
        # compass env provision requires a hyphen in the name and writes it
        # verbatim to ORES_ENV_NAME, while every declared id uses underscores.
        self._declare(tmp_path, [
            {"id": "festive_hawking", "host": "192.168.1.30", "port": 21805,
             "subjectPrefix": "ores.dev.festive.hawking"},
            {"id": "festive_hawking_local", "host": "localhost", "port": 21805,
             "subjectPrefix": "ores.dev.festive.hawking"},
        ])
        assert env_init._resolve_web_env_id(
            tmp_path, 21805, "ores.dev.festive.hawking", "festive-hawking"
        ) == "festive_hawking"

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
