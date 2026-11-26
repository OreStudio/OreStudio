"""Tests for the vault's key provisioning: five raw key files, generated once.

Run::

    python3 -m pytest projects/ores.compass/tests/test_vault_credentials.py
"""
import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parents[1] / "src"))

import vault_credentials  # noqa: E402

KEY_LEN = 32


def test_it_writes_every_key(tmp_path):
    keys = vault_credentials.ensure(tmp_path, announce=lambda _: None)

    assert set(keys) == {
        "kek.1",
        "kek_version",
        "transport",
        "transport_public",
        "gateway",
        "gateway_public",
    }
    for name, path in keys.items():
        assert path.is_file(), path
        expected = 2 if name == "kek_version" else KEY_LEN
        assert len(path.read_bytes()) == expected, name


def test_a_private_key_is_readable_by_its_owner_alone(tmp_path):
    keys = vault_credentials.ensure(tmp_path, announce=lambda _: None)

    for name in ("kek.1", "transport", "gateway"):
        assert keys[name].stat().st_mode & 0o777 == 0o600, name
    for name in ("transport_public", "gateway_public"):
        assert keys[name].stat().st_mode & 0o777 == 0o644, name


def test_a_second_run_leaves_every_key_as_it_was(tmp_path):
    first = vault_credentials.ensure(tmp_path, announce=lambda _: None)
    before = {name: path.read_bytes() for name, path in first.items()}

    second = vault_credentials.ensure(tmp_path, announce=lambda _: None)

    for name, path in second.items():
        assert path.read_bytes() == before[name], name


def test_it_states_the_version_a_write_uses(tmp_path):
    keys = vault_credentials.ensure(tmp_path, announce=lambda _: None)

    assert keys["kek_version"].read_text().strip() == "1"
    assert vault_credentials.current_version(tmp_path / "build" / "keys") == 1


def test_a_second_version_is_read_beside_the_first(tmp_path):
    vault_credentials.ensure(tmp_path, announce=lambda _: None)
    second = tmp_path / "build" / "keys" / "vault.kek.2"
    second.write_bytes(b"x" * 32)

    keys = vault_credentials.kek_keys(tmp_path / "build" / "keys")

    assert sorted(keys) == ["kek.1", "kek.2"]


def test_the_public_half_is_not_the_private_half(tmp_path):
    keys = vault_credentials.ensure(tmp_path, announce=lambda _: None)

    for private, public in (("transport", "transport_public"), ("gateway", "gateway_public")):
        assert keys[private].read_bytes() != keys[public].read_bytes(), private
