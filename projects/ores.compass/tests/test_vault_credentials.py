"""Tests for the vault's key provisioning: five raw key files, generated once.

Run::

    python3 -m pytest projects/ores.compass/tests/test_vault_credentials.py
"""
import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parents[1] / "src"))

import vault_credentials  # noqa: E402

KEY_LEN = 32


def test_it_writes_the_five_keys(tmp_path):
    keys = vault_credentials.ensure(tmp_path, announce=lambda _: None)

    assert set(keys) == {"kek", "transport", "transport_public", "gateway", "gateway_public"}
    for path in keys.values():
        assert path.is_file(), path
        assert len(path.read_bytes()) == KEY_LEN, path


def test_a_private_key_is_readable_by_its_owner_alone(tmp_path):
    keys = vault_credentials.ensure(tmp_path, announce=lambda _: None)

    for name in ("kek", "transport", "gateway"):
        assert keys[name].stat().st_mode & 0o777 == 0o600, name
    for name in ("transport_public", "gateway_public"):
        assert keys[name].stat().st_mode & 0o777 == 0o644, name


def test_a_second_run_leaves_every_key_as_it_was(tmp_path):
    first = vault_credentials.ensure(tmp_path, announce=lambda _: None)
    before = {name: path.read_bytes() for name, path in first.items()}

    second = vault_credentials.ensure(tmp_path, announce=lambda _: None)

    for name, path in second.items():
        assert path.read_bytes() == before[name], name


def test_the_public_half_is_not_the_private_half(tmp_path):
    keys = vault_credentials.ensure(tmp_path, announce=lambda _: None)

    for private, public in (("transport", "transport_public"), ("gateway", "gateway_public")):
        assert keys[private].read_bytes() != keys[public].read_bytes(), private
