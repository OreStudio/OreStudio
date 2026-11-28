"""Tests for the vault's provisioning document: what the deployment states.

Run::

    python3 -m pytest projects/ores.compass/tests/test_vault_provision.py
"""
import base64
import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parents[1] / "src"))

import vault_provision  # noqa: E402

KEY_LEN = 32

IAM = {
    "name": "ores.iam.service",
    "psql_var": "iam_service",
    "env_key": "IAM_SERVICE",
    "email": "iam_service@system.ores",
}
REFDATA = {
    "name": "ores.refdata.service",
    "psql_var": "refdata_service",
    "env_key": "REFDATA_SERVICE",
    "email": "refdata_service@system.ores",
}
DQ = {
    "name": "ores.dq.service",
    "psql_var": "dq_service",
    "env_key": "DQ_SERVICE",
    "email": "dq_service@system.ores",
}
VAULT = {
    "name": "ores.vault.service",
    "psql_var": "vault_service",
    "env_key": "VAULT_SERVICE",
    "email": "vault_service@system.ores",
}
def build(services, tmp_path):
    return vault_provision.build_document(services, tmp_path, announce=lambda _: None)


def test_one_consumer_and_one_secret_per_resolving_service(tmp_path):
    document, roles = build([REFDATA, DQ], tmp_path)

    _, refdata_public = vault_provision.consumer_key_paths(tmp_path, "refdata_service")
    assert document["consumers"][0] == {
        "name": "refdata_service@system.ores",
        "actor": "refdata_service@system.ores",
        "public_key": base64.b64encode(refdata_public.read_bytes()).decode("ascii"),
    }
    assert [c["name"] for c in document["consumers"]] == [
        "refdata_service@system.ores",
        "dq_service@system.ores",
    ]
    assert [r["psql_var"] for r in roles] == ["refdata_service", "dq_service"]
    assert [s["name"] for s in document["secrets"]] == [
        "db.refdata_service.password",
        "db.dq_service.password",
    ]
    assert [s["owner"] for s in document["secrets"]] == [
        "refdata_service@system.ores",
        "dq_service@system.ores",
    ]


def test_each_consumer_carries_a_public_key_of_the_length_the_box_takes(tmp_path):
    document, _ = build([REFDATA], tmp_path)

    raw = base64.b64decode(document["consumers"][0]["public_key"])

    assert len(raw) == KEY_LEN


def test_each_role_is_given_the_value_its_secret_holds(tmp_path):
    document, roles = build([REFDATA, DQ], tmp_path)

    assert [r["value"] for r in roles] == [s["value"] for s in document["secrets"]]
    assert [r["user"] for r in roles] == [
        "{ORES_REFDATA_SERVICE_DB_USER}",
        "{ORES_DQ_SERVICE_DB_USER}",
    ]
    assert all(len(r["value"]) >= 32 for r in roles)


def test_a_run_invents_a_new_password_every_time(tmp_path):
    first, _ = build([REFDATA], tmp_path)
    second, _ = build([REFDATA], tmp_path)

    assert first["secrets"][0]["value"] != second["secrets"][0]["value"]


def test_the_two_that_cannot_ask_are_roles_but_not_secrets(tmp_path):
    document, roles = build([IAM, REFDATA, VAULT], tmp_path)

    assert [c["name"] for c in document["consumers"]] == ["refdata_service@system.ores"]
    assert [s["name"] for s in document["secrets"]] == ["db.refdata_service.password"]
    # Their value is written where the unit reads it, by the caller, and never
    # becomes a vault entry they could not ask for.
    assert [r["psql_var"] for r in roles] == [
        "iam_service", "refdata_service", "vault_service"]
    assert not (tmp_path / "iam_service.vault.key").exists()
    assert not (tmp_path / "vault_service.vault.key").exists()


def test_a_key_pair_is_written_once_and_identifies_the_consumer(tmp_path):
    private, public = vault_provision.consumer_key_paths(tmp_path, "refdata_service")

    first, _ = build([REFDATA], tmp_path)
    before = private.read_bytes()

    second, _ = build([REFDATA], tmp_path)

    assert private.read_bytes() == before
    assert first["consumers"] == second["consumers"]
    assert private.stat().st_mode & 0o777 == 0o600
    assert public.stat().st_mode & 0o777 == 0o644
    assert len(private.read_bytes()) == KEY_LEN
