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
ENV = {
    "ORES_IAM_SERVICE_DB_PASSWORD": "the-iam-password",
    "ORES_REFDATA_SERVICE_DB_PASSWORD": "the-refdata-password",
    "ORES_DQ_SERVICE_DB_PASSWORD": "the-dq-password",
    "ORES_VAULT_SERVICE_DB_PASSWORD": "the-vault-password",
}


def build(services, tmp_path, env=ENV):
    return vault_provision.build_document(
        services, env, tmp_path, announce=lambda _: None)


def test_one_consumer_and_one_secret_per_resolving_service(tmp_path):
    document, skipped = build([REFDATA, DQ], tmp_path)

    assert skipped == []
    assert document["consumers"] == [
        {"name": "refdata_service@system.ores",
         "actor": "refdata_service@system.ores",
         "public_key": document["consumers"][0]["public_key"]},
        {"name": "dq_service@system.ores",
         "actor": "dq_service@system.ores",
         "public_key": document["consumers"][1]["public_key"]},
    ]
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


def test_the_value_is_the_one_the_environment_file_holds(tmp_path):
    document, _ = build([REFDATA, DQ], tmp_path)

    assert [s["value"] for s in document["secrets"]] == [
        "the-refdata-password",
        "the-dq-password",
    ]


def test_a_service_the_file_omits_is_skipped_and_named(tmp_path):
    document, skipped = build(
        [REFDATA, DQ], tmp_path,
        env={"ORES_REFDATA_SERVICE_DB_PASSWORD": "the-refdata-password"})

    assert skipped == ["ores.dq.service"]
    assert [s["name"] for s in document["secrets"]] == ["db.refdata_service.password"]
    # A consumer without the value it owns would be a key nothing answers for.
    assert [c["name"] for c in document["consumers"]] == ["refdata_service@system.ores"]


def test_the_two_that_cannot_ask_are_given_a_credential_instead(tmp_path):
    document, skipped = build([IAM, REFDATA, VAULT], tmp_path)

    assert skipped == []
    assert [c["name"] for c in document["consumers"]] == ["refdata_service@system.ores"]
    assert [s["name"] for s in document["secrets"]] == ["db.refdata_service.password"]
    assert (tmp_path / "iam_service.db-password").read_text() == "the-iam-password"
    assert (tmp_path / "vault_service.db-password").read_text() == "the-vault-password"
    assert (tmp_path / "iam_service.db-password").stat().st_mode & 0o777 == 0o600
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
