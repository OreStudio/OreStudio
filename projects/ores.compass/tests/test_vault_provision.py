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
VAULT = {
    "name": "ores.vault.service",
    "psql_var": "vault_service",
    "env_key": "VAULT_SERVICE",
    "email": "vault_service@system.ores",
}
ENV = {
    "ORES_IAM_SERVICE_DB_PASSWORD": "the-iam-password",
    "ORES_REFDATA_SERVICE_DB_PASSWORD": "the-refdata-password",
    "ORES_VAULT_SERVICE_DB_PASSWORD": "the-vault-password",
}


def build(services, env=ENV, tmp_path=None):
    return vault_provision.build_document(
        services, env, tmp_path, announce=lambda _: None)


def test_one_consumer_and_one_secret_per_database_service(tmp_path):
    document, skipped = build([IAM, REFDATA], tmp_path=tmp_path)

    assert skipped == []
    assert document["consumers"] == [
        {"name": "iam_service@system.ores",
         "actor": "iam_service@system.ores",
         "public_key": document["consumers"][0]["public_key"]},
        {"name": "refdata_service@system.ores",
         "actor": "refdata_service@system.ores",
         "public_key": document["consumers"][1]["public_key"]},
    ]
    assert document["secrets"] == [
        {"name": "db.iam_service.password",
         "owner": "iam_service@system.ores",
         "value": "the-iam-password"},
        {"name": "db.refdata_service.password",
         "owner": "refdata_service@system.ores",
         "value": "the-refdata-password"},
    ]


def test_each_consumer_carries_a_public_key_of_the_length_the_box_takes(tmp_path):
    document, _ = build([IAM], tmp_path=tmp_path)

    raw = base64.b64decode(document["consumers"][0]["public_key"])

    assert len(raw) == KEY_LEN


def test_the_vault_is_never_provisioned_into_itself(tmp_path):
    document, skipped = build([IAM, VAULT], tmp_path=tmp_path)

    assert skipped == []
    assert [c["name"] for c in document["consumers"]] == ["iam_service@system.ores"]
    assert [s["name"] for s in document["secrets"]] == ["db.iam_service.password"]
    assert not (tmp_path / "vault_service.vault.key").exists()


def test_a_service_the_environment_file_omits_is_skipped_and_named(tmp_path):
    document, skipped = build(
        [IAM, REFDATA],
        env={"ORES_IAM_SERVICE_DB_PASSWORD": "the-iam-password"},
        tmp_path=tmp_path)

    assert skipped == ["ores.refdata.service"]
    assert [s["name"] for s in document["secrets"]] == ["db.iam_service.password"]
    # A consumer without the value it owns would be a key nothing answers for.
    assert [c["name"] for c in document["consumers"]] == ["iam_service@system.ores"]


def test_a_key_pair_is_written_once_and_identifies_the_consumer(tmp_path):
    private, public = vault_provision.consumer_key_paths(tmp_path, "iam_service")

    first, _ = build([IAM], tmp_path=tmp_path)
    before = private.read_bytes()

    second, _ = build([IAM], tmp_path=tmp_path)

    assert private.read_bytes() == before
    assert first["consumers"] == second["consumers"]
    assert private.stat().st_mode & 0o777 == 0o600
    assert public.stat().st_mode & 0o777 == 0o644
    assert len(private.read_bytes()) == KEY_LEN
