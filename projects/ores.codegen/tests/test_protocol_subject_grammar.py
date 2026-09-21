"""Tests for the specification's subject grammar.

Run::

    python3 -m pytest projects/ores.codegen/tests/test_protocol_subject_grammar.py

The specification states that every subject has exactly four dot-separated
segments, that a request's last segment is one of a closed verb set, and that
an event's last is an action rather than a verb. These cases hold the builders
to that, because a grammar nothing checks is the one that drifts.
"""
import sys
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[3]
sys.path.insert(0, str(REPO_ROOT / "projects/ores.codegen/src"))

import pytest  # noqa: E402

from codegen.org_loader import (  # noqa: E402
    SPEC_EVENT_ACTIONS, SPEC_VERBS, event_subject, request_subject,
    versions_subject)


def test_every_request_verb_builds_a_four_segment_subject():
    for verb in SPEC_VERBS:
        subject = request_subject("iam", "tenants", verb)
        assert subject == f"iam.v1.tenants.{verb}"
        assert len(subject.split(".")) == 4, subject


def test_a_scoped_read_keeps_the_relation_in_the_verb_slot():
    subject = request_subject("iam", "tenants", "list_by_party_id")
    assert subject == "iam.v1.tenants.list_by_party_id"
    assert len(subject.split(".")) == 4, subject


def test_an_event_names_the_collection_and_an_action():
    subject = event_subject("iam", "tenants", "created")
    assert subject == "iam.v1.tenants_events.created"
    assert len(subject.split(".")) == 4, subject


def test_every_event_action_builds_a_four_segment_subject():
    for action in SPEC_EVENT_ACTIONS:
        subject = event_subject("iam", "tenants", action)
        assert len(subject.split(".")) == 4, subject


def test_versions_are_a_sub_resource_of_the_entity():
    assert versions_subject("iam", "tenants", "list") == \
        "iam.v1.tenants_versions.list"
    assert versions_subject("iam", "tenants", "get") == \
        "iam.v1.tenants_versions.get"


def test_the_verbs_it_replaced_are_refused():
    """``save`` and ``history`` were the old vocabulary, not additions to it."""
    for gone in ("save", "history", "hierarchy", "read", "count"):
        with pytest.raises(ValueError):
            request_subject("iam", "tenants", gone)


def test_an_action_outside_the_set_is_refused():
    with pytest.raises(ValueError):
        event_subject("iam", "tenants", "changed")


def test_a_versions_collection_refuses_a_write():
    for write in ("put", "put_many", "delete", "delete_many"):
        with pytest.raises(ValueError):
            versions_subject("iam", "tenants", write)


# --- the write record: user-owned fields, and nothing else -----------------

from codegen.org_loader import (  # noqa: E402
    CHANGE_INTENT_FIELDS, SERVER_OWNED_FIELDS, write_record_fields)


def _columns(*names):
    return [{"column": n, "cpp_type": "std::string"} for n in names]


def test_a_write_record_offers_no_server_owned_field():
    entity = _columns("code", "name", "tenant_id", "party_id", "version",
                      "modified_by", "performed_by", "recorded_at",
                      "valid_from", "valid_to")
    kept = [c["name"] for c in write_record_fields(entity)]
    assert kept == ["code", "name"]
    for field in SERVER_OWNED_FIELDS:
        assert field not in kept, field


def test_change_intent_travels_beside_the_record_not_inside_it():
    entity = _columns("code", "change_reason_code", "change_commentary")
    kept = [c["name"] for c in write_record_fields(entity)]
    assert kept == ["code"]
    for field in CHANGE_INTENT_FIELDS:
        assert field not in kept, field


def test_a_create_keeps_its_key_because_the_caller_supplies_it():
    assert [c["name"] for c in write_record_fields(_columns("id", "code"))] \
        == ["id", "code"]


def test_the_declared_order_is_preserved():
    entity = _columns("zeta", "alpha", "version", "mu")
    assert [c["name"] for c in write_record_fields(entity)] == \
        ["zeta", "alpha", "mu"]


# --- the key record: one typed field per identifying column ----------------

from codegen.org_loader import key_record_fields  # noqa: E402


def _entity(*pk_columns):
    return {"primary_key": {"columns": list(pk_columns)}}


def test_a_surrogate_key_carries_the_uuid_type_not_text():
    fields = key_record_fields(_entity({"column": "id", "is_uuid": True}))
    assert [f["name"] for f in fields] == ["id"]
    assert fields[0]["cpp_type"] == "boost::uuids::uuid"
    assert fields[0]["ts_type"] == "string"


def test_a_natural_text_key_carries_its_own_type():
    fields = key_record_fields(_entity({"column": "code", "is_uuid": False}))
    assert fields[0]["cpp_type"] == "std::string"


def test_a_composite_key_carries_every_identifying_column():
    fields = key_record_fields(_entity(
        {"column": "name", "is_uuid": False},
        {"column": "domain_name", "is_uuid": False}))
    assert [f["name"] for f in fields] == ["name", "domain_name"]


def test_a_key_is_never_a_vector_of_strings():
    fields = key_record_fields(_entity({"column": "id", "is_uuid": True}))
    assert not any("vector" in f["cpp_type"] for f in fields)


def test_an_entity_has_at_least_one_identifying_column():
    assert key_record_fields(_entity({"column": "id", "is_uuid": True}))
