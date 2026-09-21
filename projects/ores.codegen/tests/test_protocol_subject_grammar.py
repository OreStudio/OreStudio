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
