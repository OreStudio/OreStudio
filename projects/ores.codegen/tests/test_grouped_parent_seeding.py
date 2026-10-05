"""Tests for the member prefixes the eventing seed plan carries.

The generated eventing integration test writes a child row whose mandatory
soft FK references a parent, and seeds that parent first. A parent reached
through field groups -- an instrument, whose key lives in its identity group
and whose audit columns live in its audit group -- must be written and patched
member by member, or the template emits ``book_id`` for ``parties.book_id``
and the file does not compile.

Run::

    python3 -m pytest projects/ores.codegen/tests/test_grouped_parent_seeding.py
"""
import sys
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[3]
sys.path.insert(0, str(REPO_ROOT / "projects/ores.codegen/src"))

from codegen.core import (  # noqa: E402
    _column_member_prefixes,
    _plan_required_seeds,
)

TEMPLATE = (REPO_ROOT / "projects/ores.codegen/library/templates"
            / "cpp_nats_integration_test.cpp.mustache")


def test_a_flat_entity_takes_no_prefix():
    """A reference-data entity with no groups stays flat."""
    de = {
        "entity_singular": "thing",
        "primary_key": {"columns": [{"name": "id", "type": "uuid"}]},
        "columns": [{"name": "code", "type": "text"}],
    }
    assert _column_member_prefixes(de) == {}


def test_identity_and_audit_groups_prefix_their_own_columns():
    """The two group flags cover columns the generator emits itself."""
    de = {
        "entity_singular": "thing",
        "domain_identity_group": "ores.thing.thing_identity",
        "domain_audit_group": "ores.dq.audit_record",
        "primary_key": {"columns": [{"name": "id", "type": "uuid",
                                     "group": "identity"}]},
        "columns": [{"name": "code", "type": "text"}],
    }
    prefixes = _column_member_prefixes(de)
    assert prefixes["id"] == "identity."
    assert prefixes["change_reason_code"] == "audit."
    assert prefixes["recorded_at"] == "audit."
    assert "code" not in prefixes


def test_the_seed_plan_carries_both_sides_of_a_patch(monkeypatch):
    """An ancestor and the row that references it read their own members."""
    workunit_prefixes = {"id": "identity.", "app_version_id": ""}
    org_infos = {
        "ores_compute_workunits_tbl": {
            "entity_singular": "workunit",
            "generator_facet_name": "generators",
            "has_audit_group": False,
            "component": "compute",
            "column_prefixes": workunit_prefixes,
            "mandatory_fks": [],
        },
        "ores_compute_app_versions_tbl": {
            "entity_singular": "app_version",
            "generator_facet_name": "generators",
            "has_audit_group": True,
            "component": "compute",
            "column_prefixes": {"id": "identity.",
                                "change_reason_code": "audit."},
            "mandatory_fks": [],
        },
    }
    org_by_table = {tbl: {"org": Path(f"/fake/{tbl}.org")} for tbl in org_infos}
    monkeypatch.setattr(
        "codegen.core._parent_entity_info",
        lambda org_path: None if org_path is None
        else org_infos.get(org_path.name.removesuffix(".org")))
    mfks = [{"column": "app_version_id",
             "table": "ores_compute_app_versions_tbl",
             "target_column": "id"}]

    items = _plan_required_seeds(mfks, "result", org_by_table, "compute", set(),
                                 workunit_prefixes)

    assert len(items) == 1
    item = items[0]
    assert item["owner_group_prefix"] == ""
    assert item["target_group_prefix"] == "identity."
    assert item["audit_prefix"] == "audit."
    assert item["party_prefix"] == ""


def test_the_integration_template_writes_every_parent_member_through_a_prefix():
    """The template must not assume a flat parent."""
    text = TEMPLATE.read_text()

    assert "{{column}}_parent.{{{parent_audit_prefix}}}change_reason_code" in text
    assert "{{column}}_parent.{{{parent_party_prefix}}}party_id" in text
    assert "{{column}}_parent.{{{parent_target_prefix}}}{{target_column}}" in text
    assert "{{var}}.{{{audit_prefix}}}change_reason_code" in text
    assert "{{{owner_group_prefix}}}{{column}}" in text
    assert "{{{target_group_prefix}}}{{target_column}}" in text
    # The old hardcoded guess: a parent that nests its columns anywhere other
    # than the identity group was written flat and did not compile.
    assert "{{#parent_has_identity_group}}identity.{{/parent_has_identity_group}}" not in text
