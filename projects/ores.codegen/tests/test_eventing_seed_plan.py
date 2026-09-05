"""Tests for _plan_required_seeds, the eventing-test FK seed closure.

The generated eventing integration test writes a child row whose
mandatory soft FK references another entity; each written row must have
its own mandatory ancestors seeded first or its insert trigger rejects
the synthetic key. The closure walks the FK chain transitively (not one
level): result -> workunit -> app_version -> app seeds the app before
the app_version row that references it.

Run::

    python3 -m pytest projects/ores.codegen/tests/test_eventing_seed_plan.py
"""
import sys
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[3]
sys.path.insert(0, str(REPO_ROOT / "projects/ores.codegen/src"))

import pytest  # noqa: E402

from codegen.core import _plan_required_seeds  # noqa: E402


@pytest.fixture
def org_infos():
    """Entity metadata keyed by table, mirroring _parent_entity_info.

    Chain shape under test: result -> workunit -> {batch, app_version},
    with app_version -> app. batch and app are leaves.
    """
    return {
        "ores_compute_workunits_tbl": {
            "entity_singular": "workunit",
            "generator_facet_name": "generators",
            "has_audit_group": False,
            "component": "compute",
            "mandatory_fks": [
                {"column": "batch_id", "table": "ores_compute_batches_tbl",
                 "target_column": "id"},
                {"column": "app_version_id",
                 "table": "ores_compute_app_versions_tbl", "target_column": "id"},
            ],
        },
        "ores_compute_batches_tbl": {
            "entity_singular": "batch",
            "generator_facet_name": "generators",
            "has_audit_group": False,
            "component": "compute",
            "mandatory_fks": [],
        },
        "ores_compute_app_versions_tbl": {
            "entity_singular": "app_version",
            "generator_facet_name": "generators",
            "has_audit_group": False,
            "component": "compute",
            "mandatory_fks": [
                {"column": "app_id", "table": "ores_compute_apps_tbl",
                 "target_column": "id"},
            ],
        },
        "ores_compute_apps_tbl": {
            "entity_singular": "app",
            "generator_facet_name": "generators",
            "has_audit_group": False,
            "component": "compute",
            "mandatory_fks": [],
        },
    }


def _call(org_infos, parent_var="workunit_id_parent", component="compute"):
    org_by_table = {tbl: {"org": Path(f"/fake/{tbl}.org")} for tbl in org_infos}

    def fake_parent_info(org_path):
        if org_path is None:
            return None
        tbl = org_path.name.removesuffix(".org")
        return org_infos.get(tbl)

    monkeypatch = pytest.MonkeyPatch()
    monkeypatch.setattr(
        "codegen.core._parent_entity_info", fake_parent_info)
    try:
        return _plan_required_seeds(
            org_infos["ores_compute_workunits_tbl"]["mandatory_fks"],
            parent_var, org_by_table, component, set())
    finally:
        monkeypatch.undo()


def test_closure_seeds_ancestors_before_the_rows_that_reference_them(
        org_infos):
    items = _call(org_infos)
    # Depth-first, write-ordered: batch, then app (app_version's own
    # parent), then app_version itself.
    assert [i["parent_entity_singular"] for i in items] == [
        "batch", "app", "app_version"]
    batch, app, app_version = items
    assert batch["parent_var"] == "workunit_id_parent"
    assert batch["var"] == "batch_id_parent"
    # The app row patches the app_version row that references it, and the
    # app_version row patches the workunit row.
    assert app["parent_var"] == "app_version_id_parent"
    assert app["var"] == "app_id_parent"
    assert app["column"] == "app_id"
    assert app_version["parent_var"] == "workunit_id_parent"
    assert app_version["var"] == "app_version_id_parent"


def test_party_ancestors_are_skipped(org_infos):
    org_infos["ores_compute_apps_tbl"]["mandatory_fks"] = [
        {"column": "party_id", "table": "ores_parties_tbl", "target_column": "id"},
    ]
    org_infos["ores_parties_tbl"] = {
        "entity_singular": "party",
        "generator_facet_name": "generators",
        "has_audit_group": False,
        "component": "compute",
        "mandatory_fks": [],
    }
    items = _call(org_infos)
    # The app row's party need is not seeded per-item: the direct
    # parent's party branch is the single party-seeding mechanism.
    assert [i["parent_entity_singular"] for i in items] == [
        "batch", "app", "app_version"]


def test_cycle_guard_terminates(org_infos):
    org_infos["ores_compute_apps_tbl"]["mandatory_fks"] = [
        {"column": "parent_app_id", "table": "ores_compute_apps_tbl",
         "target_column": "id"},
    ]
    items = _call(org_infos)
    # The app row references itself: the chain must stop after one
    # level rather than recurse forever.
    app_items = [i for i in items if i["parent_entity_singular"] == "app"]
    assert len(app_items) == 1


def test_unresolvable_and_cross_component_ancestors_are_skipped(
        org_infos):
    org_infos["ores_compute_app_versions_tbl"]["mandatory_fks"] = [
        {"column": "app_id", "table": "ores_compute_apps_tbl",
         "target_column": "id"},
        {"column": "runtime_id", "table": "ores_hand_authored_tbl",
         "target_column": "id"},
    ]
    items = _call(org_infos)
    # The unresolvable table produces no item and no crash; the rest of
    # the chain is unaffected.
    assert [i["parent_entity_singular"] for i in items] == [
        "batch", "app", "app_version"]
