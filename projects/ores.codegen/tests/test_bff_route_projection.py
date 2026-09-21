"""Tests for the ores.ts.web BFF route-descriptor projection and template.

Run::

    pytest projects/ores.codegen/tests/test_bff_route_projection.py

The facet emits one route descriptor per entity so the BFF's generic route
factory is registered from generated data rather than hand-written per-entity
code. Two things have to hold and both are tested here: the projection reads
the derived CRUD messages rather than assuming their names, and the template
and the projection agree on every variable -- a mismatch renders a descriptor
whose subjects are blank, which is the failure this file exists to catch.
"""
import sys
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[3]
sys.path.insert(0, str(REPO_ROOT / "projects/ores.codegen/src"))

from codegen.core import (  # noqa: E402
    bff_route_projection,
    generate_from_model,
    web_declaration_projection,
)
from codegen.org_loader import entity_protocol_messages  # noqa: E402

MODEL = REPO_ROOT / "projects/ores.refdata/modeling/ores.refdata.book_status.org"
TENANT_TYPE = REPO_ROOT / "projects/ores.iam/modeling/ores.iam.tenant_type.org"
TENANT = REPO_ROOT / "projects/ores.iam/modeling/ores.iam.tenant.org"
CODEGEN = REPO_ROOT / "projects/ores.codegen"


def _entity(**overrides):
    """The enriched entity the projections read, at its smallest.

    The messages are the ones ``entity_protocol_messages`` derives, set the
    way the enrichment leaves them, so the projection reads a real derivation
    rather than a hand-built message list.
    """
    entity = {
        "component": "iam",
        "entity_singular": "tenant_type",
        "entity_plural": "tenant_types",
        "entity_plural_short": "types",
        "primary_key": {
            "column": "type",
            "columns": [{"column": "type", "is_uuid": False}],
        },
        "presentation": {
            "collection_name": "tenant_types",
            "key_field": "type",
            "columns": [{"field": "type"}],
        },
    }
    entity.update(overrides)
    entity["messages"] = entity_protocol_messages(entity)
    return entity


def test_a_model_with_no_column_table_projects_nothing():
    """The gate resolve_targets withholds the facet on."""
    entity = _entity(presentation={"collection_name": "tenant_types"})
    assert bff_route_projection(entity, MODEL) is None


def test_the_declaration_and_the_route_agree_on_whether_there_are_screens():
    """One guard, so the two projections of one model cannot disagree."""
    entity = _entity()
    assert (web_declaration_projection(entity, MODEL) is None) == (
        bff_route_projection(entity, MODEL) is None)
    bare = _entity(presentation={"collection_name": "tenant_types"})
    assert (web_declaration_projection(bare, MODEL) is None) == (
        bff_route_projection(bare, MODEL) is None)


def test_a_lookup_entity_names_the_derived_crud_subjects():
    projection = bff_route_projection(_entity(), MODEL)
    assert projection["component"] == "iam"
    assert projection["entity"] == "tenant_type"
    assert projection["entity_camel"] == "tenantType"
    assert projection["collection"] == "tenant_types"
    assert projection["key"] == "id"
    assert projection["key_field"] == "type"
    assert projection["delete_keys_field"] == "types"
    assert projection["rows_field"] == "types"
    assert projection["subjects_list"] == "get_tenant_types_request"
    assert projection["subjects_save"] == "save_tenant_type_request"
    assert projection["subjects_remove"] == "delete_tenant_type_request"
    assert projection["subjects_history"] == "get_tenant_type_history_request"


def test_a_domain_entity_has_no_single_record_get():
    """The derived CRUD set states no read of one row, so the member is absent."""
    projection = bff_route_projection(_entity(), MODEL)
    assert "subjects_get" not in projection
    assert projection["has_get"] == "false"
    assert projection["has_history"] == "true"


def test_a_current_state_entity_has_no_history_member():
    """The derivation omits the pair, so the descriptor must omit the member."""
    projection = bff_route_projection(_entity(current_state=True), MODEL)
    assert "subjects_history" not in projection
    assert projection["has_history"] == "false"
    # The delete request is keyed by the natural key and still drivable.
    assert "subjects_remove" in projection
    assert projection["has_remove"] == "true"


def test_a_natural_key_primary_key_drives_delete_and_history():
    """The path segment is the natural key and so is the request key."""
    projection = bff_route_projection(_entity(), MODEL)
    assert projection["has_remove"] == "true"
    assert projection["has_history"] == "true"
    assert "subjects_remove" in projection
    assert "delete_keys_field" in projection
    assert "subjects_history" in projection
    assert projection["history_rows_field"] == "history"


def test_a_surrogate_primary_key_withholds_delete_and_history():
    """The requests are keyed by the surrogate primary key, not the natural key.

    The path segment the web builds is the natural key, so a route keyed by the
    surrogate would send a value the service matches nothing against, or cannot
    decode at all. The routes that can be driven stay.
    """
    entity = _entity(primary_key={
        "column": "id",
        "columns": [{"column": "id", "is_uuid": True}],
    })
    projection = bff_route_projection(entity, MODEL)
    assert projection["has_remove"] == "false"
    assert projection["has_history"] == "false"
    assert "subjects_remove" not in projection
    assert "delete_keys_field" not in projection
    assert "subjects_history" not in projection
    assert "history_rows_field" not in projection
    assert projection["subjects_list"] == "get_tenant_types_request"
    assert projection["subjects_save"] == "save_tenant_type_request"


def test_a_compound_primary_key_withholds_delete_and_history():
    """The generic factory sends one key, and a compound key is more than one."""
    entity = _entity(primary_key={
        "column": "type",
        "columns": [{"column": "type", "is_uuid": False},
                    {"column": "name", "is_uuid": False}],
    })
    projection = bff_route_projection(entity, MODEL)
    assert "subjects_remove" not in projection
    assert "subjects_history" not in projection


def test_the_save_states_the_audit_timestamp_field():
    """The web seeds every member, so the save has to fill an empty timestamp."""
    projection = bff_route_projection(_entity(), MODEL)
    assert projection["timestamp_fields_block"] == "'recorded_at'"


def test_a_grouped_audit_timestamp_is_stated_with_its_member_path():
    projection = bff_route_projection(
        _entity(has_audit_group=True, audit_prefix="audit."), MODEL)
    assert projection["timestamp_fields_block"] == "'audit.recorded_at'"


def test_a_current_state_entity_states_no_audit_timestamp_field():
    projection = bff_route_projection(_entity(current_state=True), MODEL)
    assert "timestamp_fields_block" not in projection


def test_a_singular_plural_collision_does_not_invent_a_get():
    """``series`` is both singular and plural; its one list read is not a get."""
    projection = bff_route_projection(_entity(
        entity_singular="series", entity_plural="series",
        entity_plural_short="series",
        primary_key={"column": "code",
                     "columns": [{"column": "code", "is_uuid": False}]},
        presentation={"collection_name": "series", "key_field": "code",
                     "columns": [{"field": "code"}]}), MODEL)
    assert "subjects_get" not in projection
    assert projection["has_get"] == "false"
    assert projection["subjects_list"] == "get_series_request"


def test_an_entity_with_no_derived_crud_set_projects_nothing():
    """An operation-owned protocol has no derived names to import."""
    entity = _entity()
    entity["messages"] = []
    assert bff_route_projection(entity, MODEL) is None


def test_the_template_renders_the_route_for_a_real_model(tmp_path):
    """The template and the projection agree on every variable name.

    The descriptor is a pure value, so nothing but the render catches a
    template reading a variable the projection does not set.
    """
    generate_from_model(
        str(TENANT_TYPE), CODEGEN / "library" / "data",
        CODEGEN / "library" / "templates", tmp_path,
        target_template="ts_bff_route.ts.mustache",
        target_output="tenant_type_route.ts")
    rendered = (tmp_path / "tenant_type_route.ts").read_text(encoding="utf-8")

    assert ("import { subjects } from "
            "'@ores/wire-protocol/generated/iam/protocol/"
            "tenant_type_protocol';" in rendered)
    assert ("import type { EntityRouteDescriptor } from "
            "'../../entity-routes.js';" in rendered)
    assert "export const tenantTypeRoute: EntityRouteDescriptor = {" in rendered
    assert "collection: 'tenant_types'," in rendered
    assert "key: 'id'," in rendered
    assert "keyField: 'type'," in rendered
    assert "deleteKeysField: 'types'," in rendered
    assert "list: subjects.get_tenant_types_request," in rendered
    assert "save: subjects.save_tenant_type_request," in rendered
    assert "remove: subjects.delete_tenant_type_request," in rendered
    assert "history: subjects.get_tenant_type_history_request," in rendered
    assert "rowsField: 'types'," in rendered
    assert "historyRowsField: 'history'," in rendered
    assert "timestampFields: ['recorded_at']," in rendered
    # The descriptor holds values and no behaviour.
    assert "=>" not in rendered
    # No blank line left behind by the optional get member, which pystache
    # would emit if the template gated on the ``has_get`` string literal.
    assert ",\n\n" not in rendered
    assert "get: subjects." not in rendered


def test_the_template_omits_the_routes_a_surrogate_key_cannot_drive(tmp_path):
    """A real model whose delete and history requests are keyed by its UUID."""
    generate_from_model(
        str(TENANT), CODEGEN / "library" / "data",
        CODEGEN / "library" / "templates", tmp_path,
        target_template="ts_bff_route.ts.mustache",
        target_output="tenant_route.ts")
    rendered = (tmp_path / "tenant_route.ts").read_text(encoding="utf-8")

    assert "export const tenantRoute: EntityRouteDescriptor = {" in rendered
    assert "keyField: 'code'," in rendered
    assert "list: subjects.get_tenants_request," in rendered
    assert "save: subjects.save_tenant_request," in rendered
    assert "rowsField: 'tenants'," in rendered
    assert "timestampFields: ['recorded_at']," in rendered
    # The delete and history requests carry the UUID primary key, so neither
    # route is stated and neither field is left behind.
    assert "deleteKeysField" not in rendered
    assert "remove: subjects." not in rendered
    assert "history: subjects." not in rendered
    assert "historyRowsField" not in rendered
    assert ",\n\n" not in rendered
