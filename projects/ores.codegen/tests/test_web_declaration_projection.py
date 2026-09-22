"""Tests for the ores.ts.web entity-declaration projection and template.

Run::

    pytest projects/ores.codegen/tests/test_web_declaration_projection.py

The facet emits one declaration per entity so the web client holds no
per-entity TypeScript at all. Two things have to hold and both are tested
here: the projection reads the enriched presentation rather than guessing,
and the template and the projection agree on every variable name -- a
mismatch renders an empty descriptor that typechecks, which is the failure
this file exists to catch.
"""
import sys
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[3]
sys.path.insert(0, str(REPO_ROOT / "projects/ores.codegen/src"))

from codegen.core import (  # noqa: E402
    generate_from_model,
    web_declaration_projection,
)
from codegen.org_loader import entity_protocol_messages  # noqa: E402

MODEL = REPO_ROOT / "projects/ores.refdata/modeling/ores.refdata.book_status.org"
COUNTRY = REPO_ROOT / "projects/ores.refdata/modeling/ores.refdata.country.org"
CODEGEN = REPO_ROOT / "projects/ores.codegen"


def _project(columns, primary_key=None, *, current_state=False, as_of=False, **drawer):
    """Project one hand-built entity, the way the enrichment leaves it.

    The primary key defaults to the natural key the drawer states, so remove
    and history are the routes the entity can drive unless a test overrides
    the key. The messages are the ones ``entity_protocol_messages`` derives,
    set the way the enrichment leaves them, so the capability derivation
    reads a real protocol rather than a hand-built message list.
    """
    presentation = {"collection_name": "statuses", "columns": list(columns),
                    "key_field": "code", **drawer}
    entity = {"component": "refdata", "entity_singular": "book_status",
              "current_state": current_state,
              "has_as_of_lookup": as_of,
              "presentation": presentation}
    entity["primary_key"] = primary_key or {
        "column": "code", "columns": [{"column": "code"}]}
    entity.setdefault("has_audit_columns", not current_state)
    entity["messages"] = entity_protocol_messages(entity)
    return web_declaration_projection(entity, MODEL)


def test_a_model_with_no_column_table_projects_nothing():
    """The gate resolve_targets withholds the facet on."""
    assert web_declaration_projection(
        {"entity_singular": "book_status",
         "presentation": {"collection_name": "statuses"}}, MODEL) is None
    assert _project([]) is None


def test_a_writable_entity_with_history_gets_all_four_capabilities():
    projection = _project([{"field": "code"}])
    assert projection["can_create"] == "true"
    assert projection["can_edit"] == "true"
    assert projection["can_remove"] == "true"
    assert projection["can_history"] == "true"


def test_a_read_only_list_is_not_writable():
    """The model says so, rather than the projection inferring it."""
    projection = _project([{"field": "code"}], has_readonly_paginated_list=True)
    assert projection["can_create"] == "false"
    assert projection["can_edit"] == "false"
    assert projection["can_remove"] == "false"


def test_history_is_read_from_the_derived_versions_pair():
    """A current-state entity has no versions to serve.

    Deriving the capability from a version column gets exactly this case
    wrong, which is why it is read off the derived protocol messages.
    """
    projection = _project([{"field": "code"}], current_state=True)
    assert projection["can_history"] == "false"


def test_a_surrogate_primary_key_withholds_remove_and_history():
    """The BFF derives delete and history from the primary key.

    The route's path segment carries the natural key, so a declaration that
    offered them would render actions the BFF does not serve. Create and edit
    travel on the save route, which carries the whole record and is not keyed
    by the path, so they stay.
    """
    projection = _project(
        [{"field": "code"}],
        primary_key={"column": "id", "columns": [{"column": "id"}]})
    assert projection["can_create"] == "true"
    assert projection["can_edit"] == "true"
    assert projection["can_remove"] == "false"
    assert projection["can_history"] == "false"


def test_a_natural_key_primary_key_keeps_remove_and_history():
    """The primary key and the natural key agree, so both routes are drivable."""
    projection = _project(
        [{"field": "code"}],
        primary_key={"column": "code", "columns": [{"column": "code"}]})
    assert projection["can_remove"] == "true"
    assert projection["can_history"] == "true"


def test_the_route_api_and_key_are_stated_rather_than_composed_by_a_screen():
    projection = _project([{"field": "code"}],
                          collection_name="book_statuses", key_field="code")
    assert projection["entity"] == "book_status"
    assert projection["entity_camel"] == "bookStatus"
    assert projection["route_segment"] == "book-status"
    assert projection["api_base"] == "/api/book_statuses"
    assert projection["key_field"] == "code"


def test_search_covers_the_declared_columns_and_drops_the_audit_tail():
    projection = _project([{"field": "code"}, {"field": "name"},
                           {"field": "modified_by"}, {"field": "version"}])
    assert projection["search_fields_block"].count("'code',") == 1
    assert "'name'," in projection["search_fields_block"]
    assert "modified_by" not in projection["search_fields_block"]
    assert "version" not in projection["search_fields_block"]


def test_a_list_that_takes_a_window_states_it():
    """The control is offered only where the request can carry the instant."""
    assert _project([{"field": "code"}])["can_as_of"] == "false"
    assert _project([{"field": "code"}],
                    as_of=True)["can_as_of"] == "true"


def test_the_template_renders_the_declaration_for_a_real_model(tmp_path):
    """The template and the projection agree on every variable name.

    An empty descriptor typechecks, so nothing but this catches a template
    that reads a variable the projection does not set.
    """
    generate_from_model(
        str(COUNTRY), CODEGEN / "library" / "data",
        CODEGEN / "library" / "templates", tmp_path,
        target_template="ts_web_declaration.ts.mustache",
        target_output="country_declaration.ts")
    rendered = (tmp_path / "country_declaration.ts").read_text(encoding="utf-8")

    assert "export const countryDescriptor: EntityDescriptor = {" in rendered
    assert "component: 'refdata'," in rendered
    assert "entity: 'country'," in rendered
    assert "meta: countryMeta," in rendered
    assert "routeSegment: 'country'," in rendered
    assert "apiBase: '/api/countries'," in rendered
    assert "create: true," in rendered
    assert "history: true," in rendered
    assert "'alpha2_code'," in rendered
    # The descriptor holds values and no behaviour, which is what makes it
    # safe to generate; a function here would be codegen guessing.
    assert "=>" not in rendered
