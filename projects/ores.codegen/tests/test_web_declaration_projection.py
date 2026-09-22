"""Tests for the ores.ts.web entity-declaration projection and template.

Run::

    pytest projects/ores.codegen/tests/test_web_declaration_projection.py

The facet emits one declaration per entity so the web client holds no
per-entity TypeScript at all. Three things have to hold and all are tested
here: the declaration takes its capabilities from the route projection,
which is what the browser actually talks to; a hand-built protocol decides
those capabilities, so a shape the real derivation cannot produce is still
testable; and the template and the projection agree on every variable name
-- a mismatch renders an empty descriptor that typechecks, which is the
failure this file exists to catch.
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

# The list request is what makes an entity routable at all -- a route that
# cannot read the collection reaches nothing -- and the versions pair is what
# history is read from. Remove needs a delete request, and the write needs a
# put; the key record is always present, because every route addresses a row
# by it.
LIST = "list_book_statuses_request"
VERSIONS = "list_book_status_versions_request"
VERSIONS_RESPONSE = "list_book_status_versions_response"
WRITABLE = (
    LIST,
    "put_book_status_request",
    "delete_book_status_request",
    VERSIONS,
    VERSIONS_RESPONSE,
)


def _requests(*names, key_members=("code",)):
    """Hand-built derived protocol messages carrying these names.

    Only the name and the presence of a subject matter: a request with no
    subject is a plain payload and contributes no member to the protocol
    module's ``subjects``, so a response is named without one. The key record
    is supplied here rather than named by each test, because the routes
    address a row by it and its members are what the descriptor states.
    """
    key = {"name": "book_status_key", "subject": None,
           "fields": [{"name": member} for member in key_members]}
    return [key] + [
        {"name": name,
         "subject": None if name.endswith("_response") else f"refdata.v1.{name}"}
        for name in names]


def _project(columns, primary_key=None, messages=None, current_state=False,
             key_members=("code",), **drawer):
    """Project one hand-built entity, the way the enrichment leaves it.

    The primary key defaults to the natural key the drawer states. With no
    ``messages`` the entity's own protocol is derived, which is what a real
    model gives; a test that needs a shape the derivation cannot produce -- an
    entity with no put request, say -- states the protocol by hand.
    """
    presentation = {"collection_name": "statuses", "columns": list(columns),
                    "key_field": "code", **drawer}
    entity = {"component": "refdata", "entity_singular": "book_status",
              "entity_plural": "book_statuses",
              "presentation": presentation}
    entity["primary_key"] = primary_key or {
        "column": "code", "columns": [{"column": "code"}]}
    entity.setdefault("has_audit_columns", not current_state)
    entity["messages"] = (
        entity_protocol_messages(entity) if messages is None
        else _requests(*messages, key_members=key_members))
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


def test_an_entity_with_no_put_request_is_not_writable():
    """How a read-only entity's screen comes out honest.

    ``login_info`` states ``:read_only: true``, so its service derives no put
    request and the route serves no save. The declaration states what the
    route serves, so the screen offers no action the BFF would answer with
    404 -- which is what it used to do, deriving the capability a layer below
    the route.
    """
    projection = _project(
        [{"field": "code"}],
        messages=(LIST, VERSIONS, VERSIONS_RESPONSE))
    assert projection["can_create"] == "false"
    assert projection["can_edit"] == "false"
    assert projection["can_remove"] == "false"


def test_a_read_only_list_is_not_writable():
    """The model says so, rather than the projection inferring it."""
    projection = _project([{"field": "code"}], has_readonly_paginated_list=True)
    assert projection["can_create"] == "false"
    assert projection["can_edit"] == "false"
    assert projection["can_remove"] == "false"


def test_no_versions_pair_means_no_history():
    """A junction is versioned and still has no history route.

    Deriving the capability from a version column gets exactly this case
    wrong. The route serves history only when the entity derives the versions
    pair, and the declaration states what the route serves.
    """
    projection = _project(
        [{"field": "code"}],
        messages=(LIST, "put_book_status_request", "delete_book_status_request"))
    assert projection["can_history"] == "false"


def test_a_composite_key_offers_no_history_because_the_route_withholds_it():
    """The regression the coupling exists to prevent.

    The generic history request names one id, so a key of two members has no
    history route -- a pair joined into a string addresses no row. The
    declaration used to promise History here anyway, because it derived the
    capability from the protocol a layer below the route, and the screen then
    rendered an action the BFF answered with 404. It now states what the
    route serves, so the two cannot disagree.
    """
    projection = _project([{"field": "code"}], messages=WRITABLE,
                          key_members=("code", "name"))
    assert projection["can_history"] == "false"
    assert projection["can_remove"] == "true"


def test_a_surrogate_storage_key_keeps_remove_and_history():
    """The declaration promises the routes the BFF serves.

    The path segment carries the declared key and so does the request, because
    the key record is built from the same declaration. Holding a surrogate
    storage key changes neither, so the actions stay.
    """
    projection = _project(
        [{"field": "code"}],
        primary_key={"column": "id", "columns": [{"column": "id"}]})
    assert projection["can_create"] == "true"
    assert projection["can_edit"] == "true"
    assert projection["can_remove"] == "true"
    assert projection["can_history"] == "true"


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
