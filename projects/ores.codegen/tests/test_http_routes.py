"""Tests for the HTTP route facet's entity projection.

Run::

    python3 -m pytest projects/ores.codegen/tests/test_http_routes.py

One derived operation becomes one HTTP route, so the gateway renders from the
same derivation the protocol and the shell read rather than from a route list
written by hand. These cases pin the addressing, what a route carries, and the
two facts the security rule depends on: a route that needs a session states
so, and a model that never opted in renders nothing at all.
"""
import sys
from pathlib import Path

import pystache

REPO_ROOT = Path(__file__).resolve().parents[3]
sys.path.insert(0, str(REPO_ROOT / "projects/ores.codegen/src"))

from codegen.generate import resolve_targets  # noqa: E402
from codegen.org_loader import entity_http_route_plan  # noqa: E402

CODEGEN_BASE = REPO_ROOT / "projects" / "ores.codegen"
TEMPLATES = CODEGEN_BASE / "library" / "templates"
ASSETS_MODELING = REPO_ROOT / "projects" / "ores.assets" / "modeling"
IMAGE_MODEL = ASSETS_MODELING / "ores.assets.image.org"

ROUTE_OUTPUTS = [
    "projects/ores.http/assets/include/ores.http/routes/assets/image_routes.hpp",
    "projects/ores.http/assets/src/routes/assets/image_routes.cpp",
    "projects/ores.http/assets/tests/image_routes_tests.cpp",
]

MODEL_TYPES_PAGES = (
    "ores.cpp.http-route.org",
    "ores.cpp.http-route.route_header.org",
    "ores.cpp.http-route.route_implementation.org",
    "ores.cpp.http-route.route_tests.org",
)

# A minimal but complete operation model for image. The ownership rule reads
# only the frontmatter, so one message with one mapped field is enough.
IMAGE_MESSAGES = """\
:PROPERTIES:
:END:
#+title: ores.assets.image_messages
#+type: ores.codegen.operation
#+component: assets
#+subcomponent: api
#+entity_singular: image
#+namespace: ores::assets::messaging
#+brief: Test messages for the ownership rule.
#+filetags: :model:operation:

* Messages

** list_images_request
:PROPERTIES:
:subject: assets.v1.images.list
:END:

*** limit
:PROPERTIES:
:cpp_type: int
:END:
"""


def _key(name, cpp_type="std::string", **overrides):
    """A primary-key member. The loader spells these `column`, not `name`."""
    column = {"column": name, "cpp_type": cpp_type, "is_user_supplied": True}
    column.update(overrides)
    return column


def _write(name, cpp_type="std::string", **overrides):
    """A member of the wire write record, marked the way the loader marks it."""
    field = {
        "name": name,
        "cpp_type": cpp_type,
        "is_user": True,
        "is_minted": False,
        "is_session_party": False,
    }
    field.update(overrides)
    field["is_user"] = not (field["is_minted"] or field["is_session_party"])
    return field


def _operation(verb, request, response, subject, **overrides):
    operation = {
        "verb": verb,
        "request": request,
        "response": response,
        "subject": subject,
        "fields": [],
        "has_order": False,
        "has_filter": False,
        "has_scope": False,
        "leading": "",
        "requires_session": True,
    }
    operation.update(overrides)
    return operation


ALL_VERBS = [
    _operation("list", "list_widgets_request", "list_widgets_response",
               "demo.v1.widgets.list", has_order=True),
    _operation("get", "get_widget_request", "get_widget_response",
               "demo.v1.widgets.get"),
    _operation("get_many", "get_many_widgets_request",
               "get_many_widgets_response", "demo.v1.widgets.get_many"),
    _operation("put", "put_widget_request", "put_widget_response",
               "demo.v1.widgets.put"),
    _operation("put_many", "put_many_widgets_request",
               "put_many_widgets_response", "demo.v1.widgets.put_many"),
    _operation("delete", "delete_widget_request", "delete_widget_response",
               "demo.v1.widgets.delete"),
    _operation("delete_many", "delete_many_widgets_request",
               "delete_many_widgets_response", "demo.v1.widgets.delete_many"),
    _operation("list_versions", "list_widget_versions_request",
               "list_widget_versions_response",
               "demo.v1.widgets_versions.list", has_order=True),
    _operation("get_version", "get_widget_version_request",
               "get_widget_version_response", "demo.v1.widgets_versions.get"),
]


def _entity(operations=None, requires_session=True):
    """An entity shaped the way the loader hands it to the projection."""
    ops = ALL_VERBS if operations is None else operations
    if not requires_session:
        ops = [dict(op, requires_session=False) for op in ops]
    return {
        "component": "demo",
        "subcomponent": "api",
        "entity_singular": "widget",
        "entity_plural": "widgets",
        "entity_singular_upper": "WIDGET",
        "columns": [],
        "primary_key": {"column": "type", "columns": [_key("type")]},
        "write_fields": [_write("type"), _write("description")],
        "operations": ops,
    }


def _plan(operations=None, requires_session=True):
    return entity_http_route_plan(_entity(operations, requires_session))


def _addressing(plan):
    return [(route["method"], route["pattern"]) for route in plan["routes"]]


def _rendered_impl(entity):
    """The unit the impl template renders for one entity shape.

    The tangled mustache is rendered directly, so the case reads the text a
    model would produce without writing a file anywhere.
    """
    entity = dict(entity)
    entity["http_route"] = entity_http_route_plan(entity)
    template = (TEMPLATES / "cpp_http_route_impl.cpp.mustache").read_text()
    return pystache.render(template, {"cpp_license": "", "domain_entity": entity})


class TestTheRouteTable:
    """One route per derived verb, and a put makes two."""

    def test_every_derived_verb_becomes_a_route(self):
        assert _addressing(_plan()) == [
            ("get", "/api/v1/demo/widgets"),
            ("get", "/api/v1/demo/widgets/{type}"),
            ("post", "/api/v1/demo/widgets/get-many"),
            ("post", "/api/v1/demo/widgets"),
            ("put", "/api/v1/demo/widgets"),
            ("post", "/api/v1/demo/widgets/put-many"),
            ("delete_", "/api/v1/demo/widgets/{type}"),
            ("post", "/api/v1/demo/widgets/delete-many"),
            ("get", "/api/v1/demo/widgets/{type}/versions"),
            ("get", "/api/v1/demo/widgets/{type}/versions/{version}"),
        ]

    def test_a_create_and_a_replace_share_a_path_and_differ_by_method(self):
        # The whole of what separates them is the claim, so the verb carries
        # it: a POST claims the row is absent, a PUT that it may exist.
        routes = {route["command"]: route for route in _plan()["routes"]}
        assert routes["add"]["pattern"] == routes["set"]["pattern"]
        assert routes["add"]["method"] == "post"
        assert routes["set"]["method"] == "put"
        assert routes["add"]["precondition"] == "must_not_exist"
        assert routes["set"]["precondition"] == "any"

    def test_a_write_carries_the_canonical_request_as_its_body(self):
        routes = {route["command"]: route for route in _plan()["routes"]}
        for command in ("get-many", "add", "set", "put-many", "delete-many"):
            assert routes[command]["has_body"] is True, command
        for command in ("list", "get", "delete", "versions", "version"):
            assert routes[command]["has_body"] is False, command

    def test_a_key_addressed_route_states_the_key_in_its_path(self):
        routes = {route["command"]: route for route in _plan()["routes"]}
        assert routes["get"]["pattern"].endswith("/{type}")
        assert routes["delete"]["pattern"].endswith("/{type}")
        assert [key["name"] for key in routes["get"]["keys"]] == ["type"]

    def test_a_delete_takes_its_intent_from_the_query(self):
        # A delete has no body to carry an intent, so the route declares the
        # parameters a caller states it with rather than dropping it.
        delete = next(r for r in _plan()["routes"] if r["command"] == "delete")
        assert delete["has_intent"] is True

    def test_a_versions_read_is_a_sub_resource_of_the_key(self):
        routes = {route["command"]: route for route in _plan()["routes"]}
        assert routes["versions"]["pattern"].endswith("/{type}/versions")
        assert routes["version"]["pattern"].endswith(
            "/{type}/versions/{version}")

    def test_a_scoped_read_is_addressed_by_its_relation(self):
        plan = _plan(operations=[
            _operation("list_scoped", "list_by_account_id_widgets_request",
                       "list_by_account_id_widgets_response",
                       "demo.v1.widgets.list_by_account_id", has_order=True,
                       leading="account_id"),
        ])
        assert _addressing(plan) == [
            ("get", "/api/v1/demo/widgets/by-account-id/{account_id}"),
        ]
        assert plan["routes"][0]["has_scope"] is True

    def test_an_entity_with_no_operations_has_no_routes(self):
        plan = _plan(operations=[])
        assert plan["routes"] == []
        assert plan["route_count"] == 0

    def test_a_verb_with_no_route_shape_is_reported(self):
        # The projection builds a route per verb it recognises. A verb it does
        # not would simply produce nothing, and the gateway would publish fewer
        # verbs than the service answers with no failure anywhere.
        plan = _plan(operations=[
            _operation("archive", "archive_widget_request",
                       "archive_widget_response", "demo.v1.widgets.archive"),
        ])
        assert plan["uncovered_verbs"] == ["archive"]
        assert plan["routes"] == []


class TestTheSecurityRule:
    """One declaration, one enforcement point."""

    def test_a_route_states_the_authentication_its_operation_requires(self):
        plan = entity_http_route_plan(_entity(operations=[
            dict(ALL_VERBS[0]),
            dict(ALL_VERBS[1], requires_session=False),
        ]))
        flags = {route["command"]: route["requires_session"]
                 for route in plan["routes"]}
        assert flags == {"list": True, "get": False}

    def test_the_unit_sets_auth_required_where_the_model_requires_a_session(self):
        entity = _entity()
        text = _rendered_impl(entity)
        assert text.count(".auth_required()") == entity_http_route_plan(entity)[
            "route_count"]

    def test_a_public_operation_renders_no_auth_required(self):
        text = _rendered_impl(_entity(requires_session=False))
        assert ".auth_required()" not in text

    def test_the_unit_forwards_the_callers_token_not_its_own(self):
        # The service validates the caller, so the gateway takes the caller's
        # token from the HTTP request and delegates it on the NATS call.
        text = _rendered_impl(_entity())
        assert "get_bearer_token()" in text
        assert "with_delegation(" in text


class TestTheFacetResolves:
    """Opt-in, disabled by default."""

    def test_an_enabled_entity_renders_a_route_unit(self):
        units, model_type, _ = resolve_targets(
            IMAGE_MODEL, CODEGEN_BASE, address="ores.cpp.http-route")
        assert model_type == "domain_entity"
        assert [unit["output"] for unit in units] == ROUTE_OUTPUTS

    def test_a_disabled_entity_renders_nothing(self, tmp_path):
        # The same model with the one property removed, so the empty
        # intersection is the opt-in and nothing else.
        text = IMAGE_MODEL.read_text(encoding="utf-8")
        assert ":ores.cpp.http-route.enabled: true" in text
        stripped = text.replace(":ores.cpp.http-route.enabled: true\n", "")
        model = tmp_path / "ores.assets.image.org"
        model.write_text(stripped, encoding="utf-8")

        units, _, _ = resolve_targets(
            model, CODEGEN_BASE, address="ores.cpp.http-route")
        assert [unit for unit in units
                if "ores.http/" in unit["output"]] == []

    def test_the_archetype_pages_admit_a_junction(self):
        for name in MODEL_TYPES_PAGES:
            page = (TEMPLATES / name).read_text(encoding="utf-8")
            declared = [line for line in page.splitlines()
                        if line.startswith("#+model_types:")]
            assert declared, name
            assert "junction" in declared[0], name

    def test_no_route_unit_where_an_operation_model_owns_the_protocol(
            self, tmp_path):
        # The unit names the derived request types and their subjects, so an
        # entity whose protocol an operation model owns has none of them. The
        # unit is dropped rather than rendered against types that do not
        # exist.
        model = tmp_path / "ores.assets.image.org"
        model.write_text(IMAGE_MODEL.read_text(encoding="utf-8"),
                         encoding="utf-8")
        (tmp_path / "ores.assets.image_messages.org").write_text(
            IMAGE_MESSAGES, encoding="utf-8")

        units, _, _ = resolve_targets(model, CODEGEN_BASE,
                                      address="ores.cpp.http-route")
        assert units == []
