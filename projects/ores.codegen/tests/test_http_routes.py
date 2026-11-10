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
from codegen.org_loader import entity_http_route_plan, entity_web_records_plan  # noqa: E402

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


def _entity(operations=None, requires_session=True, http_area=None):
    """An entity shaped the way the loader hands it to the projection."""
    ops = ALL_VERBS if operations is None else operations
    if not requires_session:
        ops = [dict(op, requires_session=False) for op in ops]
    entity = {
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
    if http_area is not None:
        entity["http_area"] = http_area
    return entity


def _plan(operations=None, requires_session=True, http_area=None):
    return entity_http_route_plan(_entity(operations, requires_session, http_area))


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

    def test_a_declared_area_is_the_namespace_its_routes_publish(self):
        # The URL is a contract and a component rename must not move it, so the
        # area is the model's declaration and not the component's name.
        patterns = {pattern for _, pattern in
                    _addressing(_plan(http_area="reference-data"))}
        assert all(pattern.startswith("/api/v1/reference-data/")
                   for pattern in patterns), patterns

    def test_the_rendered_unit_carries_the_declared_area(self):
        # The plan is not what a caller reaches: the template's text is.
        text = _rendered_impl(_entity(http_area="reference-data"))
        assert 'router->get("/api/v1/reference-data/widgets")' in text, text[:400]
        assert "/api/v1/demo/" not in text

    def test_an_entity_with_no_area_keeps_its_component(self):
        patterns = {pattern for _, pattern in _addressing(_plan())}
        assert all(pattern.startswith("/api/v1/demo/")
                   for pattern in patterns), patterns

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

    def test_a_public_operation_renders_the_explicit_public_form(self):
        # A route that states neither position is refused at registration, so
        # the public form has to be written, not left out.
        entity = _entity(requires_session=False)
        text = _rendered_impl(entity)
        assert text.count(".auth_optional()") == entity_http_route_plan(entity)[
            "route_count"]

    def test_every_route_states_exactly_one_position(self):
        entity = _entity()
        text = _rendered_impl(entity)
        count = entity_http_route_plan(entity)["route_count"]
        assert text.count(".auth_required()") + text.count(".auth_optional()") == count

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


class TestTheSearchParameter:
    """A list a caller may search reads the parameter, and no other list does."""

    @staticmethod
    def _searchable():
        entity = _entity()
        entity["has_search"] = True
        entity["filter_type"] = "messaging::widgets_filter"
        return entity

    def test_a_searchable_list_reads_the_parameter_and_carries_it(self):
        # The model says a column is searchable, so the route states the
        # parameter and hands the text to the filter the request carries.
        text = _rendered_impl(self._searchable())
        assert 'req.get_query_param("search")' in text, text[:400]
        assert "msg.filter = messaging::widgets_filter{};" in text
        assert "msg.filter->search = raw;" in text

    def test_a_searchable_list_documents_the_parameter(self):
        text = _rendered_impl(self._searchable())
        assert '.query_param("search", "string"' in text, text[:400]

    def test_a_list_with_nothing_to_search_reads_no_parameter(self):
        # Every entity without a searchable column keeps the route it had.
        text = _rendered_impl(_entity())
        assert 'get_query_param("search")' not in text
        assert '.query_param("search"' not in text


class TestTheFilterParameters:
    """A filter member a list record states is a parameter the route reads."""

    @staticmethod
    def _filtered():
        entity = _entity()
        entity["filter_type"] = "messaging::widgets_filter"
        entity["filter_members"] = [
            {"member": "type", "column": "type", "is_equals": True,
             "cpp_type": "std::string", "read_type": "std::string", "is_nullable": False},
            {"member": "name", "column": "name", "is_equals": True,
             "cpp_type": "std::optional<std::string>",
             "read_type": "std::string", "is_nullable": True},
            {"member": "kind_one_of", "column": "kind", "is_one_of": True,
             "cpp_type": "std::vector<std::string>", "read_type": "std::string",
             "is_nullable": False},
            {"member": "created_from", "column": "created", "is_range": True,
             "is_from": True, "cpp_type": "std::string"},
        ]
        return entity

    def test_an_equals_member_is_read_under_its_own_prefix(self):
        # The BFF's lists filter on f.<member>, so the generated route states the
        # same parameter rather than inventing a second spelling.
        text = _rendered_impl(self._filtered())
        assert 'get_query_param("f.type")' in text, text[:400]
        assert 'msg.filter->type = widget_read_param<std::string>(raw, "f.type");' in text

    def test_a_nullable_member_parses_into_the_type_inside_both_wrappers(self):
        text = _rendered_impl(self._filtered())
        assert 'msg.filter->name = widget_read_param<std::string>(raw, "f.name");' in text

    def test_an_equals_member_is_documented_where_it_is_read(self):
        text = _rendered_impl(self._filtered())
        assert '.query_param("f.type", "string"' in text

    def test_a_one_of_member_reads_a_list_under_its_member_name(self):
        text = _rendered_impl(self._filtered())
        assert 'get_query_param("f.kind_one_of")' in text
        assert ('msg.filter->kind_one_of = widget_read_list<std::string>'
                '(raw, "f.kind_one_of");') in text
        assert '.query_param("f.kind_one_of", "string"' in text

    def test_a_range_member_reads_its_bound_as_text(self):
        text = _rendered_impl(self._filtered())
        assert 'get_query_param("f.created_from")' in text
        assert "msg.filter->created_from = raw;" in text
        assert '.query_param("f.created_from", "string"' in text

    def test_every_declared_member_is_allowed_and_no_other(self):
        # The BFF refuses a filter a resource does not declare, because a filter
        # that narrowed nothing would read as an answer.
        text = _rendered_impl(self._filtered())
        assert ('widget_refuse_undeclared_filters(req, { "f.type", "f.name", '
                '"f.kind_one_of", "f.created_from", });') in text

    def test_a_list_with_no_filter_record_refuses_every_filter(self):
        text = _rendered_impl(_entity())
        assert "widget_refuse_undeclared_filters(req, { });" in text

    def test_the_order_uses_the_parameters_the_browser_sends(self):
        text = _rendered_impl(self._filtered())
        assert 'get_query_param("sort")' in text
        assert 'get_query_param("descending")' in text
        assert '.query_param("sort", "string"' in text
        assert '.query_param("descending", "boolean"' in text
        assert 'get_query_param("order")' not in text
        assert 'get_query_param("desc")' not in text

    def test_a_list_with_no_filter_record_reads_no_filter(self):
        text = _rendered_impl(_entity())
        assert 'get_query_param("f.' not in text


def _rendered_tests(entity):
    """The test file the tests template renders for one entity shape."""
    entity = dict(entity)
    entity["http_route"] = entity_http_route_plan(entity)
    template = (TEMPLATES / "cpp_http_route_tests.cpp.mustache").read_text()
    return pystache.render(template, {"cpp_license": "", "domain_entity": entity})


class TestTheListHandlerTests:
    """A paged list route's test file runs the handler, not only the router."""

    def test_a_page_and_an_order_are_accepted(self):
        text = _rendered_tests(TestTheFilterParameters._filtered())
        assert 'TEST_CASE("widget_routes_list_accepts_a_page_and_an_order"' in text
        assert '{"sort", "version"}, {"descending", "true"}' in text

    def test_every_declared_filter_is_tried_under_the_name_its_reader_reads(self):
        # An equals member is read under its column; the others under the member.
        text = _rendered_tests(TestTheFilterParameters._filtered())
        assert ('names{ "f.type", "f.name", "f.kind_one_of", "f.created_from", };'
                in text)

    def test_an_undeclared_filter_is_tried_and_must_be_refused(self):
        text = _rendered_tests(TestTheFilterParameters._filtered())
        assert '{ {"f.no_such_column", "x"} }' in text
        assert "http_status::bad_request" in text

    def test_each_one_of_member_is_tried_with_an_empty_item(self):
        text = _rendered_tests(TestTheFilterParameters._filtered())
        assert ('TEST_CASE("widget_routes_list_refuses_an_empty_item_in_f_kind_one_of"'
                in text)
        assert '{ {"f.kind_one_of", ","} }' in text
        assert "refuses_an_empty_item_in_f_type" not in text

    def test_a_list_with_no_filter_record_tries_no_declared_filter(self):
        text = _rendered_tests(_entity())
        assert "names{ };" in text
        assert "refuses_an_empty_item" not in text

    def test_a_unit_with_no_paged_list_runs_no_handler(self):
        get_only = [op for op in ALL_VERBS if op.get("verb") == "get"]
        text = _rendered_tests(_entity(operations=get_only))
        assert "handle_list" not in text
        assert "refuses_an_undeclared_filter" not in text


def _web(entity=None, **declared):
    """An entity in the screens' registry, as the loader hands it over."""
    entity = dict(entity or _entity())
    entity["web_resource"] = "widgets"
    entity.update(declared)
    return entity


def _rendered_web(entity, template="cpp_http_route_web_records.cpp.mustache"):
    entity = dict(entity)
    entity["web_records"] = entity_web_records_plan(entity)
    text = (TEMPLATES / template).read_text()
    return pystache.render(text, {"cpp_license": "", "domain_entity": entity})


class TestTheWebRecordRoutes:
    """A model that names a screen resource gets the BFF's record routes."""

    def test_a_model_that_names_no_resource_has_no_record_routes(self):
        assert entity_web_records_plan(_entity()) is None
        assert _rendered_web(_entity()).strip() == ""

    def test_the_resource_is_addressed_under_its_component_by_its_key(self):
        plan = entity_web_records_plan(_web())
        assert plan["base"] == "/api/demo/widgets"
        assert plan["unit"] == "widget_web"
        assert plan["write_permission"] == "demo::widgets:write"
        assert plan["rows_member"] == "widgets"

    def test_a_one_field_key_is_read_through_the_key_one_of_filter(self):
        text = _rendered_web(_web())
        assert 'router->get("/api/demo/widgets/key/{key}")' in text
        assert "msg.filter->type_one_of = std::vector<std::string>" in text

    def test_the_routes_keep_the_bff_shapes(self):
        text = _rendered_web(_web())
        assert '"{\\"rows\\":" + widget_web_rows(rows) + "}"' in text
        assert '",\\"total\\":" + std::to_string(reply.total)' in text
        assert '"{\\"row\\":" + widget_web_row(' in text

    def test_a_read_only_resource_refuses_every_write(self):
        text = _rendered_web(_web(web_read_only=True))
        assert text.count('"widgets is read only here."') == 2
        assert '"writable":false' in text

    def test_a_writable_resource_states_no_refusal_of_its_own(self):
        text = _rendered_web(_web())
        assert "read only here" not in text
        assert '"writable":true' in text

    def test_a_row_without_versions_carries_version_zero(self):
        # The screens read a version on every row, and a junction keeps none.
        junction = _web(current_state=True)
        assert '"\\"version\\":0,"' in _rendered_web(junction)
        assert '"\\"version\\":0,"' not in _rendered_web(_web())

    def test_a_resource_with_no_parent_relation_has_no_by_route(self):
        assert "/by/{parent}" not in _rendered_web(_web())

    def test_a_named_parent_the_entity_is_not_listed_by_is_refused(self):
        import pytest
        with pytest.raises(ValueError, match="web_list_by"):
            entity_web_records_plan(_web(web_list_by="nothing"))

    def test_the_tests_try_a_refused_and_an_accepted_page(self):
        text = _rendered_web(_web(), "cpp_http_route_web_records_tests.cpp.mustache")
        assert 'TEST_CASE("widget_web_routes_refuses_a_page_it_cannot_read"' in text
        assert "http_status::bad_gateway" in text

    def test_a_model_without_a_resource_skips_the_record_archetype(self):
        # The generator skips an archetype whose output does not resolve, which is
        # how only the registry's models get a record unit.
        import pytest
        from codegen.core import resolve_output_path
        pattern = "projects/ores.http/{component}/src/routes/{component}/{web_entity}_routes.cpp"
        with pytest.raises(KeyError):
            resolve_output_path(pattern, {"domain_entity": _entity()}, "domain_entity")
        resolved = resolve_output_path(pattern, {"domain_entity": _web()}, "domain_entity")
        assert resolved == "projects/ores.http/demo/src/routes/demo/widget_web_routes.cpp"

    def test_the_catalogue_entry_is_json_naming_the_resource(self):
        import json
        import re
        text = _rendered_web(_web())
        entry = re.search(r'R"json\((.*)\)json"', text).group(1)
        parsed = json.loads(entry)
        assert parsed["key"] == "widgets"
        assert parsed["keyFields"] == ["type"]
        assert parsed["writePermission"] == "demo::widgets:write"

    def test_a_put_subject_that_names_no_resource_is_refused(self):
        import pytest
        entity = _web()
        entity["operations"] = [dict(op, subject="widgets.put") if op["verb"] == "put" else op
                                for op in entity["operations"]]
        with pytest.raises(ValueError, match="does not name its resource"):
            entity_web_records_plan(entity)
