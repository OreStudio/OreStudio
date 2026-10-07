"""Tests for the HTTP route facet's operation projection.

Run::

    python3 -m pytest projects/ores.codegen/tests/test_http_operation_routes.py

An operation model states its whole protocol rather than deriving a verb set,
so the HTTP route unit renders one route per declared message that carries a
subject, a response and its own exposure. These cases pin the addressing (the
path is the subject transliterated), what a route carries, the security rule
(the message's own ``:auth:`` property decides the position) and the exposure
rule (a message states ``:http_route: true`` or renders no route), because a
declared protocol holds operations meant for NATS callers alone.
"""
import sys
from pathlib import Path

import pystache

REPO_ROOT = Path(__file__).resolve().parents[3]
sys.path.insert(0, str(REPO_ROOT / "projects/ores.codegen/src"))

from codegen.generate import resolve_targets  # noqa: E402
from codegen.org_loader import (  # noqa: E402
    load_org_operation_model,
    operation_http_route_plan,
)

CODEGEN_BASE = REPO_ROOT / "projects" / "ores.codegen"
TEMPLATES = CODEGEN_BASE / "library" / "templates"
IAM_MODELING = REPO_ROOT / "projects" / "ores.iam" / "modeling"
ACCOUNT_MESSAGES = IAM_MODELING / "ores.iam.account_messages.org"
BOOTSTRAP_MESSAGES = IAM_MODELING / "ores.iam.bootstrap_messages.org"
LOGIN_MESSAGES = IAM_MODELING / "ores.iam.login_messages.org"

ROUTE_OUTPUTS = [
    "projects/ores.http/iam/include/ores.http/routes/iam/account_operations_routes.hpp",
    "projects/ores.http/iam/src/routes/iam/account_operations_routes.cpp",
    "projects/ores.http/iam/tests/account_operations_routes_tests.cpp",
]

OPERATION_PAGES = (
    "ores.cpp.http-route.operation_header.org",
    "ores.cpp.http-route.operation_implementation.org",
    "ores.cpp.http-route.operation_tests.org",
)


def _command(name, subject, response="demo_response", public=False,
             expose=True, fields=("value",)):
    """A shell-projected declared message, shaped the way the loader hands it.

    ``expose`` is the message's own ``:http_route:`` decision; the loader
    carries it on the projected command as ``http_route``.
    """
    positionals = [{"name": field, "cpp_type": "std::string"} for field in fields]
    return {
        "command": name.replace("_", "-").replace("-request", ""),
        "identifier": name.replace("_", "-").replace("-request", "").replace("-", "_"),
        "request": name,
        "response_type": response,
        "subject": subject,
        "public": public,
        "http_route": expose,
        "positionals": positionals,
        "flags": [],
        "positional_count": len(positionals),
        "usage": name,
    }


def _model(commands):
    return {
        "component": "demo",
        "subcomponent": "api",
        "entity_singular": "demo_operations",
        "namespace": "ores::demo::messaging",
        "shell_commands": commands,
    }


def _plan(commands):
    return operation_http_route_plan(_model(commands))


def _rendered(template_name, model):
    model = dict(model)
    model["http_route"] = operation_http_route_plan(model)
    template = (TEMPLATES / template_name).read_text()
    return pystache.render(template, {"cpp_license": "", "operation": model})


class TestTheRouteTable:
    """One route per declared message, addressed by its own subject."""

    def test_a_message_becomes_a_post_at_its_subject(self):
        plan = _plan([_command("lock_widget_request", "demo.v1.widgets.lock")])
        assert [(r["method"], r["pattern"]) for r in plan["routes"]] == [
            ("post", "/api/v1/demo/widgets/lock"),
        ]

    def test_a_subject_without_an_action_keeps_the_resource_as_the_path(self):
        # A three-segment subject names the resource and no action, so the
        # resource is the whole of what the path adds.
        plan = _plan([_command("status_request", "demo.v1.scheduler.status")])
        assert [r["pattern"] for r in plan["routes"]] == [
            "/api/v1/demo/scheduler/status",
        ]

    def test_the_route_carries_the_canonical_request_and_response(self):
        plan = _plan([_command("lock_widget_request", "demo.v1.widgets.lock",
                               response="lock_widget_response")])
        route = plan["routes"][0]
        assert route["request"] == "lock_widget_request"
        assert route["response_type"] == "lock_widget_response"
        assert route["subject"] == "demo.v1.widgets.lock"

    def test_a_message_that_states_no_field_takes_an_empty_body(self):
        plan = _plan([_command("status_request", "demo.v1.scheduler.status",
                               fields=())])
        assert plan["routes"][0]["has_fields"] is False

    def test_a_message_with_fields_parses_a_body(self):
        plan = _plan([_command("lock_widget_request", "demo.v1.widgets.lock")])
        assert plan["routes"][0]["has_fields"] is True

    def test_a_subject_off_the_canonical_grammar_is_refused(self):
        # A route cannot be invented from a subject that states no version, so
        # the model is refused rather than served a wrong path.
        try:
            _plan([_command("lock_widget_request", "widgets.lock")])
        except ValueError as exc:
            assert "widgets.lock" in str(exc)
        else:
            raise AssertionError("a malformed subject was not refused")


class TestTheSecurityRule:
    """The message's own declaration decides the position."""

    def test_a_message_that_needs_a_session_requires_authentication(self):
        plan = _plan([_command("logout_request", "demo.v1.auth.logout")])
        assert plan["routes"][0]["requires_session"] is True

    def test_a_public_message_states_the_explicit_public_form(self):
        plan = _plan([_command("login_request", "demo.v1.auth.login",
                               public=True)])
        assert plan["routes"][0]["requires_session"] is False

    def test_the_unit_renders_auth_required_where_the_message_requires_it(self):
        model = _model([_command("logout_request", "demo.v1.auth.logout")])
        text = _rendered("cpp_http_route_operation_implementation.cpp.mustache",
                         model)
        assert ".auth_required()" in text
        assert ".auth_optional()" not in text

    def test_a_public_operation_renders_the_explicit_public_form(self):
        model = _model([_command("login_request", "demo.v1.auth.login",
                                 public=True)])
        text = _rendered("cpp_http_route_operation_implementation.cpp.mustache",
                         model)
        assert ".auth_optional()" in text
        assert ".auth_required()" not in text

    def test_the_unit_forwards_the_callers_token_not_its_own(self):
        model = _model([_command("logout_request", "demo.v1.auth.logout")])
        text = _rendered("cpp_http_route_operation_implementation.cpp.mustache",
                         model)
        assert "get_bearer_token()" in text
        assert "with_delegation(" in text


class TestTheExposureRule:
    """A message states its own HTTP exposure; the default is not exposed."""

    def test_a_message_without_the_exposure_property_renders_no_route(self):
        plan = _plan([_command("lock_widget_request", "demo.v1.widgets.lock",
                               expose=False)])
        assert plan["routes"] == []
        assert plan["route_count"] == 0

    def test_a_message_with_the_exposure_property_renders_a_route(self):
        plan = _plan([_command("lock_widget_request", "demo.v1.widgets.lock",
                               expose=True)])
        assert [(r["method"], r["pattern"]) for r in plan["routes"]] == [
            ("post", "/api/v1/demo/widgets/lock"),
        ]

    def test_only_the_exposed_message_of_a_model_becomes_a_route(self):
        plan = _plan([
            _command("lock_widget_request", "demo.v1.widgets.lock", expose=True),
            _command("unlock_widget_request", "demo.v1.widgets.unlock", expose=False),
        ])
        assert [r["pattern"] for r in plan["routes"]] == [
            "/api/v1/demo/widgets/lock",
        ]

    def test_the_unit_of_a_model_with_no_exposed_message_registers_nothing(self):
        model = _model([_command("lock_widget_request", "demo.v1.widgets.lock",
                                 expose=False)])
        text = _rendered("cpp_http_route_operation_implementation.cpp.mustache",
                         model)
        assert "add_route" not in text
        assert "routes registered: " in text

    def test_the_default_is_read_from_the_loader_not_the_helper(self, tmp_path):
        # The production default, read from the loader rather than from a
        # hand-built dict: with every :http_route: removed, a model that opted
        # into the facet still exposes nothing.
        text = LOGIN_MESSAGES.read_text(encoding="utf-8")
        assert ":http_route: true" in text
        stripped = text.replace(":http_route: true\n", "")
        model = tmp_path / "ores.iam.login_messages.org"
        model.write_text(stripped, encoding="utf-8")

        operation = load_org_operation_model(model)["operation"]
        assert operation_http_route_plan(operation)["routes"] == []

    def test_a_value_that_is_not_yes_is_refused(self, tmp_path):
        # The property is a yes-or-nothing flag: a spelling that means
        # something else is an error rather than a silent "not exposed".
        text = LOGIN_MESSAGES.read_text(encoding="utf-8")
        model = tmp_path / "ores.iam.login_messages.org"
        model.write_text(text.replace(":http_route: true\n", ":http_route: maybe\n"),
                         encoding="utf-8")
        try:
            load_org_operation_model(model)
        except ValueError as exc:
            assert "http_route" in str(exc)
        else:
            raise AssertionError("an unknown :http_route: value was not refused")

    def test_the_iam_service_login_and_tenant_provisioning_stay_unexposed(self):
        # The two operations the gateway must not serve: neither was an HTTP
        # endpoint before the routes were generated.
        for path, forbidden in (
            (BOOTSTRAP_MESSAGES, "/api/v1/iam/bootstrap/provision-tenant"),
            (LOGIN_MESSAGES, "/api/v1/iam/auth/service-login"),
        ):
            operation = load_org_operation_model(path)["operation"]
            patterns = [r["pattern"]
                        for r in operation_http_route_plan(operation)["routes"]]
            assert forbidden not in patterns, path.name

    def test_the_iam_operations_the_old_file_served_are_still_exposed(self):
        operation = load_org_operation_model(LOGIN_MESSAGES)["operation"]
        patterns = [r["pattern"]
                    for r in operation_http_route_plan(operation)["routes"]]
        assert "/api/v1/iam/ops/login" in patterns
        assert "/api/v1/iam/ops/logout" in patterns


class TestTheFacetResolves:
    """Opt-in, disabled by default."""

    def test_an_enabled_operation_model_renders_a_route_unit(self):
        units, model_type, _ = resolve_targets(
            ACCOUNT_MESSAGES, CODEGEN_BASE, address="ores.cpp.http-route",
            properties={"ores.cpp.http-route.enabled": "true"})
        assert model_type == "operation"
        assert [unit["output"] for unit in units] == ROUTE_OUTPUTS

    def test_a_disabled_operation_model_renders_nothing(self, tmp_path):
        # The same model with the one property removed, so the empty
        # intersection is the opt-in and nothing else.
        text = ACCOUNT_MESSAGES.read_text(encoding="utf-8")
        assert ":ores.cpp.http-route.enabled: true" in text
        stripped = text.replace(":ores.cpp.http-route.enabled: true\n", "")
        model = tmp_path / "ores.iam.account_messages.org"
        model.write_text(stripped, encoding="utf-8")

        units, _, _ = resolve_targets(
            model, CODEGEN_BASE, address="ores.cpp.http-route")
        assert [unit for unit in units if "ores.http/" in unit["output"]] == []

    def test_the_operation_pages_admit_an_operation(self):
        for name in OPERATION_PAGES:
            page = (TEMPLATES / name).read_text(encoding="utf-8")
            declared = [line for line in page.splitlines()
                        if line.startswith("#+model_types:")]
            assert declared, name
            assert "operation" in declared[0], name

    def test_every_iam_operation_subject_derives_a_path(self):
        # The canonical grammar holds across every declared operation the iam
        # models state, so no opted-in model would be refused at render time.
        from codegen.org_loader import load_org_operation_model
        for model in sorted(IAM_MODELING.glob("ores.iam.*_messages.org")):
            operation = load_org_operation_model(model)["operation"]
            plan = operation_http_route_plan(operation)
            for route in plan["routes"]:
                assert route["pattern"].startswith("/api/v1/iam/")
