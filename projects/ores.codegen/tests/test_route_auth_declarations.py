"""Tests for the route authentication-declaration gate.

The gate only has value if it can fail. A route registered without an
authentication position is exactly the hole the builder's runtime refusal
closes, so these tests pin both halves: the real tree states every position,
and a chain that states none is reported.

Run::

    python3 -m pytest projects/ores.codegen/tests/test_route_auth_declarations.py
"""
import sys
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[3]
sys.path.insert(0, str(REPO_ROOT / "projects/ores.codegen/scripts"))

import check_route_auth_declarations as check  # noqa: E402


def _write(tmp_path: Path, body: str) -> Path:
    source = tmp_path / "src" / "routes" / "demo_routes.cpp"
    source.parent.mkdir(parents=True, exist_ok=True)
    source.write_text(body, encoding="utf-8")
    return source


def test_every_registered_route_states_a_position():
    assert check.check() == []


def test_a_chain_that_states_nothing_is_reported(tmp_path):
    _write(tmp_path, """
void register_all(router r) {
    r.add_route(r.get("/demo")
                    .summary("List")
                    .handler(handle));
}
""")
    violations = check.check(tmp_path)
    assert [v for v in violations if "GET /demo states no authentication" in v]


def test_the_explicit_public_form_satisfies_the_gate(tmp_path):
    _write(tmp_path, """
void register_all(router r) {
    r.add_route(r.get("/demo")
                    .summary("List")
                    .auth_optional()
                    .handler(handle));
}
""")
    assert not [v for v in check.check(tmp_path) if "states no authentication" in v]


def test_roles_count_as_a_statement(tmp_path):
    _write(tmp_path, """
void register_all(router r) {
    r.add_route(r.delete_("/demo")
                    .roles({"TenantAdmin"})
                    .handler(handle));
}
""")
    assert not [v for v in check.check(tmp_path) if "states no authentication" in v]


def test_a_named_builder_is_resolved(tmp_path):
    _write(tmp_path, """
void register_all(router r) {
    auto list_route = r.get("/demo")
                          .auth_required()
                          .handler(handle);
    r.add_route(list_route.build());
}
""")
    assert not [v for v in check.check(tmp_path) if "states no authentication" in v]


def test_a_named_builder_that_states_nothing_is_reported(tmp_path):
    _write(tmp_path, """
void register_all(router r) {
    auto list_route = r.get("/demo")
                          .handler(handle);
    r.add_route(list_route.build());
}
""")
    assert [v for v in check.check(tmp_path) if "GET /demo states no authentication" in v]


def test_a_declaration_named_in_a_comment_does_not_count(tmp_path):
    _write(tmp_path, """
void register_all(router r) {
    // A route must call .auth_optional() or .auth_required() here.
    r.add_route(r.get("/demo")
                    .handler(handle));
}
""")
    assert [v for v in check.check(tmp_path) if "GET /demo states no authentication" in v]


def test_a_route_declaration_is_not_mistaken_for_a_registration(tmp_path):
    _write(tmp_path, """
class router {
    void add_route(const domain::route& route);
};
""")
    assert not [v for v in check.check(tmp_path) if "states no authentication" in v]


def test_the_scan_is_not_vacuous(tmp_path):
    """A parser that found nothing would pass every tree."""
    assert [v for v in check.check(tmp_path) if "scanned almost nothing" in v]
