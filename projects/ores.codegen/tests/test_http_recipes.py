"""Tests for the literate HTTP recipe a model's HTTP surface renders.

Run::

    python3 -m pytest projects/ores.codegen/tests/test_http_recipes.py

A recipe is a view of the generated route unit: one document per model, one
section per endpoint, each section exporting its own Hurl file into the
gateway's library. These cases pin what the document states, the request a
generated file sends, and the two facts a reviewer cannot check by eye -- that
a path placeholder never reaches a URL, and that the Hurl variables survive
rendering.
"""
import sys
from pathlib import Path

import pystache

REPO_ROOT = Path(__file__).resolve().parents[3]
sys.path.insert(0, str(REPO_ROOT / "projects/ores.codegen/src"))

from codegen.org_loader import (  # noqa: E402
    _http_sample_path,
    http_recipe_document,
    recipe_org_id,
)

TEMPLATE = (REPO_ROOT / "projects/ores.codegen/library/templates"
            / "http_recipe.org.mustache")


def _route(command, method_upper, pattern, **overrides):
    """An entity route, as `entity_http_route_plan` projects one."""
    base = {
        "command": command,
        "identifier": command,
        "kind": "paged",
        "method": method_upper.lower(),
        "method_upper": method_upper,
        "pattern": pattern,
        "summary": f"{command} a thing",
        "subject": "assets.v1.things.list",
        "requires_session": True,
        "keys": [],
    }
    base.update(overrides)
    return base


def _key(name, cpp_type="std::string"):
    return {"name": name, "cpp_type": cpp_type}


def _document(routes, **overrides):
    args = {
        "component": "assets",
        "plan": {"routes": routes},
        "singular": "thing",
        "plural": "things",
    }
    args.update(overrides)
    return http_recipe_document(**args)


def _render(document):
    return pystache.render(TEMPLATE.read_text(encoding="utf-8"),
                           {"http_recipe": document})


class TestTheDocument:

    def test_one_endpoint_per_route(self):
        document = _document([
            _route("list", "GET", "/api/v1/assets/things"),
            _route("get", "GET", "/api/v1/assets/things/{code}",
                   keys=[_key("code")]),
        ])

        assert document["endpoint_count"] == 2

    def test_the_heading_states_the_method_and_the_path(self):
        document = _document([_route("list", "GET", "/api/v1/assets/things")])

        assert document["endpoints"][0]["heading"] == "GET /api/v1/assets/things"

    def test_the_group_is_the_plural(self):
        document = _document([_route("list", "GET", "/api/v1/assets/things")])

        assert document["group"] == "things"

    def test_the_block_id_and_file_derive_from_the_group_and_command(self):
        document = _document([_route("list", "GET", "/api/v1/assets/things")])
        endpoint = document["endpoints"][0]

        assert endpoint["block"] == "things-list"
        assert endpoint["name"] == "things-list"
        assert endpoint["id"] == recipe_org_id("assets.things-list")

    def test_a_session_endpoint_says_so(self):
        document = _document([
            _route("list", "GET", "/api/v1/assets/things",
                   requires_session=True)])

        assert "needs a session" in document["endpoints"][0]["commentary"]

    def test_a_public_endpoint_says_so(self):
        document = _document([
            _route("status", "POST", "/api/v1/iam/bootstrap/status",
                   requires_session=False)])

        assert "needs no session" in document["endpoints"][0]["commentary"]

    def test_a_key_becomes_a_sentinel(self):
        document = _document([
            _route("get", "GET", "/api/v1/assets/things/{code}",
                   keys=[_key("code")])])

        assert document["endpoints"][0]["pattern"] == "/api/v1/assets/things/__none__"


class TestTheSamplePath:
    """A brace in a URL is a request to a different path than the recipe claims."""

    def test_a_version_read_addresses_a_number(self):
        path = _http_sample_path(_route(
            "version", "GET", "/api/v1/assets/things/{code}/versions/{version}",
            kind="version_read", keys=[_key("code")]))

        assert path == "/api/v1/assets/things/__none__/versions/0"

    def test_a_uuid_key_addresses_a_uuid(self):
        path = _http_sample_path(_route(
            "get", "GET", "/api/v1/assets/things/{id}",
            keys=[_key("id", "boost::uuids::uuid")]))

        assert path == "/api/v1/assets/things/00000000-0000-0000-0000-000000000000"

    def test_no_placeholder_ever_reaches_the_path(self):
        path = _http_sample_path(_route(
            "get", "GET", "/api/v1/assets/{unknown}/{also_unknown}"))

        assert "{" not in path and "}" not in path


class TestTheRenderedRecipe:

    def test_the_hurl_variables_survive_rendering(self):
        text = _render(_document([
            _route("list", "GET", "/api/v1/assets/things")]))

        assert "GET {{base_url}}/api/v1/assets/things" in text
        assert "Authorization: Bearer {{token}}" in text

    def test_a_session_endpoint_carries_the_authorization_line(self):
        text = _render(_document([
            _route("list", "GET", "/api/v1/assets/things",
                   requires_session=True)]))

        assert "Authorization: Bearer {{token}}" in text

    def test_a_public_endpoint_carries_no_authorization_line(self):
        text = _render(_document([
            _route("status", "POST", "/api/v1/iam/bootstrap/status",
                   requires_session=False)]))

        assert "Authorization" not in text

    def test_each_endpoint_exports_its_own_file(self):
        text = _render(_document([
            _route("list", "GET", "/api/v1/assets/things"),
            _route("get", "GET", "/api/v1/assets/things/{code}",
                   keys=[_key("code")]),
        ]))

        assert "#+begin_src hurl :tangle things-list.hurl" in text
        assert "#+begin_src hurl :tangle things-get.hurl" in text

    def test_every_endpoint_asserts_its_status(self):
        text = _render(_document([
            _route("list", "GET", "/api/v1/assets/things")]))

        assert "HTTP 200" in text
