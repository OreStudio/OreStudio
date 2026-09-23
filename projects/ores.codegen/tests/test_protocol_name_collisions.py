"""Tests for the two protocol names that a model can collide with.

Run::

    python3 -m pytest projects/ores.codegen/tests/test_protocol_name_collisions.py

An entity model derives its protocol names from its own singular, and two
of those names are not free to take. The versions sub-resource is named
after the singular, so a sibling entity named ``<singular>_version`` in
the same component owns every one of the same names. The response
envelope states its outcome in a member named ``result``, so an entity of
that name cannot also use it for its payload. Before these cases, the
regenerated C++ for a component holding ``app`` and ``app_version``
redeclared five structs across two headers, and the entity named
``result`` declared one struct with two members of the same name, so
neither the header nor the component's core library compiled.

The cases read the derived message list and, for the names, the rendered
headers, so they fail for the reason the build failed rather than on a
name written twice in a test.
"""
import re
import sys
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[3]
sys.path.insert(0, str(REPO_ROOT / "projects/ores.codegen/src"))

from codegen.core import generate_from_model  # noqa: E402
from codegen.org_loader import (  # noqa: E402
    entity_protocol_messages,
    protocol_operations,
    sibling_entity_singulars,
)

CODEGEN = REPO_ROOT / "projects/ores.codegen"
DATA_DIR = CODEGEN / "library" / "data"
TEMPLATES_DIR = CODEGEN / "library" / "templates"

ENTITY_MODEL = """\
:PROPERTIES:
:ID: 00000000-0000-0000-0000-0000000000{uid}
:END:
#+title: ores.testcomp.{singular}
#+type: ores.codegen.entity
#+component: testcomp
#+entity_singular: {singular}
#+entity_plural: {plural}
#+entity_title: {title}
#+has_tenant_id: true
#+coding_scheme: none
#+image_id: false

A {title} for the naming cases.

* Flags
:PROPERTIES:
:schema:        public
:product:       ores
:component:     testcomp
:subcomponent:  api
:has_tenant_id: true
:END:

* Columns

** id
:PROPERTIES:
:type:            uuid
:cpp_type:        boost::uuids::uuid
:primary_key:     true
:skip_uuid_check: true
:END:

The key column.

* SQL

** Flags
:PROPERTIES:
:tablename: ores_testcomp_{plural}_tbl
:END:

* C++

** Repository
:PROPERTIES:
:entity_singular_short: {singular}
:entity_plural_short:   {plural}
:entity_singular_words: {title_lower}
:entity_plural_words:   {title_lower}s
:END:
"""

NON_ENTITY_MODEL = """\
:PROPERTIES:
:ID: 00000000-0000-0000-0000-0000000000F1
:END:
#+title: ores.testcomp.notes
#+type: ores.codegen.module
"""


def _entity_model(singular, plural, title, uid):
    return ENTITY_MODEL.format(
        singular=singular, plural=plural, title=title,
        title_lower=title.lower(), uid=uid)


def _write_models(tmp_path, models):
    for singular, body in models.items():
        (tmp_path / f"ores.testcomp.{singular}.org").write_text(
            body, encoding="utf-8")


def _protocol_for(tmp_path, models, singular):
    """Render one entity's C++ protocol header, with its siblings beside it."""
    _write_models(tmp_path, models)
    out = tmp_path / "out"
    out.mkdir(exist_ok=True)
    name = f"{singular}_protocol.hpp"
    generate_from_model(
        str(tmp_path / f"ores.testcomp.{singular}.org"),
        DATA_DIR, TEMPLATES_DIR, out,
        is_processing_batch=True,
        target_template="cpp_protocol.hpp.mustache", target_output=name)
    return (out / name).read_text(encoding="utf-8")


def _struct_names(header):
    return set(re.findall(r"^struct ([A-Za-z_][A-Za-z0-9_]*)", header, re.M))


def _app_and_app_version():
    return {
        "app": _entity_model("app", "apps", "App", "A1"),
        "app_version": _entity_model(
            "app_version", "app_versions", "App Version", "A2"),
    }


def _enriched(singular, plural):
    """The enriched entity dict ``entity_protocol_messages`` reads."""
    return {
        "component": "testcomp",
        "entity_singular": singular,
        "entity_plural": plural,
        "entity_plural_short": plural,
        "has_audit_columns": True,
        "primary_key": {
            "column": "id",
            "columns": [{"column": "id", "is_uuid": True}],
        },
    }


def test_the_sibling_scan_reads_every_entity_beside_the_model(tmp_path):
    models = _app_and_app_version()
    models["notes"] = NON_ENTITY_MODEL
    _write_models(tmp_path, models)
    assert sibling_entity_singulars(tmp_path / "ores.testcomp.app.org") == \
        frozenset({"app", "app_version"})


def test_an_entity_and_its_version_sibling_share_no_struct_name(tmp_path):
    models = _app_and_app_version()
    app = _protocol_for(tmp_path, models, "app")
    version = _protocol_for(tmp_path, models, "app_version")
    assert _struct_names(app) & _struct_names(version) == set()


def test_the_version_sibling_keeps_the_names_its_own_model_states(tmp_path):
    models = _app_and_app_version()
    version = _protocol_for(tmp_path, models, "app_version")
    assert "struct app_version_key {" in version
    assert "struct list_app_versions_request {" in version
    assert "struct list_app_versions_response {" in version
    assert "struct get_app_version_request {" in version
    assert "struct get_app_version_response {" in version


def test_the_facet_steps_aside_to_the_plural_when_a_sibling_takes_the_singular(tmp_path):
    app = _protocol_for(tmp_path, _app_and_app_version(), "app")
    assert "struct apps_version_key {" in app
    assert "struct apps_versions_filter {" in app
    assert "struct list_apps_versions_request {" in app
    assert "struct list_apps_versions_response {" in app
    assert "struct get_apps_version_request {" in app
    assert "struct get_apps_version_response {" in app


def test_the_facet_keeps_the_singular_when_no_sibling_takes_it(tmp_path):
    app = _protocol_for(tmp_path, {"app": _entity_model(
        "app", "apps", "App", "A1")}, "app")
    assert "struct app_version_key {" in app
    assert "struct list_app_versions_request {" in app


def test_the_renamed_facet_still_states_the_versions_verbs():
    messages = entity_protocol_messages(
        _enriched("app", "apps"), frozenset({"app", "app_version"}))
    operations = protocol_operations(messages)
    assert {operation["verb"] for operation in operations} >= {
        "list_versions", "get_version"}
    assert {operation["method"] for operation in operations} >= {
        "list_apps_versions", "get_apps_version"}


def test_a_result_entity_response_has_no_duplicate_member(tmp_path):
    header = _protocol_for(
        tmp_path, {"result": _entity_model("result", "results", "Result", "B1")},
        "result")
    assert ("    ores::utility::domain::result result;\n"
            "    std::optional<ores::testcomp::domain::result> result_value;\n"
            in header)
    assert ("    ores::utility::domain::result result;\n"
            "    ores::testcomp::domain::result result_value;\n"
            in header)


def test_an_ordinary_entity_keeps_its_own_payload_member_name(tmp_path):
    header = _protocol_for(
        tmp_path, {"app": _entity_model("app", "apps", "App", "A1")}, "app")
    assert "    ores::utility::domain::result result;\n" \
           "    std::optional<ores::testcomp::domain::app> app;\n" in header
