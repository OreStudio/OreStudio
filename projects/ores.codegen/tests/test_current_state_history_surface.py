"""Tests for the versions surface of a current-state entity.

Run::

    python3 -m pytest projects/ores.codegen/tests/test_current_state_history_surface.py

A current-state table has one row per key and no valid_from/valid_to axis, so
it has no versions sub-resource: no version records, no version operations and
no version subscription. The service and the repository already gate their
history methods on ``current_state``; these cases pin the rest of the surface
to the same one flag: the NATS handler and its registrar, the C++ protocol
header, the TypeScript twin, and the derivation the twin renders from. An
ordinary entity must keep every one of them.
"""
import re
import sys
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[3]
sys.path.insert(0, str(REPO_ROOT / "projects/ores.codegen/src"))

from codegen.core import generate_from_model  # noqa: E402
from codegen.org_loader import entity_protocol_messages  # noqa: E402

CODEGEN = REPO_ROOT / "projects/ores.codegen"
DATA_DIR = CODEGEN / "library" / "data"
TEMPLATES_DIR = CODEGEN / "library" / "templates"

# The same current-state model test_current_state_shape.py uses: one row per
# key, the model's own primary key, no audit tail.
CURRENT_STATE_MODEL = """\
:PROPERTIES:
:ID: 00000000-0000-0000-0000-0000000000C2
:END:
#+title: ores.testcomp.current_state_entity
#+type: ores.codegen.entity
#+component: testcomp
#+entity_singular: current_state_entity
#+entity_plural: current_state_entities
#+entity_title: Current State Entity
#+has_tenant_id: true
#+coding_scheme: none
#+image_id: false

An entity whose table carries one row per key and no history.

* Flags
:PROPERTIES:
:schema:        public
:product:       ores
:component:     testcomp
:subcomponent:  api
:has_tenant_id: true
:END:

* Columns

** account_id
:PROPERTIES:
:type:            uuid
:cpp_type:        boost::uuids::uuid
:primary_key:     true
:skip_uuid_check: true
:END:

The key column.

** locked
:PROPERTIES:
:type:     integer
:cpp_type: bool
:nullable: false
:END:

A flag column.

* SQL

** Flags
:PROPERTIES:
:tablename:        ores_testcomp_current_state_entities_tbl
:no_audit_columns: true
:current_state:    true
:END:

* C++

** Repository
:PROPERTIES:
:entity_singular_short: current_state_entity
:entity_plural_short:   current_state_entities
:entity_singular_words: current state entity
:entity_plural_words:   current state entities
:END:
"""

# The same entity without :current_state: -- the bi-temporal control. Dropping
# :no_audit_columns: too gives the control its version + audit tail.
BITEMPORAL_MODEL = CURRENT_STATE_MODEL.replace(
    ":current_state:    true\n", "").replace(
    ":no_audit_columns: true\n", "")


def _render(tmp_path, template, output_name, body):
    model_path = tmp_path / "ores.testcomp.current_state_entity.org"
    model_path.write_text(body, encoding="utf-8")
    output_dir = tmp_path / output_name
    output_dir.mkdir()
    generate_from_model(
        str(model_path), DATA_DIR, TEMPLATES_DIR, output_dir,
        is_processing_batch=True,
        target_template=template, target_output=output_name)
    return (output_dir / output_name).read_text(encoding="utf-8")


def _enriched_entity(**overrides):
    """The enriched entity dict ``entity_protocol_messages`` reads."""
    entity = {
        "component": "testcomp",
        "entity_singular": "current_state_entity",
        "entity_plural": "current_state_entities",
        "entity_plural_short": "current_state_entities",
        "primary_key": {
            "column": "account_id",
            "columns": [{"column": "account_id", "is_uuid": True}],
        },
    }
    entity.update(overrides)
    # The version column, which the enrichment derives from the two shape
    # flags. Read here rather than stated, so a case cannot claim a version
    # column and a current-state table at once.
    entity.setdefault("has_audit_columns",
                      not entity.get("current_state")
                      and not entity.get("no_audit_columns"))
    return entity


def _message_names(entity):
    return [m["name"] for m in entity_protocol_messages(entity)]


def _handler_service_calls(handler):
    """Every ``svc.<method>(`` the handler template emits, in order."""
    return re.findall(r"\bsvc\.([A-Za-z_][A-Za-z0-9_]*)\s*\(", handler)


def test_a_current_state_entity_derives_no_history_message():
    names = _message_names(_enriched_entity(current_state=True))
    assert not [n for n in names if "_history_" in n]


def test_an_ordinary_entity_still_derives_the_versions_pair():
    names = _message_names(_enriched_entity(has_audit_columns=True))
    assert names[-4:] == [
        "list_current_state_entity_versions_request",
        "list_current_state_entity_versions_response",
        "get_current_state_entity_version_request",
        "get_current_state_entity_version_response"]


def test_an_entity_with_no_version_column_derives_no_versions_pair():
    """The gate is the version column, which the model states separately from
    the current-state flag. An audit-less table with a validity window keeps
    history it cannot address a version in, so it derives no version key, no
    versions filter and no versions operation."""
    names = _message_names(_enriched_entity(no_audit_columns=True))
    assert not [n for n in names if "version" in n]


def test_the_current_state_derivation_keeps_the_rest_of_the_crud_set():
    names = _message_names(_enriched_entity(current_state=True))
    assert names == [
        "current_state_entity_key",
        "current_state_entity_write",
        "current_state_entity_change",
        "current_state_entity_removal",
        "current_state_entity_lookup",
        "list_current_state_entities_request",
        "list_current_state_entities_response",
        "get_current_state_entity_request",
        "get_current_state_entity_response",
        "get_many_current_state_entities_request",
        "get_many_current_state_entities_response",
        "put_current_state_entity_request",
        "put_current_state_entity_response",
        "put_many_current_state_entities_request",
        "put_many_current_state_entities_response",
        "delete_current_state_entity_request",
        "delete_current_state_entity_response",
        "delete_many_current_state_entities_request",
        "delete_many_current_state_entities_response",
    ]


def test_a_current_state_handler_has_no_history_method(tmp_path):
    handler = _render(tmp_path, "cpp_nats_handler.hpp.mustache",
                      "current_state_entity_handler.hpp", CURRENT_STATE_MODEL)
    assert "get_current_state_entity_history_request" not in handler
    assert "get_current_state_entity_history_response" not in handler
    assert "history(" not in handler
    assert "svc.list_current_state_entities(" in handler
    assert "svc.put_current_state_entity(" in handler


def test_an_ordinary_handler_serves_the_versions_operations(tmp_path):
    handler = _render(tmp_path, "cpp_nats_handler.hpp.mustache",
                      "current_state_entity_handler.hpp", BITEMPORAL_MODEL)
    assert "void list_current_state_entity_versions(ores::nats::message msg)" in handler
    assert "void get_current_state_entity_version(ores::nats::message msg)" in handler
    assert "svc.list_current_state_entity_versions(*req)" in handler
    assert "svc.get_current_state_entity_version(*req)" in handler


def test_the_handler_calls_only_methods_the_service_declares(tmp_path):
    """The regression this whole gate fixes: a history method the service
    does not declare. The check is general, not history-specific."""
    handler = _render(tmp_path, "cpp_nats_handler.hpp.mustache",
                      "current_state_entity_handler.hpp", CURRENT_STATE_MODEL)
    service = _render(tmp_path, "cpp_service.hpp.mustache",
                      "current_state_entity_service.hpp", CURRENT_STATE_MODEL)
    missing = [call for call in _handler_service_calls(handler)
               if call not in service]
    assert missing == []
    assert "_history" not in handler


def test_a_current_state_registrar_subscribes_to_no_history_subject(tmp_path):
    registrar = _render(tmp_path, "cpp_nats_registrar.cpp.mustache",
                        "current_state_entity_registrar.cpp", CURRENT_STATE_MODEL)
    assert "_versions" not in registrar
    assert "list_current_state_entities_request::nats_subject" in registrar


def test_an_ordinary_registrar_subscribes_to_the_versions_subjects(tmp_path):
    registrar = _render(tmp_path, "cpp_nats_registrar.cpp.mustache",
                        "current_state_entity_registrar.cpp", BITEMPORAL_MODEL)
    assert "h->list_current_state_entity_versions(std::move(msg))" in registrar
    assert "list_current_state_entity_versions_request::nats_subject" in registrar
    assert "get_current_state_entity_version_request::nats_subject" in registrar


def test_a_current_state_protocol_header_states_no_history_pair(tmp_path):
    protocol = _render(tmp_path, "cpp_protocol.hpp.mustache",
                       "current_state_entity_protocol.hpp", CURRENT_STATE_MODEL)
    assert "history" not in protocol
    assert "versions" not in protocol
    assert "struct list_current_state_entities_request {" in protocol


def test_an_ordinary_protocol_header_keeps_the_history_pair(tmp_path):
    protocol = _render(tmp_path, "cpp_protocol.hpp.mustache",
                       "current_state_entity_protocol.hpp", BITEMPORAL_MODEL)
    assert "struct list_current_state_entity_versions_request {" in protocol
    assert "struct get_current_state_entity_version_response {" in protocol
    assert '"testcomp.v1.current_state_entities_versions.list"' in protocol


def test_a_current_state_typescript_twin_states_no_versions_surface(tmp_path):
    protocol = _render(tmp_path, "ts_protocol.ts.mustache",
                       "current_state_entity_protocol.ts", CURRENT_STATE_MODEL)
    assert "Version" not in protocol
    assert "versions" not in protocol
    assert ('list_current_state_entities_request: '
            '"testcomp.v1.current_state_entities.list"') in protocol


def test_an_ordinary_typescript_twin_keeps_the_versions_surface(tmp_path):
    protocol = _render(tmp_path, "ts_protocol.ts.mustache",
                       "current_state_entity_protocol.ts", BITEMPORAL_MODEL)
    assert "export interface ListCurrentStateEntityVersionsRequest {" in protocol
    assert "export interface GetCurrentStateEntityVersionResponse {" in protocol
    assert ('list_current_state_entity_versions_request: '
            '"testcomp.v1.current_state_entities_versions.list"') in protocol


def test_the_cpp_and_typescript_twins_agree_on_the_current_state_set(tmp_path):
    """The derived list and the C++ header read the same flag, so the two
    twins cannot disagree about the versions sub-resource."""
    derived = set(_message_names(_enriched_entity(current_state=True)))
    protocol = _render(tmp_path, "cpp_protocol.hpp.mustache",
                       "current_state_entity_protocol.hpp", CURRENT_STATE_MODEL)
    for name in derived:
        assert f"struct {name} {{" in protocol
    assert "history" not in protocol
