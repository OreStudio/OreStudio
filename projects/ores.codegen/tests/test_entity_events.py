"""Tests for the event a change to an entity is announced by.

Run::

    python3 -m pytest projects/ores.codegen/tests/test_entity_events.py

An event is one payload addressed by three subjects: the collection's prefix
names the events, and the last segment is the action the payload reports. So
the payload is stated once, the subjects beside it, and the store's
notification carries the action rather than leaving a subscriber to infer it.
These cases render the templates that state each of those and hold them to it.
"""
import sys
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[3]
sys.path.insert(0, str(REPO_ROOT / "projects/ores.codegen/src"))

from codegen.core import generate_from_model  # noqa: E402
from codegen.org_loader import entity_protocol_messages  # noqa: E402

CODEGEN = REPO_ROOT / "projects/ores.codegen"
DATA_DIR = CODEGEN / "library" / "data"
TEMPLATES_DIR = CODEGEN / "library" / "templates"

MODEL = """\
:PROPERTIES:
:ID: 00000000-0000-0000-0000-0000000000E1
:END:
#+title: ores.testcomp.evented_entity
#+type: ores.codegen.entity
#+component: testcomp
#+entity_singular: evented_entity
#+entity_plural: evented_entities
#+entity_title: Evented Entity
#+has_tenant_id: true
#+coding_scheme: none
#+image_id: false

An entity whose changes are announced.

* Flags
:PROPERTIES:
:schema:        public
:product:       ores
:component:     testcomp
:subcomponent:  api
:has_tenant_id: true
:END:

* Columns

** code
:PROPERTIES:
:type:        text
:cpp_type:    std::string
:primary_key: true
:skip_uuid_check: true
:END:

The key column.

** name
:PROPERTIES:
:type:     text
:cpp_type: std::string
:nullable: false
:END:

A plain column.

* SQL

** Flags
:PROPERTIES:
:tablename: ores_testcomp_evented_entities_tbl
:END:

* C++
"""


def _render(tmp_path, template, output_name):
    model_path = tmp_path / "ores.testcomp.evented_entity.org"
    model_path.write_text(MODEL, encoding="utf-8")
    output_dir = tmp_path / output_name
    output_dir.mkdir()
    generate_from_model(
        str(model_path), DATA_DIR, TEMPLATES_DIR, output_dir,
        is_processing_batch=True,
        target_template=template, target_output=output_name)
    return (output_dir / output_name).read_text(encoding="utf-8")


def test_the_event_carries_what_the_specification_states():
    entity = {
        "component": "testcomp",
        "entity_singular": "evented_entity",
        "entity_plural": "evented_entities",
        "entity_plural_short": "evented_entities",
        "has_audit_columns": True,
        "primary_key": {
            "column": "code",
            "columns": [{"column": "code", "cpp_type": "std::string"}],
        },
        "columns": [{"column": "name", "cpp_type": "std::string"}],
    }
    messages = {m["name"]: m for m in entity_protocol_messages(entity)}
    fields = {f["name"]: f["cpp_type"]
              for f in messages["evented_entity_event"]["fields"]}
    assert fields == {
        "event_id": "boost::uuids::uuid",
        "key": "evented_entity_key",
        "action": "std::string",
        "version": "std::uint32_t",
        "occurred_at": "std::chrono::system_clock::time_point",
        "correlation_id": "std::optional<std::string>",
    }
    # An event is an announcement, so it is addressed by its subjects and
    # carries no subject of its own.
    assert "subject" not in messages["evented_entity_event"]


def test_both_protocol_twins_state_the_three_subjects(tmp_path):
    cpp = _render(tmp_path, "cpp_protocol.hpp.mustache",
                  "evented_entity_protocol.hpp")
    assert "namespace evented_entity_event_subjects {" in cpp
    assert 'created = "testcomp.v1.evented_entities_events.created"' in cpp
    assert 'updated = "testcomp.v1.evented_entities_events.updated"' in cpp
    assert 'deleted = "testcomp.v1.evented_entities_events.deleted"' in cpp
    assert "struct evented_entity_event {" in cpp

    ts = _render(tmp_path, "ts_protocol.ts.mustache",
                 "evented_entity_protocol.ts")
    assert "export const eventSubjects = {" in ts
    assert 'created: "testcomp.v1.evented_entities_events.created"' in ts
    assert 'deleted: "testcomp.v1.evented_entities_events.deleted"' in ts


def test_the_event_states_its_subject_prefix_and_its_own_conversion(tmp_path):
    header = _render(tmp_path, "cpp_nats_changed_event.hpp.mustache",
                     "evented_entity_event.hpp")
    assert 'subject_prefix = "testcomp.v1.evented_entities_events"' in header
    assert "from_notification(const entity_event_notification& notification)" in header
    # The key is the entity's own record, so the conversion is the one place
    # that knows which columns it carries.
    assert "rfl::json::read<ores::testcomp::messaging::evented_entity_key>" in header


def test_the_registrar_takes_the_subject_from_the_action(tmp_path):
    registrar = _render(tmp_path, "cpp_nats_event_registrar.cpp.mustache",
                        "evented_entity_event_registrar.cpp")
    assert "register_entity_event_mapping<" in registrar
    assert "evented_entity_event>" in registrar
    # The subject is built from the action, not stated: clang-format may wrap
    # the call, so the two halves are asserted apart.
    assert "event_subject<" in registrar
    assert "e.action)" in registrar


def test_the_notify_trigger_states_the_action_and_the_key(tmp_path):
    trigger = _render(tmp_path, "sql_schema_notify_trigger.mustache",
                      "evented_entity_notify_trigger_create.sql")
    # The payload the specification states, and nothing inferred: the action is
    # decided by the store, which is what knows whether it closed a row or
    # wrote a new version of one.
    for field in ("'event_id'", "'key'", "'action'", "'version'",
                  "'occurred_at'", "'correlation_id'", "'tenant_id'"):
        assert field in trigger, field
    assert "change_action := 'created'" in trigger
    assert "change_action := 'updated'" in trigger
    assert "change_action := 'deleted'" in trigger
    assert "changed_key := jsonb_build_object('code', changed_code)" in trigger
    # The logical change is announced once: a versioned table's update is the
    # internal close of the current row, and the insert that follows it says
    # whether the row was created or updated.
    assert "return null;" in trigger
