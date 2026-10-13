"""Tests for the junction change events.

Run::

    python3 -m pytest projects/ores.codegen/tests/test_junction_events.py

A junction that states ``:events: true`` announces its changes like any entity:
a notify trigger names the link by both columns, a registrar publishes it on the
junction's events collection, and a model that does not ask for events gets none
of it. A link is closed by ending its validity and never deleted, so the
trigger has to tell a link going away from a link replaced by a newer version.
"""
import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent))

from test_junction_history import CODEGEN_BASE, JUNCTION, render  # noqa: E402
from codegen.generate import resolve_targets  # noqa: E402

EVENTS = ":events: true\n"


def test_the_trigger_names_the_link_by_both_columns(tmp_path):
    sql = render(tmp_path, "sql_schema_notify_trigger.mustache", EVENTS)

    assert "'key', jsonb_build_object('owner_id', changed_owner_id, 'item_code', changed_item_code)::text" in sql
    assert "'entity', 'ores.testcomp.test_link'" in sql
    assert "pg_notify('ores_testcomp_test_links', notification_payload::text)" in sql


def test_a_link_going_away_is_told_and_a_replacement_is_not(tmp_path):
    sql = render(tmp_path, "sql_schema_notify_trigger.mustache", EVENTS)

    # The close is an update, judged when the transaction ends, because the
    # newer version of a replaced link does not exist yet when the close runs.
    assert "create constraint trigger ores_testcomp_test_links_notify_close_trg" in sql
    assert "deferrable initially deferred" in sql
    assert "change_action := 'deleted';" in sql
    assert "and valid_to = ores_utility_infinity_timestamp_fn()" in sql


def test_the_events_are_published_on_the_junctions_collection(tmp_path):
    header = render(tmp_path, "cpp_nats_changed_event.hpp.mustache", EVENTS)
    registrar = render(tmp_path, "cpp_nats_event_registrar.cpp.mustache", EVENTS)

    assert 'subject_prefix = "testcomp.v1.test_links_events"' in header
    assert "test_link_key" in header
    assert "register_test_link_event_mapping" in registrar
    assert '"ores_testcomp_test_links"' in registrar


def test_a_junction_that_does_not_ask_for_events_announces_nothing(tmp_path):
    protocol = render(tmp_path, "cpp_protocol.hpp.mustache", "")
    with_events = render(tmp_path, "cpp_protocol.hpp.mustache", EVENTS)

    assert "struct test_link_event" not in protocol
    assert "struct test_link_event" in with_events


def test_a_junction_asks_for_the_event_files_only_by_stating_events(tmp_path):
    asks = tmp_path / "asks" / "modeling"
    plain = tmp_path / "plain" / "modeling"
    asks.mkdir(parents=True)
    plain.mkdir(parents=True)
    name = "ores.testcomp.test_link_junction.org"
    (asks / name).write_text(JUNCTION.format(flags=EVENTS, columns=""), encoding="utf-8")
    (plain / name).write_text(JUNCTION.format(flags="", columns=""), encoding="utf-8")

    event_templates = {
        "sql_schema_notify_trigger.mustache",
        "sql_schema_notify_trigger_drop.mustache",
        "cpp_nats_changed_event.hpp.mustache",
        "cpp_nats_event_registrar.cpp.mustache",
        "cpp_nats_event_registrar.hpp.mustache",
    }
    asked, _, _ = resolve_targets(asks / name, CODEGEN_BASE)
    not_asked, _, _ = resolve_targets(plain / name, CODEGEN_BASE)

    assert event_templates <= {u["template"] for u in asked}
    assert not (event_templates & {u["template"] for u in not_asked})
