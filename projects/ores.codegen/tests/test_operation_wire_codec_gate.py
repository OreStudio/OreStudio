"""An operation's protocol header states a message's shape only where it opted in.

An operation's messages live in the namespace its model names, not in the
component's messaging namespace, so the specialisations name the message by
that namespace. A model that has not opted in renders exactly what it rendered
before: no text and no blank line.

Run::

    python3 -m pytest projects/ores.codegen/tests/test_operation_wire_codec_gate.py
"""
from pathlib import Path

import pystache

REPO_ROOT = Path(__file__).resolve().parents[3]

HEADER = REPO_ROOT / "projects/ores.codegen/library/templates/cpp_protocol.hpp.mustache"
UNIT = REPO_ROOT / "projects/ores.codegen/library/templates/cpp_protocol_codec.cpp.mustache"


def operation(opted_in):
    return {
        "entity_singular": "widget_operations",
        "entity_singular_upper": "WIDGET_OPERATIONS",
        "component": "x",
        "component_include": "x.api",
        "component_include_upper": "X_API",
        "namespace": "ores::x::service",
        "includes": ['"ores.x.api/domain/widget.hpp"'],
        "messages": [{
            "name": "move_widget_request",
            "struct_name": "move_widget_request",
            "fields": [{"name": "widget_id", "cpp_type": "std::string"}],
        }],
        "message_codec_includes": ["ores.x.api/domain/widget_codec.hpp"],
        "has_generated_wire_codec": opted_in,
    }


def render(template, opted_in):
    return pystache.render(
        template.read_text(encoding="utf-8"),
        {"domain_entity": None, "junction": None, "family": None,
         "operation": operation(opted_in)},
    )


def test_an_operation_that_did_not_opt_in_gets_no_shape_and_no_extra_bytes():
    plain = render(HEADER, False)
    assert "wire_fields" not in plain
    assert "wire_traits" not in plain
    assert "codec/" not in plain
    assert "\n\n\n" not in plain
    assert render(UNIT, False).strip() == ""


def test_an_operation_that_opted_in_names_its_messages_by_its_own_namespace():
    out = render(HEADER, True)
    assert "wire_fields(const move_widget_request&)" in out
    assert ('::ores::utility::codec::field{"widget_id", '
            "&move_widget_request::widget_id}") in out
    assert "wire_traits<ores::x::service::move_widget_request>" in out
    assert "ORES_X_API_EXPORT" in out


def test_the_unit_defines_the_four_functions_for_each_message():
    unit = render(UNIT, True)
    assert '#include "ores.x.api/messaging/widget_operations_protocol.hpp"' in unit
    assert '#include "ores.x.api/domain/widget_codec.hpp"' in unit
    for fn in ("write_json", "read_json", "write_msgpack", "read_msgpack"):
        assert f"wire_traits<ores::x::service::move_widget_request>::{fn}(" in unit
