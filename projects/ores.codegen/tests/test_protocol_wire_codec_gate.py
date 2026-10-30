"""The protocol header states a message's shape only where the component opted in.

The wire-codec facet is off by default, so the field list and the codec
specialisations the protocol template adds must vanish completely for a
component that has not opted in: no text and no blank line. A gate that leaks
whitespace rewrites every other component's protocol header, which is what the
first version of this did.

Run::

    python3 -m pytest projects/ores.codegen/tests/test_protocol_wire_codec_gate.py
"""
from pathlib import Path

import pystache

REPO_ROOT = Path(__file__).resolve().parents[3]

TEMPLATE = REPO_ROOT / "projects/ores.codegen/library/templates/cpp_protocol.hpp.mustache"


def render(opted_in, messages):
    """Render the domain branch of the protocol header for one made-up entity."""
    return pystache.render(
        TEMPLATE.read_text(encoding="utf-8"),
        {
            "domain_entity": {
                "entity_singular": "widget",
                "component": "x",
                "component_include": "x.api",
                "component_include_upper": "X_API",
                "messages": messages,
                "events": [],
                "has_generated_wire_codec": opted_in,
            },
            "junction": None,
            "operation": None,
        },
    )


def a_message():
    return {
        "name": "widget_key",
        "struct_name": "widget_key",
        "fields": [{"name": "name", "cpp_type": "std::string"}],
    }


def test_a_component_that_did_not_opt_in_gets_no_shape_and_no_extra_bytes():
    """The gate adds nothing at all, not even a newline."""
    plain = render(False, [])
    assert "wire_fields" not in plain
    assert "wire_traits" not in plain
    assert "codec/" not in plain
    # The header still declares its messages and closes its namespace.
    assert plain.count("namespace ores::x::messaging {") == 1
    assert "#endif" in plain


def test_a_component_that_opted_in_states_the_shape_beside_the_struct():
    """The field list names the message it describes, not the field twice."""
    out = render(True, [a_message()])
    assert "wire_fields(const widget_key&)" in out
    assert 'utility::codec::field{"name", &widget_key::name}' in out
    assert "wire_traits<ores::x::messaging::widget_key>" in out
    assert "static constexpr bool is_generated = true;" in out


def test_the_specialisation_declares_the_four_functions_and_defines_none():
    """The translation unit defines them, so a sender compiles a declaration."""
    out = render(True, [a_message()])
    for fn in ("write_json", "read_json", "write_msgpack", "read_msgpack"):
        assert f" {fn}(" in out
    assert "return object::" not in out
