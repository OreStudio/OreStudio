"""The generated units that print and read JSON carry the codec only where a model opted in.

Three templates state the same switch: a model that opted into the wire-codec
facet uses the generated codec, and one that did not is untouched. A gate that
leaks a byte rewrites every other component's output, so each is rendered both
ways here and the two renderings are compared rather than argued about.

Run::

    python3 -m pytest projects/ores.codegen/tests/test_wire_codec_units_gate.py
"""
from pathlib import Path

import pystache

REPO_ROOT = Path(__file__).resolve().parents[3]

JSON_IO = (REPO_ROOT / "projects/ores.codegen/library/templates"
           / "cpp_domain_type_json_io.cpp.mustache")
CHANGED_EVENT = (REPO_ROOT / "projects/ores.codegen/library/templates"
                 / "cpp_nats_changed_event.hpp.mustache")


def render_json_io(opted_in):
    return pystache.render(
        JSON_IO.read_text(encoding="utf-8"),
        {
            "cpp_license": "// license\n",
            "domain_entity": {
                "entity_singular": "widget",
                "component": "x",
                "component_include": "x.api",
                "has_generated_wire_codec": opted_in,
            },
            "junction": None,
        },
    )


def render_changed_event(opted_in):
    return pystache.render(
        CHANGED_EVENT.read_text(encoding="utf-8"),
        {
            "cpp_license": "// license\n",
            "domain_entity": {
                "entity_singular": "widget",
                "component": "x",
                "component_include": "x.api",
                "entity_title_lower": "widget",
                "event_prefix": "x.v1.widgets_events",
                "has_generated_wire_codec": opted_in,
            },
            "junction": None,
        },
    )


def test_a_model_that_did_not_opt_in_prints_its_row_with_the_reflector():
    plain = render_json_io(False)
    assert "<rfl/json.hpp>" in plain
    assert "rfl::json::write(v, s);" in plain
    assert "codec/" not in plain


def test_a_model_that_opted_in_prints_its_row_with_the_generated_codec():
    codec = render_json_io(True)
    assert 'x.api/domain/widget_codec.hpp' in codec
    assert "codec::json::write(v)" in codec
    assert "<rfl/json.hpp>" not in codec
    assert "rfl::json::write" not in codec


def test_a_model_that_did_not_opt_in_reads_the_key_with_the_reflector():
    plain = render_changed_event(False)
    assert "<rfl/json.hpp>" in plain
    assert "rfl::json::read<ores::x::messaging::widget_key>" in plain
    assert "codec/" not in plain


def test_a_model_that_opted_in_reads_the_key_with_the_generated_codec():
    codec = render_changed_event(True)
    assert "utility/codec/json_codec.hpp" in codec
    assert "codec::json::try_read<" in codec
    assert "<rfl/json.hpp>" not in codec
    assert "rfl::json::read" not in codec


def test_an_opted_in_header_carries_no_exception_handling():
    """The read answers with nothing it can read; nothing to catch."""
    codec = render_changed_event(True)
    assert "catch" not in codec
    assert "<exception>" not in codec


def test_the_gate_touches_nothing_but_the_encoder():
    """The namespace, the signature and the body around them are the same either way."""
    for render, marker in ((render_json_io, "operator<<"), (render_changed_event,
                                                          "from_notification")):
        plain = render(False)
        codec = render(True)
        assert marker in plain and marker in codec
        assert "namespace ores::" in plain and "namespace ores::" in codec
        assert [l for l in plain.splitlines() if l][-3:] == \
            [l for l in codec.splitlines() if l][-3:]
