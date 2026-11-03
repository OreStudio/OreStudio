"""The generated roundtrip test for the messages of a protocol header.

Each model kind renders in its own section of the template. Every message must
get one case, named by its qualified type, that calls the shared helper, which
roundtrips the sample and the default in both formats.

Run::

    python3 -m pytest projects/ores.codegen/tests/test_protocol_roundtrip_tests_template.py
"""
from pathlib import Path

import pystache
import pytest

REPO_ROOT = Path(__file__).resolve().parents[3]

TEMPLATE = (REPO_ROOT / "projects/ores.codegen/library/templates/"
            "cpp_protocol_codec_roundtrip_tests.cpp.mustache")

MESSAGES = [{"name": "get_widget_request"}, {"name": "get_widget_response"}]

KINDS = {
    "domain_entity": ({"entity_singular": "widget"}, "ores::x::messaging"),
    "junction": ({"name_singular": "widget"}, "ores::x::messaging"),
    "operation": ({"entity_singular": "widget", "has_generated_wire_codec": True,
                   "namespace": "ores::x::messaging::op"}, "ores::x::messaging::op"),
    "family": ({"entity_singular": "widget", "has_generated_wire_codec": True},
               "ores::x::messaging"),
}


def render(kind, **overrides):
    fields, _ = KINDS[kind]
    model = {"component": "x", "component_include": "x.api",
             "message_codec_includes": [], "messages": MESSAGES,
             **fields, **overrides}
    context = {k: None for k in KINDS}
    context[kind] = model
    context["cpp_license"] = ""
    return pystache.render(TEMPLATE.read_text(encoding="utf-8"), context)


@pytest.mark.parametrize("kind", KINDS)
def test_each_message_gets_one_case_named_by_its_type(kind):
    out = render(kind)
    namespace = KINDS[kind][1]
    for message in MESSAGES:
        qualified = f"{namespace}::{message['name']}"
        assert f'TEST_CASE("{qualified}"' in out
        assert f"check_wire_roundtrip<{qualified}>()" in out
    assert out.count("TEST_CASE(") == len(MESSAGES)


@pytest.mark.parametrize("kind", KINDS)
def test_each_kind_includes_its_protocol_header_and_the_helper(kind):
    out = render(kind)
    assert '#include "ores.x.api/messaging/widget_protocol.hpp"' in out
    assert '#include "ores.testing/wire_roundtrip.hpp"' in out
    assert '#include "ores.utility/domain/protocol_codec.hpp"' in out


@pytest.mark.parametrize("kind", ["operation", "family"])
def test_a_hand_written_codec_gets_no_tests(kind):
    assert "TEST_CASE(" not in render(kind, has_generated_wire_codec=False)


def test_a_family_entity_has_no_domain_codec_to_include():
    assert "domain/widget_codec.hpp" in render("domain_entity")
    assert "domain/widget_codec.hpp" not in render("domain_entity", family=True)


@pytest.mark.parametrize("kind", ["domain_entity", "operation", "family"])
def test_each_message_codec_include_renders(kind):
    includes = ["ores.x.api/domain/gadget_codec.hpp", "ores.y.api/domain/part_codec.hpp"]
    out = render(kind, message_codec_includes=includes)
    for include in includes:
        assert f'#include "{include}"' in out
