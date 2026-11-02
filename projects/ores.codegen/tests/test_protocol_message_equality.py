"""Every generated protocol message compares by value.

A roundtrip test requires the value it read to equal the value it wrote, so
each message the protocol template declares carries a defaulted equality. The
four model kinds render their messages in four separate sections of the
template, and each section must emit it.

Run::

    python3 -m pytest projects/ores.codegen/tests/test_protocol_message_equality.py
"""
import re
from pathlib import Path

import pystache
import pytest

REPO_ROOT = Path(__file__).resolve().parents[3]

TEMPLATE = REPO_ROOT / "projects/ores.codegen/library/templates/cpp_protocol.hpp.mustache"

KINDS = ("domain_entity", "junction", "operation", "family")


def a_message(name):
    return {
        "name": name,
        "struct_name": name,
        "fields": [{"name": "code", "cpp_type": "std::string"}],
    }


def render(kind):
    """Render one model kind's section with two messages and no codec."""
    model = {
        "entity_singular": "widget",
        "name_singular": "widget",
        "component": "x",
        "component_include": "x.api",
        "component_include_upper": "X_API",
        "namespace": "ores::x::messaging",
        "messages": [a_message("widget_key"), a_message("widget_tag")],
        "events": [],
        "has_generated_wire_codec": False,
    }
    context = {k: None for k in KINDS}
    context[kind] = model
    return pystache.render(TEMPLATE.read_text(encoding="utf-8"), context)


def struct_body(text, name):
    match = re.search(r"^struct " + name + r" \{\n(.*?)^\};", text, re.M | re.S)
    assert match, f"struct {name} not rendered"
    return match.group(1)


@pytest.mark.parametrize("kind", KINDS)
def test_each_message_declares_its_own_defaulted_equality(kind):
    out = render(kind)
    for name in ("widget_key", "widget_tag"):
        body = struct_body(out, name)
        assert f"friend bool operator==(const {name}&, const {name}&) = default;" in body


@pytest.mark.parametrize("kind", KINDS)
def test_the_equality_follows_the_last_member(kind):
    body = struct_body(render(kind), "widget_key")
    assert body.index("std::string code;") < body.index("operator==")
