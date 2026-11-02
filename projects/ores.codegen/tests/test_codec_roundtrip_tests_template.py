"""The generated roundtrip test for an entity, a junction and a field group.

Each kind renders in its own section of the template. Every section must
roundtrip the sample and the default in both formats, and must guard the
member count, because the sample fills only the members the codec names.

Run::

    python3 -m pytest projects/ores.codegen/tests/test_codec_roundtrip_tests_template.py
"""
from pathlib import Path

import pystache
import pytest

REPO_ROOT = Path(__file__).resolve().parents[3]

TEMPLATE = (REPO_ROOT / "projects/ores.codegen/library/templates/"
            "cpp_domain_type_codec_roundtrip_tests.cpp.mustache")

KINDS = {
    "domain_entity": {"entity_singular": "widget"},
    "junction": {"name_singular": "widget"},
    "field_group": {"entity_singular": "widget",
                    "cpp": {"namespace": "ores::x::domain"}},
}

CASES = ("json_sample", "msgpack_sample", "json_default", "msgpack_default")


def render(kind, left_out=0):
    model = {"component": "x", "component_include": "x.api",
             "wire_codec_includes": [], "wire_left_out": left_out,
             **KINDS[kind]}
    context = {k: None for k in KINDS}
    context[kind] = model
    context["cpp_license"] = ""
    return pystache.render(TEMPLATE.read_text(encoding="utf-8"), context)


@pytest.mark.parametrize("kind", KINDS)
def test_each_kind_renders_four_prefixed_cases(kind):
    out = render(kind)
    for case in CASES:
        assert f'TEST_CASE("widget_codec_roundtrip_{case}"' in out
    assert out.count("TEST_CASE(") == len(CASES)


@pytest.mark.parametrize("kind", KINDS)
def test_each_kind_guards_the_member_count(kind):
    assert "names_every_member<widget_roundtrip_type>(0)" in render(kind)


@pytest.mark.parametrize("kind", KINDS)
def test_the_guard_allows_the_members_left_out(kind):
    assert "names_every_member<widget_roundtrip_type>(2)" in render(kind, left_out=2)


def test_only_entities_and_junctions_go_through_the_traits():
    assert "wire_traits<widget_roundtrip_type>" in render("domain_entity")
    assert "wire_traits<widget_roundtrip_type>" in render("junction")
    assert "wire_traits" not in render("field_group")
