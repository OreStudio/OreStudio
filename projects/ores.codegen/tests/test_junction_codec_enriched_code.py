"""A junction side that enriches a code puts that code on the wire.

A side with =:enrich_code:= carries the joined row's code in a member the table
has no column for, and the repository fills it. The wire carries every member,
so the codec must name that one too, after the audit members where the struct
declares it; a side without the flag adds nothing.

Run::

    python3 -m pytest projects/ores.codegen/tests/test_junction_codec_enriched_code.py
"""
from pathlib import Path

import pystache

REPO_ROOT = Path(__file__).resolve().parents[3]
CODEC_HEADER = (REPO_ROOT / "projects/ores.codegen/library/templates"
                / "cpp_domain_type_codec.hpp.mustache")


def render(left_enriched, right_enriched):
    return pystache.render(
        CODEC_HEADER.read_text(encoding="utf-8"),
        {
            "cpp_license": "// license\n",
            "domain_entity": None,
            "junction": {
                "component": "x",
                "component_include": "x.api",
                "component_include_upper": "X_API",
                "name_singular": "widget_part",
                "name_singular_upper": "WIDGET_PART",
                "has_tenant_id": True,
                "columns": [],
                "left": {"column": "widget_id", "column_short": "widget",
                         "enrich_code": left_enriched},
                "right": {"column": "part_id", "column_short": "part",
                          "enrich_code": right_enriched},
            },
        },
    )


def test_an_enriched_side_names_its_code_after_the_audit_members():
    text = render(False, True)
    recorded = text.index('"recorded_at"')
    code = text.index('field{"part_code", &widget_part::part_code}')
    assert recorded < code


def test_a_side_without_enrichment_names_no_code():
    text = render(False, False)
    assert 'field{"widget_code"' not in text
    assert 'field{"part_code"' not in text


def test_each_enriched_side_names_its_own_code():
    text = render(True, True)
    assert 'field{"widget_code", &widget_part::widget_code}' in text
    assert 'field{"part_code", &widget_part::part_code}' in text
