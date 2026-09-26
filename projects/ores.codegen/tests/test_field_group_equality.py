"""Tests for value equality on a generated field group.

Run::

    python3 -m pytest projects/ores.codegen/tests/test_field_group_equality.py

A generated domain class defaults its comparison and reads every
member. A field group is one of those members, so a field-group struct
without a comparison deletes the entity's comparison, and a build that
treats an implicitly deleted defaulted function as an error stops there.
That is exactly what the ores.trading regeneration did: every entity
holding =instrument_identity= failed with
=-Werror,-Wdefaulted-function-deleted=.

The pipeline's diff, its typecheck and the drift gate all passed while
the defect was present, because none of them compiles the generated
header. These tests render both templates and pin the pair: the
field-group struct compares, and the domain class that holds one keeps
comparing too.
"""
import sys
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[3]
sys.path.insert(0, str(REPO_ROOT / "projects/ores.codegen/src"))

from codegen.core import render_template  # noqa: E402

TEMPLATES = REPO_ROOT / "projects/ores.codegen/library/templates"
FIELD_GROUP = TEMPLATES / "cpp_field_group.hpp.mustache"
DOMAIN_CLASS = TEMPLATES / "cpp_domain_type_class.hpp.mustache"


def _context():
    return {
        "cpp_license": "/* licence */",
        "entity_singular": "thing",
        "field_group": {
            "component_include_upper": "PROBE",
            "entity_singular_upper": "THING_BITS",
            "entity_singular": "thing_bits",
            "brief": "Probe field group.",
            "description_lines": [],
            "cpp": {"namespace": "ores::probe::domain", "includes": []},
            "fields": [
                {
                    "description": "Key.",
                    "cpp_type": "boost::uuids::uuid",
                    "name": "id",
                    "last": True,
                },
            ],
        },
    }


def test_the_field_group_struct_is_a_value():
    out = render_template(str(FIELD_GROUP), _context())
    assert "struct thing_bits {" in out
    assert (
        "friend bool operator==(const thing_bits&,\n"
        "                           const thing_bits&) = default;"
    ) in out


def test_the_domain_class_template_keeps_its_comparison():
    """The other half of the pair.

    The domain class body sits inside sections a thin context does not
    satisfy, so this test reads the template rather than rendering it.
    The assertion still fails if the line is dropped.
    """
    text = DOMAIN_CLASS.read_text(encoding="utf-8")
    assert (
        "friend bool operator==(const {{entity_singular}}&, "
        "const {{entity_singular}}&) = default;"
    ) in text
