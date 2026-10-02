"""Tests for the enum conversion a grouped write field needs.

Run::

    python3 -m pytest projects/ores.codegen/tests/test_enum_write_fields.py

A write record carries the column's own spelling on the wire. When the
column is text and the field group declares the member as an enumeration,
the service cannot assign one to the other. The ores.trading regeneration
stopped there: trade's ``product_type`` is a nullable text column whose
group member is ``domain::product_type``, and the core library did not
build. ``_prepare_enum_write_fields`` now states the parse the service
splices. These tests pin when it states one and what it falls back to.
"""
import sys
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[3]
sys.path.insert(0, str(REPO_ROOT / "projects/ores.codegen/src"))

from codegen.core import _prepare_enum_write_fields  # noqa: E402


def _entity(write_field, member):
    return {
        "domain_group_fields": [member],
        "write_fields": [write_field],
    }


def test_text_column_with_enum_member_parses_with_member_default():
    write = {"name": "product_type", "cpp_type": "std::string",
             "render_is_enum": True, "domain_member": "product_type"}
    member = {"name": "product_type", "cpp_type": "domain::product_type",
              "default_value": "domain::product_type::unknown"}
    _prepare_enum_write_fields(_entity(write, member))

    assert write["enum_from_string"] == "domain::product_type_from_string"
    assert write["enum_default"] == "domain::product_type::unknown"


def test_member_without_default_falls_back_to_column_then_value_init():
    write = {"name": "kind", "cpp_type": "std::string",
             "render_is_enum": True, "domain_member": "kind",
             "default_value": "domain::kind::plain"}
    member = {"name": "kind", "cpp_type": "domain::kind"}
    _prepare_enum_write_fields(_entity(write, member))
    assert write["enum_default"] == "domain::kind::plain"

    write.pop("default_value")
    write.pop("enum_default")
    _prepare_enum_write_fields(_entity(write, member))
    assert write["enum_default"] == "domain::kind{}"


def test_column_already_of_the_enum_type_takes_no_conversion():
    write = {"name": "product_type", "cpp_type": "domain::product_type",
             "render_is_enum": True, "domain_member": "product_type"}
    member = {"name": "product_type", "cpp_type": "domain::product_type"}
    _prepare_enum_write_fields(_entity(write, member))

    assert "enum_from_string" not in write
    assert "enum_default" not in write


def test_non_enum_field_is_left_alone():
    write = {"name": "code", "cpp_type": "std::string",
             "domain_member": "code"}
    member = {"name": "code", "cpp_type": "domain::code_type"}
    _prepare_enum_write_fields(_entity(write, member))

    assert "enum_from_string" not in write
