"""Tests for the unified Columns/:primary_key:/:natural_key: model shape.

Run::

    python3 -m pytest projects/ores.codegen/tests/test_org_loader_keys.py
"""
import sys
from pathlib import Path

import pytest

REPO_ROOT = Path(__file__).resolve().parents[3]
sys.path.insert(0, str(REPO_ROOT / "projects/ores.codegen/src"))

from codegen.org_loader import (  # noqa: E402
    parse_org,
    org_document_to_model,
    load_org_model,
    domain_entity_to_table_context,
)

REQUIRED_FLAGS = """\
* Flags
:PROPERTIES:
:schema: public
:product: ores
:component: dq
:END:
"""


def _model(columns_body: str) -> dict:
    text = REQUIRED_FLAGS + columns_body
    doc = parse_org(text)
    return org_document_to_model(doc)["domain_entity"]


def test_single_primary_key_field():
    de = _model(
        """
* Columns

** name
:PROPERTIES:
:type: text
:cpp_type: std::string
:primary_key: true
:END:

Unique name.

** description
:PROPERTIES:
:type: text
:cpp_type: std::string
:END:

Description column.
"""
    )
    assert de["primary_key"]["column"] == "name"
    assert de["primary_key"]["type"] == "text"
    assert len(de["primary_key"]["columns"]) == 1
    assert de["primary_key"]["columns"][0]["column"] == "name"
    assert [c["name"] for c in de["columns"]] == ["description"]


def test_compound_primary_key_preserves_declaration_order():
    de = _model(
        """
* Columns

** name
:PROPERTIES:
:type: text
:cpp_type: std::string
:primary_key: true
:END:

Unique name identifying this subject area within its data domain.

** domain_name
:PROPERTIES:
:type: text
:cpp_type: std::string
:nullable: false
:primary_key: true
:END:

Name of the data domain this subject area belongs to.

** description
:PROPERTIES:
:type: text
:cpp_type: std::string
:nullable: false
:END:

Human-readable description.
"""
    )
    pk = de["primary_key"]
    # Back-compat single-column scalars mirror the first flagged field.
    assert pk["column"] == "name"
    assert pk["type"] == "text"
    # New compound-aware shape: every flagged field, in declaration order.
    assert [c["column"] for c in pk["columns"]] == ["name", "domain_name"]
    assert pk["columns"][1]["nullable"] is False
    # Key-role flags never leak into the rendered field dict.
    assert "primary_key" not in pk["columns"][0]
    assert "natural_key" not in pk["columns"][0]
    assert [c["name"] for c in de["columns"]] == ["description"]


def test_natural_key_flag_populates_natural_keys_list():
    de = _model(
        """
* Columns

** id
:PROPERTIES:
:type: uuid
:cpp_type: boost::uuids::uuid
:primary_key: true
:END:

Surrogate id.

** code
:PROPERTIES:
:type: text
:cpp_type: std::string
:natural_key: true
:END:

Natural code.

** name
:PROPERTIES:
:type: text
:cpp_type: std::string
:END:

Plain column.
"""
    )
    assert de["primary_key"]["column"] == "id"
    assert "natural_keys" not in de or [nk["column"] for nk in de["natural_keys"]] == ["code"]
    assert [c["name"] for c in de["columns"]] == ["name"]


def test_field_flagged_both_primary_and_natural_key_is_rejected():
    with pytest.raises(ValueError, match="cannot be both"):
        _model(
            """
* Columns

** name
:PROPERTIES:
:type: text
:cpp_type: std::string
:primary_key: true
:natural_key: true
:END:

Conflicting flags.
"""
        )


def test_no_primary_key_flagged_fails_validation(tmp_path):
    doc_path = tmp_path / "no_pk.org"
    doc_path.write_text(
        REQUIRED_FLAGS
        + """
* Columns

** name
:PROPERTIES:
:type: text
:cpp_type: std::string
:END:

No field flagged primary_key.
""",
        encoding="utf-8",
    )
    with pytest.raises(ValueError, match="Missing primary key"):
        load_org_model(doc_path)


def test_is_enum_default_value_becomes_scoped_enum_expression():
    # Regression test: default_value on an is_enum column used to be SQL-string-quoted
    # (the same handling as any other text-typed column), which the C++ domain template
    # emits unescaped as the struct member's initializer -- producing invalid C++ (a char
    # literal assigned to a scoped enum) instead of the correct domain::asset_class::fx.
    de = _model(
        """
* Columns

** id
:PROPERTIES:
:type: uuid
:cpp_type: boost::uuids::uuid
:primary_key: true
:END:

Surrogate id.

** asset_class
:PROPERTIES:
:type: text
:cpp_type: domain::asset_class
:is_enum: true
:nullable: false
:default_value: fx
:END:

Coarse asset class taxonomy.
"""
    )
    [asset_class_col] = [c for c in de["columns"] if c["name"] == "asset_class"]
    assert asset_class_col["default_value"] == "domain::asset_class::fx"


def test_non_enum_text_default_value_is_still_sql_quoted():
    de = _model(
        """
* Columns

** id
:PROPERTIES:
:type: uuid
:cpp_type: boost::uuids::uuid
:primary_key: true
:END:

Surrogate id.

** day_count
:PROPERTIES:
:type: text
:cpp_type: std::string
:default_value: ACT/360
:END:

Day count convention.
"""
    )
    [day_count_col] = [c for c in de["columns"] if c["name"] == "day_count"]
    assert day_count_col["default_value"] == "'ACT/360'"


def test_domain_entity_to_table_context_unaffected_by_compound_key_shape():
    de = _model(
        """
* Columns

** name
:PROPERTIES:
:type: text
:cpp_type: std::string
:primary_key: true
:END:

Unique name.
"""
    )
    ctx = domain_entity_to_table_context(de)
    assert ctx["table"]["primary_key"]["column"] == "name"
    assert ctx["table"]["primary_key"]["is_text"] is True
