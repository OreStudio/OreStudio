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
    declared_key_field,
    key_is_primary,
    key_finders,
)

REQUIRED_FLAGS = """\
* Flags
:PROPERTIES:
:schema: public
:product: ores
:component: dq
:END:
"""


def _flags(*extra: str) -> str:
    """Build a Flags drawer, with any extra properties before the :END:."""
    lines = ["* Flags", ":PROPERTIES:", ":schema: public", ":product: ores",
             ":component: dq"]
    lines += list(extra)
    lines.append(":END:")
    return "\n".join(lines) + "\n"


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


COLUMNS_NO_KEY = """
* Columns

** name
:PROPERTIES:
:type: text
:cpp_type: std::string
:END:

No field flagged primary_key.
"""


def test_a_keyless_record_is_admitted_when_it_says_so(tmp_path):
    # A record that is only carried has no key to state. The flag is what tells
    # the loader which of the two it is looking at, so the requirement above
    # still holds for everything that does not declare it.
    doc_path = tmp_path / "keyless.org"
    doc_path.write_text(
        _flags(":no_subcomponent: true", ":no_primary_key: true")
        + COLUMNS_NO_KEY,
        encoding="utf-8",
    )
    de = load_org_model(doc_path)["domain_entity"]
    assert "primary_key" not in de


def test_stating_no_primary_key_and_flagging_one_is_refused(tmp_path):
    doc_path = tmp_path / "contradiction.org"
    doc_path.write_text(
        _flags(":no_subcomponent: true", ":no_primary_key: true")
        + """
* Columns

** name
:PROPERTIES:
:type: text
:cpp_type: std::string
:primary_key: true
:END:

Both at once.
""",
        encoding="utf-8",
    )
    with pytest.raises(ValueError, match="also flags a primary key"):
        load_org_model(doc_path)


def test_no_subcomponent_flag_admits_a_model_without_one(tmp_path):
    # A component that keeps its headers at its own include root has no
    # sub-component for a model to name.
    doc_path = tmp_path / "no_subcomponent.org"
    doc_path.write_text(
        _flags(":no_subcomponent: true")
        + """
* Columns

** name
:PROPERTIES:
:type: text
:cpp_type: std::string
:primary_key: true
:END:

Only the sub-component is absent.
""",
        encoding="utf-8",
    )
    de = load_org_model(doc_path)["domain_entity"]
    assert de["primary_key"]["column"] == "name"


def test_a_missing_subcomponent_without_the_flag_is_refused(tmp_path):
    doc_path = tmp_path / "missing_subcomponent.org"
    doc_path.write_text(
        REQUIRED_FLAGS
        + """
* Columns

** name
:PROPERTIES:
:type: text
:cpp_type: std::string
:primary_key: true
:END:

The key is stated, only the sub-component is absent.
""",
        encoding="utf-8",
    )
    with pytest.raises(ValueError, match="Missing required flag: subcomponent"):
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


def _entity(key_field: str, primary: str = "id") -> dict:
    """A hand-built enriched entity with a declared key and a storage key."""
    return {
        "entity_singular": "widget",
        "primary_key": {"column": primary,
                        "columns": [{"column": primary, "cpp_type": "std::string"}]},
        "columns": [{"name": "code", "cpp_type": "std::string"},
                    {"name": "name", "cpp_type": "std::string"}],
        "natural_keys": [],
        "presentation": {"collection_name": "widgets", "key_field": key_field},
    }


def test_a_model_with_no_screen_declares_no_key():
    """Its storage key is the only key it has, and callers address it by that."""
    entity = _entity("")
    assert declared_key_field(entity) == ""
    assert key_is_primary(entity) is True
    assert key_finders(entity) == []


def test_the_declared_key_is_the_storage_key_when_they_agree():
    entity = _entity("code", primary="code")
    assert declared_key_field(entity) == "code"
    assert key_is_primary(entity) is True
    assert key_finders(entity) == []


def test_a_declared_key_that_is_not_the_storage_key_gets_a_read():
    """This is the read a caller's address resolves to a row through."""
    entity = _entity("code")
    assert key_is_primary(entity) is False
    assert key_finders(entity) == [{"column": "code", "suffix": "code"}]


def test_the_older_opt_in_keeps_its_method_name_beside_the_declared_read():
    """Hand-written callers spell `read_latest_by_code`, so it cannot move.

    `counterparty` and `party` declare `short_code` as their key and opt in on
    the same column, so the read the service resolves their key through is
    spelled `read_latest_by_short_code` and needs declaring too.
    """
    entity = _entity("short_code")
    entity["service_find_by_code"] = {"column": "short_code"}
    assert key_finders(entity) == [
        {"column": "short_code", "suffix": "code"},
        {"column": "short_code", "suffix": "short_code"},
    ]


def test_a_legacy_finder_beside_a_differently_spelled_key_needs_both_reads():
    """The service resolves a caller's key through `read_latest_by_<key>`.

    pricing_model_config opts into `service_find_by_code_column: name` and
    declares `name` as its key. The legacy finder spells its method
    `read_latest_by_code`, so deduplicating the two by column alone leaves the
    generated service calling `read_latest_by_name` on a repository that never
    declares it.
    """
    entity = _entity("name")
    entity["service_find_by_code"] = {"column": "name"}
    assert key_finders(entity) == [
        {"column": "name", "suffix": "code"},
        {"column": "name", "suffix": "name"},
    ]


def test_two_finders_that_name_one_column_are_one_method():
    """Permission opts in with `code` and declares `code`, so it stays as it was."""
    entity = _entity("code")
    entity["service_find_by_code"] = {"column": "code"}
    assert key_finders(entity) == [{"column": "code", "suffix": "code"}]


def test_a_finder_the_model_states_itself_is_not_generated_twice():
    """`role` declares `read_latest_by_name` as a paste block."""
    entity = _entity("name")
    entity["implementations"] = {
        "DCA78C69-E508-48D9-9972-A9B8094D91FB": [
            "std::vector<domain::widget> read_latest_by_name("
            "context ctx, const std::string& name);",
        ],
    }
    assert key_finders(entity) == []
