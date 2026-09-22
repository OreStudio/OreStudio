"""Tests for the TypeScript UI metadata projection.

Run::

    python3 -m pytest projects/ores.codegen/tests/test_ui_meta_projection.py

`ui_meta_projection` turns one entity's presentation drawer into the data
the `ores.ts.ui` template renders. The drift gate regenerates the nine
registered components and compares the emitted files byte for byte, which
catches a change to the output but not a wrong derivation: a rule that
misreads the drawer emits a file that matches itself. These cases pin the
derivations to the request's tables.
"""
import sys
from pathlib import Path

import pytest

REPO_ROOT = Path(__file__).resolve().parents[3]
sys.path.insert(0, str(REPO_ROOT / "projects/ores.codegen/src"))

from codegen.core import generate_from_model, ui_meta_projection  # noqa: E402

MODEL = REPO_ROOT / "projects/ores.refdata/modeling/ores.refdata.book_status.org"
CODEGEN = REPO_ROOT / "projects/ores.codegen"


def _project(columns, fields=(), level1=(), **drawer):
    """Project one hand-built entity.

    ``columns`` is the drawer's column table, ``fields`` its detail-field
    table, ``level1`` the entity-level `* Columns` table the nullable flag
    is read from, and ``drawer`` any further presentation cell.
    """
    presentation = {"collection_name": "statuses", "columns": list(columns),
                    "detail_fields": list(fields), **drawer}
    entity = {"entity_singular": "book_status", "presentation": presentation,
              "columns": list(level1)}
    return ui_meta_projection(entity, MODEL)


def test_a_model_with_no_column_table_projects_nothing():
    """The gate `resolve_targets` withholds the facet on."""
    assert ui_meta_projection(
        {"entity_singular": "book_status",
         "presentation": {"collection_name": "statuses"}}, MODEL) is None
    assert _project([]) is None


def test_header_key_keeps_the_enum_name_spelling():
    """The column table's enum name is already PascalCase in every model.

    Converting it again lowercased the second word: Alpha2Code became
    Alpha2code, and the key no catalogue holds was emitted.
    """
    projection = _project([{"field": "alpha2_code", "enum_name": "Alpha2Code"}])
    assert "headerKey: 'book_status.colAlpha2Code'" in projection["columns_block"]


def test_badge_and_icon_styles_win_over_the_column_type():
    projection = _project(
        [{"field": "code", "enum_name": "Code", "is_int": True,
          "is_badge": True},
         {"field": "status", "enum_name": "Status", "is_int": True},
         {"field": "other", "enum_name": "Other", "is_int": True},
         {"field": "id", "enum_name": "Id", "is_uuid": True}],
        icon_columns=[{"column": "Status"}])
    block = projection["columns_block"]
    assert "style: 'badge_centered'" in block
    assert "style: 'icon_text_left'" in block
    assert "style: 'mono_center'" in block
    assert "style: 'mono_left'" in block


def test_the_flag_column_is_marked():
    projection = _project([{"field": "flagged", "enum_name": "Flagged"}],
                          flag_icon_column="Flagged")
    assert "flag: true" in projection["columns_block"]


def test_a_flag_declaring_entity_states_its_image():
    """The model declares a flag, and the entity's own column holds it.

    A field cannot carry the image: no form control edits one, so the shared
    screen renders a picker from this member instead.
    """
    projection = _project(
        [{"field": "alpha2_code", "enum_name": "Alpha2Code"}],
        flag_icon_column="Alpha2Code",
        level1=[{"name": "image_id", "nullable": True}])
    assert projection["has_image"] is True
    assert projection["image_block"] == "{ field: 'image_id', kind: 'flag' }"


def test_an_entity_without_a_flag_declaration_states_no_image():
    projection = _project(
        [{"field": "code", "enum_name": "Code"}],
        level1=[{"name": "image_id", "nullable": True}])
    assert projection["has_image"] is False
    assert projection["image_block"] == ""


def test_a_flag_declaration_without_an_image_column_states_no_image():
    """A derived flag -- a currency pair's -- is not the entity's own image."""
    projection = _project([{"field": "flagged", "enum_name": "Flagged"}],
                          flag_icon_column="Flagged")
    assert projection["has_image"] is False


def test_temporal_columns_are_marked():
    projection = _project(
        [{"field": "recorded_at", "enum_name": "RecordedAt",
          "is_timestamp": True}])
    assert "temporal: true" in projection["columns_block"]


def test_audit_columns_are_hidden_and_auto_width_is_omitted():
    projection = _project(
        [{"field": "code", "enum_name": "Code", "width": "auto"},
         {"field": "version", "enum_name": "Version", "width": "70"},
         {"field": "description", "enum_name": "Description"}])
    block = projection["columns_block"]
    assert "width: 70" in block
    assert "width: 'auto'" not in block
    assert block.count("hidden: true") == 2


def test_fields_drop_the_members_nobody_edits():
    projection = _project(
        [{"field": "code", "enum_name": "Code"}],
        [{"field": "code", "type": "line_edit", "is_key": True,
          "is_required": True},
         {"field": "id", "type": "line_edit"},
         {"field": "tenant_id", "type": "line_edit"},
         {"field": "version", "type": "spin_box"}],
        key_field="code")
    block = projection["fields_block"]
    assert block.count("name: ") == 1
    assert "tenant_id" not in block
    assert "isKey: true" in block
    assert "readOnlyAfterCreate: true" in block
    assert projection["key_in_fields"] is True


def test_a_key_the_form_does_not_carry_is_not_called_read_only():
    projection = _project([{"field": "code", "enum_name": "Code"}],
                          [{"field": "code", "type": "line_edit"}],
                          key_field="version")
    assert projection["key_in_fields"] is False


def test_nullable_comes_from_the_entity_level_column_table():
    projection = _project(
        [{"field": "description", "enum_name": "Description"}],
        [{"field": "description", "type": "text_edit"}],
        level1=[{"name": "description", "nullable": True}])
    assert "nullable: true" in projection["fields_block"]


def test_plain_text_edit_maps_to_the_contracts_control():
    projection = _project([{"field": "note", "enum_name": "Note"}],
                          [{"field": "note", "type": "plain_text_edit"}])
    assert "control: 'text_edit'" in projection["fields_block"]


def test_a_control_the_contract_does_not_define_is_refused():
    with pytest.raises(ValueError, match="not a FieldControl member"):
        _project([{"field": "note", "enum_name": "Note"}],
                 [{"field": "note", "type": "rich_text"}])


def test_combo_options_carry_their_own_label_keys():
    projection = _project(
        [{"field": "kind", "enum_name": "Kind"}],
        [{"field": "kind", "type": "static_combo",
          "combo_values": [{"value": "Alpha"}, {"value": "Beta"}]}])
    block = projection["fields_block"]
    assert "labelKey: 'book_status.type.Alpha'" in block
    assert "labelKey: 'book_status.type.Beta'" in block


def test_a_quote_in_a_model_value_does_not_close_the_string():
    projection = _project(
        [{"field": "kind", "enum_name": "Kind"}],
        [{"field": "kind", "type": "static_combo",
          "combo_values": [{"value": "won't"}]}])
    assert r"value: 'won\'t'" in projection["fields_block"]


def test_display_field_prefers_the_name_column():
    assert _project([{"field": "code", "enum_name": "Code"}],
                    key_field="code")["display_field"] == "code"
    assert _project([{"field": "code", "enum_name": "Code"},
                     {"field": "name", "enum_name": "Name"}],
                    key_field="code")["display_field"] == "name"


def test_an_entity_with_no_display_column_emits_an_empty_one():
    projection = _project([{"field": "modified_at", "enum_name": "ModifiedAt"}])
    assert projection["display_field"] == ""


def _emit_fields(tmp_path, key_field):
    """Emit one entity that carries no detail-field table, so the fields
    the projection reads are the auto-default shape `generate_from_model`
    builds for it.

    Four entities in the registry are in this position today, `compute.app`
    among them. Every case above hands `_project` its fields, so none of
    them reaches this block.
    """
    natural_key = "name" if key_field == "name" else "code"
    extra = "" if key_field == "name" else f"""
** name
:PROPERTIES:
:type:     text
:cpp_type: std::string
:END:

The display name.
"""
    model = tmp_path / "ores.testcomp.thing.org"
    model.write_text(f"""\
:PROPERTIES:
:ID: 00000000-0000-0000-0000-000000000042
:END:
#+title: ores.testcomp.thing
#+description: Fixture for the auto-default detail-field shape.
#+type: ores.codegen.entity
#+component: testcomp
#+entity_singular: thing
#+entity_plural: things
#+entity_title: Thing

* Flags
:PROPERTIES:
:schema:    public
:product:   ores
:component: testcomp
:profile:   uuid-identified-lookup
:END:

* Columns

** id
:PROPERTIES:
:type:        uuid
:cpp_type:    boost::uuids::uuid
:primary_key: true
:END:

UUID primary key.

** {natural_key}
:PROPERTIES:
:type:        text
:cpp_type:    std::string
:natural_key: true
:END:

The key field.
{extra}
** description
:PROPERTIES:
:type:     text
:cpp_type: std::string
:nullable: true
:END:

A description.

* SQL

** Flags
:PROPERTIES:
:tablename: ores_testcomp_things_tbl
:END:

* C++

** Flags
:PROPERTIES:
:subcomponent: api
:END:

** Repository
:PROPERTIES:
:entity_singular_short: thing
:entity_plural_short:   things
:entity_singular_words: test thing
:entity_plural_words:   test things
:END:

** Presentation
:PROPERTIES:
:domain_include:       ores.testcomp.api/domain/thing.hpp
:domain_class:         testcomp::domain::thing
:protocol_include:     ores.testcomp.api/messaging/thing_protocol.hpp
:collection_name:      things
:key_field:            {key_field}
:has_uuid_primary_key: true
:END:

*** Columns

| enum_name   | field       | header      | type | width |
|-------------+-------------+-------------+------+-------|
| Code        | code        | Code        | text | auto  |
| Name        | name        | Name        | text | auto  |
| Description | description | Description | text | auto  |
""", encoding="utf-8")
    output = tmp_path / "out"
    output.mkdir()
    generate_from_model(
        str(model),
        CODEGEN / "library" / "data",
        CODEGEN / "library" / "templates",
        output,
        is_processing_batch=True,
        target_template="ts_ui.ts.mustache",
        target_output="thing_ui.ts",
    )
    text = (output / "thing_ui.ts").read_text(encoding="utf-8")
    return text.split("Fields: readonly FieldMeta[] = [", 1)[1].split("];", 1)[0]


def test_the_auto_default_form_carries_a_name_keyed_entity_once(tmp_path):
    """An entity whose key field IS its display name must emit one field.

    The default shape is a key row plus a display-name row. When the key
    is `name` the two would bind the same column twice, so the key row
    carries it alone.
    """
    block = _emit_fields(tmp_path, key_field="name")
    assert block.count("name: 'name'") == 1
    assert "name: 'description'" in block
    assert "isKey: true" in block


def test_the_auto_default_form_keeps_code_plus_name(tmp_path):
    block = _emit_fields(tmp_path, key_field="code")
    assert "name: 'code'" in block
    assert "name: 'name'" in block
    assert "name: 'description'" in block
