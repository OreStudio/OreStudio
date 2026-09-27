"""Tests for the ``* Artefact columns`` section and the two artefact templates.

An artefact/staging table is usually a projection of the entity's own
table. It is not always: reading the 12 divergent tables in ores.dq
against their publish functions showed six deliberate differences -- a
renamed key, a natural key where the store holds a uuid, a redacted
secret, a redacted field the publisher supplies, a field only the import
needs, and a field that belongs to the dataset. The section states that
projection in the model that already owns the table.

Absent, the templates must render exactly what they rendered before the
section existed; the first test in each group pins that.

Run::

    python3 -m pytest projects/ores.codegen/tests/test_artefact_columns.py
"""
import sys
from pathlib import Path

import pytest

REPO_ROOT = Path(__file__).resolve().parents[3]
sys.path.insert(0, str(REPO_ROOT / "projects/ores.codegen/src"))

from codegen.core import generate_from_model  # noqa: E402

CODEGEN_BASE = REPO_ROOT / "projects/ores.codegen"
DATA_DIR = CODEGEN_BASE / "library" / "data"
TEMPLATES_DIR = CODEGEN_BASE / "library" / "templates"

HEADER = """\
:PROPERTIES:
:ID: 00000000-0000-0000-0000-0000000000c1
:END:
#+title: ores.testcomp.test_entity
#+type: ores.codegen.entity
#+component: testcomp
#+entity_singular: test_entity
#+entity_plural: test_entities
#+entity_title: Test Entity
#+has_tenant_id: true
#+coding_scheme: none
#+image_id: false

A test entity.
"""

FLAGS = """\

* Flags
:PROPERTIES:
:schema:    public
:product:   ores
:component: testcomp
:subcomponent: api
:END:
"""

COLUMNS = """\

* Columns

** code
:PROPERTIES:
:type:        text
:cpp_type:    std::string
:primary_key: true
:END:

The key.

** name
:PROPERTIES:
:type:     text
:cpp_type: std::string
:nullable: false
:END:

A plain column.
"""

# tag_id renames the key, version and name follow it. This is the shape
# ores.dq.tags needs: the entity table keys on id, the staging table on
# tag_id.
ARTEFACT_COLUMNS = """\

* Artefact columns

** tag_id
:PROPERTIES:
:type:     uuid
:cpp_type: std::string
:nullable: false
:END:

The staging key, renamed from the entity's id.

** version
:PROPERTIES:
:type:     integer
:cpp_type: int
:nullable: false
:END:

** name
:PROPERTIES:
:type:     text
:cpp_type: std::string
:nullable: false
:END:
"""

ARTEFACT_COLUMNS_WITH_KEY = """\

* Artefact columns
:PROPERTIES:
:key: tag_id
:END:

** version
:PROPERTIES:
:type:     integer
:cpp_type: int
:nullable: false
:END:

** tag_id
:PROPERTIES:
:type:     uuid
:cpp_type: std::string
:nullable: false
:END:

** name
:PROPERTIES:
:type:     text
:cpp_type: std::string
:nullable: false
:END:
"""

EMPTY_SECTION = """\

* Artefact columns
"""

# A declared extra index, and the unique flag the tags staging table needs:
# its seed writes `on conflict (tenant_id, dataset_id, name)`, which needs a
# unique arbiter index or PostgreSQL refuses the clause at run time.
ARTEFACT_INDEXES = """\

* Artefact indexes

** tag_identity
:PROPERTIES:
:columns: tenant_id, dataset_id, name
:unique: true
:END:
"""

ARTEFACT_INDEXES_NOT_UNIQUE = """\

* Artefact indexes

** tag_identity
:PROPERTIES:
:columns: tenant_id, dataset_id, name
:unique: false
:END:
"""

INSERT_FN_WITH_SECTION = ARTEFACT_COLUMNS


def _render(tmp_path, body, template, out):
    model_path = tmp_path / f"{out}.org"
    model_path.write_text(body, encoding="utf-8")
    output_dir = tmp_path / "out"
    output_dir.mkdir(exist_ok=True)
    generate_from_model(
        str(model_path),
        DATA_DIR,
        TEMPLATES_DIR,
        output_dir,
        is_processing_batch=True,
        target_template=template,
        target_output=f"{out}.sql",
    )
    return (output_dir / f"{out}.sql").read_text(encoding="utf-8")


def generate_domain(tmp_path, extra="", out="domain_artefact"):
    body = HEADER + FLAGS + COLUMNS + extra
    return _render(
        tmp_path,
        body,
        "sql_schema_domain_entity_artefact_create.mustache",
        out,
    )


def test_absent_section_keeps_the_projection_of_the_entity_table(tmp_path):
    sql = generate_domain(tmp_path)

    assert 'create table if not exists "ores_dq_test_entities_artefact_tbl" (' in sql
    assert '    "code" text not null,\n    "version" integer not null,\n' in sql
    assert '    "name" text not null\n);' in sql
    # The key index is built on the entity's own key.
    assert (
        "create index if not exists dq_test_entities_artefact_code_idx\n"
        "on ores_dq_test_entities_artefact_tbl (code);"
    ) in sql


def test_a_declared_section_replaces_the_body(tmp_path):
    sql = generate_domain(tmp_path, ARTEFACT_COLUMNS)

    assert (
        'create table if not exists "ores_dq_test_entities_artefact_tbl" (\n'
        '    "dataset_id" uuid not null,\n'
        '    "tenant_id" uuid not null,\n'
        '    "tag_id" uuid not null,\n'
        '    "version" integer not null,\n'
        '    "name" text not null\n'
        ");"
    ) in sql


def test_a_declared_section_drops_the_entity_columns_it_does_not_list(tmp_path):
    sql = generate_domain(tmp_path, ARTEFACT_COLUMNS)

    # The entity keys on `code`. The staging table does not carry it.
    assert '"code" text not null' not in sql


def test_the_key_index_is_built_once_for_the_declared_key(tmp_path):
    """The key index belongs to the staging table, not to each column.

    It was emitted inside the artefact-columns loop, so a three-column table
    rendered it three times and ores.dq.currencies rendered its own seventeen
    times. Asserting the exact count is the point: asserting that the text is
    present passed for the whole life of the defect.
    """
    sql = generate_domain(tmp_path, ARTEFACT_COLUMNS)

    assert sql.count("create index if not exists dq_test_entities_artefact_tag_id_idx") == 1
    assert (
        "create index if not exists dq_test_entities_artefact_tag_id_idx\n"
        "on ores_dq_test_entities_artefact_tbl (tag_id);"
    ) in sql
    assert "artefact_code_idx" not in sql


def test_a_declared_artefact_index_is_unique_when_it_says_so(tmp_path):
    sql = generate_domain(tmp_path, ARTEFACT_COLUMNS + ARTEFACT_INDEXES)

    assert (
        "create unique index if not exists dq_test_entities_artefact_tag_identity_idx\n"
        "on ores_dq_test_entities_artefact_tbl (tenant_id, dataset_id, name);"
    ) in sql


def test_a_declared_artefact_index_is_not_unique_when_it_says_false(tmp_path):
    """``:unique: false:`` must be false, not the non-empty string "false",
    which a mustache section reads as true."""
    sql = generate_domain(tmp_path, ARTEFACT_COLUMNS + ARTEFACT_INDEXES_NOT_UNIQUE)

    assert (
        "create index if not exists dq_test_entities_artefact_tag_identity_idx\n"
        "on ores_dq_test_entities_artefact_tbl (tenant_id, dataset_id, name);"
    ) in sql
    assert "create unique index if not exists dq_test_entities_artefact_tag_identity_idx" not in sql


def test_the_key_property_overrides_the_first_column(tmp_path):
    sql = generate_domain(tmp_path, ARTEFACT_COLUMNS_WITH_KEY)

    assert (
        "create index if not exists dq_test_entities_artefact_tag_id_idx\n"
        "on ores_dq_test_entities_artefact_tbl (tag_id);"
    ) in sql


def test_the_section_keeps_the_artefact_header_indexes(tmp_path):
    sql = generate_domain(tmp_path, ARTEFACT_COLUMNS)

    assert "dq_test_entities_artefact_dataset_idx" in sql
    assert "dq_test_entities_artefact_tenant_idx" in sql


def test_an_empty_section_is_refused(tmp_path):
    with pytest.raises(ValueError, match="declares no column"):
        generate_domain(tmp_path, EMPTY_SECTION)


def test_a_section_alongside_the_insert_helper_is_refused(tmp_path):
    """The generated helper is built from the entity's own columns, so it
    would insert the wrong shape into a staging table that declares its
    own."""
    body = (
        HEADER.replace("#+image_id: false", "#+image_id: false\n#+has_artefact_insert_fn: true")
        + FLAGS
        + COLUMNS
        + INSERT_FN_WITH_SECTION
    )

    with pytest.raises(ValueError, match="has_artefact_insert_fn"):
        _render(
            tmp_path,
            body,
            "sql_schema_domain_entity_artefact_create.mustache",
            "insert_fn",
        )


def test_the_lookup_entity_template_honours_the_section_too(tmp_path):
    """The FpML JSON models reach the other archetype, so it needs the
    same switch."""
    body = HEADER.replace(
        "#+type: ores.codegen.entity", "#+type: ores.codegen.lookup_entity"
    ) + FLAGS + COLUMNS + ARTEFACT_COLUMNS

    sql = _render(
        tmp_path,
        body,
        "sql_schema_artefact_create.mustache",
        "lookup_artefact",
    )

    assert '    "tag_id" uuid not null,' in sql
    assert (
        "create index if not exists dq_test_entities_artefact_tag_id_idx\n"
        "on ores_dq_test_entities_artefact_tbl (tag_id);"
    ) in sql


def test_the_lookup_entity_template_marks_a_unique_index_too(tmp_path):
    """The loader decodes the flag for both archetypes, so a template that
    ignored it would render a plain index and say nothing about it."""
    body = HEADER.replace(
        "#+type: ores.codegen.entity", "#+type: ores.codegen.lookup_entity"
    ) + FLAGS + COLUMNS + ARTEFACT_INDEXES

    sql = _render(
        tmp_path,
        body,
        "sql_schema_artefact_create.mustache",
        "lookup_artefact_unique",
    )

    assert (
        "create unique index if not exists dq_test_entities_artefact_tag_identity_idx\n"
        "on ores_dq_test_entities_artefact_tbl (tenant_id, dataset_id, name);"
    ) in sql


def test_the_lookup_entity_template_is_unchanged_without_the_section(tmp_path):
    body = HEADER.replace(
        "#+type: ores.codegen.entity", "#+type: ores.codegen.lookup_entity"
    ) + FLAGS + COLUMNS

    sql = _render(
        tmp_path,
        body,
        "sql_schema_artefact_create.mustache",
        "lookup_default",
    )

    assert '    "code" text not null,\n    "version" integer not null,\n' in sql
    assert (
        "create index if not exists dq_test_entities_artefact_code_idx\n"
        "on ores_dq_test_entities_artefact_tbl (code);"
    ) in sql


# --- natural keys -----------------------------------------------------------
#
# The templates used to render only the plain columns, so an entity with a
# natural key got a staging table missing that column -- silently, because
# the SQL still parsed. Every model that used the archetype had zero natural
# keys, so the gap stayed latent until ores.dq.badge_definition opted in.

COLUMNS_WITH_NATURAL_KEY = """\

* Columns

** code
:PROPERTIES:
:type:        text
:cpp_type:    std::string
:primary_key: true
:END:

** name
:PROPERTIES:
:type:        text
:cpp_type:    std::string
:natural_key: true
:END:

** description
:PROPERTIES:
:type:     text
:cpp_type: std::string
:nullable: false
:END:
"""

COLUMNS_ONLY_NATURAL_KEY = """\

* Columns

** code
:PROPERTIES:
:type:        text
:cpp_type:    std::string
:primary_key: true
:END:

** name
:PROPERTIES:
:type:        text
:cpp_type:    std::string
:natural_key: true
:END:
"""


def test_a_natural_key_is_rendered_before_the_plain_columns(tmp_path):
    body = HEADER + FLAGS + COLUMNS_WITH_NATURAL_KEY

    sql = _render(
        tmp_path, body, "sql_schema_domain_entity_artefact_create.mustache", "nat_key"
    )

    assert (
        '    "code" text not null,\n'
        '    "version" integer not null,\n'
        '    "name" text not null,\n'
        '    "description" text not null\n'
        ");"
    ) in sql


def test_the_last_natural_key_keeps_its_comma_when_a_plain_column_follows(tmp_path):
    body = HEADER + FLAGS + COLUMNS_WITH_NATURAL_KEY

    sql = _render(
        tmp_path, body, "sql_schema_domain_entity_artefact_create.mustache", "nat_comma"
    )

    # Without the comma the CREATE TABLE would not parse.
    assert '"name" text not null,\n' in sql


def test_an_entity_whose_only_key_is_natural_has_no_trailing_comma(tmp_path):
    body = HEADER + FLAGS + COLUMNS_ONLY_NATURAL_KEY

    sql = _render(
        tmp_path, body, "sql_schema_domain_entity_artefact_create.mustache", "nat_only"
    )

    assert (
        '    "code" text not null,\n'
        '    "version" integer not null,\n'
        '    "name" text not null\n'
        ");"
    ) in sql


def test_the_lookup_template_renders_the_natural_key_too(tmp_path):
    body = HEADER.replace(
        "#+type: ores.codegen.entity", "#+type: ores.codegen.lookup_entity"
    ) + FLAGS + COLUMNS_WITH_NATURAL_KEY

    sql = _render(
        tmp_path, body, "sql_schema_artefact_create.mustache", "lookup_nat_key"
    )

    assert '    "name" text not null,\n' in sql

