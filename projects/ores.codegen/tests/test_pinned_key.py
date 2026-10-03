"""Tests for pinned keys.

Run::

    python3 -m pytest projects/ores.codegen/tests/test_pinned_key.py

A row may copy facts from another row, such as an instrument copying its
trade's type, only when a key holds the copy to its source. The pinned key
lists the copied columns and the source columns they must equal. Against an
immutable source the database enforces it as a constraint, which needs the
source to declare those columns as a key; against a temporal source the
insert trigger checks the source's current row.
"""
import sys
from pathlib import Path

import pytest

REPO_ROOT = Path(__file__).resolve().parents[3]
sys.path.insert(0, str(REPO_ROOT / "projects/ores.codegen/src"))

from codegen.core import generate_from_model  # noqa: E402

CODEGEN = REPO_ROOT / "projects/ores.codegen"
DATA_DIR = CODEGEN / "library" / "data"
TEMPLATES_DIR = CODEGEN / "library" / "templates"

SOURCE = """\
:PROPERTIES:
:ID: 00000000-0000-0000-0000-0000000000F1
:END:
#+title: ores.testcomp.source_record
#+type: ores.codegen.entity
#+component: testcomp
#+entity_singular: source_record
#+entity_plural: source_records
#+entity_title: Source Record
#+coding_scheme: none
#+image_id: false

A row whose facts others copy.

* Flags
:PROPERTIES:
:schema:        public
:product:       ores
:component:     testcomp
:subcomponent:  api
:has_tenant_id: true
{read_only}:END:

* Columns

** id
:PROPERTIES:
:type:         uuid
:cpp_type:     boost::uuids::uuid
:primary_key:  true
:END:

The row identity.

** kind
:PROPERTIES:
:type:     text
:cpp_type: std::string
:nullable: false
:END:

A fact other rows copy.

* SQL
** Flags
:PROPERTIES:
:tablename:     ores_testcomp_source_records_tbl
{sql_flags}:END:
{indexes}"""

COPIER = """\
:PROPERTIES:
:ID: 00000000-0000-0000-0000-0000000000F2
:END:
#+title: ores.testcomp.copier_record
#+type: ores.codegen.entity
#+component: testcomp
#+entity_singular: copier_record
#+entity_plural: copier_records
#+entity_title: Copier Record
#+coding_scheme: none
#+image_id: false

A row that copies its source's kind.

* Flags
:PROPERTIES:
:schema:        public
:product:       ores
:component:     testcomp
:subcomponent:  api
:has_tenant_id: true
:END:

* Columns

** id
:PROPERTIES:
:type:         uuid
:cpp_type:     boost::uuids::uuid
:primary_key:  true
:END:

The row identity.

** source_id
:PROPERTIES:
:type:     uuid
:cpp_type: boost::uuids::uuid
:nullable: false
:END:

The source this row belongs to.

** source_kind
:PROPERTIES:
:type:     text
:cpp_type: std::string
:nullable: false
:END:

The source's kind, copied.

* SQL
** Flags
:PROPERTIES:
:tablename: ores_testcomp_copier_records_tbl
:END:

** Pinned keys

| name   | columns                | table                            | target_columns | error_message                              |
|--------+------------------------+----------------------------------+----------------+--------------------------------------------|
| source | {columns} | ores_testcomp_source_records_tbl | {target_columns}       | Invalid source_id: %. Kind must match.     |
"""

IMMUTABLE = ":current_state: true\n:immutable:     true\n"
READ_ONLY = ":read_only:     true\n"
KIND_KEY = """
** Indexes

| name        | columns                | unique | current_only | where_extra |
|-------------+------------------------+--------+--------------+-------------|
| source_kind | tenant_id, id, kind    | true   | false        |             |
"""


def _render(tmp_path, immutable=True, indexes=KIND_KEY,
            columns="source_id, source_kind", target_columns="id, kind",
            source_table="ores_testcomp_source_records_tbl",
            edit_copier=lambda body: body):
    modeling = tmp_path / "projects" / "ores.testcomp" / "modeling"
    modeling.mkdir(parents=True)
    (modeling / "ores.testcomp.source_record.org").write_text(
        SOURCE.format(read_only=READ_ONLY if immutable else "",
                      sql_flags=IMMUTABLE if immutable else "",
                      indexes=indexes),
        encoding="utf-8")
    copier = modeling / "ores.testcomp.copier_record.org"
    copier.write_text(
        edit_copier(
            COPIER.format(columns=columns, target_columns=target_columns)
            .replace("| ores_testcomp_source_records_tbl |",
                     f"| {source_table} |")),
        encoding="utf-8")
    output_dir = tmp_path / "out"
    output_dir.mkdir()
    generate_from_model(
        str(copier), DATA_DIR, TEMPLATES_DIR, output_dir,
        is_processing_batch=True,
        target_template="sql_schema_domain_entity_create.mustache",
        target_output="create.sql")
    return (output_dir / "create.sql").read_text(encoding="utf-8")


def test_a_pin_to_an_immutable_source_is_a_constraint(tmp_path):
    sql = _render(tmp_path)

    assert ('constraint ores_testcomp_copier_records_source_pin foreign key '
            '("tenant_id", "source_id", "source_kind") references '
            '"ores_testcomp_source_records_tbl" ("tenant_id", "id", "kind")'
            ) in sql
    assert "Validate the source pin" not in sql


def test_a_pin_to_a_temporal_source_is_a_trigger_check(tmp_path):
    sql = _render(tmp_path, immutable=False, indexes="")

    assert "_pin foreign key" not in sql
    assert "-- Validate the source pin to ores_testcomp_source_records_tbl" in sql
    assert "and id = NEW.source_id" in sql
    assert "and kind = NEW.source_kind" in sql
    assert "where tenant_id = NEW.tenant_id" in sql


def test_a_pin_needs_a_key_on_an_immutable_source(tmp_path):
    with pytest.raises(ValueError, match="has no primary key or unique index"):
        _render(tmp_path, indexes="")


def test_a_pin_needs_as_many_target_columns_as_columns(tmp_path):
    with pytest.raises(ValueError, match="lists 2 columns but 1 target"):
        _render(tmp_path, target_columns="id")


def test_a_pin_to_an_unknown_table_is_refused(tmp_path):
    with pytest.raises(ValueError, match="no model declares the table"):
        _render(tmp_path, source_table="ores_testcomp_missing_tbl")


def test_a_pin_message_needs_one_placeholder(tmp_path):
    def edit(body):
        return body.replace("Invalid source_id: %. Kind", "Invalid source. Kind")

    with pytest.raises(ValueError, match="exactly one %"):
        _render(tmp_path, immutable=False, indexes="", edit_copier=edit)


def test_a_pin_over_an_unknown_column_is_refused(tmp_path):
    with pytest.raises(ValueError, match="this model declares no column source_sort"):
        _render(tmp_path, columns="source_id, source_sort")


def test_a_pin_to_an_unknown_source_column_is_refused(tmp_path):
    with pytest.raises(ValueError, match="declares no column sort"):
        _render(tmp_path, immutable=False, indexes="", target_columns="id, sort")


def test_pin_names_must_not_repeat(tmp_path):
    def edit(body):
        row = next(line for line in body.splitlines()
                   if line.startswith("| source "))
        return body.replace(row, row + "\n" + row)

    with pytest.raises(ValueError, match="pinned key names repeat: source"):
        _render(tmp_path, edit_copier=edit)
