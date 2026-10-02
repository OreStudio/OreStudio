"""Tests for the immutable entity flag.

Run::

    python3 -m pytest projects/ores.codegen/tests/test_immutable_entity.py

An immutable entity is a current-state, read-only table whose rows are
written once and never changed. The trade and structure anchors are its
first consumers: everything else references them by key, so a row that
could change under its references would break them. The store refuses the
change, whoever asks, and the repository inserts rather than replaces so a
second write of the same key fails instead of overwriting.
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

MODEL = """\
:PROPERTIES:
:ID: 00000000-0000-0000-0000-0000000000D7
:END:
#+title: ores.testcomp.anchor_record
#+type: ores.codegen.entity
#+component: testcomp
#+entity_singular: anchor_record
#+entity_plural: anchor_records
#+entity_title: Anchor Record
#+coding_scheme: none
#+image_id: false

A row written once that other rows reference by key.

* Flags
:PROPERTIES:
:schema:        public
:product:       ores
:component:     testcomp
:subcomponent:  api
:has_tenant_id: false
{read_only}:END:

* Columns

** id
:PROPERTIES:
:type:         uuid
:cpp_type:     boost::uuids::uuid
:primary_key:  true
:END:

The row identity.

** label
:PROPERTIES:
:type:     text
:cpp_type: std::string
:nullable: false
:END:

A fact fixed when the row is written.

* SQL
** Flags
:PROPERTIES:
{sql_flags}:END:
"""

READ_ONLY = ":read_only:     true\n"
CURRENT_STATE = ":current_state: true\n"
IMMUTABLE = ":immutable:     true\n"


def _model(read_only=True, current_state=True, immutable=True):
    return MODEL.format(
        read_only=READ_ONLY if read_only else "",
        sql_flags=(CURRENT_STATE if current_state else "")
        + (IMMUTABLE if immutable else ""))


def _render(tmp_path, template, output_name, body):
    model_path = tmp_path / "ores.testcomp.anchor_record.org"
    model_path.write_text(body, encoding="utf-8")
    output_dir = tmp_path / output_name
    output_dir.mkdir()
    generate_from_model(
        str(model_path), DATA_DIR, TEMPLATES_DIR, output_dir,
        is_processing_batch=True,
        target_template=template, target_output=output_name)
    return (output_dir / output_name).read_text(encoding="utf-8")


def _create_sql(tmp_path, body):
    return _render(tmp_path, "sql_schema_domain_entity_create.mustache",
                   "anchor_records_create.sql", body)


def _drop_sql(tmp_path, body):
    return _render(tmp_path, "sql_schema_domain_entity_drop.mustache",
                   "anchor_records_drop.sql", body)


def _repository(tmp_path, body):
    return _render(tmp_path, "cpp_domain_type_repository.cpp.mustache",
                   "anchor_record_repository.cpp", body)


def test_the_store_refuses_update_delete_and_truncate(tmp_path):
    sql = _create_sql(tmp_path, _model())

    assert "create or replace function ores_testcomp_anchor_records_immutable_fn()" in sql
    assert "before update or delete on \"ores_testcomp_anchor_records_tbl\"" in sql
    assert "errcode = '55000'" in sql
    assert "before truncate on \"ores_testcomp_anchor_records_tbl\"" in sql


def test_the_drop_script_removes_the_guard(tmp_path):
    sql = _drop_sql(tmp_path, _model())

    assert "drop trigger if exists ores_testcomp_anchor_records_immutable_trg" in sql
    assert "drop trigger if exists ores_testcomp_anchor_records_immutable_truncate_trg" in sql
    assert "drop function if exists ores_testcomp_anchor_records_immutable_fn" in sql


def test_the_repository_inserts_and_never_replaces(tmp_path):
    repository = _repository(tmp_path, _model())

    assert "sqlgen::insert(anchor_record_mapper::map(t))" in repository
    assert "sqlgen::insert(anchor_record_mapper::map(batch))" in repository
    assert "insert_or_replace" not in repository


def test_a_mutable_current_state_table_has_no_guard(tmp_path):
    sql = _create_sql(tmp_path, _model(immutable=False))

    assert "immutable" not in sql


def test_a_mutable_current_state_repository_keeps_its_upsert(tmp_path):
    repository = _repository(tmp_path, _model(immutable=False))

    assert "insert_or_replace" in repository


@pytest.mark.parametrize("read_only,current_state", [
    (False, True),
    (True, False),
])
def test_the_flag_is_refused_without_its_prerequisites(
        tmp_path, read_only, current_state):
    body = _model(read_only=read_only, current_state=current_state)

    with pytest.raises(ValueError, match=":immutable: needs"):
        _create_sql(tmp_path, body)
