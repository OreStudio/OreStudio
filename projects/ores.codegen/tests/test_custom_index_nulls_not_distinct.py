"""Tests for the ``nulls_not_distinct`` flag on a custom model index.

Run::

    python3 -m pytest projects/ores.codegen/tests/test_custom_index_nulls_not_distinct.py

A model's ``** Indexes`` table declares an index over raw column text. Some of
those columns admit SQL NULL, and PostgreSQL treats NULL as distinct from every
other NULL in a unique index, so the index does not stop two rows that both
leave a column empty from sharing the rest. ``:nulls_not_distinct: true`` on the
row states the clause; the default is absent, so every existing index keeps the
bytes it had.
"""
import sys
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[3]
sys.path.insert(0, str(REPO_ROOT / "projects/ores.codegen/src"))

from codegen.core import generate_from_model  # noqa: E402

CODEGEN_BASE = REPO_ROOT / "projects/ores.codegen"
DATA_DIR = CODEGEN_BASE / "library" / "data"
TEMPLATES_DIR = CODEGEN_BASE / "library" / "templates"

MODEL = """\
:PROPERTIES:
:ID: 00000000-0000-0000-0000-0000000000F2
:END:
#+title: ores.testcomp.index_entity
#+type: ores.codegen.entity
#+component: testcomp
#+entity_singular: index_entity
#+entity_plural: index_entities
#+entity_title: Index Entity
#+has_tenant_id: true
#+coding_scheme: none
#+image_id: false

An entity whose model declares custom indexes.

* Flags
:PROPERTIES:
:schema:    public
:product:   ores
:component: testcomp
:subcomponent: api
:has_tenant_id: true
:END:

* Columns

** name
:PROPERTIES:
:type:        text
:cpp_type:    std::string
:END:

The name.

** id
:PROPERTIES:
:type:        uuid
:cpp_type:    boost::uuids::uuid
:primary_key: true
:END:

The surrogate key.

* SQL

** Flags
:PROPERTIES:
:tablename: ores_testcomp_index_entities_tbl
:END:

** Indexes

| name    | columns          | unique | current_only | where_extra    | nulls_not_distinct |
|---------+------------------+--------+--------------+----------------+--------------------|
| natural | tenant_id, name  | true   | false        | name <> 'none' | true               |
| plain   | tenant_id, name  | true   | false        |                | false              |
"""


def _generate_sql(tmp_path):
    model_path = tmp_path / "ores.testcomp.index_entity.org"
    model_path.write_text(MODEL, encoding="utf-8")
    output_dir = tmp_path / "out"
    output_dir.mkdir()
    generate_from_model(
        str(model_path),
        DATA_DIR,
        TEMPLATES_DIR,
        output_dir,
        is_processing_batch=True,
        target_template="sql_schema_domain_entity_create.mustache",
        target_output="index_entity_create.sql",
    )
    return (output_dir / "index_entity_create.sql").read_text(encoding="utf-8")


def test_a_flagged_index_states_nulls_not_distinct(tmp_path):
    sql = _generate_sql(tmp_path)
    assert (
        'on "ores_testcomp_index_entities_tbl" (tenant_id, name) nulls not distinct\n'
        "where name <> 'none';"
    ) in sql


def test_an_unflagged_index_states_no_null_clause(tmp_path):
    sql = _generate_sql(tmp_path)
    assert (
        'on "ores_testcomp_index_entities_tbl" (tenant_id, name);'
    ) in sql
