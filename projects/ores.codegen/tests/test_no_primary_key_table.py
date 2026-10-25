"""Tests for the keyless-table shape in sql_schema_domain_entity_create.mustache.

Run::

    python3 -m pytest projects/ores.codegen/tests/test_no_primary_key_table.py

A model that states ``:no_primary_key: true`` and renders its SQL facet is a
table with no key: rows are appended and never replaced. The create therefore
has no key clause, no validity window, no key-derived check, no insert trigger,
no delete rule and no validation function, and the drop script has no trigger,
rule or function to remove. The model's own indexes stay.

Two controls keep the shape honest. A keyed table of the same shape (hypertable,
no audit tail) keeps its key and its triggers, so the gate cannot pass by
removing them everywhere. And a keyless table still renders the index its model
declares, so the gate cannot drop the model's own DDL.
"""
import sys
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[3]
sys.path.insert(0, str(REPO_ROOT / "projects/ores.codegen/src"))

from codegen.core import generate_from_model  # noqa: E402

CODEGEN_BASE = REPO_ROOT / "projects/ores.codegen"
DATA_DIR = CODEGEN_BASE / "library" / "data"
TEMPLATES_DIR = CODEGEN_BASE / "library" / "templates"

_KEYLESS_MODEL = """\
:PROPERTIES:
:ID: 00000000-0000-0000-0000-000000000004
:END:
#+title: ores.testcomp.keyless_sample
#+type: ores.codegen.entity
#+component: testcomp
#+entity_singular: keyless_sample
#+entity_plural: keyless_samples
#+entity_title: Keyless Sample

A table with no key: rows are appended and never replaced.

* Flags
:PROPERTIES:
:schema:          public
:product:         ores
:component:       testcomp
:has_tenant_id:   true
:no_primary_key:  true
:sql_only:        true
:END:

* Columns

** party_id
:PROPERTIES:
:type:     uuid
:cpp_type: boost::uuids::uuid
:END:

The owning party.

** ts
:PROPERTIES:
:type:     timestamp with time zone
:cpp_type: std::chrono::system_clock::time_point
:END:

The instant of the row.

* SQL

** Flags
:PROPERTIES:
:tablename:                   ores_testcomp_keyless_samples_tbl
:hypertable:                  true
:hypertable_partition_column: ts
:hypertable_chunk_interval:   1 day
:no_audit_columns:            true
:index_prefix:                keyless_samples
:rls_tenant_isolation:        true
:END:

** Indexes

| name      | columns            | unique | current_only | where_extra |
|-----------+--------------------+--------+--------------+-------------|
| series_ts | series_id, ts desc | false  | false        |             |
"""

_KEYED_MODEL = _KEYLESS_MODEL.replace(
    ":no_primary_key:  true\n", "").replace(
    "** party_id", "** id").replace(
    "The owning party.", "The key.").replace(
    ":type:     uuid\n:cpp_type: boost::uuids::uuid\n",
    ":type:     uuid\n:cpp_type: boost::uuids::uuid\n:primary_key: true\n")


def _generate(tmp_path, model, template, output):
    model_path = tmp_path / "ores.testcomp.sample.org"
    model_path.write_text(model, encoding="utf-8")
    output_dir = tmp_path / "out"
    output_dir.mkdir(exist_ok=True)
    generate_from_model(
        str(model_path),
        DATA_DIR,
        TEMPLATES_DIR,
        output_dir,
        is_processing_batch=True,
        target_template=template,
        target_output=output,
    )
    return (output_dir / output).read_text(encoding="utf-8")


def _keyless_create(tmp_path):
    return _generate(tmp_path, _KEYLESS_MODEL,
                     "sql_schema_domain_entity_create.mustache",
                     "keyless_create.sql")


def _keyless_drop(tmp_path):
    return _generate(tmp_path, _KEYLESS_MODEL,
                     "sql_schema_domain_entity_drop.mustache",
                     "keyless_drop.sql")


def test_a_keyless_table_has_no_key_clause_and_no_validity_window(tmp_path):
    sql = _keyless_create(tmp_path)
    assert "primary key" not in sql
    assert "valid_from" not in sql
    assert "valid_to" not in sql
    assert 'exclude using gist' not in sql


def test_a_keyless_table_ends_its_column_list_without_a_trailing_comma(tmp_path):
    sql = _keyless_create(tmp_path)
    assert '    "ts" timestamp with time zone not null\n);' in sql


def test_a_keyless_table_has_no_trigger_rule_or_validation_function(tmp_path):
    sql = _keyless_create(tmp_path)
    assert "create or replace function" not in sql
    assert "create or replace trigger" not in sql
    assert "create or replace rule" not in sql


def test_a_keyless_table_keeps_the_index_its_model_declares(tmp_path):
    sql = _keyless_create(tmp_path)
    assert ("create index if not exists keyless_samples_series_ts_idx\n"
            'on "ores_testcomp_keyless_samples_tbl" (series_id, ts desc);') in sql
    assert "_id_uniq_idx" not in sql
    assert "_version_uniq_idx" not in sql
    assert "_tenant_idx" not in sql


def test_a_keyless_drop_script_drops_the_table_alone(tmp_path):
    sql = _keyless_drop(tmp_path)
    assert 'drop table if exists "ores_testcomp_keyless_samples_tbl";' in sql
    assert "drop trigger" not in sql
    assert "drop rule" not in sql
    assert "drop function" not in sql


def test_a_keyed_table_keeps_its_key_and_its_triggers(tmp_path):
    sql = _generate(tmp_path, _KEYED_MODEL,
                    "sql_schema_domain_entity_create.mustache",
                    "keyed_create.sql")
    assert "primary key (" in sql
    assert "valid_from" in sql
    assert "create or replace trigger" in sql
