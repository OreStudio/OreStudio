"""Tests for the current-state entity shape (:current_state:).

Run::

    python3 -m pytest projects/ores.codegen/tests/test_current_state_shape.py

A current-state table has one row per key and no temporal axis at all: no
valid_from/valid_to, no GIST exclusion, no temporal checks, no version or
audit tail, and the model's own primary key. The SQL and C++ projections
must both reflect that, and the bi-temporal default must be untouched.
"""
import sys
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[3]
sys.path.insert(0, str(REPO_ROOT / "projects/ores.codegen/src"))

from codegen.core import generate_from_model  # noqa: E402

CODEGEN_BASE = REPO_ROOT / "projects/ores.codegen"
DATA_DIR = CODEGEN_BASE / "library" / "data"
TEMPLATES_DIR = CODEGEN_BASE / "library" / "templates"

# A current-state entity: uuid primary key on its own, a partial index,
# no audit tail. Modelled on the hand-written iam_login_info table.
CURRENT_STATE_MODEL = """\
:PROPERTIES:
:ID: 00000000-0000-0000-0000-0000000000C1
:END:
#+title: ores.testcomp.current_state_entity
#+type: ores.codegen.entity
#+component: testcomp
#+entity_singular: current_state_entity
#+entity_plural: current_state_entities
#+entity_title: Current State Entity
#+has_tenant_id: true
#+coding_scheme: none
#+image_id: false

An entity whose table carries one row per key and no history.

* Flags
:PROPERTIES:
:schema:        public
:product:       ores
:component:     testcomp
:subcomponent:  api
:has_tenant_id: true
:END:

* Columns

** account_id
:PROPERTIES:
:type:            uuid
:cpp_type:        boost::uuids::uuid
:primary_key:     true
:skip_uuid_check: true
:END:

The key column.

** locked
:PROPERTIES:
:type:     integer
:cpp_type: bool
:nullable: false
:END:

A partial-indexed flag column.

* SQL

** Flags
:PROPERTIES:
:tablename:        ores_testcomp_current_state_entities_tbl
:no_audit_columns: true
:current_state:    true
:END:

** Indexes

| name   | columns | unique | current_only | where_extra |
|--------+---------+--------+--------------+-------------|
| locked | locked  | false  | false        | locked = 0  |

* C++

** Repository
:PROPERTIES:
:entity_singular_short: current_state_entity
:entity_plural_short:   current_state_entities
:entity_singular_words: current state entity
:entity_plural_words:   current state entities
:END:
"""

# The same entity without :current_state: -- the standard bi-temporal control.
# :no_audit_columns: is dropped too, so the control exercises the default
# version + audit tail as well as the temporal columns.
BITEMPORAL_MODEL = CURRENT_STATE_MODEL.replace(
    ":current_state:    true\n", "").replace(
    ":no_audit_columns: true\n", "")


def _render(tmp_path, template, output_name, body):
    model_path = tmp_path / "ores.testcomp.current_state_entity.org"
    model_path.write_text(body, encoding="utf-8")
    output_dir = tmp_path / output_name
    output_dir.mkdir()
    generate_from_model(
        str(model_path),
        DATA_DIR,
        TEMPLATES_DIR,
        output_dir,
        is_processing_batch=True,
        target_template=template,
        target_output=output_name,
    )
    return (output_dir / output_name).read_text(encoding="utf-8")


def _sql(tmp_path, body=CURRENT_STATE_MODEL):
    return _render(tmp_path, "sql_schema_domain_entity_create.mustache",
                   "entity_create.sql", body)


def test_current_state_sql_has_no_temporal_columns(tmp_path):
    sql = _sql(tmp_path)
    assert "valid_from" not in sql
    assert "valid_to" not in sql
    assert '"version" integer not null' not in sql
    assert "modified_by" not in sql


def test_current_state_primary_key_is_the_models_own_key(tmp_path):
    sql = _sql(tmp_path)
    assert "primary key (account_id)" in sql
    assert "primary key (tenant_id, account_id" not in sql


def test_current_state_sql_has_no_gist_or_temporal_check(tmp_path):
    sql = _sql(tmp_path)
    assert "exclude using gist" not in sql
    assert 'check ("valid_from"' not in sql


def test_current_state_sql_keeps_declared_indexes_without_temporal_predicate(
        tmp_path):
    sql = _sql(tmp_path)
    assert "current_state_entities_locked_idx" in sql
    assert "where locked = 0" in sql
    # A current-only index predicate has no valid_to column to read.
    assert "ores_utility_infinity_timestamp_fn()" not in sql


def test_current_state_insert_trigger_validates_tenant(tmp_path):
    sql = _sql(tmp_path)
    assert "NEW.tenant_id := ores_iam_validate_tenant_fn(NEW.tenant_id);" in sql


def test_current_state_has_no_soft_delete_rule(tmp_path):
    sql = _sql(tmp_path)
    assert "create or replace rule" not in sql
    assert "_delete_rule" not in sql


def test_bitemporal_default_still_emits_the_temporal_shape(tmp_path):
    sql = _sql(tmp_path, BITEMPORAL_MODEL)
    assert "primary key (tenant_id, account_id, valid_from, valid_to)" in sql
    assert 'check ("valid_from" < "valid_to")' in sql
    assert "exclude using gist (" in sql
    assert "create or replace rule" in sql
    assert "where valid_to = ores_utility_infinity_timestamp_fn()" in sql


def test_current_state_domain_class_has_no_temporal_members(tmp_path):
    header = _render(tmp_path, "cpp_domain_type_class.hpp.mustache",
                     "current_state_entity.hpp", CURRENT_STATE_MODEL)
    assert "account_id" in header
    assert "recorded_at" not in header
    assert "int version" not in header
    assert "modified_by" not in header
    assert "change_reason_code" not in header


def test_current_state_entity_struct_has_no_temporal_members(tmp_path):
    header = _render(tmp_path, "cpp_domain_type_entity.hpp.mustache",
                     "current_state_entity_entity.hpp", CURRENT_STATE_MODEL)
    assert "valid_from" not in header
    assert "valid_to" not in header
    assert "int version" not in header


def test_current_state_repository_upserts_and_hard_deletes(tmp_path):
    impl = _render(tmp_path, "cpp_domain_type_repository.cpp.mustache",
                   "current_state_entity_repository.cpp", CURRENT_STATE_MODEL)
    assert "sqlgen::insert_or_replace(" in impl
    assert "valid_to" not in impl
    assert "max.value()" not in impl
    assert "read_at_version" not in impl
    assert "sqlgen::delete_from<current_state_entity_entity>" in impl


def test_current_state_service_has_no_history_accessor(tmp_path):
    header = _render(tmp_path, "cpp_service.hpp.mustache",
                     "current_state_entity_service.hpp", CURRENT_STATE_MODEL)
    assert "save_current_state_entity" in header
    assert "delete_current_state_entity" in header
    assert "_history" not in header


def test_bitemporal_domain_class_keeps_recorded_at(tmp_path):
    header = _render(tmp_path, "cpp_domain_type_class.hpp.mustache",
                     "current_state_entity.hpp", BITEMPORAL_MODEL)
    assert "recorded_at" in header
    assert "int version" in header


def test_current_state_facet_gate_drops_history_only_archetypes():
    # The history field mapper projects the domain type's recorded_at member,
    # which a current-state entity does not have; generate.py drops both of
    # its archetypes by address.
    sys.path.insert(0, str(REPO_ROOT / "projects/ores.codegen/src"))
    from codegen.generate import _NO_TEMPORAL_HISTORY_ARCHETYPES  # noqa: PLC0415

    assert "ores.cpp.presentation.history_field_mapper_impl" in \
        _NO_TEMPORAL_HISTORY_ARCHETYPES
