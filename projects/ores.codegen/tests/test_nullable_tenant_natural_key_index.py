"""Tests for natural-key index scoping on a nullable-tenant entity.

Run::

    python3 -m pytest projects/ores.codegen/tests/test_nullable_tenant_natural_key_index.py

An entity whose tenant column admits SQL NULL holds rows for several tenants in
one table, so its natural key is unique within a tenant and not across them. The
single-natural-key index used to be scoped by ``has_tenant_in_pk`` alone, which
is false for every nullable-tenant entity, so the index came out on the key
column by itself and made the key globally unique. The composite-natural-key
branch already scoped by ``has_tenant_id``; this test pins the branches
together.

Scoping alone is not enough. PostgreSQL treats NULL as distinct from every
other NULL in a unique index, so a tenant-scoped index on a nullable column
still lets two system rows share a natural key. The nullable-tenant branch
therefore states ``nulls not distinct``: SQL NULL is the system scope, and it
is one tenant. The other branches must not state it, because their tenant
column cannot be NULL.
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
:ID: 00000000-0000-0000-0000-0000000000F1
:END:
#+title: ores.testcomp.natural_key_entity
#+type: ores.codegen.entity
#+component: testcomp
#+entity_singular: natural_key_entity
#+entity_plural: natural_key_entities
#+entity_title: Natural Key Entity
#+has_tenant_id: true
#+coding_scheme: none
#+image_id: false

An entity with one natural key and a tenant column whose scoping varies.

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
:natural_key: true
:END:

The natural key.

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
:tablename: ores_testcomp_natural_key_entities_tbl
{sql_flags}:END:
"""


def _generate_sql(tmp_path, sql_flags=""):
    body = MODEL.format(sql_flags=sql_flags)
    model_path = tmp_path / "ores.testcomp.natural_key_entity.org"
    model_path.write_text(body, encoding="utf-8")
    output_dir = tmp_path / "out"
    output_dir.mkdir()
    generate_from_model(
        str(model_path),
        DATA_DIR,
        TEMPLATES_DIR,
        output_dir,
        is_processing_batch=True,
        target_template="sql_schema_domain_entity_create.mustache",
        target_output="natural_key_entity_create.sql",
    )
    return (output_dir / "natural_key_entity_create.sql").read_text(encoding="utf-8")


def test_nullable_tenant_scopes_the_natural_key_index(tmp_path):
    sql = _generate_sql(tmp_path, sql_flags=":nullable_tenant_id: true\n")
    assert (
        'on "ores_testcomp_natural_key_entities_tbl" (tenant_id, name) nulls not distinct\n'
        "where valid_to = ores_utility_infinity_timestamp_fn();"
    ) in sql


def test_nullable_tenant_index_treats_every_null_as_the_same_tenant(tmp_path):
    sql = _generate_sql(tmp_path, sql_flags=":nullable_tenant_id: true\n")
    assert "nulls not distinct" in sql


def test_a_tenant_that_cannot_be_null_states_no_null_clause(tmp_path):
    sql = _generate_sql(tmp_path)
    assert "(tenant_id, name) nulls not distinct" not in sql
    assert "nulls not distinct" not in sql


def test_tenant_in_the_primary_key_scopes_the_natural_key_index(tmp_path):
    sql = _generate_sql(tmp_path)
    assert (
        'on "ores_testcomp_natural_key_entities_tbl" (tenant_id, name)\n'
        "where valid_to = ores_utility_infinity_timestamp_fn();"
    ) in sql


def test_system_scope_keeps_the_unscoped_index(tmp_path):
    sql = _generate_sql(tmp_path, sql_flags=":system_scope: true\n")
    assert (
        'on "ores_testcomp_natural_key_entities_tbl" (name)\n'
        "where valid_to = ores_utility_infinity_timestamp_fn();"
    ) in sql


def test_nullable_tenant_index_name_is_unchanged(tmp_path):
    sql = _generate_sql(tmp_path, sql_flags=":nullable_tenant_id: true\n")
    assert "natural_key_entities_name_uniq_idx" in sql
