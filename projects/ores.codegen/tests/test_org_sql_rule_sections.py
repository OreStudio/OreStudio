"""Tests for the org-authored SQL rule sections in
sql_schema_domain_entity_create.mustache: the ``** Checks`` table, the
``** Delete sets`` table, the column-level ``:unique:`` flag, and the
``* Insert trigger ** Validations`` table.

These sections exist so an entity org can declare rules the base
template cannot infer (CHECK constraints, extra soft-delete SET
clauses, non-natural-key uniqueness, code-column FK validation)
instead of dropping them at regeneration time.

Run::

    python3 -m pytest projects/ores.codegen/tests/test_org_sql_rule_sections.py
"""
import sys
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[3]
sys.path.insert(0, str(REPO_ROOT / "projects/ores.codegen/src"))

from codegen.core import generate_from_model  # noqa: E402

CODEGEN_BASE = REPO_ROOT / "projects/ores.codegen"
DATA_DIR = CODEGEN_BASE / "library" / "data"
TEMPLATES_DIR = CODEGEN_BASE / "library" / "templates"

FIXTURE = """\
:PROPERTIES:
:ID: 00000000-0000-0000-0000-000000000071
:END:
#+title: ores.testcomp.tenant
#+type: ores.codegen.entity
#+component: testcomp
#+entity_singular: tenant
#+entity_plural: tenants
#+entity_title: Tenant
#+has_tenant_id: true

Test tenant entity mirroring the ores.iam tenant model.

* Flags
:PROPERTIES:
:schema:    public
:product:   ores
:component: testcomp
:profile:   uuid-identified-lookup
:subcomponent: api
:END:

* Columns

** id
:PROPERTIES:
:type:            uuid
:cpp_type:        boost::uuids::uuid
:primary_key:     true
:skip_uuid_check: true
:END:

UUID key.

** code
:PROPERTIES:
:type:        text
:cpp_type:    std::string
:natural_key: true
:END:

Unique code.

** hostname
:PROPERTIES:
:type:     text
:cpp_type: std::string
:nullable: false
:unique:   true
:END:

Unique hostname.

** type
:PROPERTIES:
:type:     text
:cpp_type: std::string
:nullable: false
:END:

Tenant type.

** status
:PROPERTIES:
:type:     text
:cpp_type: std::string
:nullable: false
:END:

Tenant status.

* SQL

** Flags
:PROPERTIES:
:tablename:    ores_testcomp_tenants_tbl
:system_scope: true
:END:

** Checks

| expression                                                                   |
|------------------------------------------------------------------------------|
| "id" <> 'ffffffff-ffff-ffff-ffff-ffffffffffff'::uuid or "code" = 'system'    |
| "tenant_id" = ores_utility_system_tenant_id_fn()                             |
| "hostname" <> ''                                                             |

** Delete sets

| expression            |
|-----------------------+|
| status = 'terminated' |

* Insert trigger

** Validations

| column | validation_function              |
|--------+----------------------------------|
| type   | ores_iam_validate_tenant_type_fn   |
| status | ores_iam_validate_tenant_status_fn |
"""


def _generate_sql(tmp_path, body=FIXTURE):
    model_path = tmp_path / "ores.testcomp.tenant.org"
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
        target_output="tenant_create.sql",
    )
    return (output_dir / "tenant_create.sql").read_text(encoding="utf-8")


def test_checks_table_emits_extra_checks(tmp_path):
    sql = _generate_sql(tmp_path)
    assert (
        'check ("id" <> '
        "'ffffffff-ffff-ffff-ffff-ffffffffffff'::uuid or \"code\" = 'system')"
    ) in sql
    assert 'check ("tenant_id" = ores_utility_system_tenant_id_fn())' in sql
    assert 'check ("hostname" <> \'\')' in sql


def test_delete_sets_table_extends_delete_rule(tmp_path):
    sql = _generate_sql(tmp_path)
    assert (
        "set valid_to = clock_timestamp(),\n"
        "        status = 'terminated'"
    ) in sql


def test_unique_flag_emits_natural_key_style_index(tmp_path):
    sql = _generate_sql(tmp_path)
    assert "-- Unique hostname for active records" in sql
    assert (
        "create unique index if not exists tenants_hostname_uniq_idx\n"
        'on "ores_testcomp_tenants_tbl" (hostname)\n'
        "where valid_to = ores_utility_infinity_timestamp_fn();"
    ) in sql


def test_validations_table_wires_generated_validate_fns(tmp_path):
    sql = _generate_sql(tmp_path)
    assert (
        "NEW.type := ores_iam_validate_tenant_type_fn(NEW.tenant_id, NEW.type);"
    ) in sql
    assert (
        "NEW.status := ores_iam_validate_tenant_status_fn(NEW.tenant_id, NEW.status);"
    ) in sql
