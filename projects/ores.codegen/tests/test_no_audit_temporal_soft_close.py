"""Tests that the insert trigger's soft-close UPDATE is tenant-scoped.

Run::

    python3 -m pytest projects/ores.codegen/tests/test_no_audit_temporal_soft_close.py

The bi-temporal table's insert trigger closes the currently active row
for the incoming key before it inserts the replacement. That close must
be scoped by ``tenant_id`` as well as by the entity's own primary key:
without the tenant clause an insert for one tenant closes the active row
of the same id in *another* tenant.

Two branches of ``sql_schema_domain_entity_create.mustache`` emit that
close:

* the audit branch (``has_audit_columns``), which carries version
  management and is exercised by every standard bi-temporal entity;
* the no-audit temporal branch (``no_audit_columns`` without
  ``current_state``), which has no version column and, before the
  ``iam_permission`` migration, no entity anywhere in the tree to reach
  it.

Both are pinned here by name: ``no_audit_temporal_entity`` for the
branch that reaches the template's ``{{^has_audit_columns}}{{^current_state}}``
arm, and ``audit_temporal_entity`` for the ``{{#has_audit_columns}}`` arm.
"""
import re
import sys
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[3]
sys.path.insert(0, str(REPO_ROOT / "projects/ores.codegen/src"))

from codegen.core import generate_from_model  # noqa: E402

CODEGEN_BASE = REPO_ROOT / "projects/ores.codegen"
DATA_DIR = CODEGEN_BASE / "library" / "data"
TEMPLATES_DIR = CODEGEN_BASE / "library" / "templates"

# A temporal entity with no version column and no audit tail --
# :no_audit_columns: without :current_state:. Modelled on the hand-written
# iam_permissions table, the first entity in the tree to reach the
# template's no-audit temporal branch.
NO_AUDIT_TEMPORAL_MODEL = """\
:PROPERTIES:
:ID: 00000000-0000-0000-0000-0000000000D1
:END:
#+title: ores.testcomp.no_audit_temporal_entity
#+type: ores.codegen.entity
#+component: testcomp
#+entity_singular: no_audit_temporal_entity
#+entity_plural: no_audit_temporal_entities
#+entity_title: No Audit Temporal Entity
#+has_tenant_id: true
#+coding_scheme: none
#+image_id: false

A temporal entity whose table carries no version column and no audit tail.

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
:type:            uuid
:cpp_type:        boost::uuids::uuid
:primary_key:     true
:skip_uuid_check: true
:END:

The key column.

** code
:PROPERTIES:
:type:        text
:cpp_type:    std::string
:natural_key: true
:END:

The natural key column.

* SQL

** Flags
:PROPERTIES:
:tablename:        ores_testcomp_no_audit_temporal_entities_tbl
:no_audit_columns: true
:END:

* C++

** Repository
:PROPERTIES:
:entity_singular_short: no_audit_temporal_entity
:entity_plural_short:   no_audit_temporal_entities
:entity_singular_words: no audit temporal entity
:entity_plural_words:   no audit temporal entities
:END:
"""

# The same entity's default bi-temporal control: dropping
# :no_audit_columns: selects the audit branch, which carries the version
# column and the audit tail.
AUDIT_TEMPORAL_MODEL = NO_AUDIT_TEMPORAL_MODEL.replace(
    ":no_audit_columns: true\n", "")


def _render(tmp_path, template, output_name, body):
    model_path = tmp_path / "ores.testcomp.entity.org"
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


def _sql(tmp_path, body):
    return _render(tmp_path, "sql_schema_domain_entity_create.mustache",
                   "entity_create.sql", body)


def _close_current_update(sql):
    """The no-audit branch's close-current-row UPDATE statement."""
    match = re.search(
        r"-- Close the current active row before inserting the new one\n"
        r"\s*update .*?;",
        sql,
        re.DOTALL,
    )
    assert match, "the soft-close UPDATE statement was not emitted"
    return match.group(0)


def _version_management_update(sql):
    """The audit branch's version-management close-current UPDATE."""
    match = re.search(
        r"-- clock_timestamp\(\), not current_timestamp.*?\n(\s*update .*?;)",
        sql,
        re.DOTALL,
    )
    assert match, "the version-management UPDATE statement was not emitted"
    return match.group(1)


def test_no_audit_temporal_soft_close_is_tenant_scoped(tmp_path):
    # iam_permission's shape: a no-audit, temporal, non-hypertable table.
    # The close-current-row UPDATE must scope by tenant_id as well as by the
    # model's own primary key.
    sql = _sql(tmp_path, NO_AUDIT_TEMPORAL_MODEL)
    close = _close_current_update(sql)
    assert "tenant_id = NEW.tenant_id" in close
    assert "id = NEW.id" in close
    # ... and it is the no-audit branch: no version management.
    assert "current_version" not in sql
    assert "modified_by" not in sql


def test_no_audit_temporal_close_uses_the_models_own_primary_key(tmp_path):
    sql = _sql(tmp_path, NO_AUDIT_TEMPORAL_MODEL)
    close = _close_current_update(sql)
    assert 'where tenant_id = NEW.tenant_id\n      and id = NEW.id' in close


def test_audit_branch_soft_close_still_carries_the_tenant_clause(tmp_path):
    # The control: the standard bi-temporal entity (version + audit tail)
    # keeps the tenant scope its branch already had.
    sql = _sql(tmp_path, AUDIT_TEMPORAL_MODEL)
    update = _version_management_update(sql)
    assert "tenant_id = NEW.tenant_id" in update
    assert "id = NEW.id" in update
    # ... and it is the audit branch.
    assert "current_version integer;" in sql
    assert "modified_by" in sql
