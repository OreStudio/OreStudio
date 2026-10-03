"""Tests for foreign keys the database enforces.

Run::

    python3 -m pytest projects/ores.codegen/tests/test_enforced_foreign_key.py

A foreign key is normally a trigger check against the parent's current
row. A key into an immutable entity can instead be a REFERENCES constraint,
because the parent row never changes and has no closed versions for the
constraint to mistake for current ones. The trade's components reference
the trade anchor this way.
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

PARENT = """\
:PROPERTIES:
:ID: 00000000-0000-0000-0000-0000000000E1
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
:read_only:     true
:END:

* Columns

** id
:PROPERTIES:
:type:         uuid
:cpp_type:     boost::uuids::uuid
:primary_key:  true
:END:

The row identity.

* SQL
** Flags
:PROPERTIES:
:tablename:     ores_testcomp_anchor_records_tbl
:current_state: true
{immutable}:END:
"""

CHILD = """\
:PROPERTIES:
:ID: 00000000-0000-0000-0000-0000000000E2
:END:
#+title: ores.testcomp.child_record
#+type: ores.codegen.entity
#+component: testcomp
#+entity_singular: child_record
#+entity_plural: child_records
#+entity_title: Child Record
#+coding_scheme: none
#+image_id: false

A row that references an anchor.

* Flags
:PROPERTIES:
:schema:        public
:product:       ores
:component:     testcomp
:subcomponent:  api
:has_tenant_id: false
:END:

* Columns

** id
:PROPERTIES:
:type:         uuid
:cpp_type:     boost::uuids::uuid
:primary_key:  true
:END:

The row identity.

** anchor_id
:PROPERTIES:
:type:     uuid
:cpp_type: boost::uuids::uuid
:nullable: false
:END:

The anchor this row belongs to.

* Foreign keys

** anchor_id
:PROPERTIES:
:table:         ores_testcomp_anchor_records_tbl
:error_message: Invalid anchor_id: %. Anchor must exist.
:use_no_tenant: true
{enforce}:END:

* SQL
** Flags
:PROPERTIES:
:tablename: ores_testcomp_child_records_tbl
:END:
"""


def _child_sql(tmp_path, parent_immutable=True, enforce=True):
    modeling = tmp_path / "projects" / "ores.testcomp" / "modeling"
    modeling.mkdir(parents=True)
    (modeling / "ores.testcomp.anchor_record.org").write_text(
        PARENT.format(immutable=":immutable:     true\n" if parent_immutable else ""),
        encoding="utf-8")
    child = modeling / "ores.testcomp.child_record.org"
    child.write_text(
        CHILD.format(enforce=":enforce:       true\n" if enforce else ""),
        encoding="utf-8")
    output_dir = tmp_path / "out"
    output_dir.mkdir()
    generate_from_model(
        str(child), DATA_DIR, TEMPLATES_DIR, output_dir,
        is_processing_batch=True,
        target_template="sql_schema_domain_entity_create.mustache",
        target_output="child_records_create.sql")
    return (output_dir / "child_records_create.sql").read_text(encoding="utf-8")


def test_an_enforced_key_is_a_references_constraint(tmp_path):
    sql = _child_sql(tmp_path)

    assert ('constraint ores_testcomp_child_records_anchor_id_fk foreign key '
            '("anchor_id") references "ores_testcomp_anchor_records_tbl" ("id")') in sql


def test_an_enforced_key_has_no_trigger_check(tmp_path):
    sql = _child_sql(tmp_path)

    assert "Validate anchor_id" not in sql


def test_an_unenforced_key_stays_a_trigger_check(tmp_path):
    sql = _child_sql(tmp_path, enforce=False)

    assert "Validate anchor_id" in sql
    assert "_fk foreign key" not in sql


def test_an_enforced_key_into_a_mutable_table_is_refused(tmp_path):
    with pytest.raises(ValueError, match="is not an immutable entity"):
        _child_sql(tmp_path, parent_immutable=False)
