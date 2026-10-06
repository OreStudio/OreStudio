"""Tests for foreign keys the database enforces.

Run::

    python3 -m pytest projects/ores.codegen/tests/test_enforced_foreign_key.py

A foreign key is normally a trigger check against the parent's current
row. A key into an immutable entity can instead be a REFERENCES constraint,
because the parent row never changes and has no closed versions for the
constraint to mistake for current ones. The trade's components reference
the trade anchor this way. A key that matches its own tenant references
the tenant as well, so a row cannot name another tenant's anchor.
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
:has_tenant_id: {tenant}
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
:has_tenant_id: {tenant}
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
{fk_flags}:END:

* SQL
** Flags
:PROPERTIES:
:tablename: ores_testcomp_child_records_tbl
{child_sql_flags}:END:
"""


ENFORCE = ":enforce:       true\n"
NO_TENANT = ":use_no_tenant: true\n"
SYSTEM_TENANT = ":use_system_tenant: true\n"


def _render(tmp_path, parent_flags=":immutable:     true\n",
            fk_flags=ENFORCE, parent_tenant=True, child_tenant=True,
            child_sql_flags="", edit_child=lambda body: body):
    modeling = tmp_path / "projects" / "ores.testcomp" / "modeling"
    modeling.mkdir(parents=True)
    parent = modeling / "ores.testcomp.anchor_record.org"
    parent.write_text(
        PARENT.format(immutable=parent_flags,
                      tenant=str(parent_tenant).lower()),
        encoding="utf-8")
    child = modeling / "ores.testcomp.child_record.org"
    child.write_text(
        edit_child(CHILD.format(fk_flags=fk_flags,
                                tenant=str(child_tenant).lower(),
                                child_sql_flags=child_sql_flags)),
        encoding="utf-8")
    sql = {}
    for name, model in (("parent", parent), ("child", child)):
        output_dir = tmp_path / name
        output_dir.mkdir()
        generate_from_model(
            str(model), DATA_DIR, TEMPLATES_DIR, output_dir,
            is_processing_batch=True,
            target_template="sql_schema_domain_entity_create.mustache",
            target_output="create.sql")
        sql[name] = (output_dir / "create.sql").read_text(encoding="utf-8")
    return sql


def test_an_enforced_key_matches_the_tenant(tmp_path):
    sql = _render(tmp_path)

    assert ('constraint ores_testcomp_child_records_anchor_id_fk foreign key '
            '("tenant_id", "anchor_id") references '
            '"ores_testcomp_anchor_records_tbl" ("tenant_id", "id")') in sql["child"]
    assert "primary key (tenant_id, id)" in sql["parent"]


def test_an_enforced_key_without_tenants_names_one_column(tmp_path):
    sql = _render(tmp_path, fk_flags=ENFORCE + NO_TENANT,
                  parent_tenant=False, child_tenant=False)

    assert ('constraint ores_testcomp_child_records_anchor_id_fk foreign key '
            '("anchor_id") references "ores_testcomp_anchor_records_tbl" ("id")'
            ) in sql["child"]
    assert "primary key (id)" in sql["parent"]


def test_a_current_state_table_carries_the_constraint(tmp_path):
    sql = _render(tmp_path, child_sql_flags=":current_state: true\n")

    assert "valid_from" not in sql["child"]
    assert ('constraint ores_testcomp_child_records_anchor_id_fk foreign key '
            '("tenant_id", "anchor_id")') in sql["child"]


def test_an_enforced_key_has_no_trigger_check(tmp_path):
    sql = _render(tmp_path)

    assert "Validate anchor_id" not in sql["child"]


def test_an_unenforced_key_stays_a_trigger_check(tmp_path):
    sql = _render(tmp_path, fk_flags="")

    assert "Validate anchor_id" in sql["child"]
    assert "_fk foreign key" not in sql["child"]


def test_an_enforced_key_into_a_mutable_table_is_refused(tmp_path):
    with pytest.raises(ValueError, match="is not an immutable entity"):
        _render(tmp_path, parent_flags="")


def test_an_enforced_key_into_the_system_tenant_is_refused(tmp_path):
    with pytest.raises(ValueError, match="can only match the row's own tenant"):
        _render(tmp_path, fk_flags=ENFORCE + SYSTEM_TENANT)


@pytest.mark.parametrize("fk_flags,parent_tenant,child_tenant", [
    (ENFORCE + NO_TENANT, True, True),
    (ENFORCE, False, True),
    (ENFORCE, True, False),
])
def test_an_enforced_key_whose_tenant_scope_differs_is_refused(
        tmp_path, fk_flags, parent_tenant, child_tenant):
    with pytest.raises(ValueError, match="tenant scope does not match"):
        _render(tmp_path, fk_flags=fk_flags, parent_tenant=parent_tenant,
                child_tenant=child_tenant)


def test_an_enforced_key_into_an_unknown_table_is_refused(tmp_path):
    def edit(body):
        return body.replace(":table:         ores_testcomp_anchor_records_tbl",
                            ":table:         ores_testcomp_missing_tbl")

    with pytest.raises(ValueError, match="no model declares the table"):
        _render(tmp_path, edit_child=edit)


def test_a_constraint_name_postgres_would_truncate_is_refused(tmp_path):
    def edit(body):
        return body.replace("anchor_id", "anchor_id_" + "x" * 30)

    with pytest.raises(ValueError, match="longer than 63 characters"):
        _render(tmp_path, edit_child=edit)


def test_a_named_constraint_replaces_the_composed_name(tmp_path):
    sql = _render(tmp_path, fk_flags=ENFORCE + ":constraint_name: child_anchor_fk\n")

    assert ('constraint child_anchor_fk foreign key ("tenant_id", "anchor_id")'
            ) in sql["child"]
    assert "child_records_anchor_id_fk" not in sql["child"]


def test_a_named_constraint_postgres_would_truncate_is_refused(tmp_path):
    with pytest.raises(ValueError, match="longer than 63 characters"):
        _render(tmp_path, fk_flags=ENFORCE + ":constraint_name: " + "x" * 64 + "\n")
