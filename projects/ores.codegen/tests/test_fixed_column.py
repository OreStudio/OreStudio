"""Tests for fixed columns.

Run::

    python3 -m pytest projects/ores.codegen/tests/test_fixed_column.py

A fixed column keeps the value of its row's first version. A netting
agreement's parties are fixed, so a netting set that copies them and pins
the copy to the agreement stays true when the agreement is amended.
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
:ID: 00000000-0000-0000-0000-0000000000A7
:END:
#+title: ores.testcomp.agreement_record
#+type: ores.codegen.entity
#+component: testcomp
#+entity_singular: agreement_record
#+entity_plural: agreement_records
#+entity_title: Agreement Record
#+coding_scheme: none
#+image_id: false

A row whose owner never changes.

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

** owner_id
:PROPERTIES:
:type:     uuid
:cpp_type: boost::uuids::uuid
{fixed}:END:

The owner, fixed for the row's life.

* SQL
** Flags
:PROPERTIES:
:tablename: ores_testcomp_agreement_records_tbl
{sql_flags}:END:
"""


def _create_sql(tmp_path, fixed=True, sql_flags=""):
    model = tmp_path / "ores.testcomp.agreement_record.org"
    model.write_text(
        MODEL.format(fixed=":fixed:    true\n" if fixed else "",
                     sql_flags=sql_flags),
        encoding="utf-8")
    output_dir = tmp_path / "out"
    output_dir.mkdir()
    generate_from_model(
        str(model), DATA_DIR, TEMPLATES_DIR, output_dir,
        is_processing_batch=True,
        target_template="sql_schema_domain_entity_create.mustache",
        target_output="create.sql")
    return (output_dir / "create.sql").read_text(encoding="utf-8")


def test_a_new_version_that_changes_a_fixed_column_is_refused(tmp_path):
    sql = _create_sql(tmp_path)

    assert '"owner_id" is distinct from NEW."owner_id"' in sql
    assert ("owner_id cannot change: it is fixed for the life of the "
            "agreement_record.") in sql


def test_a_column_that_is_not_fixed_may_change(tmp_path):
    sql = _create_sql(tmp_path, fixed=False)

    assert "is distinct from NEW." not in sql


def test_a_fixed_column_needs_a_temporal_table(tmp_path):
    with pytest.raises(ValueError, match=":fixed: needs a temporal table"):
        _create_sql(tmp_path, sql_flags=":current_state: true\n")
