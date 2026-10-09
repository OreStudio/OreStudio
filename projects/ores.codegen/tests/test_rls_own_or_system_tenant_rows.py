"""Tests for the rls_own_or_system_tenant_rows sql feature.

The feature emits the shared-reference and installation-scoped policy shape:
every session reads its own tenant's rows and the system tenant's, and writes
stay the current tenant's. It must never emit the widening that
``check_rls_system_tenant_widenings.py`` flags -- ``current_tenant_id_fn() =
system_tenant_id_fn()`` -- and it must not be combinable with the flag that
does emit it.

Run::

    python3 -m pytest projects/ores.codegen/tests/test_rls_own_or_system_tenant_rows.py
"""
import importlib.util
import re
import sys
from pathlib import Path

import pytest

REPO_ROOT = Path(__file__).resolve().parents[3]
sys.path.insert(0, str(REPO_ROOT / "projects/ores.codegen/src"))

from codegen.core import generate_from_model, validate_rls_isolation  # noqa: E402

CODEGEN = REPO_ROOT / "projects/ores.codegen"
DATA_DIR = CODEGEN / "library" / "data"
TEMPLATES_DIR = CODEGEN / "library" / "templates"
GUARD = CODEGEN / "scripts/check_rls_system_tenant_widenings.py"

MODEL = """\
:PROPERTIES:
:ID: 00000000-0000-0000-0000-0000000000F1
:END:
#+title: ores.testcomp.installation_sample
#+type: ores.codegen.entity
#+component: testcomp
#+entity_singular: installation_sample
#+entity_plural: installation_samples
#+entity_title: Installation Sample
#+has_tenant_id: true
#+coding_scheme: none
#+image_id: false

A row the installation writes about its own operation.

* Flags
:PROPERTIES:
:schema:        public
:product:       ores
:component:     testcomp
:subcomponent:  api
:END:

* Columns

** id
:PROPERTIES:
:type:            uuid
:cpp_type:        boost::uuids::uuid
:primary_key:     true
:skip_uuid_check: true
:END:

The row identity.

** name
:PROPERTIES:
:type:     text
:cpp_type: std::string
:nullable: false
:END:

A plain column.

* SQL

** Flags
:PROPERTIES:
:tablename: ores_testcomp_installation_samples_tbl
:rls_tenant_isolation: {tenant_isolation}
{rls_flags}:END:
"""

POLICY_BODY = """\
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
    or tenant_id = ores_utility_system_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);"""

JUNCTION = """\
:PROPERTIES:
:ID: 00000000-0000-0000-0000-0000000000F2
:END:
#+title: ores.testcomp.installation_pair
#+type: ores.codegen.junction
#+component: testcomp
#+name: installation_pairs
#+name_singular: installation_pair
#+name_title: Installation Pair
#+name_singular_words: installation pair
#+brief: A junction the installation reads whole.
#+product: ores
#+schema: public
#+has_tenant_id: true

A junction that links two installation-scoped rows.

* Left
:PROPERTIES:
:column:        left_code
:column_short:  left
:type:          text
:cpp_type:      std::string
:END:

The left side.

* Right
:PROPERTIES:
:column:        right_code
:column_short:  right
:type:          text
:cpp_type:      std::string
:END:

The right side.

* SQL

** Flags
:PROPERTIES:
{flags}:END:
"""


def _generate_sql(tmp_path, tenant_isolation="true", rls_flags=""):
    model_path = tmp_path / "ores.testcomp.installation_sample.org"
    model_path.write_text(
        MODEL.format(tenant_isolation=tenant_isolation, rls_flags=rls_flags),
        encoding="utf-8")
    output_dir = tmp_path / "out"
    output_dir.mkdir()
    generate_from_model(
        str(model_path),
        DATA_DIR,
        TEMPLATES_DIR,
        output_dir,
        is_processing_batch=True,
        target_template="sql_schema_domain_entity_create.mustache",
        target_output="installation_sample_create.sql",
    )
    return (output_dir / "installation_sample_create.sql").read_text(encoding="utf-8")


def test_the_flag_emits_own_or_system_tenant_rows_and_a_tenant_scoped_write(tmp_path):
    sql = _generate_sql(
        tmp_path, rls_flags=":rls_own_or_system_tenant_rows: true\n")
    assert POLICY_BODY in sql


def test_the_flag_emits_no_widening(tmp_path):
    sql = _generate_sql(
        tmp_path, rls_flags=":rls_own_or_system_tenant_rows: true\n")
    spec = importlib.util.spec_from_file_location("check_widenings", GUARD)
    check = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(check)
    assert check.WIDENING_RE.search(sql) is None


def test_the_flag_without_tenant_isolation_is_refused(tmp_path):
    domain_entity = {
        'entity_singular': 'installation_sample',
        'sql': {'rls_own_or_system_tenant_rows': True},
    }
    with pytest.raises(
            ValueError,
            match='installation_sample: rls_own_or_system_tenant_rows requires '
                  'rls_tenant_isolation'):
        validate_rls_isolation(domain_entity)


def test_the_flag_and_the_widening_are_mutually_exclusive():
    domain_entity = {
        'entity_singular': 'installation_sample',
        'has_tenant_id': True,
        'sql': {
            'rls_tenant_isolation': True,
            'rls_own_or_system_tenant_rows': True,
            'rls_system_tenant_visible': True,
        },
    }
    with pytest.raises(
            ValueError,
            match='installation_sample: rls_own_or_system_tenant_rows and '
                  'rls_system_tenant_visible are mutually exclusive'):
        validate_rls_isolation(domain_entity)


def test_the_flag_with_tenant_isolation_is_valid():
    validate_rls_isolation({
        'has_tenant_id': True,
        'sql': {
            'rls_tenant_isolation': True,
            'rls_own_or_system_tenant_rows': True,
        },
    })


def test_plain_tenant_isolation_still_emits_no_system_rows(tmp_path):
    sql = _generate_sql(tmp_path)
    assert 'ores_utility_system_tenant_id_fn()' not in sql
    assert re.search(
        r'for all using \(\s*tenant_id = ores_iam_current_tenant_id_fn\(\)\s*\)',
        sql)


def _generate_junction_sql(tmp_path, flags):
    model_path = tmp_path / "ores.testcomp.installation_pair.org"
    model_path.write_text(JUNCTION.format(flags=flags), encoding="utf-8")
    output_dir = tmp_path / "out"
    output_dir.mkdir()
    generate_from_model(
        str(model_path),
        DATA_DIR,
        TEMPLATES_DIR,
        output_dir,
        is_processing_batch=True,
        target_template="sql_schema_junction_create.mustache",
        target_output="installation_pair_create.sql",
    )
    return (output_dir / "installation_pair_create.sql").read_text(encoding="utf-8")


def test_a_junction_emits_own_or_system_tenant_rows_and_a_tenant_scoped_write(tmp_path):
    sql = _generate_junction_sql(
        tmp_path,
        ":rls_tenant_isolation: true\n"
        ":rls_own_or_system_tenant_rows: true\n")
    assert POLICY_BODY in sql


def test_a_junction_without_tenant_isolation_is_refused(tmp_path):
    with pytest.raises(
            ValueError,
            match='installation_pair: rls_own_or_system_tenant_rows requires '
                  'rls_tenant_isolation'):
        _generate_junction_sql(
            tmp_path, ":rls_own_or_system_tenant_rows: true\n")


def test_a_junction_cannot_combine_own_or_system_with_the_widening(tmp_path):
    with pytest.raises(
            ValueError,
            match='installation_pair: rls_own_or_system_tenant_rows and '
                  'rls_system_tenant_visible are mutually exclusive'):
        _generate_junction_sql(
            tmp_path,
            ":rls_tenant_isolation: true\n"
            ":rls_own_or_system_tenant_rows: true\n"
            ":rls_system_tenant_visible: true\n")
