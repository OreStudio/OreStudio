"""Tests for compound-primary-key rendering in sql_schema_domain_entity_create.mustache.

Run::

    python3 -m pytest projects/ores.codegen/tests/test_compound_primary_key_sql.py
"""
import sys
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[3]
sys.path.insert(0, str(REPO_ROOT / "projects/ores.codegen/src"))

from codegen.core import generate_from_model  # noqa: E402

CODEGEN_BASE = REPO_ROOT / "projects/ores.codegen"
DATA_DIR = CODEGEN_BASE / "library" / "data"
TEMPLATES_DIR = CODEGEN_BASE / "library" / "templates"

REQUIRED_FLAGS = """\
:PROPERTIES:
:ID: 00000000-0000-0000-0000-000000000001
:END:
#+title: ores.testcomp.compound_key_entity
#+type: ores.codegen.entity
#+component: testcomp
#+entity_singular: compound_key_entity
#+entity_plural: compound_key_entities
#+entity_title: Compound Key Entity
#+has_tenant_id: true
#+coding_scheme: none
#+image_id: false

Test entity with a compound primary key.

* Flags
:PROPERTIES:
:schema:    public
:product:   ores
:component: testcomp
:subcomponent: api
:END:

* Columns

** name
:PROPERTIES:
:type:        text
:cpp_type:    std::string
:primary_key: true
:END:

First key column.

** domain_name
:PROPERTIES:
:type:        text
:cpp_type:    std::string
:nullable:    false
:primary_key: true
:END:

Second key column.

** description
:PROPERTIES:
:type:     text
:cpp_type: std::string
:nullable: false
:END:

A plain column.

* SQL

** Flags
:PROPERTIES:
:tablename: ores_testcomp_compound_key_entities_tbl
:END:
"""


def _generate_sql(tmp_path, body=REQUIRED_FLAGS):
    model_path = tmp_path / "ores.testcomp.compound_key_entity.org"
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
        target_output="compound_key_entity_create.sql",
    )
    return (output_dir / "compound_key_entity_create.sql").read_text(encoding="utf-8")


def test_compound_primary_key_renders_composite_constraint(tmp_path):
    sql = _generate_sql(tmp_path)
    assert 'primary key (tenant_id, name, domain_name, valid_from, valid_to)' in sql
    assert (
        "exclude using gist (\n"
        "        tenant_id WITH =,\n"
        "        name WITH =,\n"
        "        domain_name WITH =,\n"
        "        tstzrange(valid_from, valid_to) WITH &&\n"
        "    )"
    ) in sql
    assert 'check ("name" <> \'\')' in sql
    assert 'check ("domain_name" <> \'\')' in sql


def test_compound_primary_key_renders_composite_indexes(tmp_path):
    sql = _generate_sql(tmp_path)
    assert (
        'on "ores_testcomp_compound_key_entities_tbl" '
        '(tenant_id, name, domain_name, version)'
    ) in sql
    assert (
        'compound_key_entities_name_domain_name_uniq_idx' in sql
    )


def test_compound_primary_key_renders_and_joined_where_clauses(tmp_path):
    sql = _generate_sql(tmp_path)
    assert 'name = NEW.name and domain_name = NEW.domain_name' in sql
    assert 'name = OLD.name and domain_name = OLD.domain_name' in sql


def test_compound_primary_key_creates_one_column_per_field(tmp_path):
    sql = _generate_sql(tmp_path)
    assert '"name" text not null,' in sql
    assert '"domain_name" text not null,' in sql


def test_compound_primary_key_suppresses_validate_fn():
    # validate_<entity>_fn is scoped to single-column keys (task 1's Plan
    # decision) -- a compound key has no single scalar to validate against.
    body = REQUIRED_FLAGS + (
        "\n* Validation function\n"
        ":PROPERTIES:\n"
        ":tenant_scope: tenant\n"
        ":order_by:     name\n"
        ":END:\n"
    )
    import tempfile
    with tempfile.TemporaryDirectory() as td:
        sql = _generate_sql(Path(td), body=body)
    assert "validate_compound_key_entity_fn" not in sql


def test_single_column_key_unaffected_by_compound_key_support(tmp_path):
    body = """\
:PROPERTIES:
:ID: 00000000-0000-0000-0000-000000000002
:END:
#+title: ores.testcomp.single_key_entity
#+type: ores.codegen.entity
#+component: testcomp
#+entity_singular: single_key_entity
#+entity_plural: single_key_entities
#+entity_title: Single Key Entity
#+has_tenant_id: true
#+coding_scheme: none
#+image_id: false

Test entity with a single-column primary key.

* Flags
:PROPERTIES:
:schema:    public
:product:   ores
:component: testcomp
:subcomponent: api
:END:

* Columns

** code
:PROPERTIES:
:type:        text
:cpp_type:    std::string
:primary_key: true
:END:

The key column.

* SQL

** Flags
:PROPERTIES:
:tablename: ores_testcomp_single_key_entities_tbl
:END:
"""
    model_path = tmp_path / "ores.testcomp.single_key_entity.org"
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
        target_output="single_key_entity_create.sql",
    )
    sql = (output_dir / "single_key_entity_create.sql").read_text(encoding="utf-8")
    assert 'primary key (tenant_id, code, valid_from, valid_to)' in sql
    assert 'exclude using gist (\n        tenant_id WITH =,\n        code WITH =,\n' in sql
    # Single column: where_new/where_old are just "code = NEW.code" /
    # "code = OLD.code" -- no " and "-joining of a second key column.
    assert "code = NEW.code and" not in sql
    assert "code = NEW.code" in sql
