"""Tests for the :sql_key: column property in sql_schema_domain_entity_create.mustache.

Run::

    python3 -m pytest projects/ores.codegen/tests/test_sql_key_column.py
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
:ID: 00000000-0000-0000-0000-000000000003
:END:
#+title: ores.testcomp.party_key_entity
#+type: ores.codegen.entity
#+component: testcomp
#+entity_singular: party_key_entity
#+entity_plural: party_key_entities
#+entity_title: Party Key Entity
#+has_tenant_id: true
#+coding_scheme: none
#+image_id: false

Test entity whose SQL key is wider than its API key.

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

** party_id
:PROPERTIES:
:type:     uuid
:cpp_type: boost::uuids::uuid
:nullable: false
:sql_key:  true
:END:

The owning party.

* SQL

** Flags
:PROPERTIES:
:tablename: ores_testcomp_party_key_entities_tbl
:END:
"""


def _generate_sql(tmp_path):
    model_path = tmp_path / "ores.testcomp.party_key_entity.org"
    model_path.write_text(MODEL, encoding="utf-8")
    output_dir = tmp_path / "out"
    output_dir.mkdir()
    generate_from_model(
        str(model_path),
        DATA_DIR,
        TEMPLATES_DIR,
        output_dir,
        is_processing_batch=True,
        target_template="sql_schema_domain_entity_create.mustache",
        target_output="party_key_entity_create.sql",
    )
    return (output_dir / "party_key_entity_create.sql").read_text(encoding="utf-8")


def test_a_sql_key_column_joins_the_primary_key_and_exclusion(tmp_path):
    sql = _generate_sql(tmp_path)
    assert 'primary key (tenant_id, code, party_id, valid_from, valid_to)' in sql
    assert (
        "exclude using gist (\n"
        "        tenant_id WITH =,\n"
        "        code WITH =,\n"
        "        party_id WITH =,\n"
        "        tstzrange(valid_from, valid_to) WITH &&\n"
        "    )"
    ) in sql


def test_a_sql_key_column_joins_the_unique_indexes(tmp_path):
    sql = _generate_sql(tmp_path)
    assert '(tenant_id, code, party_id, version)' in sql
    assert '(tenant_id, code, party_id)\n' in sql
    assert 'party_key_entities_code_uniq_idx' in sql


def test_a_sql_key_column_scopes_the_versioning_triggers(tmp_path):
    sql = _generate_sql(tmp_path)
    assert 'code = NEW.code and party_id = NEW.party_id' in sql
    assert 'code = OLD.code and party_id = OLD.party_id' in sql
