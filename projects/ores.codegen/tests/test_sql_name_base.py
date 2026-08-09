"""Tests for sql_name_base: the base every SQL identifier for an entity is
composed from (table, insert/notify/delete functions, triggers, delete rule).

PostgreSQL truncates identifiers to 63 bytes, keeping the *first* 63
characters, so a base longer than 63 - longest_suffix silently collapses
every derived object onto one identical name (the last create or replace
wins; earlier objects vanish). The model's :tablename: (minus the trailing
_tbl) is the explicit base when set, with the conventional
<product>_<component>_<entity_plural> composition as fallback, and a
truncation safety net in both cases so a suffix always survives.

Run::

    python3 -m pytest projects/ores.codegen/tests/test_sql_name_base.py
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

# The ores.synthetic entity whose 66-char conventional base triggered the
# collision; without an explicit :tablename: the composition exceeds the
# 63-char budget and the safety net must truncate it.
LONG_PLURAL = "ir_curve_generation_config_process_parameter_values"


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


def _base_family_names(sql):
    """Return {suffix: name} for the table/function/trigger/rule identifiers."""
    table = re.search(r'create table if not exists "([^"]+)"', sql).group(1)
    fn = re.search(r"create or replace function ([a-z_0-9]+)\(", sql).group(1)
    trg = re.search(r"create or replace trigger ([a-z_0-9]+)", sql).group(1)
    rule = re.search(r"create or replace rule ([a-z_0-9]+)", sql).group(1)
    return {"_tbl": table, "_insert_fn": fn, "_insert_trg": trg, "_delete_rule": rule}


def test_explicit_tablename_is_the_base_for_all_objects(tmp_path):
    body = REQUIRED_FLAGS.replace(
        "ores_testcomp_compound_key_entities_tbl", "ores_testcomp_short_tbl"
    )
    sql = _generate_sql(tmp_path, body=body)
    names = _base_family_names(sql)
    assert names == {
        "_tbl": "ores_testcomp_short_tbl",
        "_insert_fn": "ores_testcomp_short_insert_fn",
        "_insert_trg": "ores_testcomp_short_insert_trg",
        "_delete_rule": "ores_testcomp_short_delete_rule",
    }
    # The conventional composed name must not leak into base-family objects
    # (indexes keep their own entity_plural-prefixed naming).
    assert "compound_key_entities_insert" not in sql
    assert "compound_key_entities_delete" not in sql


def test_missing_tablename_falls_back_to_conventional_composition(tmp_path):
    body = REQUIRED_FLAGS.replace(
        ":tablename: ores_testcomp_compound_key_entities_tbl\n",
        ":schema: public\n",
    )
    sql = _generate_sql(tmp_path, body=body)
    names = _base_family_names(sql)
    assert names == {
        "_tbl": "ores_testcomp_compound_key_entities_tbl",
        "_insert_fn": "ores_testcomp_compound_key_entities_insert_fn",
        "_insert_trg": "ores_testcomp_compound_key_entities_insert_trg",
        "_delete_rule": "ores_testcomp_compound_key_entities_delete_rule",
    }


def test_long_base_is_truncated_so_every_suffix_survives_distinct(tmp_path):
    body = REQUIRED_FLAGS.replace(
        "ores_testcomp_compound_key_entities_tbl", ""
    ).replace("compound_key_entities", LONG_PLURAL)
    # The SQL section now has no :tablename:; keep the section parseable.
    body = body.replace(":END:\n", ":schema: public\n:END:\n", 1)
    sql = _generate_sql(tmp_path, body=body)
    names = _base_family_names(sql)
    # All identifiers fit PostgreSQL's 63-byte budget...
    assert all(len(name) <= 63 for name in names.values())
    # ...and, critically, they are all distinct: the naive composition would
    # make every one truncate to the same first-63-chars string.
    assert len(set(names.values())) == 4
    for suffix, name in names.items():
        assert name.endswith(suffix)
    # The un-truncated 69-char composition is impossible in PostgreSQL.
    assert LONG_PLURAL + "_tbl" not in sql
