"""Tests for the junction artefact archetype.

A junction could not reach a staging table at all before this archetype:
``get_junction_template_mappings`` resolves a junction to one template,
and the domain_entity archetype reads fields a junction does not have
(``natural_keys`` and ``columns`` at the entity root, where a junction
keeps its two keys under ``left`` and ``right``). Four hand-written
staging tables existed as a result.

Run::

    python3 -m pytest projects/ores.codegen/tests/test_junction_artefact.py
"""
import sys
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[3]
sys.path.insert(0, str(REPO_ROOT / "projects/ores.codegen/src"))

from codegen.core import generate_from_model  # noqa: E402

CODEGEN_BASE = REPO_ROOT / "projects/ores.codegen"
DATA_DIR = CODEGEN_BASE / "library" / "data"
TEMPLATES_DIR = CODEGEN_BASE / "library" / "templates"

JUNCTION = """\
:PROPERTIES:
:ID: 00000000-0000-0000-0000-0000000000d1
:END:
#+title: ores.testcomp.test_junction
#+type: ores.codegen.junction
#+component: testcomp
#+name: test_junctions
#+name_singular: test_junction
#+name_title: Test Junction
#+name_singular_words: test junction
#+brief: A test junction.
#+product: ores
#+schema: public
#+has_tenant_id: true

A test junction.

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
"""

EXTRA_COLUMN = """\

* Columns

** weight
:PROPERTIES:
:type:     integer
:cpp_type: int
:nullable: false
:END:
"""


def generate(tmp_path, extra=""):
    model_path = tmp_path / "ores.testcomp.test_junction.org"
    model_path.write_text(JUNCTION + extra, encoding="utf-8")
    output_dir = tmp_path / "out"
    output_dir.mkdir(exist_ok=True)
    generate_from_model(
        str(model_path),
        DATA_DIR,
        TEMPLATES_DIR,
        output_dir,
        is_processing_batch=True,
        target_template="sql_schema_junction_artefact_create.mustache",
        target_output="out.sql",
    )
    return (output_dir / "out.sql").read_text(encoding="utf-8")


def test_the_staging_table_is_the_header_plus_both_keys_and_a_version(tmp_path):
    sql = generate(tmp_path)

    assert (
        'create table if not exists "ores_dq_test_junctions_artefact_tbl" (\n'
        '    "dataset_id" uuid not null,\n'
        '    "tenant_id" uuid not null,\n'
        '    "left_code" text not null,\n'
        '    "right_code" text not null,\n'
        '    "version" integer not null\n'
        ");"
    ) in sql


def test_each_side_gets_its_own_index_named_after_its_short_form(tmp_path):
    sql = generate(tmp_path)

    assert (
        "create index if not exists dq_test_junctions_artefact_left_idx\n"
        "on ores_dq_test_junctions_artefact_tbl (left_code);"
    ) in sql
    assert (
        "create index if not exists dq_test_junctions_artefact_right_idx\n"
        "on ores_dq_test_junctions_artefact_tbl (right_code);"
    ) in sql


def test_the_header_indexes_are_emitted(tmp_path):
    sql = generate(tmp_path)

    assert "dq_test_junctions_artefact_dataset_idx" in sql
    assert "dq_test_junctions_artefact_tenant_idx" in sql


def test_an_extra_junction_column_comes_before_the_version(tmp_path):
    sql = generate(tmp_path, EXTRA_COLUMN)

    assert (
        '    "left_code" text not null,\n'
        '    "right_code" text not null,\n'
        '    "weight" integer not null,\n'
        '    "version" integer not null\n'
        ");"
    ) in sql


def test_the_domain_entity_archetype_is_not_used_for_a_junction(tmp_path):
    """The staging table must not read entity-root fields a junction does
    not carry, which is why this archetype exists."""
    sql = generate(tmp_path)

    assert '    "code" text not null' not in sql
