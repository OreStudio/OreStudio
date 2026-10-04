"""Tests for the append_insert flag.

Run::

    python3 -m pytest projects/ores.codegen/tests/test_append_insert.py

A repository write states a claim per row and reads the current row to stamp
its version, which is one query per row. An append-only series, such as
market observations, never edits a row in place, so a bulk load has no row to
claim: the flag gives the repository an insert that writes the batch as one
statement.
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
:ID: 00000000-0000-0000-0000-0000000000E1
:END:
#+title: ores.testcomp.tick_record
#+type: ores.codegen.entity
#+component: testcomp
#+entity_singular: tick_record
#+entity_plural: tick_records
#+entity_title: Tick Record
#+coding_scheme: none
#+image_id: false

A point in a series that is never edited in place.

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

** value
:PROPERTIES:
:type:     text
:cpp_type: std::string
:nullable: false
:END:

The observed value.

* SQL
** Flags
:PROPERTIES:
{sql_flags}:END:
"""

APPEND_INSERT = ":append_insert: true\n"
CURRENT_STATE = ":current_state: true\n"


def _model(*flags):
    return MODEL.format(sql_flags="".join(flags))


def _render(tmp_path, template, output_name, body):
    model_path = tmp_path / "ores.testcomp.tick_record.org"
    model_path.write_text(body, encoding="utf-8")
    output_dir = tmp_path / output_name
    output_dir.mkdir()
    generate_from_model(
        str(model_path), DATA_DIR, TEMPLATES_DIR, output_dir,
        is_processing_batch=True,
        target_template=template, target_output=output_name)
    return (output_dir / output_name).read_text(encoding="utf-8")


def _header(tmp_path, body):
    return _render(tmp_path, "cpp_domain_type_repository.hpp.mustache",
                   "tick_record_repository.hpp", body)


def _implementation(tmp_path, body):
    return _render(tmp_path, "cpp_domain_type_repository.cpp.mustache",
                   "tick_record_repository.cpp", body)


def _function(source, signature):
    """The body of the function whose definition starts with @p signature."""
    start = source.index(signature)
    end = source.index("\n}\n", start)
    return source[start:end]


def test_the_flag_declares_a_single_and_a_batch_insert(tmp_path):
    header = _header(tmp_path, _model(APPEND_INSERT))

    assert "void insert(context ctx, const domain::tick_record& v);" in header
    assert "void insert(context ctx, const std::vector<domain::tick_record>& v);" in header


def test_an_insert_writes_without_a_claim(tmp_path):
    source = _implementation(tmp_path, _model(APPEND_INSERT))

    for signature in (
            "void tick_record_repository::insert(context ctx, const domain::tick_record& v)",
            "void tick_record_repository::insert(\n    context ctx, "
            "const std::vector<domain::tick_record>& v)"):
        body = _function(source, signature)
        assert "execute_write_query(ctx, tick_record_mapper::map(v)" in body
        assert "claim" not in body


def test_without_the_flag_there_is_no_insert(tmp_path):
    header = _header(tmp_path, _model())
    source = _implementation(tmp_path, _model())

    assert "insert(" not in header
    assert "tick_record_repository::insert" not in source


def test_a_current_state_table_cannot_append(tmp_path):
    with pytest.raises(ValueError, match="append_insert"):
        _header(tmp_path, _model(APPEND_INSERT, CURRENT_STATE))
