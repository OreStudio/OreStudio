"""Tests for a compound primary key that carries a timestamp column.

Run::

    python3 -m pytest projects/ores.codegen/tests/test_timestamp_primary_key.py

A TimescaleDB hypertable's partition column must sit in the primary key, so
the first such entity -- the IAM sessions table, keyed ``(id, start_time)``
-- gives the estate a compound key whose second column is a timestamp. The
entity, mapper and generator templates only knew how to synthesise and
convert a uuid, text or integer key column; a timestamp key column fell
through to the string fallback, which does not compile against the domain
member's ``std::chrono::system_clock::time_point``.

The entity keeps the estate-wide text binding for a timestamp
(``sqlgen::PrimaryKey<std::string>``), so the mapper parses the column on
read and formats it on write, and the generator fills it from the past.
"""
import sys
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[3]
sys.path.insert(0, str(REPO_ROOT / "projects/ores.codegen/src"))

from codegen.core import generate_from_model  # noqa: E402

CODEGEN = REPO_ROOT / "projects/ores.codegen"
DATA_DIR = CODEGEN / "library" / "data"
TEMPLATES_DIR = CODEGEN / "library" / "templates"

ENTITY = """\
:PROPERTIES:
:ID: 00000000-0000-0000-0000-000000000042
:END:
#+title: ores.testcomp.probe
#+type: ores.codegen.entity
#+component: testcomp
#+entity_singular: probe
#+entity_plural: probes
#+entity_title: Probe
#+coding_scheme: none
#+image_id: false

Probe entity.

* Flags
:PROPERTIES:
:schema:    public
:product:   ores
:component: testcomp
:subcomponent: api
:END:

* Columns

** id
:PROPERTIES:
:type:            uuid
:cpp_type:        boost::uuids::uuid
:primary_key:     true
:skip_uuid_check: true
:END:

Primary key.

** start_time
:PROPERTIES:
:type:        timestamp with time zone
:cpp_type:    std::chrono::system_clock::time_point
:primary_key: true
:nullable:    false
:END:

Partition column, so part of the primary key.

** note
:PROPERTIES:
:type:     text
:cpp_type: std::string
:nullable: false
:END:

A plain column.
"""


def _render(tmp_path, template, suffix):
    model_path = tmp_path / "ores.testcomp.probe.org"
    model_path.write_text(ENTITY, encoding="utf-8")
    output_dir = tmp_path / "out"
    output_dir.mkdir(exist_ok=True)
    generate_from_model(
        str(model_path), DATA_DIR, TEMPLATES_DIR, output_dir,
        is_processing_batch=True, target_template=template,
        target_output=f"probe.{suffix}")
    return (output_dir / f"probe.{suffix}").read_text(encoding="utf-8")


def test_the_entity_binds_the_timestamp_key_as_text(tmp_path):
    entity = _render(tmp_path, "cpp_domain_type_entity.hpp.mustache", "hpp")
    assert "sqlgen::PrimaryKey<std::string> id;" in entity
    assert "sqlgen::PrimaryKey<std::string> start_time;" in entity


def test_the_mapper_converts_the_timestamp_key(tmp_path):
    mapper = _render(tmp_path, "cpp_domain_type_mapper.cpp.mustache", "cpp")
    assert (
        "r.start_time = timestamp_to_timepoint(std::string_view{v.start_time.value()});"
        in mapper
    )
    assert (
        "r.start_time = ores::platform::time::datetime::to_db_string(v.start_time);"
        in mapper
    )
    assert '#include "ores.platform/time/datetime.hpp"' in mapper


def test_the_generator_fills_the_timestamp_key_from_the_past(tmp_path):
    generated = _render(
        tmp_path, "cpp_domain_type_generator.cpp.mustache", "cpp")
    assert "r.start_time = ctx.past_timepoint();" in generated
    assert 'r.start_time = std::string(faker::word::noun());' not in generated


def test_the_service_does_not_call_empty_on_the_timestamp_key(tmp_path):
    """``time_point`` has no ``empty()``; only the text key keeps the guard."""
    service = _render(tmp_path, "cpp_service.cpp.mustache", "cpp")
    assert "v.id.is_nil()" in service
    assert "v.start_time.empty()" not in service

