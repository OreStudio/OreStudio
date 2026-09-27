"""Tests for the entity-layer projection of a plain date column.

Run::

    python3 -m pytest projects/ores.codegen/tests/test_date_entity_projection.py

A plain (non-key) column whose ``:type:`` is ``date`` and whose domain
member is ``std::chrono::year_month_day`` is the shape every trading
instrument date column needs (defect 2, trade model cleanup). sqlgen
cannot bind ``year_month_day`` -- its parsing layer rejects the type with
a ``static_assert`` -- so the entity struct stays the ISO-8601
``std::string`` the database column stores, exactly as the refdata date
columns already do, and the mapper converts at the boundary with
``ores::platform::time::datetime::from_iso8601_date`` /
``to_iso8601_date``. The history field mapper renders the date instead of
silently dropping it.

Keys are deliberately out of scope: natural keys and primary keys carry
their own ``is_date`` projection. These cases pin the plain-column
projection, its optional form, and that an already-string ``:type: date``
column is left alone.
"""
import sys
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[3]
sys.path.insert(0, str(REPO_ROOT / "projects/ores.codegen/src"))

from codegen.core import generate_from_model  # noqa: E402

CODEGEN_BASE = REPO_ROOT / "projects/ores.codegen"
DATA_DIR = CODEGEN_BASE / "library" / "data"
TEMPLATES_DIR = CODEGEN_BASE / "library" / "templates"

ENTITY = """\
:PROPERTIES:
:ID: 00000000-0000-0000-0000-000000000043
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

** settlement_date
:PROPERTIES:
:type:     date
:cpp_type: std::chrono::year_month_day
:nullable: false
:END:

A plain NOT NULL date.

#+begin_src cpp :name generator
std::chrono::year_month_day{std::chrono::floor<std::chrono::days>(ctx.past_timepoint())}
#+end_src

** optional_settlement_date
:PROPERTIES:
:type:     date
:cpp_type: std::optional<std::chrono::year_month_day>
:nullable: true
:END:

A plain nullable date.

** legacy_date
:PROPERTIES:
:type:     date
:cpp_type: std::string
:nullable: false
:END:

An already-string date column that needs no conversion.

** label
:PROPERTIES:
:type:     text
:cpp_type: std::string
:nullable: false
:END:

An ordinary text column.

* C++

** Domain includes

#+begin_src cpp :name includes
#include <chrono>
#+end_src
"""


def _render(tmp_path, template, output):
    model_path = tmp_path / "ores.testcomp.probe.org"
    model_path.write_text(ENTITY, encoding="utf-8")
    output_dir = tmp_path / "out"
    output_dir.mkdir(exist_ok=True)
    generate_from_model(
        str(model_path),
        DATA_DIR,
        TEMPLATES_DIR,
        output_dir,
        is_processing_batch=True,
        target_template=template,
        target_output=output,
    )
    return (output_dir / output).read_text(encoding="utf-8")


def _render_domain(tmp_path):
    return _render(tmp_path, "cpp_domain_type_class.hpp.mustache", "probe.hpp")


def _render_entity(tmp_path):
    return _render(tmp_path, "cpp_domain_type_entity.hpp.mustache", "probe_entity.hpp")


def _render_mapper(tmp_path):
    return _render(tmp_path, "cpp_domain_type_mapper.cpp.mustache", "probe_mapper.cpp")


def _render_history(tmp_path):
    return _render(
        tmp_path, "cpp_history_field_mapper.cpp.mustache", "probe_history_field_mapper.cpp"
    )


def test_a_plain_date_is_a_calendar_date_in_the_domain_struct(tmp_path):
    """The domain member is the typed date, both plain and optional."""
    domain = _render_domain(tmp_path)
    assert "std::chrono::year_month_day settlement_date;" in domain
    assert "std::optional<std::chrono::year_month_day> optional_settlement_date;" in domain
    assert "#include <chrono>" in domain


def test_a_plain_date_reaches_the_entity_as_an_iso_string(tmp_path):
    """The entity is the database row type, and sqlgen cannot bind a date."""
    entity = _render_entity(tmp_path)
    assert "std::string settlement_date;" in entity
    assert "std::optional<std::string> optional_settlement_date;" in entity
    assert "std::chrono::year_month_day settlement_date;" not in entity
    assert "std::optional<std::chrono::year_month_day> optional_settlement_date;" not in entity


def test_the_mapper_converts_a_plain_date_in_both_directions(tmp_path):
    """Parse on read, format on write, with the include the helpers need."""
    mapper = _render_mapper(tmp_path)
    parse = "r.settlement_date = ores::platform::time::datetime::from_iso8601_date(v.settlement_date);"
    render = "r.settlement_date = ores::platform::time::datetime::to_iso8601_date(v.settlement_date);"
    assert parse in mapper
    assert render in mapper
    assert '#include "ores.platform/time/datetime.hpp"' in mapper


def test_the_mapper_converts_an_optional_plain_date(tmp_path):
    """The optional form guards on has_value and maps NULL to nullopt."""
    mapper = _render_mapper(tmp_path)
    parse = (
        "r.optional_settlement_date = v.optional_settlement_date.has_value() ? "
        "std::optional(ores::platform::time::datetime::from_iso8601_date(*v.optional_settlement_date)) "
        ": std::nullopt;"
    )
    render = (
        "r.optional_settlement_date = v.optional_settlement_date.has_value() ? "
        "std::optional(ores::platform::time::datetime::to_iso8601_date(*v.optional_settlement_date)) "
        ": std::nullopt;"
    )
    assert parse in mapper
    assert render in mapper


def test_the_history_field_mapper_renders_a_plain_date(tmp_path):
    """A date with no render_* branch was dropped from the diff fields."""
    history = _render_history(tmp_path)
    assert (
        '{.name = "Settlement Date", .value = '
        "ores::platform::time::datetime::to_iso8601_date(v.settlement_date)}" in history
    )
    assert (
        ".value = v.optional_settlement_date ? "
        "ores::platform::time::datetime::to_iso8601_date(*v.optional_settlement_date) : "
        "std::string{}}" in history
    )


def test_a_string_typed_date_column_needs_no_conversion(tmp_path):
    """The flag follows the domain type, so an already-string date is untouched.

    Keying the projection on ``:type: date`` alone would emit a
    from_iso8601_date call for a column whose domain member is std::string,
    which does not compile.
    """
    entity = _render_entity(tmp_path)
    assert "std::string legacy_date;" in entity

    mapper = _render_mapper(tmp_path)
    assert "r.legacy_date = v.legacy_date;" in mapper
    assert "from_iso8601_date(v.legacy_date)" not in mapper
    assert "to_iso8601_date(v.legacy_date)" not in mapper
