"""Tests for the entity-layer projection of a money column.

Run::

    python3 -m pytest projects/ores.codegen/tests/test_decimal_entity_projection.py

A =numeric= column whose domain member is
``ores::utility::decimal::decimal`` is the shape every monetary amount
column needs (decision D11, clean trading). sqlgen binds a fixed set of
types and =boost::multiprecision= is not one of them, so the entity
struct stays the exact decimal ``std::string`` the database column
stores and the mapper parses and renders at the boundary through the
type's own =from_string= / =to_string=. The history field mapper
renders the decimal instead of dropping the field.

The projection is keyed on the resolved domain type, never on
``:type: numeric`` alone: a rate, volatility or correlation column is
numeric too and stays ``double``. These cases pin the required and
optional projections, that an already-double numeric column is left
alone, and that a nullable decimal whose domain member is the bare
decimal is refused rather than silently mapping NULL to zero.
"""
import sys
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[3]
sys.path.insert(0, str(REPO_ROOT / "projects/ores.codegen/src"))

import pytest  # noqa: E402

from codegen.core import generate_from_model  # noqa: E402

CODEGEN_BASE = REPO_ROOT / "projects/ores.codegen"
DATA_DIR = CODEGEN_BASE / "library" / "data"
TEMPLATES_DIR = CODEGEN_BASE / "library" / "templates"

DECIMAL = "ores::utility::decimal::decimal"

ENTITY = """\
:PROPERTIES:
:ID: 00000000-0000-0000-0000-0000000000D1
:END:
#+title: ores.testcomp.decimal_probe
#+type: ores.codegen.entity
#+component: testcomp
#+entity_singular: decimal_probe
#+entity_plural: decimal_probes
#+entity_title: Decimal Probe
#+coding_scheme: none
#+image_id: false

Decimal probe entity.

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

** amount
:PROPERTIES:
:type:     numeric(28, 10)
:cpp_type: {decimal}
:nullable: false
:END:

A plain NOT NULL money column.

** optional_amount
:PROPERTIES:
:type:     numeric(28, 10)
:cpp_type: std::optional<{decimal}>
:nullable: true
:END:

A plain nullable money column.

** legacy_rate
:PROPERTIES:
:type:     numeric(18, 10)
:cpp_type: double
:nullable: false
:END:

A numeric rate column that stays double.

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
#include <optional>
#include <string>
#+end_src
"""

NULLABLE_BARE = ENTITY.replace(
    ":cpp_type: std::optional<{decimal}>\n:nullable: true",
    ":cpp_type: {decimal}\n:nullable: true",
)

DECIMAL_KEY = ENTITY.replace(
    ":type:     numeric(28, 10)\n:cpp_type: {decimal}\n:nullable: false\n:END:",
    ":type:     numeric(28, 10)\n:cpp_type: {decimal}\n:nullable: false\n"
    ":natural_key: true\n:END:",
)


def _render(tmp_path, template, output, body=None):
    model_path = tmp_path / "ores.testcomp.decimal_probe.org"
    model_path.write_text((body or ENTITY).format(decimal=DECIMAL), encoding="utf-8")
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
    return _render(tmp_path, "cpp_domain_type_class.hpp.mustache", "decimal_probe.hpp")


def _render_entity(tmp_path):
    return _render(tmp_path, "cpp_domain_type_entity.hpp.mustache", "decimal_probe_entity.hpp")


def _render_mapper(tmp_path):
    return _render(tmp_path, "cpp_domain_type_mapper.cpp.mustache", "decimal_probe_mapper.cpp")


def _render_history(tmp_path):
    return _render(
        tmp_path, "cpp_history_field_mapper.cpp.mustache", "decimal_probe_history_field_mapper.cpp"
    )


def _render_generator(tmp_path):
    return _render(
        tmp_path, "cpp_domain_type_generator.cpp.mustache", "decimal_probe_generator.cpp"
    )


def test_a_money_column_is_a_decimal_in_the_domain_struct(tmp_path):
    """The domain member is the exact decimal, both plain and optional."""
    domain = _render_domain(tmp_path)
    assert f"{DECIMAL} amount;" in domain
    assert f"std::optional<{DECIMAL}> optional_amount;" in domain
    assert '#include "ores.utility/decimal/decimal.hpp"' in domain


def test_a_money_column_reaches_the_entity_as_an_exact_string(tmp_path):
    """The entity is the database row type, and sqlgen cannot bind a decimal."""
    entity = _render_entity(tmp_path)
    assert "std::string amount;" in entity
    assert "std::optional<std::string> optional_amount;" in entity
    assert f"{DECIMAL} amount;" not in entity
    assert f"std::optional<{DECIMAL}> optional_amount;" not in entity


def test_the_mapper_parses_and_renders_a_money_column(tmp_path):
    """Parse on read, render on write, with the include the conversion needs."""
    mapper = _render_mapper(tmp_path)
    parse = f"r.amount = {DECIMAL}::from_string(v.amount).value();"
    render = "r.amount = v.amount.to_string();"
    assert parse in mapper
    assert render in mapper
    assert '#include "ores.utility/decimal/decimal.hpp"' in mapper


def test_the_mapper_converts_an_optional_money_column(tmp_path):
    """The optional form guards on has_value and maps NULL to nullopt."""
    mapper = _render_mapper(tmp_path)
    parse = (
        "r.optional_amount = v.optional_amount.has_value() ? "
        f"std::optional({DECIMAL}::from_string(*v.optional_amount).value()) : std::nullopt;"
    )
    render = (
        "r.optional_amount = v.optional_amount.has_value() ? "
        "std::optional(v.optional_amount->to_string()) : std::nullopt;"
    )
    assert parse in mapper
    assert render in mapper


def test_the_history_field_mapper_renders_a_money_column(tmp_path):
    """A decimal with no render_* branch was dropped from the diff fields."""
    history = _render_history(tmp_path)
    assert '{.name = "Amount", .value = v.amount.to_string()}' in history
    assert ".value = v.optional_amount ? v.optional_amount->to_string() : std::string{}" in history


def test_the_synthetic_generator_fills_a_money_column(tmp_path):
    """A decimal with no generator branch was left at the default zero."""
    generator = _render_generator(tmp_path)
    assert f"r.amount = {DECIMAL}::from_string(" in generator
    assert f"r.optional_amount = {DECIMAL}::from_string(" in generator
    assert '#include "ores.utility/decimal/decimal.hpp"' in generator


def test_a_double_numeric_column_needs_no_conversion(tmp_path):
    """The flag follows the domain type, so a rate stays a double.

    Keying the projection on ``:type: numeric`` alone would move every
    rate, volatility and correlation column, which D11 keeps as doubles.
    """
    entity = _render_entity(tmp_path)
    assert "double legacy_rate;" in entity

    mapper = _render_mapper(tmp_path)
    assert "r.legacy_rate = v.legacy_rate;" in mapper
    assert "from_string(v.legacy_rate)" not in mapper
    assert "legacy_rate.to_string()" not in mapper


def test_a_nullable_bare_decimal_is_refused(tmp_path):
    """NULL and zero must not collapse into the same member."""
    model_path = tmp_path / "ores.testcomp.decimal_probe.org"
    model_path.write_text(NULLABLE_BARE.format(decimal=DECIMAL), encoding="utf-8")
    with pytest.raises(ValueError, match="nullable decimal column"):
        generate_from_model(
            str(model_path),
            DATA_DIR,
            TEMPLATES_DIR,
            tmp_path / "out",
            is_processing_batch=True,
            target_template="cpp_domain_type_entity.hpp.mustache",
            target_output="decimal_probe_entity.hpp",
        )


def test_a_decimal_key_is_refused(tmp_path):
    """A key carries its own projection, and none of them is the decimal."""
    model_path = tmp_path / "ores.testcomp.decimal_probe.org"
    model_path.write_text(DECIMAL_KEY.format(decimal=DECIMAL), encoding="utf-8")
    with pytest.raises(ValueError, match="is a key and its"):
        generate_from_model(
            str(model_path),
            DATA_DIR,
            TEMPLATES_DIR,
            tmp_path / "out",
            is_processing_batch=True,
            target_template="cpp_domain_type_entity.hpp.mustache",
            target_output="decimal_probe_entity.hpp",
        )
