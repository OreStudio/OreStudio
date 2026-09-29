"""A nullable column whose C++ type is a bare value type must state a default.

Run::

    python3 -m pytest projects/ores.codegen/tests/test_nullable_bare_value_default.py

The entity mapper maps NULL to the zero value for a nullable numeric and
``std::optional`` carries it on the wire, so a bare ``double`` domain member is
how the estate states "absent". Only ``:default_value:`` reaches the domain
struct's member initializer, so a model that omits it leaves a
default-constructed domain object with an indeterminate member: the
2026-09-29 nightly reported 214 such reads in ``ores.analytics.core.tests``
(from ``credit_simulation_config.seed``) and 99 in ``ores.ore.core.tests``
(from ``bond_trs.funding_rate`` and ``bond_future``'s dates).

The loader refuses the shape rather than synthesising a default, because the
value the model means by "absent" is the model's to state -- and a nullable
column that genuinely wants NULL round-tripped declares
``std::optional<double>`` instead.
"""
import sys
from pathlib import Path

import pytest

REPO_ROOT = Path(__file__).resolve().parents[3]
sys.path.insert(0, str(REPO_ROOT / "projects/ores.codegen/src"))

from codegen.core import generate_from_model  # noqa: E402

CODEGEN_BASE = REPO_ROOT / "projects/ores.codegen"
DATA_DIR = CODEGEN_BASE / "library" / "data"
TEMPLATES_DIR = CODEGEN_BASE / "library" / "templates"

MODEL = """\
:PROPERTIES:
:ID: 00000000-0000-0000-0000-0000000000F4
:END:
#+title: ores.testcomp.nullable_bare_entity
#+type: ores.codegen.entity
#+component: testcomp
#+entity_singular: nullable_bare_entity
#+entity_plural: nullable_bare_entities
#+entity_title: Nullable Bare Entity
#+has_tenant_id: true
#+coding_scheme: none
#+image_id: false

An entity whose nullable numeric column states its type and maybe its default.

* Flags
:PROPERTIES:
:schema:    public
:product:   ores
:component: testcomp
:subcomponent: api
:has_tenant_id: true
:END:

* Columns

** id
:PROPERTIES:
:type:        uuid
:cpp_type:    boost::uuids::uuid
:primary_key: true
:END:

The surrogate key.

** coupon_rate
:PROPERTIES:
:type:      numeric(18, 10)
:cpp_type:  {cpp_type}
:nullable:  true
{default}:END:

The coupon rate, absent when the leg is floating.

* SQL

** Flags
:PROPERTIES:
:tablename: ores_testcomp_nullable_bare_entities_tbl
:END:
"""


def _render(tmp_path, cpp_type, default):
    model_path = tmp_path / "ores.testcomp.nullable_bare_entity.org"
    model_path.write_text(
        MODEL.format(cpp_type=cpp_type, default=default), encoding="utf-8"
    )
    output_dir = tmp_path / "out"
    output_dir.mkdir(exist_ok=True)
    generate_from_model(
        str(model_path),
        DATA_DIR,
        TEMPLATES_DIR,
        output_dir,
        is_processing_batch=True,
        target_template="cpp_domain_type_class.hpp.mustache",
        target_output="nullable_bare_entity.hpp",
    )
    return (output_dir / "nullable_bare_entity.hpp").read_text(encoding="utf-8")


def test_nullable_bare_double_without_a_default_is_refused(tmp_path):
    """The shape that produced the analytics and ore valgrind defects."""
    with pytest.raises(ValueError) as caught:
        _render(tmp_path, "double", "")
    message = str(caught.value)
    assert "coupon_rate" in message
    assert ":default_value:" in message
    assert "std::optional" in message


def test_nullable_bare_double_with_a_default_renders_it(tmp_path):
    """The fix the trading and analytics models took."""
    domain = _render(tmp_path, "double", ":default_value: 0.0\n")
    assert "double coupon_rate = 0.0;" in domain


def test_nullable_optional_needs_no_default(tmp_path):
    """The other way out, and the one a real absence takes."""
    domain = _render(tmp_path, "std::optional<double>", "")
    assert "std::optional<double> coupon_rate;" in domain


def test_non_nullable_double_is_left_alone(tmp_path):
    """A non-nullable column is the is_simple rule's business, not this one."""
    model = MODEL.format(cpp_type="double", default="").replace(
        ":nullable:  true", ":nullable:  false"
    )
    model_path = tmp_path / "ores.testcomp.nullable_bare_entity.org"
    model_path.write_text(model, encoding="utf-8")
    output_dir = tmp_path / "out2"
    output_dir.mkdir(exist_ok=True)
    generate_from_model(
        str(model_path),
        DATA_DIR,
        TEMPLATES_DIR,
        output_dir,
        is_processing_batch=True,
        target_template="cpp_domain_type_class.hpp.mustache",
        target_output="nullable_bare_entity.hpp",
    )
    domain = (output_dir / "nullable_bare_entity.hpp").read_text(encoding="utf-8")
    assert "double coupon_rate;" in domain
