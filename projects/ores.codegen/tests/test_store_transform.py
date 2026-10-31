"""Tests for a column whose model states how the store keeps it.

Run::

    python3 -m pytest projects/ores.codegen/tests/test_store_transform.py

A column that states =:store_transform:= carries a value the row must keep in
another form than the one a caller states. The model names the conversion and
the header that declares it; the mapper applies it on the way into the row and
includes the header. The read direction keeps the stored value as it stands.

The drift gate proves regeneration is stable against the tree. It stays silent
if the transform stops being emitted and the tree is regenerated to match, so
these tests pin the emitted code itself.
"""
import sys
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[3]
sys.path.insert(0, str(REPO_ROOT / "projects/ores.codegen/src"))

from codegen.core import _mapper_include_gates, render_template  # noqa: E402

TEMPLATE = "cpp_domain_type_mapper.cpp.mustache"


def _render(columns):
    entity = {
        'entity_singular': 'convention',
        'component_include': 'ores.refdata.api',
        'component_core': 'ores.refdata.core',
        'columns': columns,
        'primary_key': {'columns': [{'column': 'code'}]},
    }
    # Generation merges the include gates into the entity before it renders, so
    # a header reaches the template only through a column that asks for it.
    entity.update(_mapper_include_gates(entity))
    path = REPO_ROOT / "projects/ores.codegen/library/templates" / TEMPLATE
    return render_template(str(path), {'domain_entity': entity, 'cpp_license': ''})


def _transform_column(**extra):
    return {
        'name': 'business_day_convention',
        'store_transform': 'stored_business_day_convention',
        'store_transform_include': (
            'ores.refdata.core/repository/business_day_convention_code.hpp'),
        **extra,
    }


def test_a_plain_column_is_stored_through_the_transform():
    out = _render([_transform_column(is_simple=True)])
    assert "r.business_day_convention = stored_business_day_convention(" \
        "v.business_day_convention);" in out


def test_an_optional_column_keeps_absence_absent_through_the_transform():
    out = _render([_transform_column(is_already_optional=True)])
    assert "std::optional(stored_business_day_convention(*v.business_day_convention))" in out
    assert ": std::nullopt;" in out


def test_the_transform_is_applied_on_the_way_into_the_row_only():
    out = _render([_transform_column(is_simple=True)])
    assert out.count("stored_business_day_convention(") == 1


def test_the_transform_header_is_included():
    out = _render([_transform_column(is_simple=True)])
    assert '#include "ores.refdata.core/repository/business_day_convention_code.hpp"' in out


def test_a_column_with_no_transform_is_stored_as_it_stands():
    out = _render([{'name': 'code', 'is_simple': True}])
    assert "stored_business_day_convention" not in out
    assert "r.code = v.code;" in out
