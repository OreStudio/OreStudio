"""Tests for the conditional include gates of the generated C++ sources.

Run::

    python3 -m pytest projects/ores.codegen/tests/test_include_gates.py

A template declares a header that only some of its files need behind a context
flag, and the flag mirrors the template branches that spell the header's
symbol. These tests pin each gate to those branches, so a branch that moves
without its gate fails here rather than as an include-cleaner finding. The
whole-tree proof is the clang_tidy target: see the clang-tidy infrastructure
page.
"""
import sys
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[3]
sys.path.insert(0, str(REPO_ROOT / "projects/ores.codegen/src"))

from codegen.core import _mapper_include_gates, _service_include_gates  # noqa: E402


def _entity(columns=(), pk_columns=(), natural_keys=(), **flags):
    return {
        'columns': list(columns),
        'primary_key': {'columns': list(pk_columns), **flags.pop('primary_key', {})},
        'natural_keys': list(natural_keys),
        **flags,
    }


def test_mapper_needs_nothing_conditional_for_plain_text_columns():
    gates = _mapper_include_gates(_entity(columns=[{'name': 'code'}]))
    assert not gates['mapper_uses_optional']
    assert not gates['mapper_uses_string']
    assert not gates['mapper_uses_string_view']
    assert not gates['mapper_uses_datetime']
    assert gates['mapper_enum_includes'] == []


def test_mapper_optional_follows_a_nullable_column():
    gates = _mapper_include_gates(_entity(columns=[{'name': 'note', 'is_nullable_string': True}]))
    assert gates['mapper_uses_optional']


def test_mapper_ignores_a_sql_only_column():
    gates = _mapper_include_gates(
        _entity(columns=[{'name': 'x', 'is_optional_uuid': True, 'sql_only': True}]))
    assert not gates['mapper_uses_optional']


def test_mapper_string_follows_an_integer_primary_key():
    gates = _mapper_include_gates(_entity(pk_columns=[{'column': 'seq', 'is_int': True}]))
    assert gates['mapper_uses_string']


def test_mapper_datetime_follows_a_date_column_but_not_a_date_natural_key():
    assert _mapper_include_gates(_entity(columns=[{'name': 'd', 'is_date': True}]))[
        'mapper_uses_datetime']
    assert not _mapper_include_gates(_entity(natural_keys=[{'column': 'd', 'is_date': True}]))[
        'mapper_uses_datetime']


def test_mapper_names_the_header_of_each_enum_column():
    gates = _mapper_include_gates(_entity(columns=[
        {'name': 'nature', 'is_enum': True,
         'cpp_type': 'ores::trading::domain::booking_nature'},
        {'name': 'scope', 'render_is_enum': True,
         'render_cpp_type': 'ores::trading::domain::counterparty_scope'},
    ]))
    assert gates['mapper_enum_includes'] == [
        {'path': 'ores.trading.api/domain/booking_nature.hpp'},
        {'path': 'ores.trading.api/domain/counterparty_scope.hpp'},
    ]


def test_service_pages_versions_only_when_derived_with_a_versions_list():
    assert _service_include_gates(
        _entity(protocol_derived=True, list_versions_operations=[{}]))['service_pages_versions']
    assert not _service_include_gates(
        _entity(protocol_derived=False, list_versions_operations=[{}]))['service_pages_versions']


def test_service_uuid_follows_a_single_uuid_primary_key():
    gates = _service_include_gates(_entity(primary_key={'is_single_uuid': True}))
    assert gates['service_uses_uuid']


def test_service_parses_a_declared_uuid_key_that_is_not_primary():
    gates = _service_include_gates(_entity(key_is_primary=False, declared_key_is_uuid=True))
    assert gates['service_parses_declared_uuid']
    assert gates['service_uses_uuid']
    assert gates['service_uses_uuid_io']


def test_service_datetime_follows_a_timestamp_key():
    gates = _service_include_gates(_entity(primary_key={'has_timestamp_key': True}))
    assert gates['service_uses_datetime']
