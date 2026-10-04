"""Tests for the stated order a generated list reads in.

A column opts in with :sortable: true and the entity names its default order
with :default_order:. Every order ends in the key, so rows that tie page
reproducibly, and a scoped read keeps its own :list_by_order_by: default.

Run::

    python3 -m pytest projects/ores.codegen/tests/test_stated_order.py
"""
import sys
from pathlib import Path

import pytest

REPO_ROOT = Path(__file__).resolve().parents[3]
sys.path.insert(0, str(REPO_ROOT / "projects/ores.codegen/src"))

from codegen.core import apply_stated_order  # noqa: E402


def entity(**extra):
    base = {
        'entity_singular': 'widget',
        'primary_key': {'column': 'id', 'columns': [{'column': 'id'}]},
        'natural_keys': [{'column': 'code', 'sortable': True}],
        'columns': [{'name': 'name', 'sortable': True}, {'name': 'notes'}],
    }
    base.update(extra)
    return base


def test_sortable_fields_are_the_columns_that_opt_in():
    e = entity()
    apply_stated_order(e)
    assert e['sortable_fields'] == [{'name': 'code'}, {'name': 'name'}]


def test_no_default_order_reads_in_key_order():
    e = entity()
    apply_stated_order(e)
    assert e['default_order_columns'] == '{"id"}'
    assert e['default_order_desc'] == 'false'
    assert e['order_key_columns'] == '{"id"}'


def test_a_compound_key_orders_by_every_key_column():
    e = entity(primary_key={'column': 'a', 'columns': [{'column': 'a'}, {'column': 'b'}]})
    apply_stated_order(e)
    assert e['default_order_columns'] == '{"a", "b"}'
    assert e['order_key_columns'] == '{"a", "b"}'


def test_a_default_order_names_a_sortable_column():
    e = entity(default_order='name desc')
    apply_stated_order(e)
    assert e['default_order_columns'] == '{"name"}'
    assert e['default_order_desc'] == 'true'


def test_a_default_order_on_a_column_that_is_not_sortable_is_refused():
    with pytest.raises(ValueError, match="widget: :default_order: names 'notes'"):
        apply_stated_order(entity(default_order='notes'))


def test_a_malformed_default_order_is_refused():
    with pytest.raises(ValueError, match="must be '<column>' or '<column> desc'"):
        apply_stated_order(entity(default_order='name sideways'))


def test_a_scoped_read_keeps_its_own_default():
    fk = {'column': 'parent_id', 'list_by': True, 'list_by_order_by': 'display_order'}
    e = entity(default_order='name', foreign_keys=[fk])
    apply_stated_order(e)
    assert fk['list_by_default_columns'] == '{"display_order"}'
    assert fk['list_by_default_desc'] == 'false'


def test_a_scoped_read_with_no_default_takes_the_entity_default():
    fk = {'column': 'parent_id', 'list_by': True}
    e = entity(default_order='name desc', foreign_keys=[fk])
    apply_stated_order(e)
    assert fk['list_by_default_columns'] == '{"name"}'
    assert fk['list_by_default_desc'] == 'true'
