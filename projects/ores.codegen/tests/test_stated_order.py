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
    assert e['sortable_fields'] == [
        {'name': 'code', 'column': 'code'},
        {'name': 'name', 'column': 'name'},
    ]


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


def test_the_recording_time_is_sortable_under_its_wire_name():
    e = entity(sortable_recorded_at=True)
    apply_stated_order(e)
    assert {'name': 'recorded_at', 'column': 'valid_from'} in e['sortable_fields']


def test_a_default_order_by_the_recording_time_reads_the_stored_column():
    e = entity(sortable_recorded_at=True, default_order='recorded_at desc')
    apply_stated_order(e)
    assert e['default_order_columns'] == '{"valid_from"}'
    assert e['default_order_desc'] == 'true'
    assert e['order_key_columns'] == '{"id"}'


def test_the_recording_time_is_not_sortable_unless_the_model_says_so():
    e = entity()
    apply_stated_order(e)
    assert 'recorded_at' not in [f['name'] for f in e['sortable_fields']]
    with pytest.raises(ValueError, match="names 'recorded_at'"):
        apply_stated_order(entity(default_order='recorded_at'))


def test_a_sortable_column_named_recorded_at_collides_with_the_flag():
    e = entity(sortable_recorded_at=True,
               columns=[{'name': 'recorded_at', 'sortable': True}])
    with pytest.raises(ValueError, match="already has that name"):
        apply_stated_order(e)


def test_a_current_state_entity_has_no_recording_time_to_sort_by():
    with pytest.raises(ValueError, match="sortable_recorded_at"):
        apply_stated_order(entity(sortable_recorded_at=True, sql={'current_state': True}))
