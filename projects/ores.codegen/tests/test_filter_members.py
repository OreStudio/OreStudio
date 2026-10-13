"""Tests for the members of a generated filter record.

An equals member for each column the model already reads by, a one-of member
for a single-column primary key and each relation, and a search member when a
column is searchable. The record and the repository's condition both read
filter_members, so these cases pin the one list both depend on.

Run::

    python3 -m pytest projects/ores.codegen/tests/test_filter_members.py
"""
import sys
from pathlib import Path

import pytest

REPO_ROOT = Path(__file__).resolve().parents[3]
sys.path.insert(0, str(REPO_ROOT / "projects/ores.codegen/src"))

from codegen.org_loader import filter_members, filter_record_fields  # noqa: E402


def entity(**extra):
    base = {
        'entity_singular': 'widget',
        'primary_key': {'column': 'id', 'columns': [
            {'column': 'id', 'cpp_type': 'boost::uuids::uuid'}]},
        'natural_keys': [{'column': 'code', 'cpp_type': 'std::string'}],
        'columns': [
            {'name': 'name', 'cpp_type': 'std::string', 'searchable': True},
            {'name': 'owner_id', 'cpp_type': 'boost::uuids::uuid'},
            {'name': 'parent_id', 'cpp_type': 'std::optional<boost::uuids::uuid>'},
        ],
    }
    base.update(extra)
    return base


def by_member(described):
    return {m['member']: m for m in described['members']}


def test_a_key_alone_gives_a_one_of_member():
    members = by_member(filter_members(entity()))
    assert set(members) == {'id_one_of'}
    assert members['id_one_of']['cpp_type'] == 'std::vector<boost::uuids::uuid>'


def test_a_relation_gives_equals_and_one_of_members():
    e = entity(extra_list_requests=[{'filter_column': 'owner_id'}])
    members = by_member(filter_members(e))
    assert members['owner_id']['is_equals']
    assert members['owner_id']['cpp_type'] == 'boost::uuids::uuid'
    assert not members['owner_id']['is_nullable']
    assert members['owner_id_one_of']['is_one_of']


def test_a_nullable_relation_can_ask_for_null_and_lists_plain_values():
    e = entity(extra_list_requests=[{'filter_column': 'parent_id'}])
    members = by_member(filter_members(e))
    assert members['parent_id']['cpp_type'] == 'std::optional<boost::uuids::uuid>'
    assert members['parent_id']['is_nullable']
    assert members['parent_id_one_of']['cpp_type'] == 'std::vector<boost::uuids::uuid>'


def test_a_compound_key_gives_no_one_of_member():
    e = entity(primary_key={'column': 'a', 'columns': [
        {'column': 'a', 'cpp_type': 'std::string'},
        {'column': 'b', 'cpp_type': 'std::string'}]})
    assert filter_members(e)['members'] == []


def test_searchable_columns_add_one_search_member():
    e = entity()
    assert filter_members(e)['searchable'] == ['name']
    names = [f['name'] for f in filter_record_fields(e)]
    assert names == ['id_one_of', 'search']


def test_no_searchable_column_gives_no_search_member():
    e = entity(columns=[{'name': 'name', 'cpp_type': 'std::string'}])
    assert 'search' not in [f['name'] for f in filter_record_fields(e)]


def test_a_member_named_like_a_query_parameter_is_refused():
    e = entity(list_filter_column='limit',
               columns=[{'name': 'limit', 'cpp_type': 'std::string'}])
    with pytest.raises(ValueError, match="widget: the filter member 'limit'"):
        filter_members(e)


def test_a_filterable_column_gives_equals_and_one_of_members():
    e = entity(columns=[{'name': 'status', 'cpp_type': 'std::string', 'filterable': True}])
    members = by_member(filter_members(e))
    assert members['status']['is_equals']
    assert members['status']['cpp_type'] == 'std::string'
    assert members['status_one_of']['cpp_type'] == 'std::vector<std::string>'


def temporal(name, cpp_type, **extra):
    return {'name': name, 'cpp_type': cpp_type, 'filterable': True, **extra}


def test_a_filterable_date_column_gives_from_and_to_members():
    e = entity(columns=[temporal('agreed_on', 'std::optional<std::chrono::year_month_day>')])
    members = by_member(filter_members(e))
    assert members['agreed_on_from']['is_range']
    assert members['agreed_on_from']['is_from']
    assert members['agreed_on_to']['is_to']
    assert members['agreed_on_from']['column'] == 'agreed_on'
    assert members['agreed_on_from']['cpp_type'] == 'std::string'
    assert not members['agreed_on_from']['is_timestamp']


def test_a_filterable_timestamp_column_marks_its_range_as_a_timestamp():
    e = entity(columns=[temporal('executed_at', 'std::chrono::system_clock::time_point')])
    members = by_member(filter_members(e))
    assert members['executed_at_from']['is_timestamp']
    assert members['executed_at_to']['is_timestamp']


def test_a_temporal_column_keeps_its_equals_and_one_of_members():
    e = entity(columns=[temporal('agreed_on', 'std::chrono::year_month_day')])
    members = by_member(filter_members(e))
    assert members['agreed_on']['is_equals']
    assert members['agreed_on_one_of']['is_one_of']


def test_a_column_that_is_not_filterable_gives_no_range():
    e = entity(columns=[{'name': 'agreed_on', 'cpp_type': 'std::chrono::year_month_day'}])
    assert not any(m.get('is_range') for m in filter_members(e)['members'])


def test_a_filterable_column_of_another_type_gives_no_range():
    e = entity(columns=[temporal('status', 'std::string'), temporal('size', 'int')])
    assert not any(m.get('is_range') for m in filter_members(e)['members'])


def test_the_record_carries_each_bound_as_an_optional_string():
    e = entity(columns=[temporal('agreed_on', 'std::chrono::year_month_day')])
    fields = {f['name']: f for f in filter_record_fields(e)}
    for name in ('agreed_on_from', 'agreed_on_to'):
        assert fields[name]['cpp_type'] == 'std::optional<std::string>'
        assert fields[name]['ts_type'] == 'string | null'
