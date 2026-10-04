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
