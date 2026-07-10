"""Tests for compute_view_groups: grouping a Qt detail dialog's fields into
tabs via the optional per-field ``view_group`` cell.

Run::

    python3 -m pytest projects/ores.codegen/tests/test_view_groups.py
"""
import sys
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[3]
sys.path.insert(0, str(REPO_ROOT / "projects/ores.codegen/src"))

from codegen.core import compute_view_groups  # noqa: E402


def _field(name, view_group=None):
    return {'field': name, 'view_group': view_group}


def test_no_view_group_reproduces_legacy_single_tab_names():
    fields = [_field('code'), _field('name'), _field('description')]
    groups = compute_view_groups(fields)

    assert len(groups) == 1
    group = groups[0]
    assert group['name'] == 'General'
    assert group['tab_widget_name'] == 'generalTab'
    assert group['tab_layout_name'] == 'generalLayout'
    assert group['group_box_name'] == 'basicInfoGroup'
    assert group['group_box_title'] == 'Basic Information'
    assert group['form_layout_name'] == 'formLayout'
    assert group['spacer_name'] == 'verticalSpacer'
    assert group['_is_first'] is True
    assert group['detail_fields'] == fields


def test_no_view_group_sets_group_row_index_matching_position():
    fields = [_field('code'), _field('name'), _field('description')]
    compute_view_groups(fields)

    assert [f['_group_row_index'] for f in fields] == [0, 1, 2]


def test_multiple_view_groups_preserve_first_appearance_order():
    fields = [
        _field('code', 'General'),
        _field('name', 'General'),
        _field('symbol', 'Formatting'),
        _field('format', 'Formatting'),
        _field('rounding_type', 'Rounding'),
    ]
    groups = compute_view_groups(fields)

    assert [g['name'] for g in groups] == ['General', 'Formatting', 'Rounding']
    assert [f['field'] for f in groups[0]['detail_fields']] == ['code', 'name']
    assert [f['field'] for f in groups[1]['detail_fields']] == ['symbol', 'format']
    assert [f['field'] for f in groups[2]['detail_fields']] == ['rounding_type']


def test_multiple_view_groups_use_derived_names_not_legacy_names():
    fields = [_field('code', 'General'), _field('symbol', 'Formatting')]
    groups = compute_view_groups(fields)

    general, formatting = groups
    assert general['tab_widget_name'] == 'generalTab'
    assert general['group_box_name'] == 'generalGroup'
    assert general['group_box_title'] == 'General'  # not "Basic Information"
    assert formatting['tab_widget_name'] == 'formattingTab'
    assert formatting['group_box_name'] == 'formattingGroup'
    assert formatting['group_box_title'] == 'Formatting'


def test_multiple_view_groups_get_distinct_spacer_and_layout_names():
    fields = [_field('code', 'General'), _field('symbol', 'Formatting')]
    groups = compute_view_groups(fields)

    spacer_names = {g['spacer_name'] for g in groups}
    layout_names = {g['form_layout_name'] for g in groups}
    assert len(spacer_names) == 2
    assert len(layout_names) == 2


def test_group_row_index_resets_per_group():
    fields = [
        _field('code', 'General'),
        _field('name', 'General'),
        _field('symbol', 'Formatting'),
    ]
    compute_view_groups(fields)

    assert fields[0]['_group_row_index'] == 0
    assert fields[1]['_group_row_index'] == 1
    assert fields[2]['_group_row_index'] == 0  # first field of its own group


def test_only_first_group_marked_is_first():
    fields = [
        _field('code', 'General'),
        _field('symbol', 'Formatting'),
        _field('rounding_type', 'Rounding'),
    ]
    groups = compute_view_groups(fields)

    assert [g['_is_first'] for g in groups] == [True, False, False]


def test_field_with_no_view_group_falls_back_to_general_alongside_explicit_group():
    # Once any field declares a view_group, the entity is no longer in the
    # legacy no-view_group case -- a field left blank simply joins "General"
    # alongside any other field that named it explicitly.
    fields = [_field('code', 'General'), _field('name', None)]
    groups = compute_view_groups(fields)

    assert len(groups) == 1
    assert groups[0]['name'] == 'General'
    assert groups[0]['group_box_title'] == 'General'  # derived naming, not legacy
    assert [f['field'] for f in groups[0]['detail_fields']] == ['code', 'name']
