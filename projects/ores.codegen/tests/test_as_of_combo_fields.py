"""Tests for the has_as_of_combo_fields Qt detail-field gate.

Run::

    python3 -m pytest projects/ores.codegen/tests/test_as_of_combo_fields.py
"""
import sys
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[3]
sys.path.insert(0, str(REPO_ROOT / "projects/ores.codegen/src"))

from codegen.core import has_as_of_combo_fields  # noqa: E402


def test_no_detail_fields_is_false():
    assert has_as_of_combo_fields([]) is False


def test_dynamic_combo_without_as_of_fetch_fn_is_false():
    detail_fields = [{'type': 'dynamic_combo', 'field': 'book_status'}]
    assert has_as_of_combo_fields(detail_fields) is False


def test_dynamic_combo_with_as_of_fetch_fn_is_true():
    detail_fields = [{
        'type': 'dynamic_combo',
        'field': 'book_status',
        'combo_as_of_fetch_fn': 'fetch_book_statuses_at_timepoint',
    }]
    assert has_as_of_combo_fields(detail_fields) is True


def test_as_of_fetch_fn_on_non_dynamic_combo_field_is_ignored():
    # combo_as_of_fetch_fn only makes sense on a dynamic_combo; a
    # static_combo or flagged_combo declaring it shouldn't gate the facet.
    detail_fields = [{
        'type': 'static_combo',
        'field': 'currency',
        'combo_as_of_fetch_fn': 'fetch_currencies_at_timepoint',
    }]
    assert has_as_of_combo_fields(detail_fields) is False


def test_mixed_fields_true_when_any_dynamic_combo_declares_it():
    detail_fields = [
        {'type': 'line_edit', 'field': 'name'},
        {'type': 'dynamic_combo', 'field': 'regulatory_book_type'},
        {
            'type': 'dynamic_combo',
            'field': 'book_status',
            'combo_as_of_fetch_fn': 'fetch_book_statuses_at_timepoint',
        },
    ]
    assert has_as_of_combo_fields(detail_fields) is True
