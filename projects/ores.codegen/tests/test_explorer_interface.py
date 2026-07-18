"""Tests for the qt.explorer_interface knob gate.

Run::

    python3 -m pytest projects/ores.codegen/tests/test_explorer_interface.py
"""
import sys
from pathlib import Path

import pytest

REPO_ROOT = Path(__file__).resolve().parents[3]
sys.path.insert(0, str(REPO_ROOT / "projects/ores.codegen/src"))

from codegen.core import validate_explorer_interface  # noqa: E402


def test_explorer_interface_unset_is_a_noop():
    domain_entity = {'qt': {}}
    validate_explorer_interface(domain_entity)
    assert 'explorer_interface' not in domain_entity['qt']


def test_explorer_interface_allowed_with_has_explorer_api():
    domain_entity = {
        'qt': {'explorer_interface': 'IBusinessUnitBrowser', 'has_explorer_api': True}
    }
    validate_explorer_interface(domain_entity)
    assert domain_entity['qt']['explorer_interface'] == 'IBusinessUnitBrowser'


def test_explorer_interface_rejected_without_has_explorer_api():
    domain_entity = {
        'entity_singular': 'widget',
        'qt': {'explorer_interface': 'IWidgetBrowser'},
    }
    with pytest.raises(
        ValueError, match="widget: qt.explorer_interface requires qt.has_explorer_api"
    ):
        validate_explorer_interface(domain_entity)
