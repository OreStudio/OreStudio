"""Tests for the cache_aux_type messaging-flag gate.

Run::

    python3 -m pytest projects/ores.codegen/tests/test_cache_aux_type.py
"""
import sys
from pathlib import Path

import pytest

REPO_ROOT = Path(__file__).resolve().parents[3]
sys.path.insert(0, str(REPO_ROOT / "projects/ores.codegen/src"))

from codegen.core import validate_cache_aux_type  # noqa: E402


def test_cache_aux_type_unset_is_a_noop():
    domain_entity = {}
    validate_cache_aux_type(domain_entity)
    assert 'cache_aux_type' not in domain_entity


def test_cache_aux_type_allowed_with_cached_by():
    domain_entity = {'cache_aux_type': 'children_map', 'cached_by': 'iam'}
    validate_cache_aux_type(domain_entity)
    assert domain_entity['cache_aux_type'] == 'children_map'


def test_cache_aux_type_rejected_without_cached_by():
    domain_entity = {'cache_aux_type': 'children_map', 'entity_singular': 'widget'}
    with pytest.raises(ValueError, match="widget: cache_aux_type requires cached_by"):
        validate_cache_aux_type(domain_entity)
