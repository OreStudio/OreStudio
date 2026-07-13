"""Tests for the read_for_cache messaging-flag gate.

Run::

    python3 -m pytest projects/ores.codegen/tests/test_read_for_cache.py
"""
import sys
from pathlib import Path

import pytest

REPO_ROOT = Path(__file__).resolve().parents[3]
sys.path.insert(0, str(REPO_ROOT / "projects/ores.codegen/src"))

from codegen.core import validate_read_for_cache  # noqa: E402


def test_read_for_cache_defaults_to_false():
    domain_entity = {}
    validate_read_for_cache(domain_entity)
    assert domain_entity['read_for_cache'] is False


def test_read_for_cache_allowed_with_tenant_id():
    domain_entity = {'read_for_cache': True, 'has_tenant_id': True}
    validate_read_for_cache(domain_entity)
    assert domain_entity['read_for_cache'] is True


def test_read_for_cache_rejected_without_tenant_id():
    domain_entity = {'read_for_cache': True, 'entity_singular': 'widget'}
    with pytest.raises(ValueError, match="widget: read_for_cache requires has_tenant_id"):
        validate_read_for_cache(domain_entity)
