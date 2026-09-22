"""Tests for the cached_by messaging-flag gate.

cached_by names the consumer component a generated nats-event-cache lands
in. It stands alone: the cache warms from the canonical list read and
refreshes from the canonical entity events, so it requires no verb the
model declares. It does require tenant scoping, because the cache keeps one
partition per tenant.

Run::

    python3 -m pytest projects/ores.codegen/tests/test_cached_by.py
"""
import sys
from pathlib import Path

import pytest

REPO_ROOT = Path(__file__).resolve().parents[3]
sys.path.insert(0, str(REPO_ROOT / "projects/ores.codegen/src"))

from codegen.core import validate_cached_by  # noqa: E402


def test_cached_by_unset_is_a_noop():
    domain_entity = {}
    validate_cached_by(domain_entity)
    assert 'cached_by' not in domain_entity


def test_cached_by_stands_alone_without_read_for_cache():
    domain_entity = {'cached_by': 'iam', 'has_tenant_id': True}
    validate_cached_by(domain_entity)
    assert domain_entity['cached_by'] == 'iam'
    assert 'read_for_cache' not in domain_entity


def test_cached_by_rejected_without_tenant_id():
    domain_entity = {'cached_by': 'iam', 'entity_singular': 'widget'}
    with pytest.raises(ValueError, match="widget: cached_by requires has_tenant_id"):
        validate_cached_by(domain_entity)
