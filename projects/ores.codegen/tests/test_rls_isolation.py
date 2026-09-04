"""Tests for the validate_rls_isolation sql-flag gate.

The create template emits the AS RESTRICTIVE party-isolation policy inside
the tenant-isolation block, so party isolation without tenant isolation
would silently emit no RLS at all for the entity.

Run::

    python3 -m pytest projects/ores.codegen/tests/test_rls_isolation.py
"""
import sys
from pathlib import Path

import pytest

REPO_ROOT = Path(__file__).resolve().parents[3]
sys.path.insert(0, str(REPO_ROOT / "projects/ores.codegen/src"))

from codegen.core import validate_rls_isolation  # noqa: E402


def test_no_sql_flags_is_valid():
    validate_rls_isolation({})


def test_tenant_isolation_alone_is_valid():
    validate_rls_isolation({'sql': {'rls_tenant_isolation': True}})


def test_party_isolation_requires_tenant_isolation():
    domain_entity = {
        'entity_singular': 'market_series',
        'sql': {'rls_party_isolation': True},
    }
    with pytest.raises(
            ValueError,
            match='market_series: rls_party_isolation requires '
                  'rls_tenant_isolation'):
        validate_rls_isolation(domain_entity)


def test_party_isolation_with_tenant_isolation_is_valid():
    validate_rls_isolation({'sql': {
        'rls_tenant_isolation': True,
        'rls_party_isolation': True,
    }})
