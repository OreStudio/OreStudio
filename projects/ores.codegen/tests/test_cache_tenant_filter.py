"""Tests that a generated cache over a shared-scope entity keeps only its own
partition's rows.

A shared read is governed by RLS alone, and its policy allows the
system-tenant fallback, so the rows that come back span tenants. A generated
cache holds one partition per tenant, so without a filter it files another
tenant's rows under this partition's key and later answers a lookup with a
party the caller's tenant never had. The filter lives in the template, because
the hand-written copy this replaced was deleted by the next regeneration.

Run::

    python3 -m pytest projects/ores.codegen/tests/test_cache_tenant_filter.py
"""
import copy
import sys
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[3]
sys.path.insert(0, str(REPO_ROOT / "projects/ores.codegen/src"))

from codegen.core import derive_tenant_read_flags, render_template  # noqa: E402

TEMPLATES_DIR = REPO_ROOT / "projects/ores.codegen/library/templates"
CACHE_TEMPLATE = TEMPLATES_DIR / "cpp_nats_event_cache.hpp.mustache"

FIXTURE_ENTITY = {
    'domain_entity': {
        'entity_singular': 'widget',
        'entity_plural': 'widgets',
        'entity_plural_short': 'widgets',
        'entity_pascal': 'Widget',
        'component': 'producer',
        'component_include': 'producer',
        'cache_component': 'consumer',
        'cache_component_upper': 'CONSUMER',
        'cached_by': 'consumer',
        'primary_key': {
            'is_uuid': True,
            'cpp_type': 'boost::uuids::uuid',
            'column': 'id',
        },
    },
}


def test_shared_scope_read_is_unfiltered_but_its_cache_is_filtered():
    domain_entity = derive_tenant_read_flags({
        'has_tenant_id': True,
        'tenant_read_scope': 'shared',
    })
    assert domain_entity['read_tenant_filtered'] is False
    assert domain_entity['cache_tenant_filter'] is True


def test_tenant_scope_read_is_filtered_and_its_cache_needs_nothing():
    domain_entity = derive_tenant_read_flags({'has_tenant_id': True})
    assert domain_entity['read_tenant_filtered'] is True
    assert domain_entity['cache_tenant_filter'] is False


def test_an_entity_without_a_tenant_column_gets_neither_flag():
    domain_entity = derive_tenant_read_flags({})
    assert domain_entity['read_tenant_filtered'] is False
    assert domain_entity['cache_tenant_filter'] is False


def test_shared_scope_cache_renders_the_partition_filter():
    entity = copy.deepcopy(FIXTURE_ENTITY)
    entity['domain_entity']['cache_tenant_filter'] = True
    rendered = render_template(CACHE_TEMPLATE, entity)
    assert "if (v.tenant_id.to_string() != tenant_id)" in rendered
    assert "continue;" in rendered


def test_tenant_scope_cache_renders_no_filter():
    entity = copy.deepcopy(FIXTURE_ENTITY)
    entity['domain_entity']['cache_tenant_filter'] = False
    rendered = render_template(CACHE_TEMPLATE, entity)
    assert "v.tenant_id" not in rendered
