"""Tests that a generated cache reads each partition inside its own tenant.

A partition is one tenant's copy of the producer's rows. The cache asks its
token provider for a token for that partition's tenant, so row-level security
returns exactly that tenant's rows and no row is filtered in code. A target
that refuses the token as expired gets one renewed token and one retry.

Run::

    python3 -m pytest projects/ores.codegen/tests/test_cache_partition_identity.py
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


def render():
    return render_template(CACHE_TEMPLATE, copy.deepcopy(FIXTURE_ENTITY))


def test_a_shared_read_has_no_tenant_filter_and_a_tenant_read_has_one():
    assert derive_tenant_read_flags(
        {'has_tenant_id': True, 'tenant_read_scope': 'shared'})['read_tenant_filtered'] is False
    assert derive_tenant_read_flags({'has_tenant_id': True})['read_tenant_filtered'] is True
    assert derive_tenant_read_flags({})['read_tenant_filtered'] is False


def test_no_cache_flag_is_derived():
    assert 'cache_tenant_filter' not in derive_tenant_read_flags(
        {'has_tenant_id': True, 'tenant_read_scope': 'shared'})


def test_the_cache_takes_a_token_provider_per_partition():
    rendered = render()
    assert "partition_token_provider token_provider = nullptr" in rendered
    assert "token_provider_(tenant_id, renew)" in rendered


def test_the_cache_files_every_row_it_reads_without_a_tenant_check():
    rendered = render()
    assert "v.tenant_id" not in rendered
    assert "entries_t.set(v.id, std::move(v));" in rendered


def test_an_expired_token_is_renewed_once_and_the_page_retried():
    rendered = render()
    assert 'refusal->second == "token_expired"' in rendered
    assert "!renewed && authorise(true)" in rendered


def test_a_partition_with_no_token_fails_its_load():
    assert "no token acts inside this tenant" in render()
