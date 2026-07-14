"""Tests that the read_for_cache auth check and token-provider wiring
actually appear in rendered template output, not just in model-flag
validation.

Run::

    python3 -m pytest projects/ores.codegen/tests/test_read_for_cache_auth_rendering.py
"""
import sys
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[3]
sys.path.insert(0, str(REPO_ROOT / "projects/ores.codegen/src"))

from codegen.core import render_template  # noqa: E402

TEMPLATES_DIR = REPO_ROOT / "projects/ores.codegen/library/templates"

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
        'read_for_cache': True,
        'cached_by': 'consumer',
        'primary_key': {
            'is_uuid': True,
            'cpp_type': 'boost::uuids::uuid',
            'column': 'id',
        },
    },
}


def test_handler_read_for_cache_requires_authentication():
    rendered = render_template(TEMPLATES_DIR / "cpp_nats_handler.hpp.mustache", FIXTURE_ENTITY)
    assert "void read_for_cache(ores::nats::message msg) {" in rendered
    assert "make_request_context(" in rendered
    assert "Authentication-only, deliberately not tenant-scoped" in rendered


def test_cache_header_supports_token_provider():
    rendered = render_template(
        TEMPLATES_DIR / "cpp_nats_event_cache.hpp.mustache", FIXTURE_ENTITY)
    assert "std::function<std::string(bool)> token_provider" in rendered
    assert "void set_token_provider(" in rendered
    assert "ores::nats::headers::authorization" in rendered
    assert "ores::nats::headers::bearer_prefix" in rendered
