"""Tests that the handler serves no bulk read for cache warming, and that the
cache header's token-provider wiring appears in rendered template output
rather than only in model-flag validation.

A cache is fed by events. The specification says so outright: "There is no verb
for reading everything. A full read is a list paged to its end, and a cache is
fed by events rather than by a bulk read." A model that still names a cache
reader therefore gets no handler method for it, and the method it used to get
-- authenticated but deliberately not tenant-scoped, so that a cache-warming
account could read another tenant -- has no successor to be copied into the
wrong place.

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


def test_the_handler_serves_no_bulk_read_for_cache_warming():
    """The fixture asks for one; the template serves the canonical verbs and
    nothing for a cache, because a cache is fed by events."""
    rendered = render_template(TEMPLATES_DIR / "cpp_nats_handler.hpp.mustache", FIXTURE_ENTITY)
    assert "read_for_cache" not in rendered
    assert "Authentication-only, deliberately not tenant-scoped" not in rendered


def test_cache_header_supports_token_provider():
    rendered = render_template(
        TEMPLATES_DIR / "cpp_nats_event_cache.hpp.mustache", FIXTURE_ENTITY)
    assert "std::function<std::string(bool)> token_provider" in rendered
    assert "void set_token_provider(" in rendered
    assert "ores::nats::headers::authorization" in rendered
    assert "ores::nats::headers::bearer_prefix" in rendered
