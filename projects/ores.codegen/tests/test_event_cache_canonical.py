"""Tests that a cached entity renders the canonical protocol.

The canonical protocol retired both mechanisms the nats-event-cache facet
used to warm and refresh a cache: there is no bulk read, so a full read is
the entity's list paged to its end, and a change is announced as a
canonical entity event. A model that still names the retired cache reader
therefore renders the canonical list request and response in its cache
header, and the canonical event subjects in its registrar, and never the
retired read_<plural>_for_cache pair, event_traits or entity_change_event.

Run::

    python3 -m pytest projects/ores.codegen/tests/test_event_cache_canonical.py
"""
import sys
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[3]
sys.path.insert(0, str(REPO_ROOT / "projects/ores.codegen/src"))

from codegen.core import generate_from_model  # noqa: E402

CODEGEN_BASE = REPO_ROOT / "projects/ores.codegen"
DATA_DIR = CODEGEN_BASE / "library" / "data"
TEMPLATES_DIR = CODEGEN_BASE / "library" / "templates"

# An entity that opts into the cache facet and keeps the retired
# read_for_cache flag in its model, so the render proves the flag no longer
# drives the generated warm-up. cached_by routes the cache to a consumer.
CACHE_MODEL = """\
:PROPERTIES:
:ID: 00000000-0000-0000-0000-0000000000C2
:ores.cpp.nats-event-cache.enabled: true
:END:
#+title: ores.testcomp.widget
#+type: ores.codegen.entity
#+component: testcomp
#+entity_singular: widget
#+entity_plural: widgets
#+entity_title: Widget
#+has_tenant_id: true
#+coding_scheme: none
#+image_id: false

A widget with a consumer-side cache.

* Flags
:PROPERTIES:
:schema:        public
:product:       ores
:component:     testcomp
:subcomponent:  api
:has_tenant_id: true
:END:

* Columns

** id
:PROPERTIES:
:type:            uuid
:cpp_type:        boost::uuids::uuid
:primary_key:     true
:skip_uuid_check: true
:END:

The key column.

* SQL

** Flags
:PROPERTIES:
:tablename: ores_testcomp_widgets_tbl
:END:

* C++

** Flags
:PROPERTIES:
:subcomponent:    api
:read_for_cache:  true
:cached_by:       consumer
:END:

** Repository
:PROPERTIES:
:entity_singular_short: widget
:entity_plural_short:   widgets
:entity_singular_words: widget
:entity_plural_words:   widgets
:END:
"""


def _render(tmp_path, template, output_name):
    model_path = tmp_path / "ores.testcomp.widget.org"
    model_path.write_text(CACHE_MODEL, encoding="utf-8")
    output_dir = tmp_path / output_name
    output_dir.mkdir()
    generate_from_model(
        str(model_path),
        DATA_DIR,
        TEMPLATES_DIR,
        output_dir,
        is_processing_batch=True,
        target_template=template,
        target_output=output_name,
    )
    return (output_dir / output_name).read_text(encoding="utf-8")


def test_cache_header_warms_by_paging_the_canonical_list(tmp_path):
    header = _render(tmp_path, "cpp_nats_event_cache.hpp.mustache",
                     "widget_cache.hpp")
    assert "for_cache" not in header
    assert "list_widgets_request" in header
    assert "list_widgets_response" in header
    assert "resp->result.outcome != ores::utility::domain::outcome::ok" in header
    # The response's page and total drive the loop to the end.
    assert "resp->widgets" in header
    assert "resp->total" in header


def test_cache_registrar_subscribes_to_the_canonical_events(tmp_path):
    registrar = _render(tmp_path, "cpp_nats_event_cache_registrar.hpp.mustache",
                        "widget_cache_registrar.hpp")
    assert "for_cache" not in registrar
    # The retired event_traits/entity_change_event pair. The canonical
    # replacements contain "event_traits" inside "entity_event_traits", so
    # the assertions name the retired symbols exactly.
    assert "ores.eventing.api/domain/event_traits.hpp" not in registrar
    assert "ores::eventing::domain::event_traits<" not in registrar
    assert "entity_change_event" not in registrar
    assert "widget_changed_event.hpp" not in registrar
    assert "entity_event_traits" in registrar
    assert "entity_event_notification" in registrar
    assert "widget_event.hpp" in registrar
    # One subscription per canonical action.
    assert registrar.count("event_subject<widget_event>") == 3
    assert '"created"' in registrar
    assert '"updated"' in registrar
    assert '"deleted"' in registrar
