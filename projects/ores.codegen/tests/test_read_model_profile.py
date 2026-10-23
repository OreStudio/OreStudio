"""Tests for the read-model profile.

Run::

    python3 -m pytest projects/ores.codegen/tests/test_read_model_profile.py

A model that binds ``read-model`` is read through a database view. The profile
keeps the read-only current-state surface, replaces the table create and drop
with a view create and drop, and withdraws the notify trigger and the facets
that announce a change, because a view has no rows to announce.
"""
import sys
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[3]
sys.path.insert(0, str(REPO_ROOT / "projects/ores.codegen/src"))

from codegen.core import generate_from_model  # noqa: E402
from codegen.generate import resolve_targets  # noqa: E402

CODEGEN = REPO_ROOT / "projects/ores.codegen"
DATA_DIR = CODEGEN / "library" / "data"
TEMPLATES_DIR = CODEGEN / "library" / "templates"

ANNOUNCEMENT_TEMPLATES = frozenset({
    "cpp_nats_changed_event.hpp.mustache",
    "cpp_nats_event_registrar.hpp.mustache",
    "cpp_nats_event_registrar.cpp.mustache",
    "cpp_nats_integration_test.cpp.mustache",
})

MODEL = """\
:PROPERTIES:
:ID: 00000000-0000-0000-0000-0000000000D1
:END:
#+title: ores.testcomp.widget_summary
#+type: ores.codegen.entity
#+component: testcomp
#+entity_singular: widget_summary
#+entity_plural: widget_summaries
#+entity_title: Widget Summary
#+coding_scheme: none
#+image_id: false

One row for each widget, with its owner name.

* Flags
:PROPERTIES:
:schema:        public
:product:       ores
:component:     testcomp
:subcomponent:  api
:has_tenant_id: false
:profile:       read-model
:END:

* SQL

** Flags
:PROPERTIES:
:tablename: ores_testcomp_widget_summaries_vw
:END:

** View

#+begin_src sql :name select
select w.id, o.name as owner_name
from ores_testcomp_widgets_tbl w
join ores_testcomp_owners_tbl o on o.id = w.owner_id;
#+end_src

* Columns

** id
:PROPERTIES:
:type:         uuid
:cpp_type:     boost::uuids::uuid
:primary_key:  true
:END:

The widget identity.

** owner_name
:PROPERTIES:
:type:     text
:cpp_type: std::string
:nullable: false
:END:

The name of the widget owner.
"""


def _write(tmp_path):
    modeling = tmp_path / "ores.testcomp" / "modeling"
    modeling.mkdir(parents=True)
    model = modeling / "ores.testcomp.widget_summary.org"
    model.write_text(MODEL, encoding="utf-8")
    return model


def _templates(model):
    units, model_type, _ = resolve_targets(model, CODEGEN)
    assert model_type == "domain_entity"
    return {u["template"] for u in units}


def _render(tmp_path, template, output_name):
    output_dir = tmp_path / "out"
    output_dir.mkdir()
    generate_from_model(
        str(_write(tmp_path)), DATA_DIR, TEMPLATES_DIR, output_dir,
        is_processing_batch=True,
        target_template=template, target_output=output_name)
    return (output_dir / output_name).read_text(encoding="utf-8")


def test_the_view_replaces_the_table(tmp_path):
    templates = _templates(_write(tmp_path))

    assert "sql_schema_domain_entity_view_create.mustache" in templates
    assert "sql_schema_domain_entity_view_drop.mustache" in templates
    assert "sql_schema_domain_entity_create.mustache" not in templates
    assert "sql_schema_domain_entity_drop.mustache" not in templates


def test_nothing_announces_a_change(tmp_path):
    templates = _templates(_write(tmp_path))

    assert not (ANNOUNCEMENT_TEMPLATES & templates)
    assert "sql_schema_notify_trigger.mustache" not in templates


def test_the_reads_are_still_served(tmp_path):
    templates = _templates(_write(tmp_path))

    assert "cpp_protocol.hpp.mustache" in templates
    assert "cpp_nats_handler.hpp.mustache" in templates


def test_the_view_is_created_from_the_model_select(tmp_path):
    sql = _render(tmp_path, "sql_schema_domain_entity_view_create.mustache",
                  "widget_summaries_create.sql")

    assert 'create or replace view "ores_testcomp_widget_summaries_vw"' in sql
    assert "security_invoker = true" in sql
    assert "join ores_testcomp_owners_tbl o on o.id = w.owner_id;" in sql
    assert "owner_name" in sql
    assert ";;" not in sql


def test_the_view_is_dropped(tmp_path):
    sql = _render(tmp_path, "sql_schema_domain_entity_view_drop.mustache",
                  "widget_summaries_drop.sql")

    assert 'drop view if exists "ores_testcomp_widget_summaries_vw";' in sql


def test_the_profile_is_catalogued():
    catalogue = (REPO_ROOT / "projects/modeling/variability_profiles.org").read_text(
        encoding="utf-8")

    assert "Read model" in catalogue
