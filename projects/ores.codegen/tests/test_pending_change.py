"""Tests for src/codegen/pending_change.py.

Run::

    python3 -m pytest projects/ores.codegen/tests/test_pending_change.py

The pending change of a gated entity is derived from its model. Three shapes
reach the derivation and each needs a rule: a plain entity, an entity that
carries a hierarchy of its own, and a junction, which has two halves and no
columns section.
"""
import sys
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[3]
sys.path.insert(0, str(REPO_ROOT / "projects" / "ores.codegen" / "src"))

from codegen import pending_change as pc  # noqa: E402

ENTITY = """\
:PROPERTIES:
:ID: 11111111-1111-4111-8111-111111111111
:END:
#+title: ores.refdata.widget
#+type: ores.codegen.entity
#+component: refdata
#+entity_singular: widget
#+entity_plural: widgets
#+entity_title: Widget

* Flags
:PROPERTIES:
:component:             refdata
:parent_id_column:      parent_widget_id
:hierarchy_name_field:  full_name
:generate_touch_function: true
:profile:   self-referencing-hierarchy
%s
:END:

* Columns

** id
:PROPERTIES:
:type:        uuid
:cpp_type:    boost::uuids::uuid
:primary_key: true
:END:

The id.

** owner_id
:PROPERTIES:
:type:        uuid
:cpp_type:    boost::uuids::uuid
:natural_key: true
:END:

The owner.

#+begin_src cpp :name generator
uuid_gen()
#+end_src

* C++

** Domain includes

#+begin_src cpp :name includes
#include <string>
#+end_src
"""

JUNCTION = """\
:PROPERTIES:
:ID: 22222222-2222-4222-8222-222222222222
:END:
#+title: ores.refdata.party_widget
#+type: ores.codegen.junction
#+component: refdata
#+name: party_widgets
#+name_singular: party_widget
#+name_title: Party Widget

* Flags
:PROPERTIES:
:profile: tenant-scoped-junction
%s
:END:

* Left
:PROPERTIES:
:column:        party_id
:type:          uuid
:cpp_type:      boost::uuids::uuid
:END:

ID of the party.

* Right
:PROPERTIES:
:column:        widget_id
:type:          uuid
:cpp_type:      boost::uuids::uuid
:END:

ID of the widget.
"""


def derived(text, name="widget.org"):
    return pc.derive(Path(name), text)


def test_a_model_is_gated_by_either_flag_and_by_nothing_else():
    assert pc.is_gated(ENTITY % ":gated: true")
    assert pc.is_gated(ENTITY % ":pending_change: true")
    assert not pc.is_gated(ENTITY % ":gated: false")
    assert not pc.is_gated(ENTITY % "")


def test_neither_flag_reaches_the_derived_model():
    out = derived(ENTITY % ":pending_change: true")
    assert ":gated:" not in out
    assert ":pending_change:" not in out


def test_a_hierarchy_of_the_source_does_not_reach_the_pending_line():
    out = derived(ENTITY % ":gated: true")
    for flag in (":parent_id_column:", ":hierarchy_name_field:", ":generate_touch_function:"):
        assert flag not in out
    assert ":profile:   uuid-identified-lookup" in out


def test_a_generator_helper_of_the_source_becomes_the_shared_generator():
    out = derived(ENTITY % ":gated: true")
    assert "uuid_gen()" not in out
    assert "ctx.generate_uuid()" in out


def test_the_live_rows_natural_key_is_dropped_and_its_id_renamed():
    out = derived(ENTITY % ":gated: true")
    assert "** entity_id" in out
    # Only the line's own request and line number are natural keys.
    assert out.count(":natural_key: true") == 2


def test_a_junction_becomes_an_entity_holding_both_halves():
    out = derived(JUNCTION % ":pending_change: true", "party_widget.org")
    assert "#+type: ores.codegen.entity" in out
    assert "#+entity_singular: party_widget_change" in out
    assert "** party_id" in out
    assert "** widget_id" in out
    assert "ores_refdata_party_widget_changes_tbl" in out
    assert "** operation" in out and "** base_version" in out


def test_a_junction_without_both_halves_is_refused():
    broken = (JUNCTION % ":gated: true").replace("* Right", "* Other")
    try:
        derived(broken, "party_widget.org")
    except SystemExit as stop:
        assert "no '* Right' section" in str(stop)
    else:
        raise AssertionError("a junction with no right half was derived")


def test_the_pending_model_is_written_beside_a_junction_by_its_singular_name(tmp_path):
    source = tmp_path / "ores.refdata.party_widget_junction.org"
    source.write_text(JUNCTION % ":gated: true")
    target = pc.write_derived(source)
    assert target == tmp_path / "ores.refdata.party_widget_change.org"
    assert target.exists()
