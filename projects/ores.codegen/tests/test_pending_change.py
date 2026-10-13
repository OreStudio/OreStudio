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


def test_a_junction_half_that_lacks_a_property_names_the_half_and_the_property():
    broken = (JUNCTION % ":gated: true").replace(":cpp_type:      boost::uuids::uuid\n", "", 1)
    try:
        derived(broken, "party_widget.org")
    except SystemExit as stop:
        assert "the '* Left' half" in str(stop) and ":cpp_type:" in str(stop)
    else:
        raise AssertionError("a half with no cpp_type was derived")


def test_a_junction_half_that_is_not_a_uuid_is_refused():
    broken = (JUNCTION % ":gated: true").replace(":type:          uuid", ":type:          text", 1)
    try:
        derived(broken, "party_widget.org")
    except SystemExit as stop:
        assert "generates only a uuid half" in str(stop)
    else:
        raise AssertionError("a text half was derived")


def test_a_generator_helper_the_rewrite_cannot_read_stops_the_derivation():
    odd = (ENTITY % ":gated: true").replace("uuid_gen()", "uuid_gen(idx)")
    try:
        derived(odd)
    except SystemExit as stop:
        assert "uuid_gen" in str(stop)
    else:
        raise AssertionError("an unreadable generator helper passed")


def test_the_derived_junction_is_a_read_only_uuid_identified_entity():
    out = derived(JUNCTION % ":gated: true", "party_widget.org")
    assert ":profile:   uuid-identified-lookup" in out
    assert ":client_read_only: true" in out


def test_a_lookup_validation_is_kept_and_its_cardinality_limit_is_dropped():
    out = pc.lookups_only(
        "\n"
        "| column | validation_function | cardinality_limit_table | cardinality_scope_column |\n"
        "|--------+---------------------+-------------------------+--------------------------|\n"
        "| scheme | validate_scheme_fn  | schemes_tbl             | owner_id                 |\n"
    )
    assert "validate_scheme_fn" in out
    assert "cardinality" not in out
    assert "schemes_tbl" not in out
    # The separator is rebuilt for the two columns that are left.
    assert out.count("\n|-") == 1


def test_a_table_that_names_only_a_column_and_a_function_is_left_as_it_is():
    table = "\n| column | validation_function |\n|--------+---------------------|\n| status | validate_status_fn  |\n"
    assert pc.lookups_only(table) == table


def test_a_foreign_key_to_a_table_with_a_pending_change_is_dropped():
    body = (
        "** owner_id\n:PROPERTIES:\n:table: ores_refdata_owners_tbl\n:END:\n\n"
        "** image_id\n:PROPERTIES:\n:table: ores_assets_images_tbl\n:END:\n"
    )
    kept = pc.drop_pending_targets(body, {"ores_refdata_owners_tbl"})
    assert "owner_id" not in kept
    assert "image_id" in kept


def test_the_tables_with_a_pending_change_are_read_from_the_models_beside_it(tmp_path):
    (tmp_path / "ores.refdata.widget.org").write_text(ENTITY % ":pending_change: true")
    (tmp_path / "ores.refdata.other.org").write_text(ENTITY % "")
    assert pc.pending_tables(tmp_path) == {"ores_refdata_widgets_tbl"}
