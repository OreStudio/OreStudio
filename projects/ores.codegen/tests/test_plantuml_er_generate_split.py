"""Tests for the per-group rendering in plantuml_er_generate.py.

The schema is drawn one file per package, so the generator hands each
render a model with one package, the keys that both of whose ends are in
it, and the note naming the keys that leave it.

Run with:
    python3 -m pytest projects/ores.codegen/tests/test_plantuml_er_generate_split.py
"""
import sys
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[3]
sys.path.insert(0, str(REPO_ROOT / "projects/ores.codegen/src"))

import plantuml_er_generate as peg  # noqa: E402

# Echoes every key the generator puts in a render's context, so a case can
# assert what the real template would be given.
TEMPLATE = (
    "group={{diagram_title}} tables={{group_table_count}} "
    "index={{index_file}} file={{source_file}}\n"
    "{{#packages}}[{{name}}]{{#tables}} {{name}}{{/tables}}\n{{/packages}}"
    "keys:{{#relationships}} {{from_table}}->{{to_table}}{{/relationships}}\n"
    "crossing:{{external_note}}\n"
)

INDEX_TEMPLATE = (
    "{{total_tables}} tables in {{package_count}} groups\n"
    "{{#packages}}{{name}} -> {{file}} ({{table_count}})\n{{/packages}}"
)

MODEL = {
    "generated_at": "2026-10-09T00:00:00Z",
    "packages": [
        {"name": "alpha", "description": "First",
         "tables": [{"name": "ores_a_tbl"}, {"name": "ores_b_tbl"}]},
        {"name": "beta", "description": "Second",
         "tables": [{"name": "ores_c_tbl"}]},
    ],
    "relationships": [
        {"from_table": "ores_a_tbl", "to_table": "ores_b_tbl",
         "cardinality": "||--o{", "label": "has"},
        {"from_table": "ores_b_tbl", "to_table": "ores_c_tbl",
         "cardinality": "||--o{", "label": "has"},
    ],
}


def _group(name):
    package = next(p for p in MODEL["packages"] if p["name"] == name)
    return peg.render_group(MODEL, TEMPLATE, package,
                            "ores_schema.puml", f"ores_schema.{name}.puml")


def test_a_group_carries_its_own_package_and_table_count():
    rendered = _group("alpha")

    assert "group=alpha tables=2" in rendered
    assert "index=ores_schema.puml" in rendered
    assert "file=ores_schema.alpha.puml" in rendered


def test_a_group_renders_only_its_own_tables():
    rendered = _group("alpha")

    assert "[alpha] ores_a_tbl ores_b_tbl" in rendered
    assert "ores_c_tbl" in rendered  # only in the crossing note below
    assert "[beta]" not in rendered


def test_a_group_draws_only_the_keys_it_holds_inside_itself():
    rendered = _group("alpha")

    assert "keys: ores_a_tbl->ores_b_tbl" in rendered
    assert "ores_b_tbl->ores_c_tbl" not in rendered


def test_a_key_that_leaves_the_group_is_named_with_its_group():
    alpha = _group("alpha")
    beta = _group("beta")

    assert "crossing:- ores_c_tbl (beta)" in alpha
    # The far end is named from the group that does not own it, too.
    assert "crossing:- ores_b_tbl (alpha)" in beta


def test_a_group_with_no_crossing_key_carries_no_note():
    model = dict(MODEL, relationships=[])
    package = next(p for p in MODEL["packages"] if p["name"] == "alpha")

    rendered = peg.render_group(model, TEMPLATE, package,
                                "ores_schema.puml", "ores_schema.alpha.puml")

    assert "crossing:\n" in rendered


def test_the_index_names_every_group_and_the_file_it_is_drawn_in():
    rendered = peg.render_index(MODEL, INDEX_TEMPLATE,
                                Path("ores_schema.puml"))

    assert "3 tables in 2 groups" in rendered
    assert "alpha -> ores_schema.alpha.puml (2)" in rendered
    assert "beta -> ores_schema.beta.puml (1)" in rendered
