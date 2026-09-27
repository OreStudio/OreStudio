"""Tests for the generated HTTP recipe inventory.

Run::

    python3 -m pytest projects/ores.codegen/tests/test_http_recipe_inventory.py

The inventory is what makes a generated recipe visible: a recipe that is not
listed is a page nobody browsing opens, and nothing else looks. These cases pin
the scope -- the generated corpus, one directory deep, and not the curated
index of hand-written recipes beside it -- and the gate that fails when the
index goes stale.
"""
import importlib.util
import sys
from pathlib import Path

import pytest

REPO_ROOT = Path(__file__).resolve().parents[3]
SCRIPT = REPO_ROOT / "projects/ores.codegen/scripts/regenerate_http_recipe_inventory.py"

sys.path.insert(0, str(REPO_ROOT / "projects/ores.codegen/src"))


def _load():
    spec = importlib.util.spec_from_file_location("http_recipe_inventory", SCRIPT)
    module = importlib.util.module_from_spec(spec)
    sys.modules[spec.name] = module
    spec.loader.exec_module(module)
    return module


inventory = _load()

RECIPE = """\
:PROPERTIES:
:ID: {id}
:END:
#+title: {title}
#+description: A recipe.
#+type: recipe
#+level: cross
#+filetags: {tags}
"""


@pytest.fixture
def tree(tmp_path, monkeypatch):
    """A recipe tree with one generated recipe, one flat one, and an index."""
    root = tmp_path / "recipes"
    (root / "things").mkdir(parents=True)
    (root / "things" / "thing.org").write_text(RECIPE.format(
        id="11111111-1111-1111-1111-111111111111",
        title="How do I call the things endpoints over HTTP?",
        tags=":recipe:http:things:"), encoding="utf-8")
    # The hand-written recipes predate the facet and sit flat in the root.
    (root / "handwritten.org").write_text(RECIPE.format(
        id="22222222-2222-2222-2222-222222222222",
        title="List Accounts",
        tags=":recipe:http:accounts_verb:"), encoding="utf-8")
    index = root / "generated_recipes.org"
    monkeypatch.setattr(inventory, "RECIPES_ROOT", root)
    monkeypatch.setattr(inventory, "INDEX", index)
    return root, index


class TestTheScope:

    def test_a_generated_recipe_is_indexed(self, tree):
        grouped = inventory.gather()

        assert [i["title"] for i in grouped["things"]] == [
            "How do I call the things endpoints over HTTP?"]

    def test_a_flat_recipe_is_not_indexed(self, tree):
        grouped = inventory.gather()

        assert "accounts_verb" not in grouped
        assert all(i["title"] != "List Accounts"
                   for entries in grouped.values() for i in entries)

    def test_the_index_lists_the_recipe(self, tree):
        _, index = tree
        text = inventory.render("")

        assert "[[id:11111111-1111-1111-1111-111111111111]" \
               "[How do I call the things endpoints over HTTP?]]" in text
        assert "List Accounts" not in text
        index.write_text(text, encoding="utf-8")

    def test_a_note_beside_a_link_survives_regeneration(self, tree):
        text = inventory.render("")
        noted = text.replace(
            "]]", "]] (this one is currently broken)", 1)

        assert "(this one is currently broken)" in inventory.render(noted)


class TestTheGate:

    def test_check_fails_when_the_index_is_stale(self, tree):
        _, index = tree
        index.write_text("", encoding="utf-8")

        assert inventory.main(["--check"]) == 1

    def test_check_passes_once_the_index_is_written(self, tree):
        _, index = tree
        assert inventory.main([]) == 0

        assert inventory.main(["--check"]) == 0
        assert index.exists()

    def test_a_merge_conflict_is_refused(self, tree):
        _, index = tree
        index.write_text("<<<<<<< HEAD\n", encoding="utf-8")

        with pytest.raises(SystemExit):
            inventory.main(["--check"])
