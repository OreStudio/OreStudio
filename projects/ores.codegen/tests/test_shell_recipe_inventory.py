"""Tests for regenerate_shell_recipe_inventory.py.

Run::

    python3 -m pytest projects/ores.codegen/tests/test_shell_recipe_inventory.py

The inventory is derived data, so the properties that matter are the ones a
plain regeneration would quietly destroy: the head of the document and the
paragraph under each heading are a writer's, not the generator's, and a note
beside a link is something only a human knows. A second run must also change
nothing, or the CI gate built on it would fail on every commit.
"""
import sys
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[3]
sys.path.insert(0, str(REPO_ROOT / "projects/ores.codegen/scripts"))

import regenerate_shell_recipe_inventory as rsri  # noqa: E402

RECIPE = """\
:PROPERTIES:
:ID: {id}
:END:
#+title: {title}
#+description: {title}
#+type: recipe
#+filetags: :recipe:shell:{category}:

{title}

#+begin_src ores-shell :exports none
{command}
#+end_src
"""

HEAD = """\
:PROPERTIES:
:ID: 11111111-1111-1111-1111-111111111111
:END:
#+title: Shell recipes
#+type: knowledge

An introduction a writer wrote.
"""


def _tree(tmp_path, recipes, index):
    """A throw-away recipes root with an index, wired into the module."""
    root = tmp_path / "shell"
    root.mkdir()
    for category, name, recipe_id, title in recipes:
        directory = root / category
        directory.mkdir(exist_ok=True)
        (directory / f"{name}.org").write_text(
            RECIPE.format(id=recipe_id, title=title, category=category,
                          command=f"{category} {name}"))
    index_path = root / "shell.org"
    index_path.write_text(index)
    rsri.RECIPES_ROOT = root
    rsri.INDEX = index_path
    return index_path


INDEX = HEAD + """
* Accounts

Who manages accounts.

- [[id:AAAA][Zeta]]
- [[id:BBBB][Alpha]] (known to fail)
"""

RECIPES = [
    ("accounts", "alpha", "BBBB", "Alpha"),
    ("accounts", "zeta", "AAAA", "Zeta"),
    ("accounts", "new", "CCCC", "New One"),
    ("tenants", "one", "DDDD", "Tenant One"),
]


class TestTheInventory:
    def test_every_recipe_on_disk_is_listed(self, tmp_path):
        _tree(tmp_path, RECIPES, INDEX)
        text = rsri.build_index()
        for recipe_id in ("AAAA", "BBBB", "CCCC", "DDDD"):
            assert f"[[id:{recipe_id}]" in text

    def test_links_are_sorted_so_they_do_not_churn(self, tmp_path):
        _tree(tmp_path, RECIPES, INDEX)
        text = rsri.build_index()
        accounts = text.split("* Accounts")[1].split("* Tenants")[0]
        titles = [line.split("][", 1)[1].split("]")[0]
                  for line in accounts.splitlines() if line.startswith("- ")]
        assert titles == ["Alpha", "New One", "Zeta"]

    def test_the_head_and_the_prose_are_kept(self, tmp_path):
        _tree(tmp_path, RECIPES, INDEX)
        text = rsri.build_index()
        assert "An introduction a writer wrote." in text
        assert "Who manages accounts." in text

    def test_a_note_beside_a_link_is_kept(self, tmp_path):
        # Only a human knows that a recipe is known to be broken.
        _tree(tmp_path, RECIPES, INDEX)
        assert "(known to fail)" in rsri.build_index()

    def test_a_category_with_no_section_is_appended(self, tmp_path):
        _tree(tmp_path, RECIPES, INDEX)
        assert "\n* Tenants" in rsri.build_index()

    def test_a_second_run_changes_nothing(self, tmp_path):
        # The CI gate is `--check`, so a run that differs from its own
        # output would fail on every commit after the first.
        index_path = _tree(tmp_path, RECIPES, INDEX)
        first = rsri.build_index()
        index_path.write_text(first)
        assert rsri.build_index() == first

    def test_a_removed_recipe_leaves_the_inventory(self, tmp_path):
        index_path = _tree(tmp_path, RECIPES, INDEX)
        index_path.write_text(rsri.build_index())
        (rsri.RECIPES_ROOT / "accounts" / "new.org").unlink()
        assert "New One" not in rsri.build_index()

    def test_the_inventory_does_not_list_itself(self, tmp_path):
        _tree(tmp_path, RECIPES, INDEX)
        text = rsri.build_index()
        assert "11111111-1111-1111-1111-111111111111" in text.split("* ")[0]
        assert text.count("11111111-1111-1111-1111-111111111111") == 1


class TestTheHeadings:
    def test_a_key_titles_as_words(self):
        assert rsri.heading_for("account_types") == "Account Types"

    def test_the_irregular_ones_are_named(self):
        # These read badly title-cased, and a heading a reader sees is worth
        # an exception list of two.
        assert rsri.heading_for("marketdata") == "Market data"
        assert rsri.heading_for("ore") == "ORE documents"
