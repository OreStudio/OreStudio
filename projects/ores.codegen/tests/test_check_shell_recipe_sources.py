"""Tests that the shell recipe provenance check finds a recipe whose document
is gone, and passes when every recipe names a document that exists.

Run::

    python3 -m pytest projects/ores.codegen/tests/test_check_shell_recipe_sources.py
"""
import importlib.util
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[3]
SCRIPT = REPO_ROOT / "projects/ores.codegen/scripts/check_shell_recipe_sources.py"

spec = importlib.util.spec_from_file_location("check_recipe_sources", SCRIPT)
check = importlib.util.module_from_spec(spec)
spec.loader.exec_module(check)

RECIPE = """# Add a country
#
# GENERATED from {source} — do not edit by hand.
# Regenerate with: ./compass.sh build --direct tangle_shell_scripts
#
countries add GB GBR 826 "United Kingdom" "United Kingdom" {image}
"""


def recipe(tmp_path, name: str, source: str) -> Path:
    """A recipe written where the check expects to find one, naming a source."""
    path = tmp_path / name
    path.write_text(RECIPE.format(source=source, image="x"), encoding="utf-8")
    return path


def test_a_recipe_whose_document_is_gone_is_reported(tmp_path):
    reason = check.orphan_reason(recipe(tmp_path, "add.ores", "doc/recipes/gone.org"))
    assert reason is not None
    assert "does not exist" in reason


def test_a_recipe_whose_document_exists_passes(tmp_path):
    source = tmp_path / "country.org"
    source.write_text("# a document\n", encoding="utf-8")
    assert check.orphan_reason(recipe(tmp_path, "add.ores", str(source))) is None


def test_a_recipe_with_no_provenance_header_is_reported(tmp_path):
    path = tmp_path / "hand.ores"
    path.write_text("countries list\n", encoding="utf-8")
    assert check.orphan_reason(path) is not None


def test_the_tree_has_no_orphan_recipes():
    assert check.main() == 0
