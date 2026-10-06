#!/usr/bin/env python3
"""Check that every shell recipe names the document it was tangled from.

A recipe under projects/ores.shell/scripts/library is derived: the tangle of a
document under doc/recipes/shell writes it, and its header records which
document that was. When the document is deleted or renamed and the recipe is
not, the recipe survives as an orphan. Nothing regenerates it, so it keeps the
verbs and the arguments of the day it was written, and replaying it against the
live fleet fails on a command or a signature that has since moved on.

A census on 2026-10-06 found ten orphans, all in countries and currencies. They
were the leftovers of the history dialogs that became versions.

Run::

    python3 projects/ores.codegen/scripts/check_shell_recipe_sources.py
"""
import re
import sys
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[3]
RECIPE_ROOT = REPO_ROOT / "projects" / "ores.shell" / "scripts" / "library"

PROVENANCE = re.compile(r"^# GENERATED from (\S+) — do not edit by hand\.$", re.MULTILINE)


def display(recipe: Path) -> str:
    """The recipe's path relative to the repository, when it is inside it."""
    try:
        return recipe.relative_to(REPO_ROOT).as_posix()
    except ValueError:
        return recipe.as_posix()


def orphan_reason(recipe: Path) -> str | None:
    """Why the recipe has no document behind it, or None."""
    match = PROVENANCE.search(recipe.read_text())
    if not match:
        return f"{display(recipe)}: header names no source document"
    source = REPO_ROOT / match.group(1)
    if not source.is_file():
        return f"{display(recipe)}: source {match.group(1)} does not exist"
    return None


def main() -> int:
    recipes = sorted(RECIPE_ROOT.glob("*/*.ores"))
    failures = [r for recipe in recipes if (r := orphan_reason(recipe))]

    if failures:
        print("Shell recipes with no document behind them:")
        for failure in failures:
            print(f"  {failure}")
        print()
        print("Delete the recipe, or restore the document it was tangled from.")
        return 1

    print(f"Every shell recipe names a document that exists ({len(recipes)} recipes).")
    return 0


if __name__ == "__main__":
    sys.exit(main())
