#!/usr/bin/env python3
"""Regenerate the generated-recipe inventory, doc/recipes/http/generated_recipes.org.

The recipes the generator writes are the single source of truth; this inventory
is nothing but a browsable index of them, grouped by the category in each
recipe's filetags. A recipe that is never listed is invisible to a reader
browsing the index, and nothing else catches that, so this runs as a check in
CI as well as a generator.

It indexes the generated corpus only. Those recipes live one directory deep,
under doc/recipes/http/<group>/, while the hand-written recipes that predate
the facet sit flat in doc/recipes/http/ and are catalogued by hand in
doc/recipes/http/http.org. Regenerating that curated index from filetags would
replace its headings -- "RBAC (Role-Based Access Control)" becomes "Rbac verb"
-- so this leaves it alone and keeps its own document.

The paragraph under each heading, and any note beside a link, are hand-written.
Both are read back from the current inventory and carried forward, so
regenerating never costs a writer their words.

Usage:
    regenerate_http_recipe_inventory.py            # write the inventory
    regenerate_http_recipe_inventory.py --check    # exit non-zero if stale
"""

from __future__ import annotations

import argparse
import re
import sys
from collections import defaultdict
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[3]
RECIPES_ROOT = REPO_ROOT / "doc/recipes/http"
INDEX = RECIPES_ROOT / "generated_recipes.org"

CATEGORY_RE = re.compile(r":recipe:http:([^:]+):")
# A link line, plus whatever a writer appended to it. The note is kept: "this
# one is currently broken" is not something the generator knows, and dropping
# it would lose the only warning a reader gets.
LINK_RE = re.compile(r"^- \[\[id:([^]]+)\]\[(.+?)\]\]\s*(.*)$")
HEADING_RE = re.compile(r"^\* (.+?)\s*$")
KEYWORD_RE = re.compile(r"^#\+(\w[\w-]*):\s*(.*)$")
# What git leaves in a file it could not merge. A bare separator counts too:
# a botched resolution strands those outside their pair.
CONFLICT_MARKER_RE = re.compile(r"^(?:<{7}|>{7})(?: |$)|^={7}$")

FRONTMATTER = """\
:PROPERTIES:
:ID: 3EB4B454-B837-41FA-8A8D-2D9C49DE967A
:END:
#+title: Generated HTTP recipes
#+description: Inventory of the generated HTTP recipes grouped by category.
#+type: knowledge
#+level: cross
#+filetags: :recipe:http:index:knowledge:
#+created: 2026-09-27
#+updated: 2026-09-27

The request recipes the =ores.doc.http-recipe= facet generates, one per
resource, listed by category. The link lists are generated from the recipes
themselves by
=projects/ores.codegen/scripts/regenerate_http_recipe_inventory.py=, so a new
recipe appears here as soon as that runs; the paragraph under each heading, and
any note beside a link, are hand-written and are kept.

The hand-written recipes that predate the facet are catalogued separately in
=doc/recipes/http/http.org=.
"""


def heading_for(category: str) -> str:
    """The section heading a category is listed under."""
    return " ".join(word.capitalize() for word in category.split("_"))


def read_recipe(path: Path) -> dict[str, str]:
    """A recipe's id, title, description and filetags, from its frontmatter."""
    info: dict[str, str] = {}
    for line in path.read_text(encoding="utf-8").splitlines():
        stripped = line.strip()
        if not info.get("id") and stripped.startswith(":ID:"):
            info["id"] = stripped.split(":ID:", 1)[1].strip()
        keyword = KEYWORD_RE.match(stripped)
        if keyword and keyword.group(1).lower() in ("title", "description",
                                                    "filetags"):
            info[keyword.group(1).lower()] = keyword.group(2).strip()
    return info


def category_of(path: Path, info: dict[str, str]) -> str | None:
    """The category a recipe belongs to, or None when the file is not one.

    The inventory is itself a file in this tree and carries no recipe category,
    so it is not listed. The hand-written recipes that predate the generator do
    carry one, so they are listed beside the generated ones: the index is an
    index of the recipes that exist, not only of the ones the generator wrote.
    """
    if path == INDEX:
        return None
    match = CATEGORY_RE.search(info.get("filetags", ""))
    return match.group(1) if match else None


def gather() -> dict[str, list[dict[str, str]]]:
    """Every generated recipe, grouped by category.

    One directory deep is what distinguishes the generated corpus from the
    hand-written recipes, which sit flat in the root: the facet writes each
    resource into its own group directory.
    """
    grouped: dict[str, list[dict[str, str]]] = defaultdict(list)
    for path in sorted(RECIPES_ROOT.glob("*/*.org")):
        info = read_recipe(path)
        category = category_of(path, info)
        if category and info.get("id"):
            grouped[category].append(info)
    return grouped


def hand_written(index_text: str) -> tuple[dict[str, str], dict[str, str]]:
    """The paragraphs and the link notes the current inventory carries.

    Both are keyed so they can be put back: a paragraph by its heading, a note
    by the id of the entry it was written beside.
    """
    paragraphs: dict[str, str] = {}
    notes: dict[str, str] = {}
    current: str | None = None
    body: list[str] = []

    def flush() -> None:
        if current is not None:
            text = "\n".join(body).strip()
            if text:
                paragraphs[current] = text

    for line in index_text.splitlines():
        heading = HEADING_RE.match(line)
        if heading:
            flush()
            current = heading.group(1)
            body = []
            continue
        note = LINK_RE.match(line)
        if note:
            notes[note.group(1)] = note.group(3).strip()
            continue
        if current is not None:
            body.append(line)
    flush()
    return paragraphs, notes


def render(index_text: str) -> str:
    """The whole inventory, generated from the recipes and the current file."""
    paragraphs, notes = hand_written(index_text)
    grouped = gather()
    lines = [FRONTMATTER.rstrip("\n"), ""]
    for category in sorted(grouped):
        lines.append(f"* {heading_for(category)}")
        lines.append("")
        paragraph = paragraphs.get(heading_for(category), "")
        if paragraph:
            lines.append(paragraph)
            lines.append("")
        for info in grouped[category]:
            note = notes.get(info["id"], "")
            entry = f"- [[id:{info['id']}][{info.get('title', '')}]]"
            lines.append(f"{entry} {note}".rstrip())
        lines.append("")
    return "\n".join(lines).rstrip("\n") + "\n"


def reject_conflict_markers(text: str) -> None:
    """Refuse an inventory git left a merge conflict in."""
    for line in text.splitlines():
        if CONFLICT_MARKER_RE.match(line):
            raise SystemExit(
                "doc/recipes/http/http.org carries a merge conflict; resolve "
                "it, then regenerate")


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--check", action="store_true",
                        help="Exit non-zero if the inventory is stale (CI gate).")
    args = parser.parse_args(argv)

    current = INDEX.read_text(encoding="utf-8") if INDEX.exists() else ""
    reject_conflict_markers(current)
    wanted = render(current)

    if args.check:
        if current != wanted:
            print(f"{INDEX.relative_to(REPO_ROOT)} is stale; regenerate it with "
                  f"{Path(__file__).name}", file=sys.stderr)
            return 1
        print(f"{INDEX.relative_to(REPO_ROOT)} is current.")
        return 0

    INDEX.write_text(wanted, encoding="utf-8")
    grouped = gather()
    print(f"wrote {INDEX.relative_to(REPO_ROOT)}: "
          f"{sum(len(v) for v in grouped.values())} recipes "
          f"in {len(grouped)} categories.")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
