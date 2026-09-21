#!/usr/bin/env python3
"""
Regenerate the shell recipe inventory, doc/recipes/shell/shell.org.

Every recipe under doc/recipes/shell/ is a document whose frontmatter carries a
title, an id and the category in its filetags. The inventory is nothing but
those documents grouped by category and linked by id, so it is derived data and
is generated rather than edited: a recipe added to a category and not linked
here is invisible to a reader browsing the inventory, and nothing else catches
that. The generated shell recipes made the gap unmissable -- twenty documents
and a hundred and twenty-four scripts with no entry in the index at all.

The narrative is the document's, not the script's. A category's prose explains
what the category is for, so the head of the file and the paragraph under each
heading are read back from the current inventory and carried forward
unchanged; only the link list under each heading is rewritten. A category with
no section yet is appended, and a section whose recipes have all gone is
dropped. Links are sorted by title, so a list does not churn when a file moves.

Modes:
  (default)   Rewrite doc/recipes/shell/shell.org in place.
  --check     Exit non-zero if the inventory is stale (CI gate).
"""
from __future__ import annotations

import argparse
import re
import sys
from collections import defaultdict
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[3]
RECIPES_ROOT = REPO_ROOT / "doc/recipes/shell"
INDEX = RECIPES_ROOT / "shell.org"

CATEGORY_RE = re.compile(r":recipe:shell:([^:]+):")
# A link line, plus whatever a writer appended to it. The note is kept: "this
# one is currently broken" is not something the generator knows, and dropping
# it would lose the only warning a reader gets.
LINK_RE = re.compile(r"^- \[\[id:([^]]+)\]\[(.+?)\]\]\s*(.*)$")
HEADING_RE = re.compile(r"^\* (.+?)\s*$")
KEYWORD_RE = re.compile(r"^#\+(\w[\w-]*):\s*(.*)$")

# Categories whose heading is not the title-cased key. Everything else reads
# acceptably from the key alone, so this stays a list of the exceptions rather
# than a table every category has to be added to.
HEADING_EXCEPTIONS = {
    "marketdata": "Market data",
    "ore": "ORE documents",
    "entity": "Entity commands",
    "index": "Inventory",
}


def heading_for(category: str) -> str:
    """The section heading a category is listed under."""
    if category in HEADING_EXCEPTIONS:
        return HEADING_EXCEPTIONS[category]
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
            info.setdefault(keyword.group(1).lower(), keyword.group(2).strip())
        if stripped.startswith("* "):
            break
    return info


def category_of(path: Path, info: dict[str, str]) -> str | None:
    """The category a recipe belongs to: its filetag, else its directory."""
    found = CATEGORY_RE.search(info.get("filetags", ""))
    if found:
        return found.group(1)
    parent = path.parent.name
    return parent if parent != RECIPES_ROOT.name else None


def gather() -> dict[str, list[dict[str, str]]]:
    """Every recipe, grouped by category. The inventory itself is not a recipe."""
    by_category: dict[str, list[dict[str, str]]] = defaultdict(list)
    for path in sorted(RECIPES_ROOT.rglob("*.org")):
        if path == INDEX:
            continue
        info = read_recipe(path)
        if not info.get("id") or not info.get("title"):
            continue
        category = category_of(path, info)
        if not category or category == "index":
            continue
        by_category[category].append(info)
    for recipes in by_category.values():
        recipes.sort(key=lambda r: r["title"].lower())
    return by_category


def split_index(text: str) -> tuple[list[str], list[dict]]:
    """The head of the file, and its sections with their prose and links.

    A section is a heading, the lines under it that are not links, and the
    links. The prose is kept as lines rather than reflowed so a hand-written
    paragraph survives regeneration byte for byte, and a link's trailing note
    is kept beside its id so an annotation survives too.
    """
    head: list[str] = []
    sections: list[dict] = []
    current: dict | None = None
    for line in text.splitlines():
        heading = HEADING_RE.match(line)
        link = LINK_RE.match(line)
        if heading:
            current = {"title": heading.group(1), "prose": [], "notes": {}}
            sections.append(current)
        elif current is None:
            head.append(line)
        elif link:
            current["notes"][link.group(1)] = link.group(3).strip()
        else:
            current["prose"].append(line)
    return head, sections


def render_links(recipes: list[dict[str, str]],
                 notes: dict[str, str]) -> str:
    """One bullet per recipe, sorted, with any hand-written note restored."""
    lines = []
    for recipe in recipes:
        line = f"- [[id:{recipe['id']}][{recipe['title']}]]"
        note = notes.get(recipe["id"], "")
        lines.append(f"{line} {note}" if note else line)
    return "\n".join(lines)


def read_index() -> str:
    """The inventory as it stands.

    A missing index is a misconfiguration rather than a crash: the file is
    derived from the recipes beside it, so the useful thing to say is where
    it was looked for.
    """
    if not INDEX.exists():
        raise SystemExit(
            f"no inventory at {INDEX}\n"
            f"  the inventory is derived from the recipes under {RECIPES_ROOT}, "
            "so both have to exist")
    return INDEX.read_text(encoding="utf-8")


def build_index(by_category: dict[str, list[dict[str, str]]] | None = None) -> str:
    """The whole inventory: the file's own head, then a section per category."""
    by_category = gather() if by_category is None else by_category
    head, existing = split_index(read_index())

    # A section is matched to a category by its heading, which heading_for()
    # derives from the key. A heading no category claims is dropped, so a
    # renamed category does not leave its old section behind.
    by_heading = {heading_for(category): category for category in by_category}

    # Blocks joined by a blank line rather than lines appended one at a time:
    # the head ends with a blank line of its own, and appending to it let that
    # blank accumulate into the head on every run.
    # A note is keyed by the recipe it annotates, not by the section it sits
    # in, so moving a recipe between categories does not lose its warning.
    notes: dict[str, str] = {}
    for section in existing:
        notes.update(section["notes"])
    blocks = ["\n".join(head).rstrip("\n")]
    listed: set[str] = set()
    for section in existing:
        category = by_heading.get(section["title"])
        if category is None:
            continue
        listed.add(category)
        prose = "\n".join(section["prose"]).strip("\n")
        block = [f"* {section['title']}", ""]
        if prose:
            block += [prose, ""]
        links = render_links(by_category[category], notes)
        if links:
            block.append(links)
        blocks.append("\n".join(block))
    for category in sorted(set(by_category) - listed):
        blocks.append(f"* {heading_for(category)}\n\n"
                      f"{render_links(by_category[category], notes)}")
    return "\n\n".join(blocks).rstrip("\n") + "\n"


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--check", action="store_true",
                        help="Exit non-zero if the inventory is stale.")
    args = parser.parse_args()

    by_category = gather()
    desired = build_index(by_category)
    current = read_index()
    count = sum(len(recipes) for recipes in by_category.values())
    if args.check:
        if current != desired:
            print("stale shell recipe inventory:", file=sys.stderr)
            print(f"  {INDEX.relative_to(REPO_ROOT)}", file=sys.stderr)
            print("\nrun: python3 "
                  "projects/ores.codegen/scripts/regenerate_shell_recipe_inventory.py",
                  file=sys.stderr)
            return 1
        return 0
    if current == desired:
        print(f"{INDEX.relative_to(REPO_ROOT)} is up to date "
              f"({count} recipes)")
        return 0
    INDEX.write_text(desired, encoding="utf-8", newline="\n")
    print(f"wrote {INDEX.relative_to(REPO_ROOT)} "
          f"({count} recipes in {len(by_category)} categories)")
    return 0


if __name__ == "__main__":
    sys.exit(main())
