#!/usr/bin/env python3
"""
Regenerate the facet inventories of the template-library group docs.

Walks projects/ores.codegen/library/templates/*.org, finds every doc
with #+type: facet, and rewrites the * Facets table of the group doc
(<group>_group.org) named by each facet's #+facet_group: keyword. Each
facet's row carries Facet (id-linked title), the number of templates it
tangles (its ":tangle" mustache blocks), and its description.

Modes:
  (default)   Rewrite the group docs in place.
  --check     Exit non-zero if any inventory is stale (CI gate).
"""

from __future__ import annotations

import argparse
import re
import sys
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[3]
TEMPLATES_DIR = REPO_ROOT / "projects/ores.codegen/library/templates"

KEYWORD_RE = re.compile(r"^#\+(\w+):\s*(.*)$")
ID_RE = re.compile(r"^:ID:\s*(\S+)", re.MULTILINE)
TANGLE_RE = re.compile(r"^#\+begin_src\s+mustache\s.*:tangle\s+\S+",
                       re.MULTILINE | re.IGNORECASE)


def parse_doc(path: Path) -> dict:
    """Extract id, keywords, and tangle-block count from an org doc."""
    text = path.read_text(encoding="utf-8")
    doc = {"path": path, "text": text, "keywords": {}}
    m = ID_RE.search(text)
    doc["id"] = m.group(1) if m else ""
    for line in text.splitlines():
        km = KEYWORD_RE.match(line)
        if km:
            doc["keywords"][km.group(1).lower()] = km.group(2).strip()
        elif line.startswith("* "):
            break  # frontmatter ends at the first heading
    doc["templates"] = len(TANGLE_RE.findall(text))
    return doc


def build_table(facets: list[dict]) -> str:
    """Render the * Facets org table for a group's facet docs."""
    lines = ["| Facet | Templates | Description |",
             "|-------+-----------+-------------|"]
    for f in sorted(facets, key=lambda d: d["keywords"].get("title", "")):
        title = f["keywords"].get("title", f["path"].stem)
        desc = f["keywords"].get("description", "")
        lines.append(f"| [[id:{f['id']}][{title}]] | {f['templates']} | {desc} |")
    return "\n".join(lines)


def replace_facets_table(text: str, table: str, path: Path) -> str:
    """Replace the org table inside the * Facets section."""
    section = re.search(
        r"(^\* Facets\n)(.*?)(?=^\* |\Z)", text, re.MULTILINE | re.DOTALL)
    if not section:
        sys.exit(f"error: {path} has no * Facets section.")
    body = section.group(2)
    table_re = re.compile(r"^\|.*(?:\n\|.*)*", re.MULTILINE)
    if table_re.search(body):
        new_body = table_re.sub(lambda _: table, body, count=1)
    else:
        new_body = body.rstrip("\n") + "\n\n" + table + "\n"
    start, end = section.start(2), section.end(2)
    return text[:start] + new_body + text[end:]


def main(argv=None) -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--check", action="store_true",
                        help="Exit non-zero if any inventory is stale.")
    args = parser.parse_args(argv)

    docs = [parse_doc(p) for p in sorted(TEMPLATES_DIR.glob("*.org"))]
    facets = [d for d in docs if d["keywords"].get("type") == "facet"]
    groups = {d["path"].stem[:-len("_group")]: d for d in docs
              if d["keywords"].get("type") == "facet_group"}

    by_group: dict[str, list[dict]] = {g: [] for g in groups}
    for f in facets:
        group = f["keywords"].get("facet_group", "")
        if group not in groups:
            print(f"warning: {f['path'].name} names unknown facet group "
                  f"{group!r}; skipped.", file=sys.stderr)
            continue
        by_group[group].append(f)

    stale = []
    for group, members in by_group.items():
        doc = groups[group]
        new_text = replace_facets_table(
            doc["text"], build_table(members), doc["path"])
        if new_text != doc["text"]:
            stale.append(doc["path"])
            if not args.check:
                doc["path"].write_text(new_text, encoding="utf-8")
                print(f"wrote {doc['path'].relative_to(REPO_ROOT)} "
                      f"({len(members)} facet(s))")

    if args.check and stale:
        for path in stale:
            print(f"stale: {path.relative_to(REPO_ROOT)}", file=sys.stderr)
        return 1
    if not stale:
        print(f"{len(groups)} group doc(s) up to date.")
    return 0


if __name__ == "__main__":
    sys.exit(main())
