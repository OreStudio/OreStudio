#!/usr/bin/env python3
"""
Regenerate the child-inventory tables of the physical-space doc graph.

The graph is one org document per node, named for its MASD address:

    ores            (root)              -> * Technical spaces
      ores.cpp      (technical_space)   -> * Facets
        ores.cpp.qt (facet)            -> * Archetypes
          ores.cpp.qt.controller_header (archetype, leaf)

Each parent's child table is GENERATED from the children's frontmatter —
never hand-edited. Parent links are the frontmatter keywords set at scaffold
time: a facet's #+facet_group names its technical space; an archetype's
#+facet names its facet; technical spaces are discovered by type. The node's
own #+title IS its address, which is how children resolve their parent.

Modes:
  (default)   Rewrite the inventory tables in place.
  --check     Exit non-zero if any inventory is stale (CI gate).
"""
from __future__ import annotations

import argparse
import re
import sys
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[3]
TEMPLATES_DIR = REPO_ROOT / "projects/ores.codegen/library/templates"

KEYWORD_RE = re.compile(r"^#\+(\w[\w-]*):\s*(.*)$")
ID_RE = re.compile(r"^:ID:\s*(\S+)", re.MULTILINE)


def parse_doc(path: Path) -> dict:
    text = path.read_text(encoding="utf-8")
    doc = {"path": path, "text": text, "keywords": {}}
    m = ID_RE.search(text)
    doc["id"] = m.group(1) if m else ""
    for line in text.splitlines():
        km = KEYWORD_RE.match(line)
        if km:
            doc["keywords"][km.group(1).lower()] = km.group(2).strip()
        elif line.startswith("* "):
            break
    return doc


def title(d: dict) -> str:
    return d["keywords"].get("title", d["path"].stem)


def build_table(headers: list[str], rows: list[list[str]]) -> str:
    sep = "|" + "+".join("-" * (len(h) + 2) for h in headers) + "|"
    out = ["| " + " | ".join(headers) + " |", sep]
    for r in rows:
        out.append("| " + " | ".join(r) + " |")
    return "\n".join(out)


def replace_section_table(text: str, heading: str, table: str, path: Path) -> str:
    """Replace (or create) the org table inside `* <heading>`."""
    sec = re.search(rf"(^\* {re.escape(heading)}\n)(.*?)(?=^\* |\Z)",
                    text, re.MULTILINE | re.DOTALL)
    if not sec:
        # Create the section just before * See also (or at end).
        block = f"* {heading}\n\n{table}\n\n"
        m = re.search(r"^\* See also", text, re.MULTILINE)
        if m:
            return text[:m.start()] + block + text[m.start():]
        return text.rstrip("\n") + "\n\n" + block
    body = sec.group(2)
    table_re = re.compile(r"^\|.*(?:\n\|.*)*", re.MULTILINE)
    if table_re.search(body):
        new_body = table_re.sub(lambda _: table, body, count=1)
    else:
        new_body = body.rstrip("\n") + "\n\n" + table + "\n"
    return text[:sec.start(2)] + new_body + text[sec.end(2):]


def link(d: dict) -> str:
    return f"[[id:{d['id']}][{title(d)}]]"


def main(argv=None) -> int:
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument("--check", action="store_true",
                    help="Exit non-zero if any inventory is stale.")
    args = ap.parse_args(argv)

    docs = [parse_doc(p) for p in sorted(TEMPLATES_DIR.glob("*.org"))]
    by_type: dict[str, list[dict]] = {}
    for d in docs:
        by_type.setdefault(d["keywords"].get("type", ""), []).append(d)

    tspaces = by_type.get("technical_space", [])
    facets = by_type.get("facet", [])
    archetypes = by_type.get("archetype", [])

    # Only operate on the addressed (ores.*) graph; ignore legacy literate docs.
    root = next((d for d in tspaces if title(d) == "ores"), None)
    real_ts = [d for d in tspaces if title(d).startswith("ores.")]
    desc = lambda d: d["keywords"].get("description", "")

    # children groupings
    facets_by_ts: dict[str, list[dict]] = {}
    for f in facets:
        if title(f).startswith("ores."):
            facets_by_ts.setdefault(f["keywords"].get("facet_group", ""), []).append(f)
    arch_by_facet: dict[str, list[dict]] = {}
    for a in archetypes:
        arch_by_facet.setdefault(a["keywords"].get("facet", ""), []).append(a)

    edits: list[tuple[dict, str, str]] = []  # (doc, heading, table)

    if root:
        rows = [[link(d), str(len(facets_by_ts.get(title(d), []))), desc(d)]
                for d in sorted(real_ts, key=title)]
        edits.append((root, "Technical spaces",
                      build_table(["Technical space", "Facets", "Description"], rows)))

    for ts in real_ts:
        members = sorted(facets_by_ts.get(title(ts), []), key=title)
        rows = [[link(f), str(len(arch_by_facet.get(title(f), []))), desc(f)]
                for f in members]
        edits.append((ts, "Facets",
                      build_table(["Facet", "Archetypes", "Description"], rows)))

    for f in facets:
        if not title(f).startswith("ores."):
            continue
        members = sorted(arch_by_facet.get(title(f), []), key=title)
        rows = [[link(a), desc(a)] for a in members]
        edits.append((f, "Archetypes",
                      build_table(["Archetype", "Description"], rows)))

    stale = []
    for doc, heading, table in edits:
        new_text = replace_section_table(doc["text"], heading, table, doc["path"])
        if new_text != doc["text"]:
            stale.append(doc["path"])
            doc["text"] = new_text
            if not args.check:
                doc["path"].write_text(new_text, encoding="utf-8")
                print(f"wrote {doc['path'].relative_to(REPO_ROOT)} ({heading})")

    if args.check and stale:
        for p in stale:
            print(f"stale: {p.relative_to(REPO_ROOT)}", file=sys.stderr)
        return 1
    if not stale:
        print(f"{len(edits)} inventory table(s) up to date.")
    else:
        print(f"updated {len(stale)} doc(s).")
    return 0


if __name__ == "__main__":
    sys.exit(main())
