#!/usr/bin/env python3
"""
Print the ``* Artefact columns`` model section for a hand-written staging table.

An entity whose staging table is not a plain projection of its own table
declares the staging body in a ``* Artefact columns`` section. Writing
that section by transcribing a hand-written CREATE TABLE by eye is slow
and easy to get subtly wrong: a dropped ``null`` turns a nullable column
into a NOT NULL one, and the SQL still parses.

This script reads one staging table and prints the section, so the
transcription is mechanical and a reviewer can compare the two side by
side. It prints only; it never edits a model, and it never writes a file.

The output is a *transcription*, not an endorsement. Whether the
hand-written table is the contract the pipeline actually needs is a
separate question, answered by checking what the populate scripts write
and what the publish functions read. A stale hand-written table
transcribed faithfully is still stale.

Usage:
  emit_artefact_section.py projects/ores.sql/create/dq/dq_books_artefact_create.sql
  emit_artefact_section.py --table ores_dq_currencies_artefact_tbl
"""
from __future__ import annotations

import argparse
import re
import sys
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[3]
CREATE_ROOT = REPO_ROOT / "projects" / "ores.sql" / "create"

# The two columns the artefact header always supplies, so a section never
# repeats them.
_HEADER_COLUMNS = ("dataset_id", "tenant_id")

# ``"name" type [not] null [default x]``, up to the comma that ends the entry.
_COLUMN_RE = re.compile(
    r'^\s*"(?P<name>[^"]+)"\s+(?P<rest>.+?),?\s*$',
    re.MULTILINE,
)

_CREATE_RE = re.compile(
    r"create\s+table\s+(?:if\s+not\s+exists\s+)?\"(?P<table>[^\"]+)\"\s*\((?P<body>.*?)\)\s*;",
    re.IGNORECASE | re.DOTALL,
)


def strip_comments(text: str) -> str:
    text = re.sub(r"/\*.*?\*/", " ", text, flags=re.DOTALL)
    return re.sub(r"--[^\n]*", " ", text)


def split_top_level(body: str) -> list:
    parts: list = []
    depth = 0
    current: list = []
    for char in body:
        if char == "(":
            depth += 1
        elif char == ")":
            depth -= 1
        if char == "," and depth == 0:
            parts.append("".join(current))
            current = []
            continue
        current.append(char)
    parts.append("".join(current))
    return parts


def columns(body: str) -> list:
    """The (name, type, nullable, default) of one CREATE TABLE body."""
    found: list = []
    for part in split_top_level(body):
        stripped = part.strip()
        if not stripped:
            continue
        first = stripped.split(None, 1)[0].lower()
        if first in ("primary", "unique", "check", "constraint", "exclude", "foreign"):
            continue
        match = _COLUMN_RE.match(stripped)
        if not match:
            continue
        rest = " ".join(match.group("rest").split())
        nullable = not re.search(r"\bnot\s+null\b", rest, re.IGNORECASE)
        default_match = re.search(r"\bdefault\s+(.+?)(?:\s+not\s+null)?$", rest, re.IGNORECASE)
        col_type = re.split(r"\s+(?:not\s+null|null|default)\b", rest, maxsplit=1, flags=re.IGNORECASE)[0]
        found.append(
            {
                "name": match.group("name"),
                "type": col_type.strip(),
                "nullable": nullable,
                "default": default_match.group(1).strip() if default_match else None,
            }
        )
    return found


def find_table(text: str, table: str | None) -> tuple:
    bare = strip_comments(text)
    for match in _CREATE_RE.finditer(bare):
        if table is None or match.group("table") == table:
            return match.group("table"), match.group("body")
    raise SystemExit(f"no CREATE TABLE{' for ' + table if table else ''} found")


def section(table: str, body: str) -> str:
    lines = ["* Artefact columns", ""]
    for col in columns(body):
        if col["name"] in _HEADER_COLUMNS:
            continue
        lines.append(f"** {col['name']}")
        lines.append(":PROPERTIES:")
        lines.append(f":type: {col['type']}")
        if col["nullable"]:
            lines.append(":nullable: true")
        if col["default"] is not None and col["default"] != "":
            lines.append(f":default: {col['default']}")
        lines.append(":END:")
        lines.append("")
    return "\n".join(lines) + "\n"


def main() -> int:
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument("path", nargs="?", help="a staging-table .sql file")
    ap.add_argument("--table", default=None, help="table name, searched across every product")
    args = ap.parse_args()

    if args.table:
        for candidate in sorted(CREATE_ROOT.rglob("*.sql")):
            text = candidate.read_text(encoding="utf-8")
            if f'"{args.table}"' not in text:
                continue
            try:
                name, body = find_table(text, args.table)
            except SystemExit:
                continue
            print(f"-- {candidate.relative_to(REPO_ROOT)}", file=sys.stderr)
            print(section(name, body))
            return 0
        print(f"table not found: {args.table}", file=sys.stderr)
        return 1

    if not args.path:
        ap.error("give a path or --table")
    path = Path(args.path)
    name, body = find_table(path.read_text(encoding="utf-8"), None)
    print(f"-- {name}", file=sys.stderr)
    print(section(name, body))
    return 0


if __name__ == "__main__":
    sys.exit(main())
