#!/usr/bin/env python3
"""SQL hygiene checks that create/drop pairing cannot see.

Two defects shipped through every existing gate and were found by a
database recreation and a seed failure instead:

* An unterminated block comment. PostgreSQL *nests* ``/* */``, so a doc
  comment opened inside another block comment does not end it -- the
  inner ``*/`` closes the inner comment and the outer one runs to the end
  of the file. With ``ON_ERROR_STOP=on`` the whole recreation fails and
  nothing after it runs.
* A duplicate index. A template that emits an index inside a per-column
  loop writes the same ``create index if not exists`` once per column.
  PostgreSQL accepts the repeats as no-ops, so the table silently loses
  the shape the generator intended and nothing reports it.

The comment scan follows PostgreSQL's lexer: dollar-quoted bodies,
``--`` line comments and both quote styles are skipped, and while a block
comment is open only ``/*`` and ``*/`` are meaningful, so an apostrophe in
the comment's prose cannot swallow its closer.

Usage: check_sql_hygiene.py <sql-dir> [<sql-dir> ...]
"""

import re
import sys
from pathlib import Path

DOLLAR = re.compile(r"\$[A-Za-z_0-9]*\$")
INDEX = re.compile(
    r"create\s+(?P<unique>unique\s+)?index\s+if\s+not\s+exists\s+"
    r"(?P<name>[a-z0-9_]+)\s*\n?\s*on\s+\"?(?P<table>[a-z0-9_]+)\"?\s*\((?P<columns>[^)]*)\)",
    re.IGNORECASE,
)


def unterminated_comment(text):
    """The block-comment depth left open at end of file, or -1 on an extra */."""
    depth = 0
    i = 0
    n = len(text)
    while i < n:
        rest = text[i:]
        if depth > 0:
            if rest.startswith("/*"):
                depth += 1
                i += 2
                continue
            if rest.startswith("*/"):
                depth -= 1
                i += 2
                continue
            i += 1
            continue
        if rest.startswith("--"):
            end = text.find("\n", i)
            i = n if end < 0 else end
            continue
        if rest.startswith("/*"):
            depth += 1
            i += 2
            continue
        tag = DOLLAR.match(rest)
        if tag:
            end = text.find(tag.group(0), i + len(tag.group(0)))
            i = n if end < 0 else end + len(tag.group(0))
            continue
        c = text[i]
        if c in ("'", '"'):
            i += 1
            while i < n:
                if text[i] == c:
                    if c == "'" and text.startswith("''", i):
                        i += 2
                        continue
                    break
                i += 1
            i += 1
            continue
        i += 1
    return depth


def duplicate_indexes(text):
    """Indexes a file defines more than once, as (name, table, columns, count)."""
    seen = {}
    for m in INDEX.finditer(text):
        key = (
            m.group("name").lower(),
            m.group("table").lower(),
            " ".join(m.group("columns").split()).lower(),
        )
        seen[key] = seen.get(key, 0) + 1
    return [(*key, count) for key, count in seen.items() if count > 1]


def main(dirs):
    files = sorted(f for d in dirs for f in Path(d).rglob("*.sql"))
    problems = []
    for path in files:
        text = path.read_text(errors="replace")
        depth = unterminated_comment(text)
        if depth > 0:
            problems.append(f"{path}: unterminated block comment, depth {depth}")
        elif depth < 0:
            problems.append(f"{path}: more */ than /*")
        for name, table, columns, count in duplicate_indexes(text):
            problems.append(
                f"{path}: index {name} on {table} ({columns}) is defined {count} times"
            )
    for problem in problems:
        print(problem)
    print(f"{len(files)} file(s) checked, {len(problems)} problem(s)")
    return 1 if problems else 0


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:] or ["projects/ores.sql"]))
