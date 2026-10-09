#!/usr/bin/env python3
"""Static check: does every populate INSERT into a table supply every NOT NULL
column that has no DEFAULT?

This is the class of break that took out `compass db recreate`: a model gains a
`not null` column, the generated DDL gains it, and the hand-written seed still
inserts the old column list. Postgres reports only the first offending row, so
a recreate surfaces them one per multi-minute cycle.

Crude by design, but comment- and quote-aware: a ')' or ',' inside a SQL
comment or string must not truncate a column list, or the check reports the
very seeds it is meant to clear.

Usage:  python3 check_artefact_seeds.py [repo_root]
Exit 0 when clean, 1 with a report otherwise.
"""
from __future__ import annotations

import re
import sys
from pathlib import Path

ROOT = Path(sys.argv[1] if len(sys.argv) > 1 else ".").resolve()
CREATE = ROOT / "projects/ores.sql/create"
POPULATE = ROOT / "projects/ores.sql/populate"

CREATE_RE = re.compile(
    r'create\s+table\s+(?:if\s+not\s+exists\s+)?"?(?P<name>[a-z0-9_]+)"?\s*'
    r'\((?P<body>.*?)\n\)\s*;',
    re.IGNORECASE | re.DOTALL,
)
ALTER_ADD_RE = re.compile(
    r'alter\s+table\s+"?(?:only\s+)?"?(?P<name>[a-z0-9_]+)"?\s+add\s+column\s+'
    r'(?:if\s+not\s+exists\s+)?"?(?P<col>[a-z0-9_]+)"?\s+(?P<rest>[^;]*);',
    re.IGNORECASE | re.DOTALL,
)
COL_RE = re.compile(r'^\s*"?(?P<col>[a-z0-9_]+)"?\s+(?P<rest>[^,]*)$')
INSERT_HEAD_RE = re.compile(
    r'insert\s+into\s+"?(?P<table>[a-z0-9_]+)"?\s*\(', re.IGNORECASE
)

CONSTRAINT_STARTS = (
    "constraint", "primary", "unique", "foreign", "check", "exclude", "like",
)
# Bookkeeping columns a BEFORE INSERT trigger fills in. A seed may omit these
# legitimately; omitting anything else is the break this check hunts.
TRIGGER_FILLED = {
    "valid_from", "valid_to", "performed_by", "modified_by", "recorded_at",
    "change_reason_code", "change_commentary", "is_provisional",
}


def skip_comment(text: str, i: int) -> int | None:
    """If a comment starts at i, return the index just past it; else None."""
    if text.startswith("--", i):
        nl = text.find("\n", i)
        return len(text) if nl < 0 else nl + 1
    if text.startswith("/*", i):
        end = text.find("*/", i + 2)
        return len(text) if end < 0 else end + 2
    return None


def skip_quoted(text: str, i: int) -> int:
    """Return the index just past the quoted literal starting at i."""
    quote = text[i]
    i += 1
    while i < len(text):
        if text[i] == quote:
            if quote == "'" and text.startswith("''", i):
                i += 2
                continue
            return i + 1
        i += 1
    return i


def split_top_level(body: str) -> list[str]:
    """Split on commas that are not inside parentheses, quotes or comments."""
    parts, depth, cur, i = [], 0, [], 0
    while i < len(body):
        nxt = skip_comment(body, i)
        if nxt is not None:
            i = nxt
            continue
        ch = body[i]
        if ch in "'\"":
            cur.append(body[i : skip_quoted(body, i)])
            i = skip_quoted(body, i)
            continue
        if ch == "(":
            depth += 1
        elif ch == ")":
            depth -= 1
        elif ch == "," and depth == 0:
            parts.append("".join(cur))
            cur = []
            i += 1
            continue
        cur.append(ch)
        i += 1
    if cur:
        parts.append("".join(cur))
    return parts


def extract_column_list(text: str, open_paren: int) -> str | None:
    """Return the text between the paren at open_paren and its match."""
    depth, i = 0, open_paren
    while i < len(text):
        nxt = skip_comment(text, i)
        if nxt is not None:
            i = nxt
            continue
        ch = text[i]
        if ch in "'\"":
            i = skip_quoted(text, i)
            continue
        if ch == "(":
            depth += 1
        elif ch == ")":
            depth -= 1
            if depth == 0:
                return text[open_paren + 1 : i]
        i += 1
    return None


def parse_column_names(body: str) -> set[str]:
    """Column names from an insert's column list, dropping any comment text."""
    names = set()
    for part in split_top_level(body):
        stripped = re.sub(r"--[^\n]*", " ", part)
        stripped = re.sub(r"/\*.*?\*/", " ", stripped, flags=re.DOTALL).strip()
        if not stripped:
            continue
        names.add(stripped.strip('"').lower())
    return names


def is_required(rest: str) -> bool:
    flat = re.sub(r"\s+", " ", rest.lower())
    return " not null" in " " + flat and " default " not in " " + flat + " "


def parse_create_tables() -> dict[str, set[str]]:
    """table -> set of NOT NULL, no-default columns."""
    required: dict[str, set[str]] = {}
    for path in CREATE.rglob("*.sql"):
        text = path.read_text(encoding="utf-8", errors="replace")
        for m in CREATE_RE.finditer(text):
            cols = required.setdefault(m.group("name").lower(), set())
            for part in split_top_level(m.group("body")):
                cdef = COL_RE.match(part.strip())
                if not cdef:
                    continue
                col = cdef.group("col").lower()
                if col in CONSTRAINT_STARTS or col in TRIGGER_FILLED:
                    continue
                if is_required(cdef.group("rest")):
                    cols.add(col)
        for m in ALTER_ADD_RE.finditer(text):
            col = m.group("col").lower()
            if col in TRIGGER_FILLED or not is_required(m.group("rest")):
                continue
            required.setdefault(m.group("name").lower(), set()).add(col)
    return required


def parse_inserts() -> list[tuple[str, set[str], Path, int]]:
    out = []
    for path in POPULATE.rglob("*.sql"):
        text = path.read_text(encoding="utf-8", errors="replace")
        for m in INSERT_HEAD_RE.finditer(text):
            body = extract_column_list(text, m.end() - 1)
            if body is None:
                continue
            line = text[: m.start()].count("\n") + 1
            out.append(
                (m.group("table").lower(), parse_column_names(body), path, line)
            )
    return out


def main() -> int:
    required = parse_create_tables()
    problems = []
    for table, cols, path, line in parse_inserts():
        need = required.get(table)
        if not need:
            continue
        missing = need - cols
        if missing:
            problems.append((path, line, table, sorted(missing)))
    if not problems:
        print("OK: every populate INSERT supplies all required columns.")
        return 0
    print(f"{len(problems)} INSERT(s) omit a NOT NULL, no-default column:\n")
    for path, line, table, missing in problems:
        print(f"  {path.relative_to(ROOT)}:{line}")
        print(f"      table  : {table}")
        print(f"      missing: {', '.join(missing)}")
    return 1


if __name__ == "__main__":
    sys.exit(main())
