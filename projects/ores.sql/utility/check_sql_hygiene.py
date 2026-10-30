#!/usr/bin/env python3
"""SQL hygiene checks that create/drop pairing cannot see.

Three defects shipped through every existing gate and were found by a
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

* A comment between two string literals. PostgreSQL concatenates two
  constants separated only by whitespace, and a comment is not
  whitespace, so the comment ends the concatenation. A block comment
  written inside a long ``format()`` string turns the rest of the string
  into an argument or a syntax error, and the recreation stops. This one
  also shipped through every gate.

A fourth check covers DDL that cannot run twice. A ``create trigger`` that
does not say ``or replace``, a ``create policy`` with no preceding ``drop
policy if exists``, or a ``create table`` with no ``if not exists`` is fine
on a fresh database and stops the second ``setup_schema.sql`` run. The
database lifecycle only ever ran the setup once, so nothing reported it
until a re-run was attempted.

The comment scan follows PostgreSQL's lexer: dollar-quoted bodies,
``--`` line comments and both quote styles are skipped, and while a block
comment is open only ``/*`` and ``*/`` are meaningful, so an apostrophe in
the comment's prose cannot swallow its closer. The idempotency check
searches the same masked text, so a statement inside a comment or a
function body is not mistaken for one the server runs.

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


def _after_string(text, i):
    """The offset just past the string literal that starts at ``i``."""
    n = len(text)
    j = i + 1
    while j < n:
        if text[j] == "'":
            if text.startswith("''", j):
                j += 2
                continue
            return j + 1
        j += 1
    return n


def _after_block_comment(text, i):
    """The offset just past the block comment that starts at ``i``."""
    n = len(text)
    depth = 1
    j = i + 2
    while j < n:
        if text.startswith("/*", j):
            depth += 1
            j += 2
            continue
        if text.startswith("*/", j):
            depth -= 1
            j += 2
            if depth == 0:
                return j
            continue
        j += 1
    return n


def split_concatenation(text):
    """Comments that stand between two string literals, as offsets.

    PostgreSQL joins two constants separated only by whitespace, so the
    literals either side of the comment were meant to be one string. The
    comment ends the join, and the server reads the rest as an argument or
    reports a syntax error. A dollar-quoted body is scanned as well, because
    a function's body is SQL the server runs; the offsets it yields are
    shifted to name the file's own lines.
    """
    problems = []
    i, n = 0, len(text)
    while i < n:
        if text.startswith("--", i):
            end = text.find("\n", i)
            i = n if end < 0 else end
            continue
        if text.startswith("/*", i):
            i = _after_block_comment(text, i)
            continue
        tag = DOLLAR.match(text, i)
        if tag:
            body = i + len(tag.group(0))
            end = text.find(tag.group(0), body)
            body_end = n if end < 0 else end
            problems.extend(body + off for off in split_concatenation(text[body:body_end]))
            i = n if end < 0 else end + len(tag.group(0))
            continue
        if text[i] == '"':
            end = text.find('"', i + 1)
            i = n if end < 0 else end + 1
            continue
        if text[i] != "'":
            i += 1
            continue

        i = _after_string(text, i)
        j, comment_at = i, -1
        while j < n:
            if text[j] in " \t\r\n":
                j += 1
                continue
            if text.startswith("--", j):
                comment_at = j if comment_at < 0 else comment_at
                end = text.find("\n", j)
                j = n if end < 0 else end + 1
                continue
            if text.startswith("/*", j):
                comment_at = j if comment_at < 0 else comment_at
                j = _after_block_comment(text, j)
                continue
            break
        if comment_at >= 0 and j < n and text[j] == "'":
            problems.append(comment_at)
    return problems


def mask_non_code(text):
    """The text with comments, string literals and dollar bodies blanked out.

    Offsets are preserved, so a match position still names the right line. The
    DDL below is searched on the masked text only: a ``create policy`` inside a
    comment or a function body is not a statement the server runs at the point
    the mask hides it.
    """
    out = list(text)
    i, n = 0, len(text)
    depth = 0
    while i < n:
        rest = text[i:]
        if depth > 0:
            if rest.startswith("/*"):
                out[i] = out[i + 1] = " "
                depth += 1
                i += 2
                continue
            if rest.startswith("*/"):
                out[i] = out[i + 1] = " "
                depth -= 1
                i += 2
                continue
            if out[i] != "\n":
                out[i] = " "
            i += 1
            continue
        if rest.startswith("--"):
            end = text.find("\n", i)
            end = n if end < 0 else end
            for j in range(i, end):
                if out[j] != "\n":
                    out[j] = " "
            i = end
            continue
        if rest.startswith("/*"):
            out[i] = out[i + 1] = " "
            depth += 1
            i += 2
            continue
        tag = DOLLAR.match(rest)
        if tag:
            end = text.find(tag.group(0), i + len(tag.group(0)))
            end = n if end < 0 else end + len(tag.group(0))
            for j in range(i, end):
                if out[j] != "\n":
                    out[j] = " "
            i = end
            continue
        c = text[i]
        if c == '"':
            # A double quote delimits an identifier, not a string, so the name
            # it holds has to survive the mask: the checks below match on it.
            # The region is skipped rather than blanked, which also keeps an
            # apostrophe inside an identifier from opening a string.
            j = i + 1
            while j < n:
                if text[j] == '"':
                    if text.startswith('""', j):
                        j += 2
                        continue
                    j += 1
                    break
                j += 1
            i = j
            continue
        if c == "'":
            j = i + 1
            while j < n:
                if text[j] == "'":
                    if text.startswith("''", j):
                        j += 2
                        continue
                    j += 1
                    break
                j += 1
            for k in range(i, min(j, n)):
                if out[k] != "\n":
                    out[k] = " "
            i = j
            continue
        i += 1
    return "".join(out)


# DDL that a second run cannot repeat: the statement either refuses to replace
# what exists or collides with it. Each pattern matches the spelling that fails
# and the message names the spelling that works. `create or replace ...` never
# matches, because the keyword has to follow `create` directly.
SIMPLE = (
    (re.compile(r"\bcreate\s+table\s+(?!if\s+not\s+exists)", re.IGNORECASE),
     "create table without `if not exists`"),
    (re.compile(r"\bcreate\s+sequence\s+(?!if\s+not\s+exists)", re.IGNORECASE),
     "create sequence without `if not exists`"),
    (re.compile(r"\bcreate\s+extension\s+(?!if\s+not\s+exists)", re.IGNORECASE),
     "create extension without `if not exists`"),
    (re.compile(r"\bcreate\s+schema\s+(?!if\s+not\s+exists)", re.IGNORECASE),
     "create schema without `if not exists`"),
    (re.compile(r"\bcreate\s+materialized\s+view\s+(?!if\s+not\s+exists)", re.IGNORECASE),
     "create materialized view without `if not exists`"),
    (re.compile(r"\bcreate\s+trigger\s+", re.IGNORECASE),
     "create trigger without `or replace`"),
    (re.compile(r"\bcreate\s+rule\s+", re.IGNORECASE),
     "create rule without `or replace`"),
    (re.compile(r"\bcreate\s+view\s+", re.IGNORECASE),
     "create view without `or replace`"),
    (re.compile(r"\bcreate\s+function\s+", re.IGNORECASE),
     "create function without `or replace`"),
    (re.compile(r"\bcreate\s+procedure\s+", re.IGNORECASE),
     "create procedure without `or replace`"),
)

INDEX_START = re.compile(r"\bcreate\s+(?:unique\s+)?index\s+", re.IGNORECASE)

POLICY = re.compile(r"\bcreate\s+policy\s+(?P<name>[a-z0-9_\"]+)\s+on\s+(?P<table>[a-z0-9_\"]+)",
                    re.IGNORECASE)
POLICY_DROP = re.compile(
    r"\bdrop\s+policy\s+if\s+exists\s+(?P<name>[a-z0-9_\"]+)\s+on\s+(?P<table>[a-z0-9_\"]+)",
    re.IGNORECASE)
TYPE = re.compile(r"\bcreate\s+(?P<kind>type|domain)\s+(?P<name>[a-z0-9_\"]+)", re.IGNORECASE)
TYPE_DROP = re.compile(r"\bdrop\s+(?P<kind>type|domain)\s+if\s+exists\s+(?P<name>[a-z0-9_\"]+)",
                       re.IGNORECASE)


def _bare(name):
    return name.lower().strip('"')


def not_re_runnable(text):
    """Statements a second run of this file would fail on, as (offset, why)."""
    masked = mask_non_code(text)
    problems = []

    for pattern, why in SIMPLE:
        for m in pattern.finditer(masked):
            problems.append((m.start(), why))

    for m in INDEX_START.finditer(masked):
        tail = masked[m.end():m.end() + 48].lstrip().lower()
        if not (tail.startswith("if not exists") or tail.startswith("concurrently if not exists")):
            problems.append((m.start(), "create index without `if not exists`"))

    dropped_policies = {(_bare(m.group("name")), _bare(m.group("table")))
                        for m in POLICY_DROP.finditer(masked)}
    for m in POLICY.finditer(masked):
        key = (_bare(m.group("name")), _bare(m.group("table")))
        if key not in dropped_policies:
            problems.append(
                (m.start(), f"create policy {m.group('name')} with no `drop policy if exists`"))

    dropped_types = {(m.group("kind").lower(), _bare(m.group("name")))
                     for m in TYPE_DROP.finditer(masked)}
    for m in TYPE.finditer(masked):
        key = (m.group("kind").lower(), _bare(m.group("name")))
        if key not in dropped_types:
            problems.append(
                (m.start(), f"create {m.group('kind')} {m.group('name')} with no `drop ... if exists`"))

    return problems


def line_of(text, offset):
    return text.count("\n", 0, offset) + 1


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
        for offset in split_concatenation(text):
            problems.append(
                f"{path}:{line_of(text, offset)}: a comment splits two string literals;"
                " PostgreSQL joins them only across whitespace, so move the comment"
                " above the statement")
        for offset, why in not_re_runnable(text):
            problems.append(f"{path}:{line_of(text, offset)}: {why}; a second run would fail")
    for problem in problems:
        print(problem)
    print(f"{len(files)} file(s) checked, {len(problems)} problem(s)")
    return 1 if problems else 0


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:] or ["projects/ores.sql"]))
