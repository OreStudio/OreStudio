#!/usr/bin/env python3
"""Check that application code writes entity tables through the repositories.

An entity table is a table named ``ores_<component>_<entity>_tbl``. The
generated repositories are the only code that should insert into, update or
delete from one; a SQL function or a hand-written C++ file that writes a table
directly bypasses the repository, its claim checking and its audit stamping.

Two kinds of writer are found:

* A SQL function whose body runs DML against an entity table.
* A hand-written C++ file whose source contains DML against an entity table.

Two exception classes are honoured:

* The sanctioned purge, ``ores_iam_purge_*``, which deletes across the
  component tables on purpose.
* Test fixtures. The SQL test tree and every ``tests`` directory are not
  scanned, because a fixture writes the rows a repository would.

Existing offenders are listed in ``entity_table_writes_baseline.json``. The
check is a ratchet: it fails on a writer that is not in the baseline, and it
fails on a baseline entry that no longer writes anything, so the list can only
shrink. Regenerate the baseline with ``--write-baseline`` when a writer is
fixed or a new one is accepted.

Run::

    python3 projects/ores.codegen/scripts/check_entity_table_writes.py
    python3 projects/ores.codegen/scripts/check_entity_table_writes.py --list
    python3 projects/ores.codegen/scripts/check_entity_table_writes.py --write-baseline
"""

from __future__ import annotations

import argparse
import json
import re
import sys
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[3]
BASELINE = Path(__file__).resolve().parent / "entity_table_writes_baseline.json"
SQL_CREATE = REPO_ROOT / "projects" / "ores.sql" / "create"

# The one component allowed to write entity tables outside a repository, and
# its test-tenant helper. Both delete across components as the purge.
PURGE_PREFIX = "ores_iam_purge_"

FUNC = re.compile(r"create\s+(?:or\s+replace\s+)?function\s+([a-z0-9_]+)\s*\(", re.IGNORECASE)
DOLLAR = re.compile(r"\$\$")
DML = re.compile(
    r"\b(insert\s+into|update|delete\s+from)\s+(ores_[a-z0-9_]*_tbl)\b",
    re.IGNORECASE,
)


def function_bodies(text: str):
    """Yield (name, body) for every function the SQL text defines."""
    for match in FUNC.finditer(text):
        name = match.group(1)
        opened = DOLLAR.search(text, match.end())
        if not opened:
            continue
        closed = DOLLAR.search(text, opened.end())
        if not closed:
            continue
        yield name, text[opened.end() : closed.start()]


def sql_writers():
    """Every SQL function whose body writes an entity table, minus the purge."""
    found = []
    for path in sorted(SQL_CREATE.rglob("*.sql")):
        text = path.read_text(encoding="utf-8", errors="ignore")
        for name, body in function_bodies(text):
            if name.startswith(PURGE_PREFIX):
                continue
            if DML.search(body):
                found.append(
                    {
                        "kind": "sql_function",
                        "name": name,
                        "file": str(path.relative_to(REPO_ROOT)),
                    }
                )
    return found


def cpp_files():
    """Hand-written C++ under a component, never a test or a build tree."""
    for path in sorted(REPO_ROOT.glob("projects/ores.*/**/*.cpp")):
        parts = path.relative_to(REPO_ROOT).parts
        if "tests" in parts or "build" in parts or parts[1] == "ores.testing":
            continue
        yield path
    for path in sorted(REPO_ROOT.glob("projects/ores.*/**/*.hpp")):
        parts = path.relative_to(REPO_ROOT).parts
        if "tests" in parts or "build" in parts or parts[1] == "ores.testing":
            continue
        yield path


def cpp_writers():
    """Every hand-written C++ file that writes an entity table in raw SQL."""
    found = []
    for path in cpp_files():
        text = path.read_text(encoding="utf-8", errors="ignore")
        if DML.search(text):
            found.append(
                {
                    "kind": "cpp",
                    "name": "",
                    "file": str(path.relative_to(REPO_ROOT)),
                }
            )
    return found


def writers():
    return sql_writers() + cpp_writers()


def key(entry):
    return (entry["kind"], entry["name"] or entry["file"])


def load_baseline():
    if not BASELINE.exists():
        return []
    return json.loads(BASELINE.read_text(encoding="utf-8"))["writers"]


def write_baseline(found):
    entries = []
    for entry in found:
        entry = dict(entry)
        entry["reason"] = "predates the check"
        entries.append(entry)
    entries.sort(key=lambda e: (e["kind"], e["name"] or e["file"]))
    BASELINE.write_text(
        json.dumps({"writers": entries}, indent=2) + "\n", encoding="utf-8"
    )
    return len(entries)


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--list", action="store_true", help="print every writer and exit")
    parser.add_argument(
        "--write-baseline",
        action="store_true",
        help="replace the baseline with the writers found now",
    )
    args = parser.parse_args()

    found = writers()

    if args.list:
        for entry in found:
            where = entry["name"] or entry["file"]
            print(f"{entry['kind']}\t{where}\t{entry['file']}")
        return 0

    if args.write_baseline:
        print(f"wrote {write_baseline(found)} writer(s) to {BASELINE.name}")
        return 0

    baseline = {key(entry) for entry in load_baseline()}
    current = {key(entry): entry for entry in found}

    new = [entry for k, entry in current.items() if k not in baseline]
    stale = [entry for entry in load_baseline() if key(entry) not in current]

    for entry in new:
        print(
            f"new writer: {entry['kind']} {entry['name'] or entry['file']} "
            f"({entry['file']}) writes an entity table",
            file=sys.stderr,
        )
    for entry in stale:
        print(
            f"stale baseline entry: {entry['kind']} {entry['name'] or entry['file']} "
            f"({entry['file']}) no longer writes an entity table; drop it from "
            f"{BASELINE.name}",
            file=sys.stderr,
        )

    if new or stale:
        return 1

    print(f"entity-table writes: {len(found)} known writer(s), no new ones")
    return 0


if __name__ == "__main__":
    sys.exit(main())
