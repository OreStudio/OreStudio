#!/usr/bin/env python3
"""Rename the three quant and market data interfaces to snake_case.

IStochasticProcess, IYieldCurveProcess and IFeed predate the snake_case rule
in doc/knowledge/architecture/code_review_checklist.org. This script makes the
three renames in one pass, so the diff reads as one shape and no corner of the
tree keeps the old spelling.

It edits hand-written text only. A file whose banner says the generator owns it
is reported, not changed: regenerate it from its model instead. The component
class diagrams and their SVGs are generated too, for the same reason.

A record under doc/agile/ keeps the old spelling on purpose, because it says
what was true when it was written. The check still reports those files, so the
gate never hides a match.

Run it from the repository root.

Usage:
  python3 scripts/rename_interfaces_to_snake_case.py            # report
  python3 scripts/rename_interfaces_to_snake_case.py --write    # apply
  python3 scripts/rename_interfaces_to_snake_case.py --check    # verify
"""

import argparse
import re
import sys
from pathlib import Path

# Old name, new name. Order does not matter: no old name is a suffix of
# another.
RENAMES = [
    ("IStochasticProcess", "i_stochastic_process"),
    ("IYieldCurveProcess", "i_yield_curve_process"),
    ("IFeed", "i_feed"),
]

ROOTS = ["projects", "doc", "scripts"]

SUFFIXES = {
    ".hpp",
    ".cpp",
    ".h",
    ".inl",
    ".ipp",
    ".org",
    ".md",
    ".txt",
    ".puml",
    ".svg",
    ".sql",
    ".ts",
}

SKIP_DIR_NAMES = {
    ".git",
    "venv",
    "node_modules",
    "dist",
    "__pycache__",
    ".audit",
    "tmp",
}

# A record of what was true then, not a claim about the code today. The rename
# task and the parent story live here too, and they must name the old spelling
# to say what changed.
RECORD_PREFIXES = ("doc/agile/",)

GENERATED_MARKERS = ("AUTO-GENERATED", "GENERATED FILE", "DO NOT EDIT MANUALLY")

# Drawn from the headers and the SQL models by a generator, and committed as
# output. They follow their source; they are never edited by hand.
GENERATED_PATHS = (re.compile(r"^projects/.*/modeling/.*\.(puml|svg)$"),)

PATTERNS = [
    (re.compile(r"\b" + re.escape(old) + r"\b"), new)
    for old, new in RENAMES
]


def is_generated(rel, text):
    """True when the file is output, so the model behind it must change."""
    if any(pattern.match(rel) for pattern in GENERATED_PATHS):
        return True
    head = "\n".join(text.splitlines()[:40])
    return any(marker in head for marker in GENERATED_MARKERS)


def is_record(rel):
    """True when the file records history, so the old spelling belongs."""
    return rel.startswith(RECORD_PREFIXES)


def candidates():
    """Yield every text file to inspect: (path, relative path)."""
    for root in ROOTS:
        for path in Path(root).rglob("*"):
            if not path.is_file():
                continue
            rel = path.as_posix()
            if any(part in SKIP_DIR_NAMES for part in Path(rel).parts):
                continue
            if path.suffix.lower() not in SUFFIXES:
                continue
            yield path, rel


def rename(text):
    for pattern, new in PATTERNS:
        text = pattern.sub(new, text)
    return text


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    group = parser.add_mutually_exclusive_group()
    group.add_argument(
        "--write", action="store_true", help="write the rename to disk"
    )
    group.add_argument(
        "--check",
        action="store_true",
        help="exit non-zero when editable or generated text keeps an old name",
    )
    args = parser.parse_args()

    missing = [root for root in ROOTS if not Path(root).is_dir()]
    if missing:
        parser.error(
            "run this from the repository root; missing: " + ", ".join(missing)
        )

    edited = []
    generated = []
    records = []
    unreadable = []

    for path, rel in candidates():
        try:
            text = path.read_text(encoding="utf-8")
        except (OSError, UnicodeDecodeError) as error:
            unreadable.append(f"{rel}: {error}")
            continue
        if not any(pattern.search(text) for pattern, _ in PATTERNS):
            continue
        count = sum(len(pattern.findall(text)) for pattern, _ in PATTERNS)
        if is_record(rel):
            records.append((rel, count))
        elif is_generated(rel, text):
            generated.append((rel, count))
        else:
            edited.append((rel, count))
            if args.write:
                path.write_text(rename(text), encoding="utf-8")

    if args.check:
        problems = edited + generated
        if problems or unreadable:
            if problems:
                print(
                    "Files must not name an old interface:", file=sys.stderr
                )
                for rel, count in problems:
                    print(f"  {count:3d}  {rel}", file=sys.stderr)
            for line in unreadable:
                print(f"  unreadable: {line}", file=sys.stderr)
            return 1
        total = sum(count for _, count in records)
        print(f"No file names an old interface, except {len(records)} record(s).")
        for rel, count in records:
            print(f"  {count:3d}  {rel}  (record, kept)")
        print(f"{total} occurrence(s) allowed in records.")
        return 0

    verb = "Renamed" if args.write else "Would rename"
    total = sum(count for _, count in edited)
    print(f"{verb} {total} occurrences in {len(edited)} files:")
    for rel, count in edited:
        print(f"  {count:3d}  {rel}")

    for label, rows in (
        ("Generated files, which their models must change", generated),
        ("Records, which keep the old spelling", records),
    ):
        if not rows:
            continue
        print()
        print(f"{len(rows)} {label}:")
        for rel, count in rows:
            print(f"  {count:3d}  {rel}")

    if unreadable:
        print()
        print("Unreadable files:", file=sys.stderr)
        for line in unreadable:
            print(f"  {line}", file=sys.stderr)

    if not args.write:
        print()
        print("Rerun with --write to apply.")
    return 0


if __name__ == "__main__":
    sys.exit(main())
