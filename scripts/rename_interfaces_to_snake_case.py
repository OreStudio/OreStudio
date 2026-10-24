#!/usr/bin/env python3
"""Rename the three quant and market data interfaces to snake_case.

IStochasticProcess, IYieldCurveProcess and IFeed predate the snake_case rule
in doc/knowledge/architecture/code_review_checklist.org. This script makes the
three renames in one pass, so the diff reads as one shape and no corner of the
tree keeps the old spelling.

It edits hand-written text only. A file whose banner says the generator owns it
is reported, not changed: regenerate it from its model instead. The component
class diagrams and their SVGs are generated too, for the same reason. The past
sprint records under doc/agile/ keep the old spelling on purpose, because they
are a record of what was true then.

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

ROOTS = ["projects", "doc"]

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

# A record of what was true then, not a claim about the code today.
SKIP_PREFIXES = ("doc/agile/",)

GENERATED_MARKERS = ("AUTO-GENERATED", "GENERATED FILE", "DO NOT EDIT MANUALLY")

# Drawn from the headers and the SQL models by a generator, and committed as
# output. They follow their source; they are never edited by hand.
GENERATED_PATHS = (
    re.compile(r"^projects/.*/modeling/.*\.(puml|svg)$"),
    re.compile(r"^projects/ores\.sql/.*\.(puml|svg)$"),
    re.compile(r"^projects/ores\.web/packages/wire-protocol/dist/"),
)

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


def candidates():
    """Yield paths to inspect: text files under the roots, minus the skips."""
    for root in ROOTS:
        for path in Path(root).rglob("*"):
            if not path.is_file():
                continue
            rel = path.as_posix()
            if any(part in SKIP_DIR_NAMES for part in Path(rel).parts):
                continue
            if rel.startswith(SKIP_PREFIXES):
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
        help="exit non-zero when an editable file keeps an old name",
    )
    args = parser.parse_args()

    edited = []
    generated = []

    for path, rel in candidates():
        try:
            text = path.read_text(encoding="utf-8")
        except (OSError, UnicodeDecodeError):
            continue
        if not any(pattern.search(text) for pattern, _ in PATTERNS):
            continue
        if is_generated(rel, text):
            generated.append(rel)
            continue
        count = sum(len(pattern.findall(text)) for pattern, _ in PATTERNS)
        edited.append((rel, count))
        if args.write:
            path.write_text(rename(text), encoding="utf-8")

    if args.check:
        if edited:
            print("Editable files still name an old interface:", file=sys.stderr)
            for rel, count in edited:
                print(f"  {count:3d}  {rel}", file=sys.stderr)
            return 1
        print("No editable file names an old interface.")
        return 0

    verb = "Renamed" if args.write else "Would rename"
    total = sum(count for _, count in edited)
    print(f"{verb} {total} occurrences in {len(edited)} files:")
    for rel, count in edited:
        print(f"  {count:3d}  {rel}")

    if generated:
        print()
        print(f"{len(generated)} generated files name an old interface.")
        print("Regenerate them from their models; do not edit them by hand:")
        for rel in generated:
            print(f"  {rel}")

    if not args.write:
        print()
        print("Rerun with --write to apply.")
    return 0


if __name__ == "__main__":
    sys.exit(main())
