#!/usr/bin/env python3
"""Keep org headings free of hand-written section numbers.

Org numbers every exported section. A heading that also carries its own
number renders as "2.1. 1. The document's shape has one owner", and the
number in the text is a copy that goes stale the moment a section is
inserted above it.

This script is the sweep and the gate: --write strips the number from
every heading that has one, and --check fails when any is left. Headings
inside a source, example or quote block are left alone, because they are
content rather than structure.

Usage: check_heading_numbers.py [--write]
"""
import argparse
import pathlib
import re
import sys

ROOT = pathlib.Path(__file__).resolve().parents[2]
SEARCH = (ROOT / "doc", ROOT / "projects")
SKIP_DIRS = {"build", "external", "venv", "node_modules", ".git", ".cache"}

HEADING = re.compile(r"^(\*+ )(?:\d+(?:\.\d+)*)[.)]\s+(\S.*?)(\r\n|\r|\n)?$")
BLOCK_START = re.compile(r"^\s*#\+begin_(src|example|quote|export|comment)\b",
                         re.IGNORECASE)
BLOCK_END = re.compile(r"#\+end_(src|example|quote|export|comment)\b",
                       re.IGNORECASE)


def org_files():
    for base in SEARCH:
        for path in base.rglob("*.org"):
            if any(part in SKIP_DIRS for part in path.parts):
                continue
            yield path


def strip_numbers(text):
    """The text with each heading's own number removed, and the count.

    A pasted transcript in doc/analysis glues the end tag to the text after
    the code, on the same line that opens the block. An opening tag counts
    only as the first token of the line, which is the only place org reads
    one, and an end tag counts wherever it sits, so that line closes.

    The file is read and written as bytes, and each heading keeps its own
    line ending, because the agile record holds files with mixed CRLF and
    LF: decoding and re-encoding them whole would rewrite every line of a
    file for the sake of one heading.
    """
    out, removed, depth = [], 0, 0
    for line in text.splitlines(keepends=True):
        starts = 1 if BLOCK_START.match(line) else 0
        ends = len(BLOCK_END.findall(line))
        depth = max(0, depth + starts - ends)
        if depth == 0:
            match = HEADING.match(line)
            if match:
                line = (match.group(1) + match.group(2)
                        + (match.group(3) or ""))
                removed += 1
        out.append(line)
    if depth:
        print(f"⚠  {depth} block(s) left open; headings after the first are"
              f" not inspected", file=sys.stderr)
    return "".join(out), removed


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--write", action="store_true",
                        help="strip the numbers instead of reporting them")
    args = parser.parse_args()

    found = []
    for path in sorted(org_files()):
        text = path.read_bytes().decode("utf-8")
        stripped, removed = strip_numbers(text)
        if not removed:
            continue
        found.append((path, removed))
        if args.write:
            path.write_bytes(stripped.encode("utf-8"))

    total = sum(count for _, count in found)
    if args.write:
        for path, count in found:
            print(f"  {path.relative_to(ROOT)}: {count} heading(s)")
        print(f"✅ stripped {total} hand-written number(s) from "
              f"{len(found)} file(s).")
        return 0

    if found:
        for path, count in found:
            print(f"❌ {path.relative_to(ROOT)}: {count} numbered heading(s)",
                  file=sys.stderr)
        print("   run: python3 build/scripts/check_heading_numbers.py --write",
              file=sys.stderr)
        return 1

    print(f"✅ no org heading carries its own number ({len(found)} file(s)).")
    return 0


if __name__ == "__main__":
    sys.exit(main())
