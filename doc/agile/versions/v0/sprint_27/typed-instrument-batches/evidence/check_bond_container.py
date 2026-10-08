#!/usr/bin/env python3
"""Check the generated bond document structs against the hand-written ones.

The bond document container was hand-written C++ and is now generated from
field_group models. This check proves the migration kept the wire shape: every
struct must declare the same members, in the same order, with the same C++
types, as the hand-written header it replaced.

The two hand-written headers are read from git rather than copied here, so the
comparison is against the revision the migration replaced. Name that revision
in the task's Measurements table.

Usage:
  python3 check_bond_container.py --original-rev origin/main
  python3 check_bond_container.py --original-rev 3096504bcd

Exit status is 0 when every struct matches, 1 otherwise.
"""
from __future__ import annotations

import argparse
import re
import subprocess
import sys
from pathlib import Path

REPO = Path(__file__).resolve().parents[7]
DOMAIN = REPO / "projects/ores.trading/api/include/ores.trading.api/domain"

ORIGINALS = ["projects/ores.trading/api/include/ores.trading.api/domain/"
             "bond_schedule_data.hpp",
             "projects/ores.trading/api/include/ores.trading.api/domain/"
             "bond_instrument_data.hpp"]

_MEMBER_RE = re.compile(r"^\s{4}([\w:<>,\s\*&]+?)\s+(\w+)\s*[\{;=]", re.M)
_METHOD_RE = re.compile(r"^\s{4}([\w:<>,\s\*&]+?)\s+(\w+)\s*\(", re.M)


def structs(text: str) -> dict[str, list[tuple[str, str]]]:
    """Every struct in the text, as its (type, name) member list."""
    text = re.sub(r"/\*.*?\*/", "", text, flags=re.S)
    text = re.sub(r"//.*", "", text)
    out: dict[str, list[tuple[str, str]]] = {}
    for match in re.finditer(r"struct\s+(\w+)\s+(?:final\s+)?\{(.*?)\n\};", text, re.S):
        body = match.group(2)
        # A member function is not data; is_empty() is the one the migration
        # moved to a free function, and the generated header adds operator==.
        methods = {name for _, name in _METHOD_RE.findall(body)}
        members = [(t.strip(), n) for t, n in _MEMBER_RE.findall(body)
                   if n not in methods and n != "operator"]
        out[match.group(1)] = members
    return out


def original_at(rev: str, path: str) -> str:
    result = subprocess.run(["git", "show", f"{rev}:{path}"],
                            cwd=REPO, capture_output=True, text=True)
    if result.returncode != 0:
        sys.exit(f"git show {rev}:{path} failed:\n{result.stderr}")
    return result.stdout


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--original-rev", default="origin/main",
                        help="The revision that still holds the hand-written headers.")
    args = parser.parse_args()

    original: dict[str, list[tuple[str, str]]] = {}
    for path in ORIGINALS:
        original.update(structs(original_at(args.original_rev, path)))

    failures = 0
    for name in sorted(original):
        generated_path = DOMAIN / f"{name}.hpp"
        if not generated_path.is_file():
            print(f"FAIL {name}: no generated header at {generated_path}")
            failures += 1
            continue
        generated = structs(generated_path.read_text(encoding="utf-8"))
        if name not in generated:
            print(f"FAIL {name}: the generated header declares no such struct")
            failures += 1
            continue
        if original[name] != generated[name]:
            failures += 1
            print(f"FAIL {name}:")
            print(f"  hand-written: {original[name]}")
            print(f"  generated:    {generated[name]}")
        else:
            print(f"ok   {name}: {len(original[name])} members")

    print(f"\n{len(original) - failures}/{len(original)} structs match "
          f"against {args.original_rev}.")
    return 1 if failures else 0


if __name__ == "__main__":
    sys.exit(main())
