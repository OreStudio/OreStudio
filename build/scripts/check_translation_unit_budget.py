#!/usr/bin/env python3
"""A translation unit reflects at most one convention family.

Reflecting a family type instantiates the serialisation of every field it
holds. A unit that reflects every family at once is the shape the conventions
document had, and it is the shape that broke the MSVC build. The documents are
gone, and this check keeps that shape from coming back quietly: it counts, for
each source file in the scopes below, the distinct convention family value
types the file names while it reflects, and fails when a file names more than
its budget.

A file reflects when it includes an ``rfl`` header, calls ``rfl::`` or includes one
of the project headers that reflect on its behalf. Headers are measured as well
as sources, because an inline template reflects wherever it is included. The
families are the registry's: the check reads the value types out of the
generated registry header, so a family added to the modelling tree joins the
count on the next regeneration.

The budget is one. A file that is over it must be named in ``KNOWN`` with the
ceiling it holds today and the reason. The ceiling only goes down: a file
that falls below its ceiling fails the check until the entry is lowered or
removed, so the debt is recorded where it can be seen and cannot grow.

Usage:
  check_translation_unit_budget.py           check the tree
  check_translation_unit_budget.py --list    print every file that names a family
"""

from __future__ import annotations

import argparse
import re
import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]

REGISTRY = ("projects/ores.refdata/api/include/ores.refdata.api/domain/"
            "convention_family_registry.hpp")

SCOPES = (
    "projects/ores.refdata/core/src",
    "projects/ores.refdata/core/include",
    "projects/ores.ore/core/src",
    "projects/ores.ore/core/include",
    "projects/ores.ore/service/src",
)

SUFFIXES = (".cpp", ".hpp", ".inc")

BUDGET = 1

# Project headers whose templates reflect the type a unit hands them, so the
# unit reflects although it never names an ``rfl`` header itself.
REFLECTING_HEADERS = ("conventions_family_pack.hpp",)

# Files over the budget, with the ceiling each holds and why.
KNOWN: dict[str, tuple[int, str]] = {
    "projects/ores.ore/core/src/domain/conventions_family_tenor_basis_swap.cpp": (
        3,
        "the three tenor basis swap families are one document element, mapped together"),
    "projects/ores.ore/core/src/domain/conventions_diff.cpp": (
        3,
        "the three tenor basis swap families are one document element, compared together"),
    "projects/ores.refdata/core/src/service/conventions_bundle_service.cpp": (
        3,
        "the three tenor basis swap families are one document element, applied together"),
}

_DOMAIN_TYPE = re.compile(r'"ores::refdata::domain::([a-z_0-9]+)"')


def family_types(root: Path = ROOT) -> list[str]:
    """The value type name of every family the registry header states."""
    text = (root / REGISTRY).read_text(encoding="utf-8")
    return _DOMAIN_TYPE.findall(text)


def reflects(text: str) -> bool:
    return ("rfl::" in text or "<rfl/" in text or
            any(f'"{header}"' in text for header in REFLECTING_HEADERS))


def families_named(text: str, types: list[str]) -> set[str]:
    """The distinct family types a source text names, as ``domain::<type>``."""
    named = set()
    for name in types:
        if re.search(rf"(?<![A-Za-z0-9_])domain::{re.escape(name)}(?![A-Za-z0-9_])", text):
            named.add(name)
    return named


def measure(root: Path = ROOT) -> dict[str, int]:
    """Every reflecting file in the scopes that names a family, with its count."""
    types = family_types(root)
    counts: dict[str, int] = {}
    for scope in SCOPES:
        base = root / scope
        if not base.is_dir():
            continue
        for path in sorted(base.rglob("*")):
            if path.suffix not in SUFFIXES or not path.is_file():
                continue
            text = path.read_text(encoding="utf-8", errors="replace")
            if not reflects(text):
                continue
            count = len(families_named(text, types))
            if count:
                counts[str(path.relative_to(root))] = count
    return counts


def judge(counts: dict[str, int], known: dict[str, tuple[int, str]] | None = None) -> list[str]:
    """The failures, one line each; empty when the tree agrees with the budget."""
    known = KNOWN if known is None else known
    failures = []
    for path, count in sorted(counts.items()):
        ceiling = known[path][0] if path in known else BUDGET
        if count > ceiling:
            failures.append(
                f"{path} reflects {count} convention families; its budget is {ceiling}. "
                "Move each family's serialisation into a unit of its own.")
    for path, (ceiling, reason) in sorted(known.items()):
        count = counts.get(path, 0)
        if count < ceiling:
            failures.append(
                f"{path} now reflects {count} convention families, below its recorded "
                f"ceiling of {ceiling} ({reason}). Lower the ceiling or remove the entry.")
    return failures


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description=__doc__.split("\n\n")[0])
    parser.add_argument("--list", action="store_true",
                        help="Print every reflecting file that names a family.")
    args = parser.parse_args(argv)

    counts = measure()
    if args.list:
        for path, count in sorted(counts.items(), key=lambda item: (-item[1], item[0])):
            print(f"{count:3d}  {path}")
        return 0

    failures = judge(counts)
    if failures:
        print("The translation unit budget is exceeded:\n")
        for failure in failures:
            print(f"  {failure}")
        return 1
    print(f"No unit reflects more than its budget of convention families "
          f"({len(counts)} reflecting file(s) name one).")
    return 0


if __name__ == "__main__":
    sys.exit(main())
