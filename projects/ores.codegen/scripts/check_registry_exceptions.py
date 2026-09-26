#!/usr/bin/env python3
"""Check the accepted exceptions a listed component carries.

A component is listed in ``COMPONENTS_UNDER_TEST`` when every checklist item
that applies to it passes, or when each item that does not is recorded as an
accepted exception in ``ACCEPTED_EXCEPTIONS``. This script is what keeps that
claim honest: it reads the checklist's own item list from the standard, and
refuses an exception that names an item the standard does not define, states no
reason, names nobody, carries no date, or is recorded against a component that
is not listed at all.

The point is that an item is never simply omitted. Without this check a
component could be listed while quietly passing fewer items than the standard
asks for, and the one thing the registry exists to prevent -- a component
treated as clean because nobody wrote down what it is not -- would happen in the
place built to stop it.

Exit code is non-zero when any exception is malformed.
"""

from __future__ import annotations

import re
import sys
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[3]
sys.path.insert(0, str(REPO_ROOT / "projects/ores.codegen/scripts"))

from component_registry import (  # noqa: E402
    ACCEPTED_EXCEPTIONS,
    COMPONENTS_UNDER_TEST,
)

STANDARD = REPO_ROOT / "doc/knowledge/architecture/component_clean_standard.org"

# A checklist row: | B01 | All | item | evidence |
_ITEM_ROW = re.compile(r"^\|\s*([A-Z]\d{2})\s*\|", re.MULTILINE)

_DATE = re.compile(r"^\d{4}-\d{2}-\d{2}$")


def checklist_items(path: Path) -> set[str]:
    """Every item id the standard defines, read from its own tables.

    Derived rather than listed here, so renaming or retiring an item changes
    what the registry will accept without a second list to keep in step.
    """
    text = path.read_text(encoding="utf-8")
    return set(_ITEM_ROW.findall(text))


def main() -> int:
    items = checklist_items(STANDARD)
    if not items:
        print(f"no checklist items found in {STANDARD}", file=sys.stderr)
        return 1

    listed = set(COMPONENTS_UNDER_TEST)
    problems: list[str] = []

    for component, exceptions in sorted(ACCEPTED_EXCEPTIONS.items()):
        if component not in listed:
            problems.append(
                f"{component}: carries accepted exceptions but is not listed in "
                "COMPONENTS_UNDER_TEST; an exception only means something for a "
                "component the gates check")

        seen: set[str] = set()
        for entry in exceptions:
            where = f"{component} {entry.item}"
            if entry.item not in items:
                problems.append(
                    f"{where}: not an item the standard defines")
            if entry.item in seen:
                problems.append(f"{where}: recorded twice")
            seen.add(entry.item)
            if not entry.reason.strip():
                problems.append(f"{where}: states no reason")
            if not entry.accepted_by.strip():
                problems.append(f"{where}: names nobody as having accepted it")
            if not _DATE.match(entry.accepted_on.strip()):
                problems.append(
                    f"{where}: accepted_on is {entry.accepted_on!r}, not an "
                    "ISO date")

    total = sum(len(v) for v in ACCEPTED_EXCEPTIONS.values())
    for component in sorted(listed):
        n = len(ACCEPTED_EXCEPTIONS.get(component, ()))
        if n:
            names = ", ".join(e.item for e in ACCEPTED_EXCEPTIONS[component])
            print(f"{component}: listed with {n} accepted exception(s): {names}")

    if problems:
        for problem in problems:
            print(f"FAIL {problem}", file=sys.stderr)
        return 1

    if total:
        print(f"{len(listed)} component(s) listed, {total} accepted exception(s), "
              f"{len(items)} checklist items defined")
    else:
        print(f"{len(listed)} component(s) listed, no accepted exceptions, "
              f"{len(items)} checklist items defined")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
