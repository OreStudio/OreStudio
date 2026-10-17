#!/usr/bin/env python3
"""Check the generated convention family registry against the modelling tree.

The registry header is generated from the refdata entity models that declare
``#+convention_family: <key>``; see
``projects/ores.codegen/scripts/regenerate_convention_family_registry.py``.
This check fails when:

* a marked convention entity in the modelling tree is missing from the
  registry, which is a model added without a regeneration;
* a registry entry names an entity that no longer carries the declaration or no
  longer exists, which is a model removed without a regeneration;
* an entry's key, domain type, subject stem or scope disagrees with what its
  model states, which is a generated header edited by hand or a model changed
  without a regeneration.

It reads the models and the header only, so it needs neither codegen nor a
formatter.

Usage::

    check_convention_family_registry.py
"""

from __future__ import annotations

import argparse
import sys
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[3]
sys.path.insert(0, str(REPO_ROOT / "projects" / "ores.codegen" / "src"))

from codegen.convention_family_registry import (  # noqa: E402
    HEADER_RELATIVE_PATH,
    discover_entries,
    parse_header,
    render_header,
)


def _field_problems(expected, stated) -> list[str]:
    problems: list[str] = []
    for field in ("key", "domain_type", "subject_stem", "party_scoped"):
        want = getattr(expected, field)
        got = getattr(stated, field)
        if want != got:
            problems.append(
                f"{expected.entity}: registry {field} is {got!r},"
                f" the model states {want!r}"
            )
    return problems


def check(project_root: Path) -> list[str]:
    """Every way the registry disagrees with the modelling tree."""
    header = project_root / HEADER_RELATIVE_PATH
    if not header.exists():
        return [f"{HEADER_RELATIVE_PATH} is missing"]

    try:
        expected = discover_entries(project_root)
    except ValueError as error:
        return [str(error)]

    text = header.read_text(encoding="utf-8")
    stated = parse_header(text)
    wanted = {entry.entity: entry for entry in expected}
    actual = {entry.entity: entry for entry in stated}

    problems: list[str] = []
    for entity, entry in wanted.items():
        if entity not in actual:
            problems.append(
                f"convention entity {entity} (family key {entry.key}) is"
                " missing from the registry"
            )
        else:
            problems.extend(_field_problems(entry, actual[entity]))
    for entity, entry in actual.items():
        if entity not in wanted:
            problems.append(
                f"registry entry {entity} (family key {entry.key}) names an"
                " entity that no longer declares a convention family"
            )

    if not problems and text != render_header(expected):
        problems.append(
            f"{HEADER_RELATIVE_PATH} differs from its generated form"
        )
    return problems


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "--root",
        type=Path,
        default=REPO_ROOT,
        help="project root to check (default: the checkout this script is in)",
    )
    args = parser.parse_args()
    root = args.root.resolve()

    problems = check(root)
    if problems:
        for problem in problems:
            print(problem, file=sys.stderr)
        print(
            "regenerate with"
            " projects/ores.codegen/scripts/regenerate_convention_family_registry.py",
            file=sys.stderr,
        )
        return 1

    print(
        f"{len(discover_entries(root))} convention families match"
        f" {HEADER_RELATIVE_PATH}"
    )
    return 0


if __name__ == "__main__":
    sys.exit(main())
