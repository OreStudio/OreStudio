#!/usr/bin/env python3
"""Check that every generated history-provider registrar is composed.

A component's codegen emits one <entity>_history_provider_registrar.hpp/.cpp
pair per audited entity, declaring register_<entity>_history_provider(). The
composition point is the component's core/src/messaging/registrar.cpp, where the
dispatch registry is built.

A registrar that nothing calls never registers its provider. The service still
builds, starts and serves every other subject, and only the history verb fails,
with "No history provider registered for entity_type: <entity>". Nothing else
in the tree notices: the generated file compiles, the check that guards event
registrars guards a different facet, and the web screen that calls history shows
an empty tab.

Only the components in CHECKED_COMPONENTS are checked. A component joins the
list when its composition point carries every registrar it generates; the rest
are to-do, not exempt. A census on 2026-10-28 found: ores.refdata 60 of 167
uncomposed, ores.trading 24 of 96, ores.analytics 16 of 27, ores.synthetic 3 of
13, ores.workflow 2 of 4, ores.dq 1 of 21, ores.iam 1 of 18, and ores.assets,
ores.compute, ores.reporting and ores.variability composed none of theirs (2, 7,
28 and 1). Each of those needs its own story; this check keeps the components
that are complete from regressing.

Run::

    python3 projects/ores.codegen/scripts/check_history_registrar_composition.py
    python3 projects/ores.codegen/scripts/check_history_registrar_composition.py --fix
"""
from __future__ import annotations

import re
import sys
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[3]
PROJECTS = REPO_ROOT / "projects"

DECLARATION = re.compile(r"register_(\w+)_history_provider\s*\(")
INCLUDE = '#include "{namespace}/{stem}_history_provider_registrar.hpp"\n'

CHECKED_COMPONENTS = ("ores.refdata",)


def component_parts(component: Path) -> tuple[str, str]:
    """The C++ namespace and the include prefix of a component directory."""
    namespace = component.name.replace(".", "::")
    return namespace, f"{component.name}.core/messaging"


def registrar_entities(component: Path) -> dict[str, str]:
    """The entities a component's registrar headers declare, by header stem."""
    include = component / "core" / "include"
    if not include.is_dir():
        return {}
    found: dict[str, str] = {}
    for header in sorted(include.rglob("*_history_provider_registrar.hpp")):
        match = DECLARATION.search(header.read_text())
        if match:
            found[match.group(1)] = header.name.removesuffix(
                "_history_provider_registrar.hpp")
    return found


def composition_file(component: Path) -> Path:
    return component / "core" / "src" / "messaging" / "registrar.cpp"


def check_component(component: Path) -> str | None:
    """Why the component's registrars are not all composed, or None."""
    entities = registrar_entities(component)
    if not entities:
        return None
    composition = composition_file(component)
    if not composition.is_file():
        return (
            f"{component.name}: generates {len(entities)} registrar(s) but has no "
            "core/src/messaging/registrar.cpp"
        )
    composed = set(DECLARATION.findall(composition.read_text()))
    uncomposed = sorted(set(entities) - composed)
    if uncomposed:
        return (
            f"{component.name}: {len(uncomposed)} registrar(s) not composed: "
            f"{', '.join(uncomposed)}"
        )
    return None


def member_run(lines: list[str], is_member) -> tuple[int, int]:
    """The run of consecutive member lines that holds the last member.

    The provider includes are one such run, and so are the calls that build the
    registry. An earlier run of includes, sorted on its own, is left alone.
    """
    last = max(index for index, line in enumerate(lines) if is_member(line))
    first = last
    while first > 0 and is_member(lines[first - 1]):
        first -= 1
    return first, last


def fix_component(component: Path) -> list[str]:
    """Wire every uncomposed registrar of the component, returning the names."""
    _, prefix = component_parts(component)
    entities = registrar_entities(component)
    composition = composition_file(component)
    lines = composition.read_text().splitlines(keepends=True)
    missing = sorted(set(entities) - set(DECLARATION.findall("".join(lines))))
    if not missing:
        return []

    include_prefix = f'#include "{prefix}/'
    start, end = member_run(
        lines,
        lambda line: line.startswith(include_prefix)
        and line.rstrip().endswith("_registrar.hpp\""))
    includes = lines[start:end + 1]
    includes.extend(INCLUDE.format(namespace=prefix, stem=entities[entity])
                    for entity in missing)
    includes.sort()
    lines[start:end + 1] = includes

    start, end = member_run(
        lines,
        lambda line: line.strip().startswith("register_")
        and line.rstrip().endswith("_history_provider(hist_registry);"))
    indent = lines[end][:len(lines[end]) - len(lines[end].lstrip())]
    calls = lines[start:end + 1]
    calls.extend(f"{indent}register_{entity}_history_provider(hist_registry);\n"
                 for entity in missing)
    lines[start:end + 1] = calls

    composition.write_text("".join(lines), encoding="utf-8", newline="\n")
    return missing


def main() -> int:
    fix = "--fix" in sys.argv[1:]
    if fix:
        for name in CHECKED_COMPONENTS:
            wired = fix_component(PROJECTS / name)
            print(f"{name}: wired {len(wired)} registrar(s)")

    failures = [f for name in CHECKED_COMPONENTS if (f := check_component(PROJECTS / name))]

    if failures:
        print("History-provider registrars with no composition point:")
        for failure in failures:
            print(f"  {failure}")
        print()
        print("Call each registrar from its component's core/src/messaging/registrar.cpp,")
        print("or rerun this check with --fix.")
        return 1

    print(f"Every generated history-provider registrar is composed in "
          f"{', '.join(CHECKED_COMPONENTS)}.")
    return 0


if __name__ == "__main__":
    sys.exit(main())
