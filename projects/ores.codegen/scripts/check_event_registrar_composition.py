#!/usr/bin/env python3
"""Check that every event-mapping registrar of a checked component is composed.

A component's codegen emits one <entity>_event_registrar.hpp per entity that
publishes events, declaring register_<entity>_event_mapping(). Something has to
call each of them. An entity whose registrar nothing calls publishes nothing,
so every consumer of that entity follows a stale record, and the generated code
looks present while doing nothing.

The composition point is the component's service/src/messaging/event_registrar.cpp.
The check fails when a registrar of a checked component is not called there, or
when a checked component has no composition file at all.

Only the components in CHECKED_COMPONENTS are checked. A component joins the
list when its composition point carries every registrar it generates; the rest
are to-do, not exempt. A census on 2026-10-06 found the gap is wide outside
this list: ores.analytics had 16 registrars uncomposed, ores.iam 2, and nine
components generated registrars with no composition file at all, ores.trading
with 89. Each of those needs its own story; this check keeps the components
that are complete from regressing.

Run::

    python3 projects/ores.codegen/scripts/check_event_registrar_composition.py
"""
import re
import sys
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[3]
PROJECTS = REPO_ROOT / "projects"

DECLARATION = re.compile(r"register_(\w+)_event_mapping\s*\(")

# Components whose every generated registrar is composed. ores.refdata joined
# with the story Migrate the remaining refdata entities to the generated event
# registrar; ores.dq and ores.synthetic were already complete.
CHECKED_COMPONENTS = ("ores.dq", "ores.refdata", "ores.synthetic")


def registrar_entities(component: Path) -> dict[str, str]:
    """The entities a component's registrar headers declare, by header name."""
    include = component / "service" / "include"
    if not include.is_dir():
        return {}
    found: dict[str, str] = {}
    for header in sorted(include.rglob("*_event_registrar.hpp")):
        match = DECLARATION.search(header.read_text())
        if match:
            found[match.group(1)] = header.name
    return found


def check_component(component: Path) -> str | None:
    """Why the component's registrars are not all composed, or None."""
    entities = registrar_entities(component)
    if not entities:
        return None
    composition = component / "service" / "src" / "messaging" / "event_registrar.cpp"
    if not composition.is_file():
        return (
            f"{component.name}: generates {len(entities)} registrar(s) but has no "
            "service/src/messaging/event_registrar.cpp"
        )
    composed = set(DECLARATION.findall(composition.read_text()))
    uncomposed = sorted(set(entities) - composed)
    if uncomposed:
        return f"{component.name}: {len(uncomposed)} registrar(s) not composed: {', '.join(uncomposed)}"
    return None


def main() -> int:
    failures = [f for name in CHECKED_COMPONENTS if (f := check_component(PROJECTS / name))]

    if failures:
        print("Event registrars with no composition point:")
        for failure in failures:
            print(f"  {failure}")
        print()
        print("Call each registrar from its component's event_registrar.cpp.")
        return 1

    print(f"Every generated event registrar is composed in {', '.join(CHECKED_COMPONENTS)}.")
    return 0


if __name__ == "__main__":
    sys.exit(main())
