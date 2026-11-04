#!/usr/bin/env python3
"""Check that every generated handler registrar is called.

A component's codegen emits one <entity>_registrar.hpp/.cpp pair per entity
with messaging, declaring register_<entity>_handlers(), which subscribes the
entity's subjects. A registrar that nothing calls subscribes nothing. The
service still builds, starts and serves every other subject; a request on the
entity's subjects finds no responder and times out.

That happened to the synthetic calibration policy, provider order and fallback
on 2026-11-01: their registrars were generated, nothing called them, and the
calibration screen timed out reading its policy. Neither the build nor a test
noticed. The history-provider facet has its own check,
check_history_registrar_composition.py.

A registrar passes when the component calls it outside its own files and the
tests, or when every subject it would subscribe is subscribed by hand elsewhere
in the component.

Only the components in CHECKED_COMPONENTS are checked: those whose registrars
all pass. A census on 2026-11-01 found subjects with no responder in
ores.analytics (16 registrars), ores.refdata (17), ores.trading (10),
ores.reporting (7), ores.workflow (2), ores.iam (1) and ores.inbox (1). Some may
be entities not yet exposed on purpose; each component needs its own look before
it joins the list. This check keeps the components that are complete from
regressing.

Run::

    python3 projects/ores.codegen/scripts/check_handler_registrar_composition.py
"""
from __future__ import annotations

import re
import sys
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[3]
PROJECTS = REPO_ROOT / "projects"

DECLARATION = re.compile(r"register_(\w+)_handlers\s*\(")
SUBJECT = re.compile(r"(\w+_request)::nats_subject")

CHECKED_COMPONENTS = ("ores.assets", "ores.compute", "ores.dq", "ores.marketdata",
                      "ores.scheduler", "ores.synthetic", "ores.variability")


def registrar_entities(component: Path) -> dict[str, Path]:
    """The entities the component's generated handler registrars declare."""
    include = component / "core" / "include"
    if not include.is_dir():
        return {}
    found: dict[str, Path] = {}
    for header in sorted(include.rglob("*_registrar.hpp")):
        if header.name == "registrar.hpp" or header.name.endswith(
                "_history_provider_registrar.hpp"):
            continue
        match = DECLARATION.search(header.read_text())
        if match:
            found[match.group(1)] = header
    return found


def sources(component: Path):
    """The component's own C++ sources, outside the tests."""
    for source in component.rglob("*"):
        if source.suffix in (".cpp", ".hpp") and "tests" not in source.parts:
            yield source


def own_entity(source: Path) -> str | None:
    """The entity whose generated registrar the file is, if it is one."""
    stem = source.stem
    return stem.removesuffix("_registrar") if stem.endswith("_registrar") else None


def scan(component: Path) -> tuple[set[str], set[str], dict[str, set[str]]]:
    """What the component calls and subscribes, outside each registrar's own files.

    Returns the registrars called, the request subjects named elsewhere, and the
    subjects each generated registrar subscribes. A registrar's own header
    declares it and its own source defines it, so those two name it without
    calling it.
    """
    called: set[str] = set()
    named: set[str] = set()
    subscribes: dict[str, set[str]] = {}
    for source in sources(component):
        text = source.read_text(errors="ignore")
        own = own_entity(source)
        if own is not None and source.suffix == ".cpp":
            subscribes[own] = set(SUBJECT.findall(text))
        called |= {name for name in DECLARATION.findall(text) if name != own}
        if own is None:
            named |= set(SUBJECT.findall(text))
    return called, named, subscribes


def check_component(component: Path) -> str | None:
    """Why some of the component's registrars are not called, or None."""
    entities = registrar_entities(component)
    if not entities:
        return None
    called, named, subscribes = scan(component)
    # A registrar nothing calls is fine when every subject it would subscribe is
    # subscribed by hand elsewhere in the component.
    unserved = sorted(entity for entity in set(entities) - called
                      if not subscribes.get(entity) or not subscribes[entity] <= named)
    if not unserved:
        return None
    return (f"{component.name}: {len(unserved)} registrar(s) whose subjects have no responder: "
            f"{', '.join(unserved)}")


def main() -> int:
    components = [PROJECTS / name for name in CHECKED_COMPONENTS]
    failures = [f for component in components if (f := check_component(component))]
    if failures:
        print("Generated handler registrars whose subjects have no responder:")
        for failure in failures:
            print(f"  {failure}")
        print()
        print("Call each one from its component's core/src/messaging/registrar.cpp.")
        return 1
    print(f"Every generated handler registrar has a responder in "
          f"{', '.join(CHECKED_COMPONENTS)}.")
    return 0


if __name__ == "__main__":
    sys.exit(main())
