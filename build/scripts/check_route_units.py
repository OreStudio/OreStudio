#!/usr/bin/env python3
"""Every route unit a component opts into is reachable from its aggregate.

The HTTP route facet writes one unit per model, and the component's aggregate calls
each unit's ``register_routes`` by hand: no generator composes that list. A unit the
aggregate leaves out compiles, links, passes its own tests and is never registered, so
the gateway answers nothing for it and no error appears anywhere. The route inventory
cannot see it either, because the inventory reads the unit's own router calls.

The two rules below state the link over the source: a model that opts into the facet
has its unit called exactly once by its component's aggregate, and the aggregate's
self-include is not counted as a unit.

Usage: check_route_units.py [<repo root>]
"""

import re
import sys
from collections import Counter
from pathlib import Path

ENABLED = re.compile(r"^:ores\.cpp\.http-route\.enabled:\s*true\s*$", re.M)
SINGULAR = re.compile(r"^#\+(entity_singular|name_singular):\s*(\S+)\s*$", re.M)
REGISTER = re.compile(r"^\s*(\w+)_routes::register_routes\(")
ADAPTER = "projects/ores.http"


def opted_in_units(modeling_dir):
    """The unit each opted-in model of a component owns, by unit name."""
    units = {}
    for path in sorted(Path(modeling_dir).glob("*.org")):
        text = path.read_text(encoding="utf-8", errors="replace")
        if not ENABLED.search(text):
            continue
        found = SINGULAR.search(text)
        if not found:
            continue
        units[found.group(2)] = path
    return units


def registered_units(text):
    return [match.group(1) for line in text.splitlines() if (match := REGISTER.match(line))]


def aggregate_of(component_dir):
    """A component's route aggregate: the unit named for the component itself."""
    component = Path(component_dir).name
    return Path(component_dir) / "src" / "routes" / component / f"{component}_routes.cpp"


def check(root):
    problems = []
    checked = 0
    for component_dir in sorted(Path(root, ADAPTER).iterdir()):
        if not component_dir.is_dir():
            continue
        aggregate = aggregate_of(component_dir)
        if not aggregate.is_file():
            continue
        short = component_dir.name
        modeling = Path(root) / "projects" / f"ores.{short}" / "modeling"
        expected = opted_in_units(modeling) if modeling.is_dir() else {}
        called = registered_units(aggregate.read_text(encoding="utf-8", errors="replace"))
        checked += 1

        for unit, path in sorted(expected.items()):
            if unit not in called:
                problems.append(
                    f"{aggregate.relative_to(root)}: never registers {unit}_routes, "
                    f"which {path.relative_to(root)} opts into; the gateway answers "
                    "nothing for it and nothing reports it")
        for unit, count in Counter(called).items():
            if count > 1:
                problems.append(
                    f"{aggregate.relative_to(root)}: registers {unit}_routes {count} "
                    "times; the second registration claims routes the first stated")

        # A unit the aggregate calls must exist in the adapter, or the file is stale.
        routes_dir = Path(component_dir) / "src" / "routes" / short
        for unit in called:
            if not (routes_dir / f"{unit}_routes.cpp").is_file():
                problems.append(
                    f"{aggregate.relative_to(root)}: registers {unit}_routes, which has "
                    "no unit in this component")
    return problems, checked


def main(argv):
    root = Path(argv[1]).resolve() if len(argv) > 1 else Path(__file__).resolve().parents[2]
    problems, checked = check(root)
    for problem in problems:
        print(problem)
    print(f"{checked} component aggregate(s) checked, {len(problems)} problem(s)")
    return 1 if problems else 0


if __name__ == "__main__":
    sys.exit(main(sys.argv))
