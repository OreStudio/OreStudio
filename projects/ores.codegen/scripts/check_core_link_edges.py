#!/usr/bin/env python3
"""Check that no component's core library links another component's core.

A component is split into an api contract, a core and a service. The core is the
domain and its repositories; it may depend on other components' *api* contracts
and on the shared libraries every component uses, and it may not depend on
another component's *core* or *service*. An edge of that kind is how a codec
comes to reach into another component's repositories, and it is not visible in a
review of either component alone.

The rule is stated here, in one table, because this is the check that enforces
it:

============  =====================================================
Kind linked   Allowed from a core?
============  =====================================================
core          no, unless it is the same component
service       no, unless it is the same component
api           yes
anything else yes -- it is not a component library
============  =====================================================

The edges are read from the CMake tree rather than from the build, so the check
needs no configure step: the ``target_link_libraries`` calls are the declaration
of what a library links. A dependency the check cannot resolve, and a core file
that states no name, are failures rather than silence: a gate that cannot see an
edge must not pass.

Existing offenders are listed in ``core_link_edges_baseline.json``. The check is
a ratchet: it fails on an edge that is not in the baseline, and it fails on a
baseline entry that no longer links anything, so the list can only shrink.
``--write-baseline`` drops the entries whose edges are gone and refuses to add
one unless ``--accept-new`` is passed, so accepting an edge is a deliberate
act.

Run::

    python3 projects/ores.codegen/scripts/check_core_link_edges.py
    python3 projects/ores.codegen/scripts/check_core_link_edges.py --list
    python3 projects/ores.codegen/scripts/check_core_link_edges.py --write-baseline
"""

from __future__ import annotations

import argparse
import json
import re
import sys
from pathlib import Path
from typing import NamedTuple

REPO_ROOT = Path(__file__).resolve().parents[3]
BASELINE = Path(__file__).resolve().parent / "core_link_edges_baseline.json"
PROJECTS = REPO_ROOT / "projects"

# The library kinds a core may not link across a component boundary.
FORBIDDEN_KINDS = ("core", "service")

NAME_SETTING = re.compile(r'set\(\s*name\s+"?([A-Za-z0-9_.]+)"?\s*\)')


class ForbiddenEdge(NamedTuple):
    """A core, the library it links, the component that library belongs to, and its kind."""

    core: str
    links: str
    owner: str
    kind: str
def variables_of(component: str) -> dict[str, str]:
    """The CMake variables a generated library file uses, and what they hold."""
    return {
        "${lib_target_name}": f"{component}.lib",
        "${lib_binary_name}": component,
        "${name}": component,
        "${component}": component,
    }


def without_comments(text: str) -> str:
    """The file with its whole-line and trailing comments removed."""
    lines = []
    for line in text.splitlines():
        stripped = line.strip()
        if stripped.startswith("#"):
            continue
        lines.append(line.split("#", 1)[0])
    return "\n".join(lines)


def argument_lists(text: str, command: str) -> list[str]:
    """The parenthesised argument text of every ``command(...)`` in the file."""
    lists: list[str] = []
    for match in re.finditer(re.escape(command) + r"\s*\(", text):
        depth = 1
        at = match.end()
        while at < len(text) and depth:
            if text[at] == "(":
                depth += 1
            elif text[at] == ")":
                depth -= 1
            at += 1
        lists.append(text[match.end() : at - 1])
    return lists


def target_and_dependencies(arguments: str, component: str) -> tuple[str, list[str]]:
    """The target the call appends to, and the libraries it names."""
    words = [word for word in arguments.split() if word not in ("PUBLIC", "PRIVATE", "INTERFACE")]
    if not words:
        return "", []
    variables = variables_of(component)
    resolved = []
    for word in words:
        for variable, value in variables.items():
            word = word.replace(variable, value)
        resolved.append(word)
    return resolved[0], resolved[1:]


def edges_of(path: Path) -> list[tuple[str, str]]:
    """Every ``(core library, library it links)`` the file declares."""
    text = without_comments(path.read_text())
    match = NAME_SETTING.search(text)
    if not match:
        return []
    component = match.group(1)
    if not component.startswith("ores.") or not component.endswith(".core"):
        return []

    core = f"{component}.lib"
    edges = []
    for arguments in argument_lists(text, "target_link_libraries"):
        target, dependencies = target_and_dependencies(arguments, component)
        if target != core:
            continue
        edges.extend((core, dependency) for dependency in dependencies)
    return edges


def component_names() -> set[str]:
    """Every component the CMake tree declares, by the names its files state."""
    names = set()
    for path in PROJECTS.glob("**/CMakeLists.txt"):
        for match in NAME_SETTING.finditer(without_comments(path.read_text())):
            parts = match.group(1).split(".")
            if len(parts) == 3 and parts[0] == "ores":
                names.add(parts[1])
    return names


def where(path: Path) -> str:
    """The path as the repository states it, or as it stands outside the tree."""
    return str(path.relative_to(REPO_ROOT)) if path.is_relative_to(REPO_ROOT) else str(path)


def scan() -> tuple[list[ForbiddenEdge], list[str]]:
    """Every edge a core may not have, and every edge the scan could not read."""
    edges: list[ForbiddenEdge] = []
    problems: list[str] = []
    seen = set()
    components = component_names()

    for path in sorted(PROJECTS.glob("**/CMakeLists.txt")):
        text = without_comments(path.read_text())
        for core, links in edges_of(path):
            if (core, links) in seen:
                continue
            seen.add((core, links))
            if "$" in links:
                problems.append(
                    f"{where(path)}: {core} links '{links}', which names a variable the check "
                    f"cannot resolve."
                )
                continue
            parts = links.split(".")
            if len(parts) != 4 or parts[0] != "ores":
                continue
            kind = parts[2]
            if kind not in FORBIDDEN_KINDS or parts[1] not in components:
                continue
            own = core[: -len(".lib")].split(".")[1]
            if parts[1] == own:
                continue
            edges.append(ForbiddenEdge(core, links, parts[1], kind))

    for directory in sorted(PROJECTS.glob("*/core/src/CMakeLists.txt")):
        if not NAME_SETTING.search(without_comments(directory.read_text())):
            problems.append(
                f"{where(directory)}: a core library file that states no name, so the check "
                f"cannot tell which core it builds."
            )

    return sorted(edges), problems


def load_baseline() -> set[tuple[str, str]]:
    if not BASELINE.exists():
        return set()
    data = json.loads(BASELINE.read_text())
    return {(entry["core"], entry["links"]) for entry in data.get("edges", [])}


def write_baseline(edges: list[ForbiddenEdge], accept_new: bool) -> int:
    """Records the edges, refusing to grow the list unless told to."""
    accepted = load_baseline()
    current = {(edge.core, edge.links) for edge in edges}
    new = current - accepted
    if new and not accept_new:
        for core, links in sorted(new):
            print(
                f"{core}: links {links}, which the baseline does not accept. "
                f"Pass --accept-new to record it, and say why in the task.",
                file=sys.stderr,
            )
        return 1

    data = {
        "description": (
            "Component cores that link another component's core or service, "
            "accepted until each edge is removed."
        ),
        "edges": [{"core": edge.core, "links": edge.links} for edge in edges],
    }
    BASELINE.write_text(json.dumps(data, indent=2) + "\n")
    return 0


def main(argv: list[str]) -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--list", action="store_true", help="Print every forbidden edge and exit.")
    parser.add_argument(
        "--write-baseline",
        action="store_true",
        help="Drop the entries whose edges are gone.",
    )
    parser.add_argument(
        "--accept-new",
        action="store_true",
        help="With --write-baseline, record an edge the baseline does not accept yet.",
    )
    args = parser.parse_args(argv)

    edges, problems = scan()

    if args.write_baseline:
        failures = write_baseline(edges, args.accept_new)
        if failures:
            return failures
        location = BASELINE.relative_to(REPO_ROOT) if BASELINE.is_relative_to(REPO_ROOT) else BASELINE
        print(f"Wrote {len(edges)} edge(s) to {location}.")
        return 0

    if args.list:
        for edge in edges:
            print(f"{edge.core} links {edge.links} ({edge.owner} {edge.kind})")
        print(f"{len(edges)} forbidden edge(s).")
        return 0

    baseline = load_baseline()
    current = {(edge.core, edge.links) for edge in edges}

    failures = 0
    for problem in problems:
        print(problem, file=sys.stderr)
        failures += 1

    for edge in edges:
        if (edge.core, edge.links) in baseline:
            continue
        print(
            f"{edge.core}: links {edge.links}, which is {edge.owner}'s {edge.kind} library. "
            f"A core may link another component's api contract, not its {edge.kind}.",
            file=sys.stderr,
        )
        failures += 1

    for core, links in sorted(baseline - current):
        print(
            f"{core}: the baseline accepts a link to {links}, and the link is gone. "
            f"Run --write-baseline to drop the entry.",
            file=sys.stderr,
        )
        failures += 1

    if failures:
        print(f"{failures} core link edge(s) failed the check.", file=sys.stderr)
        return 1

    print(f"Every component core links no other component's core or service ({len(current)} accepted).")
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
