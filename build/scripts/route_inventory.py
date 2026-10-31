#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""The inventory of every route the two HTTP surfaces answer.

The classification work needs a list that cannot be short. This reads the routes
from the code that registers them -- the BFF's Fastify calls and ores.http's
router calls -- and writes one table, with the design's four-way decision beside
each route.

The decisions themselves live in build/scripts/route_classification.py, as rules
and exceptions over the route's method and path. This tool applies them: `--write`
rewrites every Kind, Area and Note cell from them, and `--check` fails when the
table and the decisions disagree, or when the table and the tree do.
"""

import argparse
import pathlib
import re
import sys
from collections import Counter

sys.path.insert(0, str(pathlib.Path(__file__).resolve().parent))

from route_classification import (  # noqa: E402
    AREAS,
    COMPOSED,
    EXCEPTIONS,
    KINDS,
    NO_AREA,
    RULES,
    VIEWS,
)

ROOT = pathlib.Path(__file__).resolve().parents[2]
INVENTORY = ROOT / "doc/knowledge/architecture/http_route_inventory.org"

BFF_ROUTE = re.compile(r"\.(get|post|put|patch|delete)\(\s*(['\"`])(/api/[^'\"`]*)\2")
CPP_ROUTE = re.compile(
    r"router->(get|post|put|patch|delete_|head|options)\(\s*\"(/[^\"]*)\"")
# A path the code composes is still a route: the storage capability builds its
# two patterns from a prefix constant. The row names the variable and the line
# that declares it, so the list stays complete and a person can read the path.
CPP_ROUTE_VARIABLE = re.compile(
    r"router->(get|post|put|patch|delete_|head|options)\(\s*([A-Za-z_][\w:]*)\s*[,)]")

CPP_METHODS = {
    "delete_": "delete",
}

HEADER = r"""\
#+title: The HTTP route inventory
#+description: Every route the BFF and ores.http answer today, read from the code that registers them, decided against the design's four-way rule, with the published area map and the composed views' obligations.
#+type: knowledge
#+level: cross
#+filetags: :knowledge:architecture:http:
#+created: 2026-10-31
#+updated: 2026-10-31

Every HTTP route either server answers, read from its own registration rather than
typed by hand. The design's four-way rule (a resource, a composed view, a transport
capability, or a rendering concern that is not an API) decides every row, and the
=Kind=, =Area= and =Note= columns state that decision.

* How to regenerate

#+begin_src sh :results verbatim
python3 build/scripts/route_inventory.py --write
#+end_src

The decisions are data in =build/scripts/route_classification.py=: an ordered list
of rules over the method and path, an exception for a route no rule decides, and the
owner, computation and cache policy each composed view owes. A regeneration rewrites
every cell from them, so the table states the decisions and never holds a second copy
of them.

Two sections are derived from the same decisions and appear before the table: the
area map, which is the published namespace of design section 11.2 and the areas this
table publishes beside it, and the composed views, whose owner column names the
component that must answer each one after the port.

* How it is enforced

=python3 build/scripts/route_inventory.py --check= fails when a route answered by
either server is absent from this table, when a row names a route neither server
answers, when two routes share one identity, when a route is unclassified, and when a
decision names no published kind or area, carries no evidence, or calls itself a
composed view without stating its owner, computation and cache policy. The gate runs
it as the =route-inventory= check.

* What it reads, and what it cannot

The BFF declares every route with a literal path beginning =/api/=, so a route is a
call whose first argument is that literal. A path built from a template literal in
these sources is a =Map= lookup and not a route; none registers a route that way
today.

ores.http declares its routes with =router->get/post/put/patch/delete_/head=, and
five of them state a path the code composes from a prefix constant; those rows name
the variable and the line that declares it.

What neither shape covers is a route registered some other way: a generated route
table (K1), or a loop that registers a family. When such a route appears, the answer
is to teach this tool that shape, not to add a row by hand: the row would not be
verified.
"""


def relative(path):
    return path.relative_to(ROOT).as_posix()


def bff_routes():
    """The routes the BFF registers, with the module that registers them."""
    for path in sorted((ROOT / "projects/ores.web/packages/bff/src").rglob("*.ts")):
        if path.name.endswith(".test.ts"):
            continue
        text = path.read_text(errors="replace")
        for line_number, line in enumerate(text.splitlines(), start=1):
            for method, _, route in BFF_ROUTE.findall(line):
                yield {
                    "method": method,
                    "path": route,
                    "server": "bff",
                    "owner": path.stem,
                    "where": f"{relative(path)}:{line_number}",
                }


def declaring_line(text, name):
    """The line that declares @p name as a string in this file, or nothing."""
    declaration = re.compile(
        r"^\s*(?:const\s+)?(?:auto|std::string|std::string_view)\s+" + re.escape(name) + r"\s*=")
    for line_number, line in enumerate(text.splitlines(), start=1):
        if declaration.search(line):
            return line_number
    return None


def cpp_routes():
    """The routes ores.http registers, with the component that registers them."""
    pattern = "projects/ores.http/*/src/**/*.cpp"
    for path in sorted(ROOT.glob(pattern)):
        if "/tests/" in path.as_posix():
            continue
        text = path.read_text(errors="replace")
        parts = path.relative_to(ROOT / "projects/ores.http").parts
        for line_number, line in enumerate(text.splitlines(), start=1):
            for method, route in CPP_ROUTE.findall(line):
                yield {
                    "method": CPP_METHODS.get(method, method),
                    "path": route,
                    "server": "ores.http",
                    "owner": parts[0],
                    "where": f"{relative(path)}:{line_number}",
                    "note": "",
                }
            for method, name in CPP_ROUTE_VARIABLE.findall(line):
                declared = declaring_line(text, name)
                yield {
                    "method": CPP_METHODS.get(method, method),
                    "path": "{" + name + "}",
                    "server": "ores.http",
                    "owner": parts[0],
                    "where": f"{relative(path)}:{line_number}",
                    "note": f"path composed from {name}"
                            + (f" (declared at {declared})" if declared else "")
                            + ".",
                }


def routes():
    found = list(bff_routes()) + list(cpp_routes())
    found.sort(key=lambda r: (r["path"], r["method"], r["server"]))
    return found


def table_cells(line):
    stripped = line.strip()
    if not stripped.startswith("|"):
        return None
    cells = [cell.strip() for cell in stripped.strip("|").split("|")]
    if len(cells) != 7 or cells[0] == "Kind":
        return None
    # The rule under the header is cells of dashes and plus signs; a data row has
    # a method in it.
    if all(cell and set(cell) <= set("-+: ") for cell in cells):
        return None
    return cells


def read_inventory():
    """The decisions the table states, keyed by (server, method, path)."""
    if not INVENTORY.exists():
        return {}
    rows = {}
    for line in INVENTORY.read_text().splitlines():
        cells = table_cells(line)
        if cells is None:
            continue
        kind, area, method, path, server, where, note = cells
        if not method or not path:
            continue
        rows[(server, method, path.strip("="))] = {
            "kind": kind, "area": area, "note": note, "where": where,
        }
    return rows


def matched(rule, target):
    shape, pattern = rule.match
    if shape == "exact":
        return target == pattern
    if shape == "prefix":
        return target.startswith(pattern)
    return re.search(pattern, target) is not None


def decide(route):
    """The rule or exception that decides one route, or None."""
    key = (route["server"], route["method"].upper(), route["path"])
    if key in EXCEPTIONS:
        return EXCEPTIONS[key]
    target = f"{route['method'].upper()} {route['path']}"
    for rule in RULES:
        if rule.server is not None and rule.server != route["server"]:
            continue
        if matched(rule, target):
            return rule
    return None


def cells_of(route, decision):
    """The Kind, Area and Note cells the page states for one route."""
    if decision is None:
        return "", "", route.get("note", "")
    note = " ".join(part for part in (route.get("note", ""), decision.note) if part)
    return decision.kind, decision.area, note


def area_map(found):
    """The areas the decisions publish, with their owners, sizes and kinds."""
    areas = {}
    for route in found:
        decision = decide(route)
        area = decision.area if decision is not None else ""
        entry = areas.setdefault(area, {"owners": set(), "routes": 0, "kinds": {}})
        entry["owners"].add(route["owner"])
        entry["routes"] += 1
        kind = decision.kind if decision is not None else "unclassified"
        entry["kinds"][kind] = entry["kinds"].get(kind, 0) + 1

    def order(area):
        return (area == NO_AREA, area)

    lines = ["* The area map", ""]
    lines.append("The published areas (design section 11.2) and the areas this table")
    lines.append("publishes beside them, each with the components that answer it today,")
    lines.append("its size, and the kinds its routes are decided as. A route that is not")
    lines.append("an API publishes no area, so it is counted under =(none)=.")
    lines.append("")
    lines.append("| Area | Owner components | Routes | Kinds |")
    lines.append("|------+------------------+--------+-------|")
    for area in sorted(areas, key=order):
        entry = areas[area]
        kinds = ", ".join(f"{kind} {count}"
                          for kind, count in sorted(entry["kinds"].items()))
        lines.append(f"| {area} | {', '.join(sorted(entry['owners']))} "
                     f"| {entry['routes']} | {kinds} |")
    lines.append("")
    return lines


def composed_views(found):
    """The four obligations each composed view owes, one row per view."""
    views = {}
    for route in found:
        decision = decide(route)
        if decision is None or decision.kind != COMPOSED:
            continue
        entry = views.setdefault(decision.name, {"routes": [], "decision": decision})
        entry["routes"].append(f"{route['method'].upper()} {route['path']}")
    if not views:
        return []

    lines = ["* The composed views", ""]
    lines.append("A composed view owes four things, and the fourth is why the owner column")
    lines.append("exists: without it the view is the old BFF again. The name is the view's")
    lines.append("own; the schema is K1's to generate; the owner is the component that must")
    lines.append("answer it after the port, which is not always the one that answers it")
    lines.append("today.")
    lines.append("")
    lines.append("| View | Routes | Owner | Computation | Cache policy |")
    lines.append("|------+--------+-------+-------------+--------------|")
    for name in sorted(views):
        entry = views[name]
        owner, computation, cache = VIEWS.get(name, ("", "", ""))
        routes = ", ".join(sorted(entry["routes"]))
        lines.append(f"| {name} | {routes} | {owner} | {computation} | {cache} |")
    lines.append("")
    return lines


def render(found):
    bff = sum(1 for route in found if route["server"] == "bff")
    lines = [HEADER.rstrip(), ""]
    lines.extend(area_map(found))
    lines.extend(composed_views(found))
    lines.append("* The inventory")
    lines.append("")
    lines.append(f"{len(found)} routes: {bff} answered by the BFF and "
                 f"{len(found) - bff} by ores.http.")
    lines.append("")
    lines.append("| Kind | Area | Method | Path | Server | Where | Note |")
    lines.append("|------+------+--------+------+--------+-------+------|")
    for route in found:
        kind, area, note = cells_of(route, decide(route))
        lines.append(
            f"| {kind} | {area} | {route['method']} "
            f"| ={route['path']}= | {route['server']} | {route['where']} "
            f"| {note} |")
    lines.append("")
    return "\n".join(lines)


def write():
    INVENTORY.parent.mkdir(parents=True, exist_ok=True)
    found = routes()
    INVENTORY.write_text(render(found))
    print(f"wrote {INVENTORY.relative_to(ROOT)}: {len(found)} route(s)")
    undecided = [r for r in found if decide(r) is None]
    if undecided:
        print(f"{len(undecided)} route(s) are unclassified and their cells are empty")
        return 1
    return 0


def bad_decisions():
    """The decisions that name no published kind or area, carry no evidence, or call
    themselves a composed view without stating the owner, computation and cache the
    design demands of one."""
    problems = []
    for decision in list(RULES) + list(EXCEPTIONS.values()):
        if decision.kind not in KINDS or decision.area not in AREAS + (NO_AREA,):
            problems.append((decision.name, f"{decision.kind!r} in {decision.area!r}"))
        if not decision.note.strip():
            problems.append((decision.name, "no note"))
        if decision.kind == COMPOSED:
            obligations = VIEWS.get(decision.name)
            if obligations is None:
                problems.append((decision.name, "composed view with no owner, "
                                                "computation or cache policy"))
            elif not all(part.strip() for part in obligations):
                problems.append((decision.name, "composed view with a blank obligation"))
    composed = {decision.name for decision in list(RULES) + list(EXCEPTIONS.values())
                if decision.kind == COMPOSED}
    for name in VIEWS:
        if name not in composed:
            problems.append((name, "states view obligations for no composed view"))
    return problems


def check():
    found = routes()
    in_table = read_inventory()
    current = {(r["server"], r["method"], r["path"]) for r in found}
    missing = [key for key in current if key not in in_table]
    stale = [key for key in in_table if key not in current]
    disagreeing = []
    for route in found:
        key = (route["server"], route["method"], route["path"])
        row = in_table.get(key)
        if row is None:
            continue
        kind, area, note = cells_of(route, decide(route))
        if (row["kind"], row["area"], row["note"]) != (kind, area, note):
            disagreeing.append((route, decide(route)))

    duplicates = [key for key, count in Counter(
        (r["server"], r["method"], r["path"]) for r in found).items() if count > 1]
    undecided = [route for route in found if decide(route) is None]
    unpublished = bad_decisions()
    if not (missing or stale or disagreeing or duplicates or undecided or unpublished):
        print(f"route inventory is current: {len(found)} route(s)")
        return 0

    def show(keys):
        for key in sorted(keys)[:15]:
            print(f"  {key[1].upper():6} {key[2]} ({key[0]})")
        if len(keys) > 15:
            print(f"  and {len(keys) - 15} more")

    if missing:
        print(f"{len(missing)} route(s) answered but absent from the inventory:")
        show(missing)
    if stale:
        print(f"{len(stale)} row(s) name a route neither server answers:")
        show(stale)
    if disagreeing:
        print(f"{len(disagreeing)} row(s) disagree with the classification:")
        for route, decision in disagreeing[:15]:
            state = decision.name if decision is not None else "unclassified"
            print(f"  {route['method'].upper():6} {route['path']} "
                  f"({route['server']}, {state})")
        if len(disagreeing) > 15:
            print(f"  and {len(disagreeing) - 15} more")
    if duplicates:
        print(f"{len(duplicates)} route(s) are registered more than once, so one table "
              f"row would stand for both:")
        show(duplicates)
    if undecided:
        print(f"{len(undecided)} route(s) are unclassified, so no rule says what they "
              f"are:")
        show([(r["server"], r["method"], r["path"]) for r in undecided])
    if unpublished:
        print(f"{len(unpublished)} decision(s) name no published kind or area, or carry "
              f"no note:")
        for name, problem in unpublished[:15]:
            print(f"  {name}: {problem}")
        if len(unpublished) > 15:
            print(f"  and {len(unpublished) - 15} more")
    print("run: python3 build/scripts/route_inventory.py --write")
    return 1



def main(argv):
    parser = argparse.ArgumentParser(description=__doc__)
    mode = parser.add_mutually_exclusive_group(required=True)
    mode.add_argument("--write", action="store_true",
                      help="Regenerate the inventory, filling every decision cell from "
                           "route_classification.")
    mode.add_argument("--check", action="store_true",
                      help="Fail when the inventory and the tree, or the inventory and the "
                           "classification, disagree.")
    args = parser.parse_args(argv)
    return write() if args.write else check()


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
