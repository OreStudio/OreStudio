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

# The classification module sits beside this script, so it is imported after the
# path is fixed rather than at the top of the file.
from route_classification import (  # noqa: E402
    AREAS,
    COMPOSED,
    EXCEPTIONS,
    KINDS,
    NO_AREA,
    RULES,
    VIEWS,
)
from route_pairing import FAMILIES, PAIRS  # noqa: E402

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
composed view without stating its owner, computation and cache policy. It also fails
when a pair in =build/scripts/route_pairing.py= names a route that neither server
answers, and when the coverage section does not match the pairs. The gate runs it as
the =route-inventory= check.

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


def line_of(text, offset):
    """The 1-based line an offset into @p text falls on."""
    return text.count("\n", 0, offset) + 1


def bff_routes():
    """The routes the BFF registers, with the module that registers them."""
    for path in sorted((ROOT / "projects/ores.web/packages/bff/src").rglob("*.ts")):
        if path.name.endswith(".test.ts"):
            continue
        text = path.read_text(errors="replace")
        for match in BFF_ROUTE.finditer(text):
            yield {
                "method": match.group(1),
                "path": match.group(3),
                "server": "bff",
                "owner": path.stem,
                "where": f"{relative(path)}:{line_of(text, match.start())}",
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
        for match in CPP_ROUTE.finditer(text):
            method, route = match.group(1), match.group(2)
            yield {
                "method": CPP_METHODS.get(method, method),
                "path": route,
                "server": "ores.http",
                "owner": parts[0],
                "where": f"{relative(path)}:{line_of(text, match.start())}",
                "note": "",
            }
        for match in CPP_ROUTE_VARIABLE.finditer(text):
            method, name = match.group(1), match.group(2)
            declared = declaring_line(text, name)
            yield {
                "method": CPP_METHODS.get(method, method),
                "path": "{" + name + "}",
                "server": "ores.http",
                "owner": parts[0],
                "where": f"{relative(path)}:{line_of(text, match.start())}",
                "note": f"path composed from {name}"
                        + (f" (declared at {declared})" if declared else "")
                        + ".",
            }


BFF_REGISTRATION = re.compile(r"\b(server|app|fastify)\.(get|post|put|patch|delete)\s*\(")
BFF_LITERAL = re.compile(r"\s*(['\"`])(/[^'\"`]*)\1")
CPP_ANY_REGISTRATION = re.compile(
    r"router->(get|post|put|patch|delete_|head|options)\(")


def unreadable_registrations(found):
    """A registration on either surface that the scanner cannot turn into a route.

    The scanner is the only thing that keeps the inventory complete, so a shape it
    cannot read is a route it silently drops. That is how six BFF routes stayed out of
    the table until a verifier counted registrations against the inventory: their path
    literal sat on the line after the call.
    """
    known = {(route["server"], route["method"], route["path"]) for route in found}
    unreadable = []

    for path in sorted((ROOT / "projects/ores.web/packages/bff/src").rglob("*.ts")):
        if path.name.endswith(".test.ts"):
            continue
        text = path.read_text(errors="replace")
        for match in BFF_REGISTRATION.finditer(text):
            literal = BFF_LITERAL.match(text, match.end())
            if literal is None:
                unreadable.append((f"{relative(path)}:{line_of(text, match.start())}",
                                   "the path is not a literal"))
            elif ("bff", match.group(2), literal.group(2)) not in known:
                unreadable.append((f"{relative(path)}:{line_of(text, match.start())}",
                                   "the route is not in the table"))

    for path in sorted(ROOT.glob("projects/ores.http/*/src/**/*.cpp")):
        if "/tests/" in path.as_posix():
            continue
        text = path.read_text(errors="replace")
        for match in CPP_ANY_REGISTRATION.finditer(text):
            where = f"{relative(path)}:{line_of(text, match.start())}"
            tail = text[match.end():match.end() + 120]
            literal = re.match(r'\s*"(/[^"]*)"', tail)
            variable = re.match(r"\s*([A-Za-z_][\w:]*)\s*[,)]", tail)
            if literal is not None:
                if ("ores.http", CPP_METHODS.get(match.group(1), match.group(1)),
                        literal.group(1)) not in known:
                    unreadable.append((where, "the route is not in the table"))
            elif variable is not None:
                if ("ores.http", CPP_METHODS.get(match.group(1), match.group(1)),
                        "{" + variable.group(1) + "}") not in known:
                    unreadable.append((where, "the route is not in the table"))
            else:
                unreadable.append((where, "the path is neither a literal nor a variable"))
    return unreadable


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
    """The area map section, one row per published area."""
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


CLIENT_ROOTS = (ROOT / "projects/ores.web/packages",)


def called_by(route_path):
    """The first client module that names @p route_path, or nothing.

    A composed view earns its place by answering a question a screen asks, so the table
    says which screen asks it. The scan stops at the first parameter, because a client
    builds those from values rather than naming them.
    """
    literal = route_path.split(":")[0]
    if len(literal) < 6:
        return ""
    for root in CLIENT_ROOTS:
        for path in sorted(root.rglob("*.ts")):
            if "/bff/" in path.as_posix() or path.name.endswith((".test.ts", ".d.ts")):
                continue
            if literal in path.read_text(errors="replace"):
                return path.relative_to(root).as_posix()
    return ""


def composed_views(found):
    """The composed views section, one row per view, or nothing when there are none."""
    views = {}
    for route in found:
        decision = decide(route)
        if decision is None or decision.kind != COMPOSED:
            continue
        entry = views.setdefault(decision.name, {"routes": [], "decision": decision})
        address = f"{route['method'].upper()} {route['path']}"
        # A view both servers answer at one address is listed once.
        if address not in entry["routes"]:
            entry["routes"].append(address)
    if not views:
        return []

    lines = ["* The composed views", ""]
    lines.append("A composed view owes four things, and the fourth is why the owner column")
    lines.append("exists: without it the view is the old BFF again. The name is the view's")
    lines.append("own; the schema is K1's to generate; the owner is the component that must")
    lines.append("answer it after the port, which is not always the one that answers it")
    lines.append("today.")
    lines.append("")
    lines.append("Every route here is one a screen calls now, and the =Called by= column says")
    lines.append("which client module calls it, so the port can prove a view with the same")
    lines.append("request against either server. A view keeps its current route until the")
    lines.append("cutover for that reason: moving it to the published namespace is a versioned")
    lines.append("change of its own, not part of the port.")
    lines.append("")
    lines.append("| View | Routes today | Called by | Owner | Computation | Cache policy |")
    lines.append("|------+--------------+-----------+-------+-------------+--------------|")
    for name in sorted(views):
        entry = views[name]
        owner, computation, cache = VIEWS.get(name, ("", "", ""))
        routes = ", ".join(sorted(entry["routes"]))
        caller = called_by(entry["routes"][0].split(" ", 1)[1]) or "no client calls it"
        lines.append(f"| {name} | {routes} | {caller} | {owner} | {computation} | {cache} |")
    lines.append("")
    return lines


REGISTRY_ARRAY = re.compile(r"^(?:export )?const (\w+)\b[^=]*= \[\n(.*?)^\];", re.MULTILINE | re.DOTALL)
REGISTRY_ENTRY = re.compile(r"^    \{\n(.*?)^    \},?$", re.MULTILINE | re.DOTALL)
# A group of entries spread into the array, each completed by the members a
# trailing map adds, such as the reference data lookups the synthetic screens read.
REGISTRY_SPREAD = re.compile(
    r"^    \.\.\.\[\n(.*?)^    \]\.map\([^\n]*\n(.*?)^    \}\)\),?$", re.MULTILINE | re.DOTALL)
SPREAD_ENTRY = re.compile(r"^        \{\n(.*?)^        \},?$", re.MULTILINE | re.DOTALL)


def members_of(body, indent):
    """An entry's own members, one per line from its first column: nested values are deeper."""
    return "\n".join(line[indent:] for line in body.splitlines()
                     if line.startswith(" " * indent) and line[indent:indent + 1] != " ")


def resource_of(members):
    """What one registry entry answers, from its members, or None when it names no key.

    The two registries state a resource in two forms. The reference data one
    names =keyFields=, =listBy= and =writable=; the synthetic one names one
    =keyField=, a =parentField= for a read by parent, and a =writes= schema for
    a resource a screen writes.
    """
    key = re.search(r"^key: '([^']+)'", members, re.MULTILINE)
    if key is None:
        return None
    fields = re.search(r"^keyFields: \[([^\]]*)\]", members, re.MULTILINE)
    writable = re.search(r"^writable: (true|false)", members, re.MULTILINE)
    named = lambda member: re.search(rf"^{member}:", members, re.MULTILINE) is not None
    return {
        "key": key.group(1),
        "every": True,
        "one_key": named("keyField") or (
            fields is not None and len(re.findall(r"'[^']+'", fields.group(1))) == 1),
        "list_by": named("listBy") or named("parentField"),
        "writable": writable.group(1) == "true" if writable is not None else named("writes"),
    }


def registry_resources(spec):
    """The resources a BFF registry declares, each with what it answers.

    The spec is a file and the name of the array it declares. Each entry states
    its key, how many fields name a row, whether it is read by a parent and
    whether a screen writes it, which decide which of a family's routes the
    resource must answer.
    """
    path, name = spec.rsplit(":", 1)
    text = (ROOT / path).read_text(encoding="utf-8")
    array = next((m.group(2) for m in REGISTRY_ARRAY.finditer(text) if m.group(1) == name), None)
    if array is None:
        return []
    found = []
    for entry in REGISTRY_ENTRY.finditer(array):
        found.append(resource_of(members_of(entry.group(1), 8)))
    for spread in REGISTRY_SPREAD.finditer(array):
        shared = members_of(spread.group(2), 8)
        for entry in SPREAD_ENTRY.finditer(spread.group(1)):
            found.append(resource_of(members_of(entry.group(1), 12) + "\n" + shared))
    return [resource for resource in found if resource is not None]


def family_targets(family):
    """The C++ routes a family pairs a BFF route with: one per resource that must answer it."""
    spec, method, template, needs = family
    return [(method, template.replace("{resource}", resource["key"]))
            for resource in registry_resources(spec) if resource[needs]]


def target_of(key):
    """The C++ equivalent of a BFF route as the pairing section prints it, or none."""
    if key in PAIRS:
        return " ".join(PAIRS[key])
    if key in FAMILIES:
        _, method, template, _ = FAMILIES[key]
        return f"{method} {template}, one per resource ({len(family_targets(FAMILIES[key]))})"
    return "none"


def coverage(found):
    """The coverage and pairing sections, counted from route_pairing."""
    per_area = {}
    rows = []
    for route in (r for r in found if r["server"] == "bff"):
        key = (route["method"].upper(), route["path"])
        decision = decide(route)
        area = decision.area if decision is not None else NO_AREA
        counts = per_area.setdefault(area, [0, 0])
        counts[0] += 1
        counts[1] += key in PAIRS or key in FAMILIES
        target = target_of(key)
        rows.append(f"| {key[0]} ={key[1]}= | {target} |")

    lines = ["* The coverage", ""]
    lines.append("How many BFF routes each area pairs with a C++ route, counted from")
    lines.append("=build/scripts/route_pairing.py=. A pair records the mapping and does not")
    lines.append("prove it; the port tasks prove each route against the answer the BFF gives.")
    lines.append("")
    lines.append("| Area | BFF routes | Paired | Gaps |")
    lines.append("|------+------------+--------+------|")
    for area in sorted(per_area, key=lambda a: (a == NO_AREA, a)):
        total, paired = per_area[area]
        lines.append(f"| {area} | {total} | {paired} | {total - paired} |")
    total = sum(counts[0] for counts in per_area.values())
    paired = sum(counts[1] for counts in per_area.values())
    lines.append(f"| total | {total} | {paired} | {total - paired} |")
    lines.extend(["", "* The pairing", ""])
    lines.append("| BFF route | C++ equivalent |")
    lines.append("|-----------+----------------|")
    lines.extend(rows)
    lines.append("")
    return lines


def bad_pairs(found):
    """The pairs that name a BFF route or a C++ route that neither server answers."""
    bff = {(r["method"].upper(), r["path"]) for r in found if r["server"] == "bff"}
    cpp = {(r["method"].upper(), r["path"]) for r in found if r["server"] != "bff"}
    problems = []
    for source, target in PAIRS.items():
        if source not in bff:
            problems.append(f"{' '.join(source)}: the BFF does not answer this route")
        if target not in cpp:
            problems.append(f"{' '.join(source)}: ores.http does not answer {' '.join(target)}")
    for source, family in FAMILIES.items():
        if source not in bff:
            problems.append(f"{' '.join(source)}: the BFF does not answer this route")
        targets = family_targets(family)
        if not targets:
            problems.append(f"{' '.join(source)}: {family[0]} declares no resource")
        for target in targets:
            if target not in cpp:
                problems.append(f"{' '.join(source)}: ores.http does not answer {' '.join(target)}")
    return problems


def render(found):
    bff = sum(1 for route in found if route["server"] == "bff")
    lines = [HEADER.rstrip(), ""]
    lines.extend(area_map(found))
    lines.extend(composed_views(found))
    lines.extend(coverage(found))
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
    unreadable = unreadable_registrations(found)
    unpublished = bad_decisions()
    broken_pairs = bad_pairs(found)
    stale_coverage = ("\n".join(coverage(found)) not in INVENTORY.read_text()
                      if INVENTORY.exists() else True)
    if not (missing or stale or disagreeing or duplicates or undecided or unreadable
            or unpublished or broken_pairs or stale_coverage):
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
    if unreadable:
        print(f"{len(unreadable)} registration(s) cannot be read into the table, so the "
              f"inventory may be short:")
        for where, reason in unreadable[:15]:
            print(f"  {where}: {reason}")
        if len(unreadable) > 15:
            print(f"  and {len(unreadable) - 15} more")
    if unpublished:
        print(f"{len(unpublished)} decision(s) name no published kind or area, or carry "
              f"no note:")
        for name, problem in unpublished[:15]:
            print(f"  {name}: {problem}")
        if len(unpublished) > 15:
            print(f"  and {len(unpublished) - 15} more")
    if broken_pairs:
        print(f"{len(broken_pairs)} pair(s) in route_pairing name a route neither server "
              f"answers:")
        for problem in broken_pairs:
            print(f"  {problem}")
    if stale_coverage:
        print("the coverage and pairing sections do not match route_pairing")
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
