#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""The inventory of every route the two HTTP surfaces answer.

The classification work needs a list that cannot be short. This reads the routes
from the code that registers them -- the BFF's Fastify calls and ores.http's
router calls -- and writes one table. `--check` compares the table against the
tree, so a route added to either server without a row fails the gate.

The table carries the classification the design's rule asks for beside each
route. This tool never writes those columns: it keeps what it finds and leaves a
new route's cells empty, so the deciding stays with a person and the list stays
with the code.
"""

import argparse
import pathlib
import re
import sys

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
#+description: Every route the BFF and ores.http answer today, read from the code that registers them, with the design's classification beside each one.
#+type: knowledge
#+level: cross
#+filetags: :knowledge:architecture:http:
#+created: 2026-10-31
#+updated: 2026-10-31

Every HTTP route either server answers, read from its own registration rather than
typed by hand. The inventory exists so the classification has a complete list to
work from: the design's four-way rule (a resource, a composed view, a transport
capability, or a rendering concern that is not an API) is decided route by route in
the columns to the right, and a route without a =Kind= is one nobody has decided.

* How to regenerate

#+begin_src sh :results verbatim
python3 build/scripts/route_inventory.py --write
#+end_src

The tool keeps every =Kind=, =Area= and =Note= cell it finds and leaves a new
route's cells empty, so a regeneration never overwrites a decision.

* How it is enforced

=python3 build/scripts/route_inventory.py --check= fails when a route is answered
but absent from this table, and when the table names a route neither server
answers. The gate runs it as the =route-inventory= check.

* What it reads, and what it cannot

The BFF declares every route with a literal path beginning =/api/=, so a route is a
call whose first argument is that literal. A path built from a template literal in
these sources is a =Map= lookup and not a route; none registers a route that way
today.

ores.http declares its routes with =router->get/post/put/patch/delete_/head=, and
five of them state a path the code composes from a prefix constant; those rows name
the variable and the line that declares it.

What neither shape covers is a route registered some other way — a generated route
table (K1), or a loop that registers a family. When such a route appears, the answer
is to teach this tool that shape, not to add a row by hand: the row would not be
verified.

* The inventory
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
                            + (f" (declared at {declared})" if declared else ""),
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
    # a method in it, and an undecided route has an empty first cell.
    if all(cell and set(cell) <= set("-+: ") for cell in cells):
        return None
    return cells


def read_inventory():
    """The decisions already taken, keyed by what identifies a route."""
    if not INVENTORY.exists():
        return {}
    decisions = {}
    for line in INVENTORY.read_text().splitlines():
        cells = table_cells(line)
        if cells is None:
            continue
        kind, area, method, path, server, where, note = cells
        if not method or not path:
            continue
        key = (server, method, path.strip("="), where)
        decisions[key] = {"kind": kind, "area": area, "note": note}
    return decisions


def render(found):
    decisions = read_inventory()
    bff = sum(1 for route in found if route["server"] == "bff")
    lines = [HEADER.rstrip(), ""]
    lines.append(f"{len(found)} routes: {bff} answered by the BFF and "
                 f"{len(found) - bff} by ores.http.")
    lines.append("")
    lines.append("| Kind | Area | Method | Path | Server | Where | Note |")
    lines.append("|------+------+--------+------+--------+-------+------|")
    for route in found:
        key = (route["server"], route["method"], route["path"], route["where"])
        decision = decisions.get(key, {"kind": "", "area": "", "note": ""})
        note = " ".join(part for part in (route.get("note", ""), decision["note"]) if part)
        lines.append(
            f"| {decision['kind']} | {decision['area']} | {route['method']} "
            f"| ={route['path']}= | {route['server']} | {route['where']} "
            f"| {note} |")
    lines.append("")
    return "\n".join(lines)


def write():
    INVENTORY.parent.mkdir(parents=True, exist_ok=True)
    found = routes()
    INVENTORY.write_text(render(found))
    print(f"wrote {INVENTORY.relative_to(ROOT)}: {len(found)} route(s)")
    return 0


def check():
    found = routes()
    in_table = read_inventory()
    current = {(r["server"], r["method"], r["path"], r["where"]) for r in found}
    missing = [key for key in current if key not in in_table]
    stale = [key for key in in_table if key not in current]
    if not missing and not stale:
        print(f"route inventory is current: {len(found)} route(s)")
        return 0

    def show(keys):
        for key in sorted(keys)[:15]:
            print(f"  {key[1].upper():6} {key[2]} ({key[0]}, {key[3]})")
        if len(keys) > 15:
            print(f"  … and {len(keys) - 15} more")

    if missing:
        print(f"{len(missing)} route(s) answered but absent from the inventory:")
        show(missing)
    if stale:
        print(f"{len(stale)} row(s) name a route neither server answers:")
        show(stale)
    print("run: python3 build/scripts/route_inventory.py --write")
    return 1


def main(argv):
    parser = argparse.ArgumentParser(description=__doc__)
    mode = parser.add_mutually_exclusive_group(required=True)
    mode.add_argument("--write", action="store_true",
                      help="Regenerate the inventory, keeping the decisions it holds.")
    mode.add_argument("--check", action="store_true",
                      help="Fail when the inventory and the tree disagree.")
    args = parser.parse_args(argv)
    return write() if args.write else check()


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
