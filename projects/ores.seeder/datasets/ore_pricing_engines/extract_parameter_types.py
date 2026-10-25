# -*- mode: python; tab-width: 4; indent-tabs-mode: nil -*-
#
# Copyright (C) 2026 Marco Craveiro <marco.craveiro@gmail.com>
#
# This program is free software; you can redistribute it and/or modify it under
# the terms of the GNU General Public License as published by the Free Software
# Foundation; either version 3 of the License, or (at your option) any later
# version.
#
# This program is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
# FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
# details.
#
# You should have received a copy of the GNU General Public License along with
# this program; if not, write to the Free Software Foundation, Inc., 51
# Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
"""Read the type of every ORE parameter from the code that reads it.

The guide states a parameter's type for some settings and omits it for the rest.
The code always states it: a call site wraps its engineParameter or
modelParameter in a parser, and an enumerated parser maps the document's string
to a C++ enum, so the parser's map is the enum's own list of values.

The result is evidence for curation, not the seed: a person reviews it, and the
curation file records the value set with the parser and the enum as its source.

    python3 extract_parameter_types.py <ore-checkout> <out.json>
"""

import json
import os
import re
import sys

SOURCES = ("OREData/ored", "OREAnalytics/orea")
PARSERS = ("OREData/ored/utilities/parsers.cpp",
           "OREData/ored/utilities/parsers.hpp")

CALL = re.compile(
    r'(parse[A-Za-z0-9_]+)\s*(?:<[^>]*>)?\s*\((.{0,200}?)(engine|model)Parameter\("([A-Za-z0-9_.]+)"',
    re.S)
INNER = re.compile(r'&(parse[A-Za-z0-9_]+)')
DEF = re.compile(r'^\s*[A-Za-z_:<>0-9]+\s+(parse[A-Za-z0-9_]+)\s*\(\s*const\s+std::string&')
MAP_KEY = re.compile(r'"([^"]+)"\s*,')


def parser_values(ore):
    """The document strings each enumerated parser accepts, in its own order."""
    values = {}
    for relative in PARSERS:
        path = os.path.join(ore, relative)
        if not os.path.exists(path):
            continue
        lines = open(path, encoding="utf-8", errors="replace").read().splitlines()
        for index, line in enumerate(lines):
            match = DEF.match(line)
            if not match:
                continue
            name = match.group(1)
            if name in ("parseBool", "parseInteger", "parseReal", "parsePeriod",
                        "parseDate", "parseListOfValues", "parseString"):
                continue
            found = []
            for following in lines[index + 1:index + 40]:
                keys = MAP_KEY.findall(following)
                found.extend(keys)
                if found and "};" in following:
                    break
                if "QL_FAIL" in following:
                    break
            if found:
                values[name] = found
    return values


def types(ore):
    """Every parameter the code reads, with the parser that states its type."""
    enums = parser_values(ore)
    found = {}
    for folder in SOURCES:
        for dirpath, _dirnames, filenames in os.walk(os.path.join(ore, folder)):
            for filename in filenames:
                if not filename.endswith((".cpp", ".hpp")):
                    continue
                path = os.path.join(dirpath, filename)
                text = open(path, encoding="utf-8", errors="replace").read()
                for match in CALL.finditer(text):
                    parser, between = match.group(1), match.group(2)
                    scope, name = match.group(3), match.group(4)
                    if parser == "parseListOfValues":
                        inner = INNER.search(between)
                        if inner:
                            parser = inner.group(1)
                    kind, values = classify(parser, enums)
                    if kind is None:
                        continue
                    number = text.count("\n", 0, match.start()) + 1
                    found.setdefault(name, {
                        "kind": kind,
                        "scope": scope,
                        "parser": parser,
                        "values": values,
                        "source_ref": f"{os.path.relpath(path, ore)}:{number}",
                    })
    return found, enums


def classify(parser, enums):
    if parser == "parseBool":
        return "boolean", []
    if parser == "parseInteger":
        return "integer", []
    if parser in ("parseReal", "parseDouble"):
        return "real", []
    if parser == "parsePeriod":
        return "period", []
    if parser == "parseDate":
        return "date", []
    if parser in enums:
        return "enum", enums[parser]
    return None, []


def slug(name):
    return re.sub(r"[^a-z0-9]+", "_", name.lower()).strip("_")


def sets_of(found):
    """The value sets the code states: a flag for a boolean, the enum for an enum.

    Every setting the code types gets one set, and every set is named by the
    setting in its scope, so the curation is complete by construction and a
    person reviews labels rather than hunting for gaps.
    """
    sets = {}
    links = {}
    flags = 0
    for name in sorted(found):
        entry = found[name]
        kind = entry["kind"]
        scope = "ModelParameters" if entry["scope"] == "model" else "EngineParameters"
        if kind == "boolean":
            set_code = "flag"
            if set_code not in sets:
                sets[set_code] = {
                    "name": "Flag",
                    "description": "A switch that is either on or off.",
                    "source_kind": "boolean",
                    "is_ordered": False,
                }
            flags += 1
        elif kind == "enum":
            set_code = slug(name)
            values = [
                {
                    "value": value,
                    "label": value,
                    "description": f"{value} is one of the values the ORE code accepts for {name}.",
                    "order": index,
                }
                for index, value in enumerate(entry["values"])
            ]
            sets[set_code] = {
                "name": name.replace("_", " ").replace(".", " ").strip().capitalize(),
                "description": f"The values the {name} setting accepts.",
                "source_kind": "enumerated",
                "is_ordered": False,
                "values": values,
            }
        else:
            continue
        links[f"{scope}|{name}"] = set_code
    return sets, links, flags


def merge(curation_path, sets, links):
    """Merge the code's sets into the curation and report what changed.

    The code is authoritative for a setting's type, so a code set replaces the
    guide's set of the same name. A set no setting names is pruned: it cannot be
    seeded, and its link now points at the set the code states.
    """
    with open(curation_path, encoding="utf-8") as handle:
        curation = json.load(handle)
    before = set(curation["sets"])
    curation["sets"].update(sets)
    curation["links"].update(links)
    named = set(curation["links"].values())
    pruned = sorted(code for code in curation["sets"] if code not in named)
    for code in pruned:
        del curation["sets"][code]
    with open(curation_path, "w", encoding="utf-8") as handle:
        json.dump(curation, handle, indent=2, ensure_ascii=False)
        handle.write("\n")
    added = sorted(set(sets) - before)
    print(f"{len(added)} sets added, {len(pruned)} pruned, {len(curation['sets'])} total")
    for code in pruned:
        print(f"  pruned {code}: the code types its setting differently")


def main():
    if len(sys.argv) < 3:
        print(__doc__.strip().splitlines()[3], file=sys.stderr)
        return 2
    found, enums = types(sys.argv[1])
    with open(sys.argv[2], "w", encoding="utf-8") as handle:
        json.dump(found, handle, indent=2, sort_keys=True)
        handle.write("\n")
    if "--sets" in sys.argv:
        path = sys.argv[sys.argv.index("--sets") + 1]
        sets, links, flags = sets_of(found)
        with open(path, "w", encoding="utf-8") as handle:
            json.dump({"sets": sets, "links": links}, handle, indent=2, sort_keys=True)
            handle.write("\n")
        print(f"{len(sets)} sets ({flags} flag links) and {len(links)} links written to {path}")
    if "--merge" in sys.argv:
        path = sys.argv[sys.argv.index("--merge") + 1]
        sets, links, _flags = sets_of(found)
        merge(path, sets, links)
    by_kind = {}
    for entry in found.values():
        by_kind[entry["kind"]] = by_kind.get(entry["kind"], 0) + 1
    print(f"{len(found)} parameters typed from the code")
    for kind in sorted(by_kind):
        print(f"  {kind}: {by_kind[kind]}")
    print(f"{len(enums)} enumerated parsers read")
    return 0


if __name__ == "__main__":
    sys.exit(main())
