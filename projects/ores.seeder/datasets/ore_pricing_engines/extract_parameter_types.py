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
    r'(?:(parse[A-Za-z0-9_]+)\s*(?:<[^>]*>)?\s*\(\s*)?'
    r'(?:[A-Za-z_][A-Za-z0-9_]*\.)*(engine|model|global)Parameter\("([A-Za-z0-9_.]+)"',
    re.S)
INNER = re.compile(r'&\s*(parse[A-Za-z0-9_]+)')
COMPARE = re.compile(r'(?:==|!=)\s*"([^"]+)"')
GLOBAL = re.compile(
    r'global(?:Engine)?Parameters(?:\(\)|_)?\s*(?:\.\s*(?:at|find|count)\(|\[)\s*"([A-Za-z0-9_.]+)"')
COMPARED_CALL = re.compile(
    r'Parameter\("([A-Za-z0-9_.]+)"\)\s*(?:==|!=)\s*"([^"]+)"')
TERNARY = re.compile(
    r'(engine|model)Parameter\(.{0,200}?\?\s*"([A-Za-z0-9_.]+)"\s*:\s*"([A-Za-z0-9_.]+)"',
    re.S)
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
                    parser, scope, name = match.group(1) or "", match.group(2), match.group(3)
                    tail = text[match.end():match.end() + 160]
                    kind, values = None, []
                    if parser == "parseListOfValues":
                        inner = INNER.search(tail)
                        if inner:
                            parser = inner.group(1)
                            kind, values = classify(parser, enums)
                    else:
                        kind, values = classify(parser, enums)
                    if kind is None:
                        compared = []
                        for literal in COMPARE.findall(tail):
                            if literal not in compared:
                                compared.append(literal)
                        if len(compared) >= 2:
                            kind, values = "enum", compared
                        else:
                            kind, values = "text", []
                    number = text.count("\n", 0, match.start()) + 1
                    found.setdefault(f"{scope}|{name}", {
                        "kind": kind,
                        "scope": scope,
                        "name": name,
                        "parser": parser,
                        "values": values,
                        "source_ref": f"{os.path.relpath(path, ore)}:{number}",
                    })
    for folder in SOURCES:
        for dirpath, _dirnames, filenames in os.walk(os.path.join(ore, folder)):
            for filename in filenames:
                if not filename.endswith((".cpp", ".hpp")):
                    continue
                path = os.path.join(dirpath, filename)
                text = open(path, encoding="utf-8", errors="replace").read()
                for match in GLOBAL.finditer(text):
                    name = match.group(1)
                    before = text[max(0, match.start() - 60):match.start()]
                    kind = "boolean" if ("parseBool(" in before or "matchesViaBoolOrRegex" in before) else "text"
                    number = text.count("\n", 0, match.start()) + 1
                    found.setdefault(f"global|{name}", {
                        "kind": kind, "scope": "global", "name": name, "parser": "",
                        "values": [],
                        "source_ref": f"{os.path.relpath(path, ore)}:{number}",
                    })
                for match in TERNARY.finditer(text):
                    scope, left, right = match.group(1), match.group(2), match.group(3)
                    before = text[max(0, match.start() - 40):match.start()]
                    kind = ("boolean" if "parseBool(" in before else
                            "integer" if "parseInteger(" in before else
                            "real" if "parseReal(" in before or "parseDouble(" in before else "text")
                    number = text.count("\n", 0, match.start()) + 1
                    for name in (left, right):
                        found.setdefault(f"{scope}|{name}", {
                            "kind": kind, "scope": scope, "name": name, "parser": "",
                            "values": [],
                            "source_ref": f"{os.path.relpath(path, ore)}:{number}",
                        })
    compared = {}
    for folder in SOURCES:
        for dirpath, _dirnames, filenames in os.walk(os.path.join(ore, folder)):
            for filename in filenames:
                if not filename.endswith((".cpp", ".hpp")):
                    continue
                text = open(os.path.join(dirpath, filename), encoding="utf-8",
                            errors="replace").read()
                for name, literal in COMPARED_CALL.findall(text):
                    compared.setdefault(name, [])
                    if literal not in compared[name]:
                        compared[name].append(literal)
    for name, literals in compared.items():
        entry = next((value for key, value in found.items() if value["name"] == name), None)
        if entry is None or len(literals) < 2:
            continue
        if {literal.lower() for literal in literals} == {"true", "false"}:
            entry["kind"], entry["values"] = "boolean", []
        elif entry["kind"] in ("text", "enum"):
            entry["kind"], entry["values"] = "enum", literals
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
    by_name = {}
    for key in sorted(found):
        entry = found[key]
        kind = entry["kind"]
        original = entry["name"]
        name = original
        scope = ("ModelParameters" if entry["scope"] == "model"
                 else "GlobalParameters" if entry["scope"] == "global"
                 else "EngineParameters")
        signature = (kind, tuple(entry["values"]))
        if original in by_name and by_name[original] != signature:
            name = f"{entry['scope']}_{original}"
        by_name.setdefault(original, signature)
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
        elif kind in ("integer", "real", "period"):
            set_code = slug(name)
            sets[set_code] = {
                "name": name.replace("_", " ").replace(".", " ").strip().capitalize(),
                "description": f"The values the {name} setting accepts.",
                "source_kind": {"integer": "integer", "real": "number", "period": "text"}[kind],
                "is_ordered": False,
            }
        elif kind == "enum":
            set_code = slug(name)
            sets[set_code] = {
                "name": name.replace("_", " ").replace(".", " ").strip().capitalize(),
                "description": f"The values the {name} setting accepts.",
                "source_kind": "enumerated",
                "is_ordered": False,
                "values": [
                    {
                        "value": value,
                        "label": value,
                        "description": f"{value} is one of the values the ORE code accepts for {original}.",
                        "order": index,
                    }
                    for index, value in enumerate(entry["values"])
                ],
            }
        elif kind == "text":
            set_code = slug(name)
            sets[set_code] = {
                "name": name.replace("_", " ").replace(".", " ").strip().capitalize(),
                "description": f"The values the {name} setting accepts.",
                "source_kind": "text",
                "is_ordered": False,
            }
        else:
            continue
        links[f"{scope}|{original}"] = set_code
    return sets, links, flags


def merge(path, sets, links):
    """Write the code's sets as their own curation file.

    The guide's sets and the code's sets stay apart: the guide's file is edited
    by a person, this one is rewritten on every extraction, and the seed
    generator composes them with the code over the guide. Writing a whole file
    keeps a re-extraction from leaving a stale link behind.
    """
    with open(path, "w", encoding="utf-8") as handle:
        json.dump({"sets": sets, "links": links}, handle, indent=2,
                  sort_keys=True, ensure_ascii=False)
        handle.write("\n")
    flags = sum(1 for set_code in sets if sets[set_code]["source_kind"] == "boolean")
    print(f"{len(sets)} sets ({flags} flags) and {len(links)} links written to {path}")


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
