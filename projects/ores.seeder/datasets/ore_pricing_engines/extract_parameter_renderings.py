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
"""Name every ORE parameter whose reader accepts more than one rendering.

A rendering is a string a document states for a value whose meaning is one of a
closed set. ORE accepts more than one rendering in three ways, and this script
reads all three out of the engine's own source:

- an alias map, where several strings map to one enumerator;
- a case-insensitive comparison, where the parser upper-cases its input before
  comparing, so any casing of the same word is accepted;
- the writer, =operator<<=, which states the spelling the engine itself emits.
  That spelling is the canonical form a store should keep.

The result is evidence for curation, not a rule: a person reads the table, and
the value set records the canonical form it adopts.

    python3 extract_parameter_renderings.py <ore-checkout> <out.json>
"""

import json
import os
import re
import sys

SOURCES = ("OREData/ored", "OREAnalytics/orea")

CALL = re.compile(
    r'(?:(parse[A-Za-z0-9_]+)\s*(?:<[^>]*>)?\s*\(\s*)?'
    r'(?:[A-Za-z_][A-Za-z0-9_]*\.)*(engine|model|global)Parameter\("([A-Za-z0-9_.]+)"',
    re.S)
DEF = re.compile(r'\b(parse[A-Za-z0-9_]+)\s*\(\s*const\s+(?:std::)?string\s*&\s*([A-Za-z0-9_]+)\s*\)\s*\{')
WRITER = re.compile(
    r'operator\s*<<\s*\([^)]*\b([A-Za-z0-9_:]+)\s*&\s*[A-Za-z0-9_]+\s*\)\s*\{|'
    r'(?:std::)?string\s+to_string\s*\(\s*const\s+([A-Za-z0-9_:]+)\s*&')
COMPARE = re.compile(r'(?:to_upper_copy|to_lower_copy)?\s*\(?\s*[A-Za-z0-9_]+\s*\)?\s*==\s*"([^"]+)"')
MAP_KEY = re.compile(r'"([^"]+)"\s*,')
UPPER = re.compile(r'to_(?:upper|lower)_copy')
ENUM_RETURN = re.compile(r'return\s+([A-Za-z0-9_:]+)::([A-Za-z0-9_]+)\s*;')
WRITTEN = re.compile(r'oss\s*<<\s*"([^"]+)"')
BRANCH = re.compile(r'([A-Za-z0-9_:]+)::([A-Za-z0-9_]+)\s*\)')


def body_at(text, brace):
    """The text of the block that starts at this brace, with its braces matched."""
    depth = 0
    for index in range(brace, len(text)):
        if text[index] == '{':
            depth += 1
        elif text[index] == '}':
            depth -= 1
            if depth == 0:
                return text[brace:index + 1]
    return text[brace:]


def sources(ore):
    """Every file the engine may define a parser or a writer in."""
    for folder in SOURCES:
        for dirpath, _dirnames, filenames in os.walk(os.path.join(ore, folder)):
            for filename in filenames:
                if filename.endswith((".cpp", ".hpp")):
                    yield os.path.join(dirpath, filename)


def parsers(ore):
    """The renderings each parser accepts, whether it ignores case, and its enum."""
    found = {}
    for path in sources(ore):
        text = open(path, encoding="utf-8", errors="replace").read()
        for match in DEF.finditer(text):
            name = match.group(1)
            body = body_at(text, text.index('{', match.end() - 1))
            renderings = []
            for literal in COMPARE.findall(body):
                if literal not in renderings:
                    renderings.append(literal)
            for key in MAP_KEY.findall(body):
                if key not in renderings and ' ' not in key:
                    renderings.append(key)
            enumerated = ENUM_RETURN.search(body)
            if not renderings and not enumerated:
                continue
            found.setdefault(name, {
                "parser": name,
                "origin": os.path.relpath(path, ore),
                "renderings": renderings,
                "ignores_case": bool(UPPER.search(body)),
                "enum": f"{enumerated.group(1)}::{enumerated.group(2)}" if enumerated else "",
            })
    return found


def writers(ore):
    """The spelling the engine writes for each enumerator of each type."""
    written = {}
    for path in sources(ore):
        text = open(path, encoding="utf-8", errors="replace").read()
        for match in WRITER.finditer(text):
            opened = match.end() - 1
            if text[opened] != '{':
                continue
            body = body_at(text, opened)
            for branch in re.finditer(r'([A-Za-z0-9_:]+)::([A-Za-z0-9_]+)', body):
                enum = branch.group(1)
                rest = body[branch.end():branch.end() + 200]
                spelling = WRITTEN.search(rest)
                if spelling:
                    written.setdefault(enum, {}).setdefault(branch.group(2), spelling.group(1))
    return written


def parameters(ore, parser_map):
    """Every parameter, with the parsers its call sites name."""
    found = {}
    for path in sources(ore):
        text = open(path, encoding="utf-8", errors="replace").read()
        for match in CALL.finditer(text):
            parser, scope, name = match.group(1) or "", match.group(2), match.group(3)
            if not parser or parser not in parser_map:
                continue
            entry = parser_map[parser]
            key = f"{scope}|{name}"
            existing = found.setdefault(key, {
                "scope": scope, "name": name, "parsers": [],
                "renderings": [], "ignores_case": False, "origin": entry["origin"],
            })
            if parser not in existing["parsers"]:
                existing["parsers"].append(parser)
            for rendering in entry["renderings"]:
                if rendering not in existing["renderings"]:
                    existing["renderings"].append(rendering)
            existing["ignores_case"] = existing["ignores_case"] or entry["ignores_case"]
    return found


def census(ore):
    """The parameters that accept more than one rendering, with the canonical form."""
    parser_map = parsers(ore)
    written = writers(ore)
    rows = []
    for key, entry in sorted(parameters(ore, parser_map).items()):
        renderings = entry["renderings"]
        if len(renderings) < 2 and not entry["ignores_case"]:
            continue
        canonical = []
        for parser in entry["parsers"]:
            enum = parser_map[parser]["enum"]
            enum_type = enum.rsplit("::", 1)[0] if enum else ""
            for enumerator, spelling in sorted(written.get(enum_type, {}).items()):
                if spelling not in canonical:
                    canonical.append(spelling)
        rows.append({
            "scope": entry["scope"],
            "name": entry["name"],
            "parsers": entry["parsers"],
            "renderings": renderings,
            "ignores_case": entry["ignores_case"],
            "canonical": canonical,
            "origin": entry["origin"],
        })
    return rows


def main():
    if len(sys.argv) != 3:
        print(__doc__.strip().splitlines()[-1], file=sys.stderr)
        return 2
    rows = census(sys.argv[1])
    with open(sys.argv[2], "w") as out:
        json.dump({"parameters": rows}, out, indent=2, sort_keys=True)
        out.write("\n")
    ignoring = sum(1 for row in rows if row["ignores_case"])
    without_canonical = sum(1 for row in rows if not row["canonical"])
    print(f"{len(rows)} parameter(s) accept more than one rendering")
    print(f"  {ignoring} ignore case")
    print(f"  {without_canonical} have no writer this scan found")
    return 0


if __name__ == "__main__":
    sys.exit(main())
