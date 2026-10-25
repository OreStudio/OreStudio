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
"""Report what the pricing engine screens do not document, in one run.

The screens read their words from the ore parameter contract, and the contract
is built from the ORE guide. A setting the ORE code reads but the guide does not
list has no text anywhere, so a screen shows that no documentation is held. This
script names those settings, the reverse case, and the product types without an
instrument, so a curation pass starts from a list rather than from a screen.

    python3 check_documentation_coverage.py <ore-checkout>
    python3 check_documentation_coverage.py <ore-checkout> --check

--check exits non-zero when the code reads a setting the contract does not
document, or when the contract names a setting the code does not read. It is for
a tree that has been curated, not for the run that produces the list.

The names are read from the string literals the code passes to engineParameter
and modelParameter. A name built at run time, such as a loop over the letters of
a currency, is not seen; the globals a configuration sets are read from their own
map and are documented by the global parameter curation.
"""

import json
import os
import re
import sys

HERE = os.path.dirname(os.path.abspath(__file__))
VOCABULARY = os.path.join(HERE, "pricing_vocabulary.json")
TYPES = os.path.join(HERE, "curation", "types.json")
CODE_SETTINGS = os.path.join(HERE, "curation", "settings-code.json")

CALL = re.compile(r'(?:engine|model)Parameter\("([A-Za-z0-9_.]+)"')
# A name the code chooses at run time, or reads from the parameter map, is not
# the first literal of a call, so the literals on such a line are read as names.
MAP_CALL = re.compile(r'(?:engine|model)Parameter[s]?_\.at\("([A-Za-z0-9_.]+)"')
LINE_CALL = re.compile(r'(?:engine|model)Parameter\(')
LINE_NAME = re.compile(r'"([A-Za-z0-9_.]+)"')
SOURCES = ("OREData/ored", "OREAnalytics/orea")


def read_json(path):
    with open(path, encoding="utf-8") as handle:
        return json.load(handle)


def code_settings(ore):
    """The parameter names the ORE code reads, and every name-like literal it holds.

    A name the code chooses at run time, or reaches through the parameter map,
    is not the literal of a call. Such a name still appears in the sources, so
    the second half of the answer is every name-like literal, and a name the
    contract holds is stale only when the code never mentions it.
    """
    found = {}
    literals = set()
    for folder in SOURCES:
        root = os.path.join(ore, folder)
        for dirpath, _dirnames, filenames in os.walk(root):
            for filename in filenames:
                if not filename.endswith((".cpp", ".hpp")):
                    continue
                path = os.path.join(dirpath, filename)
                with open(path, encoding="utf-8", errors="replace") as handle:
                    for number, line in enumerate(handle, start=1):
                        literals.update(LINE_NAME.findall(line))
                        names = CALL.findall(line) + MAP_CALL.findall(line)
                        if not names and LINE_CALL.search(line):
                            names = LINE_NAME.findall(line)[:1]
                        for name in names:
                            found.setdefault(name, []).append(
                                f"{os.path.relpath(path, ore)}:{number}")
    return found, literals


def guide_settings(vocabulary):
    """The setting names the contract holds, by scope, and the rows that carry them."""
    names = {}
    for type_code, entry in vocabulary.items():
        for pair in entry.get("pairs", []):
            for setting in pair.get("settings", []):
                names.setdefault(setting["scope"], set()).add(setting["name"])
    return names


def report(ore, check):
    vocabulary = read_json(VOCABULARY)
    types = read_json(TYPES)
    code, literals = code_settings(ore)
    documented = guide_settings(vocabulary)
    documented_names = set().union(*documented.values()) if documented else set()
    # A setting the guide does not list is documented when the curation states it.
    if os.path.exists(CODE_SETTINGS):
        documented_names |= set(read_json(CODE_SETTINGS))

    pairs = sum(len(entry.get("pairs", [])) for entry in vocabulary.values())
    settings = sum(len(pair.get("settings", []))
                   for entry in vocabulary.values() for pair in entry.get("pairs", []))
    documented_types = sum(1 for entry in types.values()
                           if str(entry.get("documentation", "")).strip() != "")

    print(f"Product types:           {len(types)} ({documented_types} documented)")
    print(f"Model and engine pairs:  {pairs}")
    print(f"Settings the guide lists: {len(documented_names)} distinct names, "
          f"{settings} rows")
    print(f"Parameter names the ORE code reads: {len(code)}")
    print()

    undocumented = sorted(name for name in code if name not in documented_names)
    print(f"UNDOCUMENTED: {len(undocumented)} parameter name(s) the code reads and the "
          f"guide does not list")
    for name in undocumented:
        where = code[name]
        print(f"  {name}  ({len(where)} site(s), e.g. {where[0]})")
    print()

    stale = sorted(name for name in documented_names
                   if name not in code and name not in literals)
    print(f"STALE: {len(stale)} name(s) the contract holds and the code does not read")
    for name in stale:
        scopes = ", ".join(sorted(scope for scope, names in documented.items() if name in names))
        print(f"  {name}  ({scopes})")
    print()

    unmapped = sorted(code_name for code_name, entry in types.items()
                      if entry.get("instrument") == "NONE")
    print(f"NO INSTRUMENT: {len(unmapped)} product type(s) with no instrument class")
    print("  " + ", ".join(unmapped) if unmapped else "  none")
    print()

    if check:
        return 1 if undocumented or stale else 0
    return 0


def main():
    if len(sys.argv) < 2:
        print(__doc__.strip().splitlines()[3], file=sys.stderr)
        return 2
    return report(sys.argv[1], "--check" in sys.argv[2:])


if __name__ == "__main__":
    sys.exit(main())
