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
"""Write the populate script of the convention kind terms.

A term ties a column of a convention entity to the leaf of ORE's conventions
document that holds it. The pairs come from the ores.ore mapper, which reads and
writes the document, so a term names the column and the element the import and the
export already use: a read such as r.fixed_calendar = text_of(v, "FixedCalendar")
in map_swap is the term SWAP:swap_convention.fixed_calendar. A variable takes its
entity from its declaration, and a file takes its kind from its name.

The XPath of each element is the one convention_terms.json records for the kind's
element, which extract_convention_terms.py reads from the ORE source. A column the
mapper derives rather than reads, such as a pip factor computed from PointsFactor,
has no term.

    generate_convention_kind_terms.py          write the populate script
    generate_convention_kind_terms.py --check  fail if the committed script differs
"""

import argparse
import glob
import json
import os
import re
import sys

HERE = os.path.dirname(os.path.abspath(__file__))
ROOT = os.path.abspath(os.path.join(HERE, "..", "..", ".."))
MAPPERS = os.path.join(ROOT, "projects", "ores.ore", "core", "src", "domain")
TERMS = os.path.join(HERE, "convention_terms.json")
OUT = os.path.join(ROOT, "projects", "ores.sql", "populate", "refdata",
                   "refdata_convention_kind_terms_populate.sql")

SCHEMA_NAME = "conventions.xsd"

# The kind a mapper file serves, where its name does not say it.
FILE_KIND = {"conventions_fx_projection.cpp": "CURRENCY_PAIR"}

# A term whose element the mapper spells otherwise than the ORE parser reads it,
# with the leaf the parser reads and why. The contract is keyed by what ORE reads.
SPELLED_OTHERWISE = {
    "BMA_BASIS_SWAP:bma_basis_swap_convention.overnight_lockout_days": {
        "xpath": "/Conventions/BMABasisSwap/OvernightLockouDays",
        "reason": "ORE's fromXML reads OvernightLockouDays and its toXML writes "
                  "OvernightLockoutDays; the mapper uses the written spelling.",
    },
}

# A block of the document another ORE parser reads whole, so its parts are not
# leaves of the conventions parser and take no term, with why.
READ_ELSEWHERE = {
    ("INFLATION_SWAP", "PublicationSchedule"):
        "a ScheduleData block that ScheduleData::fromXML reads, not the conventions parser",
}

# The element a kind is written as, where no pack_family call names it.
ELEMENT_OF = {"TENOR_BASIS_SWAP": "TenorBasisSwap", "CURRENCY_PAIR": "FX"}

FUNCTION = re.compile(r"\n(?:refdata::domain::[\w:]+|[\w:]+)\s+conventions_mapper::map_\w+\([^)]*\)\s*\{"
                      r"(.*?)\n\}", re.S)
DECLARED = re.compile(r"refdata::domain::(\w+)\s+(\w+)\s*[;{=]")
ALIAS = re.compile(r"auto&\s+(\w+)\s*=\s*\w+\.(\w+)\s*;")
CHILD = re.compile(r'auto\s+(\w+)\s*=\s*\*?(\w+)(?:->|\.)child\("(\w+)"\)')
READ = re.compile(r'(?:(\w+)\.(\w+)\s*=\s*\w+\(\s*\*?(\w+),\s*"(\w+)"'
                  r'|assign\(\s*(\w+)\.(\w+),\s*\w+\(\s*\*?(\w+),\s*"(\w+)")')
PACK = re.compile(r'pack_family\(\s*bundle,\s*"(\w+)",\s*root\.children\("(\w+)"\)')

# The entity a member of a projection struct holds, where a mapper reaches an
# entity through an alias, as the FX mapper reaches the currency pair.
MEMBER_ENTITY = {"pair": "currency_pair", "convention": "currency_pair_convention"}

LICENCE = """/* -*- sql-product: postgres; tab-width: 4; indent-tabs-mode: nil -*-
 *
 * Copyright (C) 2026 Marco Craveiro <marco.craveiro@gmail.com>
 *
 * This program is free software; you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation; either version 3 of the License, or (at your option) any later
 * version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
 * details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51
 * Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */
"""


def q(value):
    return "'" + str(value).replace("'", "''") + "'"


def kind_of(path):
    name = os.path.basename(path)
    if name in FILE_KIND:
        return FILE_KIND[name]
    return name[len("conventions_family_"):-len(".cpp")].upper()


def mapper_terms():
    """Every (kind, entity, column, path below the kind's element) the mapper reads.

    A reader is the record of the convention, v, or a child of it the mapper
    takes with child("X"). A child reader is resolved where it is read, because
    one name is reused for different children, so a read names the element path
    it stands under.
    """
    elements = dict(ELEMENT_OF)
    found = []
    paths = sorted(glob.glob(os.path.join(MAPPERS, "conventions_family_*.cpp"))) + \
        [os.path.join(MAPPERS, name) for name in FILE_KIND]
    for path in paths:
        text = open(path, encoding="utf-8").read()
        kind = kind_of(path)
        for pack in PACK.finditer(text):
            elements[pack.group(1).upper()] = pack.group(2)
        for function in FUNCTION.finditer(text):
            body = function.group(1)
            types = {var: entity for entity, var in DECLARED.findall(body)}
            types.update({var: MEMBER_ENTITY[member] for var, member in ALIAS.findall(body)
                          if member in MEMBER_ENTITY})
            readers = [(0, "v", [])]

            def path_of(name, at):
                for start, reader, below in reversed(readers):
                    if reader == name and start <= at:
                        return below
                return None

            for child in CHILD.finditer(body):
                parent = path_of(child.group(2), child.start())
                if parent is not None:
                    readers.append((child.start(), child.group(1), parent + [child.group(3)]))
                    readers.sort(key=lambda r: r[0])
            for read in READ.finditer(body):
                groups = read.groups()
                var, column, reader, element = groups[:4] if groups[0] else groups[4:]
                below = path_of(reader, read.start())
                if var in types and below is not None:
                    found.append((kind, types[var], column, below + [element]))
    return found, elements


def xpaths(elements):
    """Every XPath of the conventions document, by kind."""
    with open(TERMS, encoding="utf-8") as handle:
        extracted = json.load(handle)["conventions"]
    out = {}
    for kind, element in elements.items():
        convention = extracted.get(element)
        if convention is None:
            continue
        for term in convention["terms"]:
            leaf = term["name"] + (f"/{term['list_item']}" if term.get("list_item") else "")
            out.setdefault(kind, set()).add("/Conventions/" + "/".join(term["path"] + [leaf]))
    return out


def rows():
    found, elements = mapper_terms()
    known = xpaths(elements)
    out, problems, seen = [], [], set()
    position = {}
    for kind, entity, column, below in found:
        code = f"{kind}:{entity}.{column}"
        if code in seen:
            continue
        seen.add(code)
        if (kind, below[0]) in READ_ELSEWHERE:
            continue
        xpath = "/Conventions/" + "/".join([elements.get(kind, "?")] + below)
        if code in SPELLED_OTHERWISE:
            xpath = SPELLED_OTHERWISE[code]["xpath"]
        elif xpath not in known.get(kind, set()):
            problems.append(f"{code}: {xpath} is not a leaf the ORE parser reads")
            continue
        out.append({"code": code, "kind": kind, "entity": entity, "column": column,
                    "xpath": xpath, "position": position.setdefault(kind, 0)})
        position[kind] += 1
    return out, problems


def render(all_rows):
    values = ",\n".join(
        f"    ({q(r['code'])}, ores_utility_system_tenant_id_fn(), 0, {q(r['kind'])}, "
        f"{q(r['entity'])}, {q(r['column'])}, {q(SCHEMA_NAME)}, {q(r['xpath'])}, "
        f"{r['position']}, 'ores_refdata_service', 'system.initial_load', "
        f"'Seed convention kind terms')"
        for r in all_rows)
    return f"""{LICENCE}
/**
 * Convention Kind Terms Population Script
 *
 * The terms of each convention kind: the entity and the column a value travels
 * under, and the leaf of ORE's conventions document that holds it, which names
 * the term's entry in the ORE parameter contract. Generated by
 * projects/ores.sql/utility/generate_convention_kind_terms.py from the ores.ore
 * mapper and convention_terms.json. Do not edit it by hand. This script is
 * idempotent.
 */

\\echo '--- Convention Kind Terms ---'

insert into ores_refdata_convention_kind_terms_tbl (
    code, tenant_id, version, kind_code, entity_name, column_name, schema_name, xpath,
    position, modified_by, change_reason_code, change_commentary
) values
{values}
on conflict (tenant_id, code)
where valid_to = ores_utility_infinity_timestamp_fn()
do nothing;

select 'Convention Kind Terms' as entity, count(*) as count
from ores_refdata_convention_kind_terms_tbl
where valid_to = ores_utility_infinity_timestamp_fn();
"""


def main():
    parser = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    parser.add_argument("--check", action="store_true",
                        help="fail if the committed script differs from the mapper")
    args = parser.parse_args()
    all_rows, problems = rows()
    if problems:
        print("\n".join(problems), file=sys.stderr)
        return 1
    content = render(all_rows)
    kinds = len({r["kind"] for r in all_rows})
    if args.check:
        current = open(OUT, encoding="utf-8").read() if os.path.exists(OUT) else ""
        if current != content:
            print(f"{OUT} differs from the mapper; regenerate it.", file=sys.stderr)
            return 1
        print(f"{len(all_rows)} terms of {kinds} kinds match the script.")
        return 0
    with open(OUT, "w", encoding="utf-8") as handle:
        handle.write(content)
    print(f"Wrote {OUT}: {len(all_rows)} terms of {kinds} kinds")
    return 0


if __name__ == "__main__":
    sys.exit(main())
