# -*- mode: python; tab-width: 4; indent-tabs-mode: nil -*-
"""Writes the one ORE element map: the leaf, and the columns that carry it.

The map says which value set governs a leaf; the child says which projection
columns carry that leaf. Both are seeded here, from what the tree already
knows, so a leaf's rule is stated once and a guard can reach it from the row it
is about to write.

Two sources name the columns: the curve configuration projection (123 leaves,
212 columns) and the convention mappers (176 leaves, 270 columns). The value set
of a leaf comes from the parameter contract and its setting links, which is the
path the guards take today; a leaf the contract does not describe takes =text=,
so it arrives in the map as a decision instead of vanishing.

    generate_ore_element_map.py            write both populate scripts
    generate_ore_element_map.py --check    fail if either differs
"""
import argparse
import json
import os
import re
import sys

HERE = os.path.dirname(os.path.abspath(__file__))
ROOT = os.path.dirname(os.path.dirname(os.path.dirname(HERE)))
sys.path.insert(0, HERE)

import generate_curve_column_guards as curves  # noqa: E402
import generate_convention_kind_terms as terms  # noqa: E402

MAP_JSON = os.path.join(ROOT, "projects", "ores.sql", "utility",
                        "ore_parameter_vocabularies.json")
CONTRACT_SQL = os.path.join(ROOT, "projects", "ores.sql", "populate", "reporting",
                            "reporting_ore_parameters_artefact_populate.sql")
LINKS_SQL = os.path.join(ROOT, "projects", "ores.sql", "populate", "reporting",
                         "reporting_ore_parameter_value_sets_populate.sql")
CHILD = os.path.join(ROOT, "projects", "ores.sql", "populate", "reporting",
                     "reporting_ore_element_map_columns_populate.sql")
MAP = os.path.join(ROOT, "projects", "ores.sql", "populate", "reporting",
                   "reporting_ore_parameter_vocabularies_populate.sql")

TENANT = "ores_utility_system_tenant_id_fn()"
ACTOR = "'ores_reporting_service', 'system.initial_load'"
ELEMENT = re.compile(r"/([^/\[]+)(?:\[[^\]]*\])?$")

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
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51 Franklin
 * Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */
"""


def q(value):
    return "'" + str(value).replace("'", "''") + "'"


def split_values(body):
    """The tuples of an insert's values clause, as lists of strings."""
    rows, field, depth, quoted, i = [], [], 0, False, 0
    while i < len(body):
        ch = body[i]
        if quoted:
            if ch == "'" and i + 1 < len(body) and body[i + 1] == "'":
                field.append("'")
                i += 2
                continue
            if ch == "'":
                quoted = False
            else:
                field.append(ch)
        elif ch == "'":
            quoted = True
        elif ch == "(":
            depth += 1
            if depth == 1:
                field = []
        elif ch == ")":
            depth -= 1
            if depth == 0:
                rows.append("".join(field))
                field = []
                rest = body[i + 1:].lstrip().lower()
                if rest.startswith((";", "on ", "returning", "do ")):
                    break
        elif ch == "," and depth == 1:
            field.append("\x00")
        elif depth == 1:
            field.append(ch)
        i += 1
    return [[part.strip() for part in row.split("\x00")] for row in rows]


def seeded(path, table):
    """The rows of `insert into <table>` in a populate script, by column."""
    with open(path, encoding="utf-8") as handle:
        text = handle.read()
    header = re.search(r"insert into\s+" + table + r"\s*\(([^)]*)\)\s*values",
                       text, re.I)
    if not header:
        raise ValueError(f"{os.path.basename(path)} seeds no {table}")
    names = [n.strip().strip('"') for n in header.group(1).split(",")]
    return [dict(zip(names, values)) for values in split_values(text[header.end():])]


def contract_sets():
    """The value set each leaf takes, from the contract and its links."""
    links = {f"{r['scope']}:{r['name']}": r["value_set_code"]
             for r in seeded(LINKS_SQL,
                             "ores_reporting_ore_parameter_setting_value_sets_tbl")}
    found = {}
    for row in seeded(CONTRACT_SQL, "ores_dq_ore_parameters_artefact_tbl"):
        match = ELEMENT.search(row.get("xpath", ""))
        if not match:
            continue
        code = links.get(f"{row['scope']}:{row['name']}")
        if code:
            found.setdefault((row["schema_name"], match.group(1)), code)
    return found


def column_rows():
    """Every column that carries a leaf, from both projection families."""
    term_rows, problems = terms.rows()
    if problems:
        raise ValueError("\n".join(problems))
    families = (
        curves.rows(),
        (dict(row, schema="conventions.xsd",
              table=f"ores_refdata_{row['entity']}s_tbl") for row in term_rows),
    )
    found = {}
    for rows in families:
        for row in rows:
            match = ELEMENT.search(row["xpath"])
            if not match:
                raise ValueError(f"{row['code']}: no element in {row['xpath']}")
            element = match.group(1)
            map_code = f"{row['schema']}|{element}"
            code = f"{map_code}|{row['entity']}|{row['column']}"
            found[code] = {
                "code": code,
                "map_code": map_code,
                "schema": row["schema"],
                "element": element,
                "entity": row["entity"],
                "table": row["table"],
                "column": row["column"],
                "xpath": row["xpath"],
            }
    ordered = []
    seen = {}
    for code in sorted(found):
        row = found[code]
        row["position"] = seen.get(row["map_code"], 0)
        seen[row["map_code"]] = row["position"] + 1
        ordered.append(row)
    return ordered


def map_rows(columns):
    """The map: one row per leaf, with the set that governs it."""
    with open(MAP_JSON, encoding="utf-8") as handle:
        declared = json.load(handle)

    sets = contract_sets()
    found = {}
    for row in columns:
        found.setdefault(row["map_code"], row)

    out = {}
    for code, row in found.items():
        declared_row = declared.get(code)
        if declared_row:
            out[code] = {
                "code": code,
                "schema": row["schema"],
                "element": row["element"],
                "set": declared_row["value_set"],
                "name": declared_row["name"],
                "source": declared_row["source"],
                "source_ref": declared_row["source_ref"],
                "reason": declared_row["reason"],
            }
            continue
        leaf = (row["schema"], row["element"])
        code_of_set = sets.get(leaf)
        out[code] = {
            "code": code,
            "schema": row["schema"],
            "element": row["element"],
            "set": code_of_set or "text",
            "name": row["element"],
            "source": "ore_schema",
            "source_ref": f"external/ore/xsd/{row['schema']}",
            "reason": (
                f"The ORE schema declares {row['element']}, and the parameter "
                "contract states the value set its leaves take."
                if code_of_set else
                f"The ORE schema declares {row['element']} as a leaf no value "
                "set governs yet: it is text until a rule is stated, so it "
                "arrives here as a decision rather than as a silent pass."),
        }
    return [out[code] for code in sorted(out)]


def render_columns(columns):
    values = ",\n".join(
        "    (" + ", ".join([
            q(row["code"]), TENANT, "0", q(row["map_code"]), q(row["entity"]),
            q(row["table"]), q(row["column"]), q(row["xpath"]),
            str(row["position"]), ACTOR,
            q("Seed the ORE element map columns"),
        ]) + ")" for row in columns)
    return f"""{LICENCE}
/**
 * Ore Element Map Columns Population Script
 *
 * The projection columns that carry an ORE leaf, and the map row that states the
 * leaf's value set. Generated by
 * projects/ores.sql/utility/generate_ore_element_map.py from the curve
 * configuration projection, the convention mappers and the ORE schema. Do not
 * edit it by hand. This script is idempotent.
 */

insert into ores_reporting_ore_element_map_columns_tbl (
    code, tenant_id, version, element_map_code, entity_name, table_name,
    column_name, xpath, position, modified_by, change_reason_code,
    change_commentary
) values
{values}
on conflict (tenant_id, code)
where valid_to = ores_utility_infinity_timestamp_fn()
do nothing;

select 'Ore Element Map Columns' as entity, count(*) as count
from ores_reporting_ore_element_map_columns_tbl
where valid_to = ores_utility_infinity_timestamp_fn();
"""


def render_map(rows):
    values = ",\n".join(
        "    (" + ", ".join([
            q(row["code"]), TENANT, "0", q(row["schema"]), q(row["element"]),
            q(row["set"]), q(row["name"]), q(row["source"]),
            q(row["source_ref"]), q(row["reason"]), ACTOR,
            q("Seed the ORE element map"),
        ]) + ")" for row in rows)
    return f"""{LICENCE}
/**
 * ORE Element Map Population Script
 *
 * One row per ORE leaf: the value set that governs it, where the rule comes
 * from, and why. Generated by
 * projects/ores.sql/utility/generate_ore_element_map.py from the parameter
 * contract, its setting links and ore_parameter_vocabularies.json. Do not edit
 * it by hand. This script is idempotent.
 *
 * A leaf the contract does not describe takes =text=, so the map names every
 * leaf the guards check and a gap is a stated decision rather than a silent
 * pass.
 */

insert into ores_reporting_ore_parameter_vocabularies_tbl (
    code, tenant_id, version, schema_name, element_name, value_set_code, name,
    source, source_ref, reason, modified_by, change_reason_code,
    change_commentary
) values
{values}
on conflict (tenant_id, code)
where valid_to = ores_utility_infinity_timestamp_fn()
do nothing;

select 'ORE Element Map' as entity, count(*) as count
from ores_reporting_ore_parameter_vocabularies_tbl
where valid_to = ores_utility_infinity_timestamp_fn();
"""


def main():
    parser = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    parser.add_argument("--check", action="store_true",
                        help="fail if a committed script differs from the tree")
    args = parser.parse_args()

    columns = column_rows()
    rows = map_rows(columns)
    outputs = [(CHILD, render_columns(columns)), (MAP, render_map(rows))]
    for path, content in outputs:
        if args.check:
            current = (open(path, encoding="utf-8").read()
                       if os.path.exists(path) else "")
            if current != content:
                print(f"{path} differs from the tree; regenerate it.",
                      file=sys.stderr)
                return 1
        else:
            with open(path, "w", encoding="utf-8") as handle:
                handle.write(content)
    if args.check:
        print(f"{len(rows)} leaves over {len(columns)} columns.")
        return 0
    print(f"Wrote {CHILD} and {MAP}: {len(rows)} leaves, {len(columns)} columns")
    return 0


if __name__ == "__main__":
    sys.exit(main())
