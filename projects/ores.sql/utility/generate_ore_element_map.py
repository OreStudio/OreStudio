# -*- mode: python; tab-width: 4; indent-tabs-mode: nil -*-
"""Writes the one ORE element map: the leaf, and the columns that carry it.

The map says which value set governs a leaf; the child says which projection
columns carry that leaf. Both are seeded here, from what the tree already
knows, so a leaf's rule is stated once and a guard can reach it from the row it
is about to write.

Two sources name the columns: the curve configuration projection (123 leaves,
212 columns) and the convention mappers (176 leaves, 270 columns).

The value set of a leaf comes from the parser the ORE source reads it with,
through the same curation the value set seed is written from: the rule of a leaf
and the rule of the setting that shares its name are one fact, so neither is
derived from the other. A leaf no ORE source read names states =text=, and the
row says so and names the extractor gap, which is what =UNREAD_ALLOWED= holds
down.

=seeded()= reads a populate script back, one row per column, so the evidence
report can be written from the committed data rather than from a second
derivation of it.

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
import generate_value_sets as value_sets  # noqa: E402
import convention_value_sets as convention_sets  # noqa: E402
import curve_value_sets as curve_sets  # noqa: E402

MAP_JSON = os.path.join(ROOT, "projects", "ores.sql", "utility",
                        "ore_parameter_vocabularies.json")
CHILD = os.path.join(ROOT, "projects", "ores.sql", "populate", "reporting",
                     "reporting_ore_element_map_columns_populate.sql")
MAP = os.path.join(ROOT, "projects", "ores.sql", "populate", "reporting",
                   "reporting_ore_parameter_vocabularies_populate.sql")

TENANT = "ores_utility_system_tenant_id_fn()"
ACTOR = "'ores_reporting_service', 'system.initial_load'"
ELEMENT = re.compile(r"/([^/\[]+)(?:\[[^\]]*\])?$")

# The leaves no ORE source read names, so no parser states a rule for them. A
# projection column carries each of them, so the count is a coverage gap and not
# a choice: the map states text for a value ORE may well constrain. The number
# may only fall. It falls when extract_curve_terms.py learns a form of read it
# does not know yet; it rises when a projection column adds a leaf the extractor
# cannot read, which is a rule stated as nothing.
UNREAD_ALLOWED = 28

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


def leaf_rules():
    """The value set every leaf the ORE sources read takes, by address.

    The rule of a leaf and the rule of the setting that names it are one fact, so
    this reads the curation the value sets are seeded from rather than deriving a
    second answer from the parameter contract. A leaf the ORE sources read is
    governed by the parser they read it with; a leaf no source read names is not
    here, and map_rows() states that gap rather than inventing a rule.

    Each rule is a row's worth of facts: the set, the name a person reads, where
    the rule comes from, the file and line the ORE code reads the leaf at, and
    why the set holds that rule, so a reviewer can check it.
    """
    data, problems = value_sets.resolved()
    if problems:
        raise ValueError("\n".join(problems))
    links, sets = data["links"], data["sets"]
    found = {}
    for section, term in curve_sets.terms():
        code = links.get(f"{section}|{term['name']}")
        if not code:
            continue
        address = "curveconfig.xsd", "/CurveConfiguration/" + "/".join(term["path"])
        found.setdefault(address, describe(code, sets[code], term))
    for element, convention in terms_extracted()["conventions"].items():
        for term in convention["terms"]:
            code = links.get(f"{element}|{term['name']}")
            if not code:
                continue
            leaf = term["name"]
            if term.get("list_item"):
                leaf = f"{term['name']}/{term['list_item']}"
            address = "conventions.xsd", "/Conventions/" + "/".join(term["path"] + [leaf])
            _, overridden = convention_sets.override_of(element, term)
            found.setdefault(address, describe(code, sets[code], term, overridden))
    return found


def describe(code, entry, term, overridden=None):
    """Why one leaf takes one set, in the words a reviewer can check."""
    kind, parser = entry["source_kind"], term.get("parser")
    element, spelling = term["name"], term.get("type")
    if overridden:
        reason = overridden
    elif kind == "catalogue":
        reason = (f"ORE reads {element} with {parser}, which accepts the spellings "
                  f"reference data holds, so the set names that catalogue instead of "
                  f"copying it.")
    elif kind == "enumerated":
        reason = (f"ORE reads {element} with {parser}, and the set lists every value "
                  f"that parser's map accepts.")
    elif kind == "text" and spelling == "calendar":
        reason = (f"ORE parses {element} with {parser}, and a calendar may be a joint "
                  f"calendar such as US,MWK that no catalogue row holds, so the set "
                  f"states text.")
    elif kind == "text" and parser:
        reason = (f"ORE parses {element} with {parser}, and this tree holds no "
                  f"catalogue of the names it accepts, so the set states text.")
    elif kind == "text":
        reason = (f"ORE reads {element} and never parses it, so no vocabulary governs "
                  f"the value and the set states text.")
    else:
        reason = (f"ORE reads {element} as {spelling or kind}, and the set states the "
                  f"values that kind admits.")
    return {"set": code, "name": entry["name"], "source": "ore_code",
            "source_ref": term.get("source_ref", ""), "reason": reason}


def unread_reason(row):
    """Why a leaf the map carries holds no rule yet.

    A leaf arrives in the map because a projection column carries it. Not every
    such leaf is one the term extractor knows how to read, and a leaf it does not
    read has no parser to take a rule from. The gap is stated here, counted by
    coverage() and listed by the report the check prints, so it cannot grow
    unnoticed.
    """
    return (f"No ORE source read names {row['element']} as a term: the extractor "
            f"does not know the form this leaf is read in, so no parser states a "
            f"rule for it and the set states text. Listed by the element map "
            f"coverage report.")


def terms_extracted():
    """The convention terms, as extract_convention_terms.py wrote them."""
    with open(terms.TERMS, encoding="utf-8") as handle:
        return json.load(handle)


def coverage(rows):
    """The leaves the map states a rule for, and the ones it does not."""
    unread = [row for row in rows if not row["ruled"]]
    text = [row for row in rows if row["set"] == "text"]
    return unread, text

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
            map_code = f"{row['schema']}|{row['xpath']}"
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
    """The map: one row per leaf address, with the set that governs it."""
    with open(MAP_JSON, encoding="utf-8") as handle:
        declared_json = json.load(handle)
    declared = {(entry["schema"], entry["element"]): entry
                for entry in declared_json.values()}

    sets = leaf_rules()
    found = {}
    for row in columns:
        found.setdefault(row["map_code"], row)

    out = {}
    for code, row in found.items():
        declared_row = declared.get((row["schema"], row["element"]))
        if declared_row:
            out[code] = {
                "code": code,
                "schema": row["schema"],
                "xpath": row["xpath"],
                "element": row["element"],
                "set": declared_row["value_set"],
                "name": declared_row["name"],
                "source": declared_row["source"],
                "source_ref": declared_row["source_ref"],
                "reason": declared_row["reason"],
                "ruled": True,
            }
            continue
        rule = sets.get((row["schema"], row["xpath"]))
        out[code] = {
            "code": code,
            "schema": row["schema"],
            "xpath": row["xpath"],
            "element": row["element"],
            "set": rule["set"] if rule else "text",
            "name": rule["name"] if rule else row["element"],
            "source": rule["source"] if rule else "ore_schema",
            "source_ref": rule["source_ref"] if rule
                          else f"external/ore/xsd/{row['schema']}",
            "reason": rule["reason"] if rule else unread_reason(row),
            "ruled": rule is not None,
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
            q(row["code"]), TENANT, "0", q(row["schema"]), q(row["xpath"]),
            q(row["element"]), q(row["set"]), q(row["name"]), q(row["source"]),
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
    code, tenant_id, version, schema_name, xpath, element_name, value_set_code,
    name, source, source_ref, reason, modified_by, change_reason_code,
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

    unread, text = coverage(rows)
    print(f"{len(rows)} leaves: {len(rows) - len(text)} state a rule, "
          f"{len(text)} state text.", file=sys.stderr)
    print(f"{len(unread)} of the leaves name no term any ORE source reads:", file=sys.stderr)
    for row in sorted(unread, key=lambda r: (r["schema"], r["xpath"])):
        print(f"  {row['schema']}|{row['xpath']}", file=sys.stderr)
    if len(unread) > UNREAD_ALLOWED:
        print(f"{len(unread)} leaves name no term, which is above the {UNREAD_ALLOWED} "
              "the map allows: teach the extractor the form the leaf is read in, or "
              "state the rule another way.", file=sys.stderr)
        return 1
    if args.check:
        print(f"{len(rows)} leaves over {len(columns)} columns.")
        return 0
    print(f"Wrote {CHILD} and {MAP}: {len(rows)} leaves, {len(columns)} columns")
    return 0


if __name__ == "__main__":
    sys.exit(main())
