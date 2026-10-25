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
"""Write the populate script of the ORE parameter value sets.

The curated data lives in the seeder's curation file, one entry per set and one
per link, with the catalogues a set may draw on. This writes the four tables'
rows: the catalogues, the sets, the values and the setting links.

    generate_value_sets.py          write the populate script
    generate_value_sets.py --check  fail if the committed script differs
"""

import argparse
import json
import os
import sys

ROOT = os.path.abspath(os.path.join(os.path.dirname(__file__), "..", "..", ".."))
GUIDE = os.path.join(ROOT, "projects", "ores.seeder", "datasets", "ore_pricing_engines",
                     "curation", "value-sets.json")
CODE = os.path.join(ROOT, "projects", "ores.seeder", "datasets", "ore_pricing_engines",
                    "curation", "value-sets-code.json")
OUT = os.path.join(ROOT, "projects", "ores.sql", "populate", "reporting",
                   "reporting_ore_parameter_value_sets_populate.sql")

TENANT = "ores_utility_system_tenant_id_fn()"
ACTOR = "'ores_reporting_service', 'system.initial_load'"
SEED = "Seed parameter value sets"

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
    if value is None or value == "":
        return "null"
    if isinstance(value, bool):
        return "true" if value else "false"
    if isinstance(value, (int, float)):
        return str(value)
    return "'" + str(value).replace("'", "''") + "'"


def write(label, table, columns, rows, where):
    values = ",\n".join("    (" + ", ".join(r) + ")" for r in rows)
    return f"""\\echo '--- {label} ---'

insert into {table} (
    {columns}
) values
{values}
on conflict (tenant_id, code)
where valid_to = ores_utility_infinity_timestamp_fn()
do nothing;

select '{label}' as entity, count(*) as count
from {table}
where valid_to = ores_utility_infinity_timestamp_fn();
"""


def rows_of(data):
    catalogues, sets, values, links = [], [], [], []
    registry = data["catalogues"]
    if isinstance(registry, list):
        registry = {entry["code"]: entry for entry in registry}
    for code, entry in sorted(registry.items()):
        catalogues.append([
            q(code), TENANT, "0", q(entry["name"]), q(entry["relation"]),
            q(entry["description"]), q(entry.get("is_active", True)), ACTOR, q(SEED)])
    for code, entry in sorted(data["sets"].items()):
        sets.append([
            q(code), TENANT, "0", q(entry["name"]), q(entry["description"]),
            q(entry["source_kind"]), q(entry.get("catalogue_code")),
            q(entry.get("minimum")), q(entry.get("maximum")), q(entry.get("pattern")),
            q(entry.get("is_ordered", False)), ACTOR, q(SEED)])
        for value in entry.get("values", []):
            values.append([
                q(f"{code}:{value['value']}"), TENANT, "0", q(code), q(value["value"]),
                q(value["label"]), q(value["description"]), str(value["order"]),
                q(value.get("is_active", True)), ACTOR, q(SEED)])
    for key, set_code in sorted(data["links"].items()):
        scope, _, name = key.partition("|")
        links.append([
            q(f"{scope}:{name}"), TENANT, "0", q(scope), q(name), q(set_code),
            ACTOR, q(SEED)])
    return catalogues, sets, values, links


CONTRACT_SOURCES = {
    "vocabulary": os.path.join(ROOT, "projects", "ores.seeder", "datasets",
                               "ore_pricing_engines", "pricing_vocabulary.json"),
    "code": os.path.join(ROOT, "projects", "ores.seeder", "datasets",
                         "ore_pricing_engines", "curation", "settings-code.json"),
    "globals": os.path.join(ROOT, "projects", "ores.seeder", "datasets",
                            "ore_pricing_engines", "curation", "globals.json"),
}


def contract_pairs():
    """Every parameter the contract holds, as the (scope, name) its rows use."""
    pairs = set()
    with open(CONTRACT_SOURCES["vocabulary"], encoding="utf-8") as handle:
        vocabulary = json.load(handle)
    for entry in vocabulary.values():
        for pair in entry.get("pairs", []):
            for setting in pair.get("settings", []):
                pairs.add((scope_of(setting["scope"]), setting["name"]))
    with open(CONTRACT_SOURCES["code"], encoding="utf-8") as handle:
        for name, entry in json.load(handle).items():
            pairs.add((scope_of(entry["scope"]), name))
    with open(CONTRACT_SOURCES["globals"], encoding="utf-8") as handle:
        for name in json.load(handle):
            pairs.add(("GlobalParameters", name))
    return {pair for pair in pairs if pair[1]}


def scope_of(scope):
    return {"model": "ModelParameters", "engine": "EngineParameters",
            "global": "GlobalParameters", "GlobalParameters": "GlobalParameters",
            "ModelParameters": "ModelParameters",
            "EngineParameters": "EngineParameters"}.get(scope, scope)


def resolve(data):
    """Give every contract parameter its set, failing on one that has none."""
    linked = {}
    for key, set_code in data["links"].items():
        scope, _, name = key.partition("|")
        linked.setdefault((scope, name), set_code)
    by_name = {}
    for (scope, name), set_code in linked.items():
        by_name.setdefault(name, set()).add(set_code)
    problems = []
    for scope, name in sorted(contract_pairs()):
        if (scope, name) in linked:
            continue
        candidates = by_name.get(name, set())
        if not candidates:
            folded = name.lower()
            candidates = {set_code for other, set_code in by_name.items()
                          if other.lower() == folded}
            if len(candidates) == 1:
                candidates = set(candidates)
        if len(candidates) == 1:
            data["links"][f"{scope}|{name}"] = next(iter(candidates))
        elif len(candidates) > 1:
            problems.append(f"{scope}|{name} could be {sorted(candidates)}")
        else:
            problems.append(f"{scope}|{name} has no value set")
    contract = contract_pairs()
    for key in [key for key in data["links"]
                if (key.partition("|")[0], key.partition("|")[2]) not in contract]:
        del data["links"][key]
    named = set(data["links"].values())
    for code in [code for code in data["sets"] if code not in named]:
        del data["sets"][code]
    return problems


def load():
    """The guide's sets, with the code's sets over the top.

    The guide states some choices; the code states a setting's type for every
    setting. A code set replaces the guide's set of the same name. A guide set no
    setting names after that is dropped, because its link now points at the set
    the code states.
    """
    with open(GUIDE, encoding="utf-8") as handle:
        data = json.load(handle)
    with open(CODE, encoding="utf-8") as handle:
        code = json.load(handle)
    data["sets"].update(code["sets"])
    data["links"].update(code["links"])
    named = set(data["links"].values())
    for name in [code_ for code_ in data["sets"] if code_ not in named]:
        del data["sets"][name]
    return data


def validate(data):
    problems = []
    for code, entry in data["sets"].items():
        kind = entry["source_kind"]
        if kind not in ("enumerated", "catalogue", "range", "regex", "boolean",
                        "integer", "number", "text"):
            problems.append(f"{code}: unknown source kind {kind}")
        registry = data["catalogues"]
        if isinstance(registry, list):
            registry = {catalog: None for catalog in registry}
        if kind == "catalogue" and entry.get("catalogue_code") not in registry:
            problems.append(f"{code}: names a catalogue that is not registered")
        if kind == "enumerated" and not entry.get("values"):
            problems.append(f"{code}: enumerated with no values")
    named = set(data["links"].values())
    for code in data["sets"]:
        if code not in named:
            problems.append(f"{code}: no setting names it")
    for key, set_code in data["links"].items():
        if set_code not in data["sets"]:
            problems.append(f"{key}: names an unknown set {set_code}")
    return problems


def render(data):
    catalogues, sets, values, links = rows_of(data)
    return (
        f"""{LICENCE}
/**
 * Parameter Value Sets Population Script
 *
 * The allowed values of an ORE parameter: the enumerated sets, the catalogues a
 * set may draw on, and the setting each set belongs to. Generated by
 * projects/ores.sql/utility/generate_value_sets.py from the curation file.
 * Do not edit it by hand. This script is idempotent.
 */

"""
        + write("Value Set Catalogues", "ores_reporting_ore_parameter_value_set_catalogues_tbl",
                "code, tenant_id, version, name, relation, description, is_active, modified_by, "
                "change_reason_code, change_commentary", catalogues, "catalogue")
        + write("Value Sets", "ores_reporting_ore_parameter_value_sets_tbl",
                "code, tenant_id, version, name, description, source_kind, catalogue_code, "
                "minimum, maximum, pattern, is_ordered, modified_by, change_reason_code, "
                "change_commentary", sets, "set")
        + write("Value Set Values", "ores_reporting_ore_parameter_value_set_values_tbl",
                "code, tenant_id, version, value_set_code, value, label, description, "
                "display_order, is_active, modified_by, change_reason_code, change_commentary",
                values, "value")
        + write("Setting Value Sets", "ores_reporting_ore_parameter_setting_value_sets_tbl",
                "code, tenant_id, version, scope, name, value_set_code, modified_by, "
                "change_reason_code, change_commentary", links, "link"))


def main():
    parser = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    parser.add_argument("--check", action="store_true",
                        help="fail if the committed script differs from the data")
    args = parser.parse_args()
    data = load()
    problems = resolve(data)
    problems = problems + validate(data)
    if problems:
        print("\n".join(problems), file=sys.stderr)
        return 1
    content = render(data)
    if args.check:
        current = open(OUT, encoding="utf-8").read() if os.path.exists(OUT) else ""
        if current != content:
            print(f"{OUT} differs from the curation; regenerate it.", file=sys.stderr)
            return 1
        print(f"{len(data['sets'])} sets and {len(data['links'])} links match the script.")
        return 0
    with open(OUT, "w", encoding="utf-8") as handle:
        handle.write(content)
    print(f"Wrote {OUT}: {len(data['sets'])} sets, {len(data['links'])} links")
    return 0


if __name__ == "__main__":
    sys.exit(main())
