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
per link, with the catalogues a set may draw on. This writes two scripts: the four tables'
rows: the catalogues, the sets, the values and the setting links.

    generate_value_sets.py          write the populate script
    generate_value_sets.py --check  fail if the committed script differs
"""

import argparse
import json
import os
import sys

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import convention_value_sets  # noqa: E402
import curve_value_sets  # noqa: E402
import setup_value_sets  # noqa: E402

ROOT = os.path.abspath(os.path.join(os.path.dirname(__file__), "..", "..", ".."))
GUIDE = os.path.join(ROOT, "projects", "ores.seeder", "datasets", "ore_pricing_engines",
                     "curation", "value-sets.json")
CODE = os.path.join(ROOT, "projects", "ores.seeder", "datasets", "ore_pricing_engines",
                    "curation", "value-sets-code.json")
REVIEW = os.path.join(ROOT, "projects", "ores.seeder", "datasets", "ore_pricing_engines",
                      "curation", "value-sets-review.json")
GUIDE_ENUMS = os.path.join(ROOT, "projects", "ores.seeder", "datasets", "ore_pricing_engines",
                           "curation", "value-sets-guide-enums.json")
OUT = os.path.join(ROOT, "projects", "ores.sql", "populate", "reporting",
                   "reporting_ore_parameter_value_sets_populate.sql")
VOCABULARIES_OUT = os.path.join(ROOT, "projects", "ores.sql", "populate", "reporting",
                                "reporting_ore_parameter_vocabularies_populate.sql")

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


def vocabulary_rows():
    """One insert row per declared vocabulary, from the declaration file."""
    declared = curve_value_sets.declared_vocabularies()
    rows = []
    for code, entry in sorted(declared.items()):
        if entry["source"] not in curve_value_sets.SOURCES:
            raise ValueError(f"{code}: source {entry['source']} is not one of "
                             f"{', '.join(curve_value_sets.SOURCES)}")
        rows.append([
            q(code), TENANT, "0",
            q(entry["schema"]), q(entry["element"]), q(entry["value_set"]),
            q(entry["name"]), q(entry["source"]), q(entry["source_ref"]),
            q(entry["reason"]), ACTOR, q(SEED),
        ])
    return rows


def render_vocabularies():
    rows = vocabulary_rows()
    return (
        f"""{LICENCE}
/**
 * Parameter Vocabulary Population Script
 *
 * The ORE element names whose values are a closed list, and the value set each
 * one takes. Generated by projects/ores.sql/utility/generate_value_sets.py from
 * =ore_parameter_vocabularies.json=. Do not edit it by hand. This script is
 * idempotent.
 */

"""
        + write("Parameter Vocabularies", "ores_reporting_ore_parameter_vocabularies_tbl",
                "code, tenant_id, version, schema_name, element_name, value_set_code, name, "
                "source, source_ref, reason, modified_by, change_reason_code, "
                "change_commentary", rows, "vocabulary"))


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
        # A link is curated, never inferred. The same setting name in
        # another scope may want another set, and a guess would state a rule
        # nobody chose.
        problems.append(f"{scope}|{name} has no value set, and a link is curated, not inferred")
    contract = contract_pairs()
    for key in [key for key in data["links"]
                if (key.partition("|")[0], key.partition("|")[2]) not in contract]:
        del data["links"][key]
    named = set(data["links"].values())
    for code in [code for code in data["sets"] if code not in named]:
        del data["sets"][code]
    return problems


def keep_guide_catalogues(guide_sets, data):
    """Let a guide set that names a catalogue stand over the code's set.

    The code states a setting's type for every setting and cannot state where
    its choices come from: a parser sees a string. Only the guide names a
    catalogue, so its set replaces the code's set of the same name and states
    no values, because the relation the catalogue names holds them.
    """
    for name, entry in guide_sets.items():
        if entry.get("source_kind") != "catalogue" or name not in data["sets"]:
            continue
        data["sets"][name] = dict(entry)


def review(path):
    """A reviewed override, read only when it names its provenance.

    A person settles a disagreement between the guide, the code and the data in
    a file of its own, and the file states the reason, the source, the source
    reference, the curator and the date once for every entry it holds. A file
    that states none of them is a mistake, not a default.
    """
    with open(path, encoding="utf-8") as handle:
        data = json.load(handle)
    missing = [key for key in ("reason", "source", "source_ref", "curator", "date")
               if not str(data.get(key) or "").strip()]
    if missing:
        raise SystemExit(f"{path} has no {', '.join(missing)}")
    if data["source"] not in ("ore_guide", "ore_code", "authored", "seed"):
        raise SystemExit(f"{path} has an unknown source {data['source']!r}")
    return data


def load():
    """The guide's sets, with the code's sets over the top.

    The guide states some choices; the code states a setting's type for every
    setting. A code set replaces the guide's set of the same name, except where
    the guide names a catalogue. A guide set no setting names after that is
    dropped, because its link now points at the set the code states. A reviewed
    file is applied last, because a person settling a disagreement is the last
    word: the review file corrects a type the contract states, and the guide's
    enumerated sets correct a type the code could only read as a string.
    """
    with open(GUIDE, encoding="utf-8") as handle:
        data = json.load(handle)
    guide_sets = dict(data["sets"])
    with open(CODE, encoding="utf-8") as handle:
        code = json.load(handle)
    data["sets"].update(code["sets"])
    data["links"].update(code["links"])
    keep_guide_catalogues(guide_sets, data)
    for path in (REVIEW, GUIDE_ENUMS):
        reviewed = review(path)
        data["links"].update(reviewed.get("links", {}))
        data["sets"].update(reviewed.get("sets", {}))
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


def add_conventions(data):
    """Add the convention terms' sets after the pricing links are resolved.

    They are added last so the name fallback in resolve never reaches a convention
    set, and the pricing rows stay what they were. A code or a link that pricing
    already holds is a clash, not an override.
    """
    extra = convention_value_sets.data()
    problems = [f"{code}: a convention set clashes with an existing set"
                for code in extra["sets"] if code in data["sets"]]
    problems += [f"{key}: names {code}, which no set holds"
                 for key, code in extra["links"].items()
                 if code not in extra["sets"] and code not in data["sets"]]
    problems += [f"{key}: a convention link clashes with an existing link"
                 for key in extra["links"] if key in data["links"]]
    registry = data["catalogues"]
    if isinstance(registry, list):
        registry = {entry["code"]: entry for entry in registry}
    registry.update(extra["catalogues"])
    data["catalogues"] = registry
    for code, entry in extra["sets"].items():
        data["sets"].setdefault(code, entry)
    data["links"].update(extra["links"])
    return problems


def add_curves(data):
    """Add the curve configuration leaves' sets after the convention ones.

    A curve leaf's set comes from a catalogue, a curated parser link, or its
    type, so a code or a link that already exists is a clash, not an override.
    """
    extra = curve_value_sets.data()
    problems = [f"{code}: a curve set clashes with an existing set"
                for code in extra["sets"] if code in data["sets"]]
    problems += [f"{key}: names {code}, which no set holds"
                 for key, code in extra["links"].items()
                 if code not in extra["sets"] and code not in data["sets"]]
    problems += [f"{key}: a curve link clashes with an existing link"
                 for key in extra["links"] if key in data["links"]]
    registry = data["catalogues"]
    if isinstance(registry, list):
        registry = {entry["code"]: entry for entry in registry}
    registry.update(extra["catalogues"])
    data["catalogues"] = registry
    for code, entry in extra["sets"].items():
        data["sets"].setdefault(code, entry)
    data["links"].update(extra["links"])
    return problems


def add_setup(data):
    """Add the engine run settings' sets, after the curve ones.

    A setup setting is drawn by the engine settings screen and takes the =Setup=
    scope, so its set and link cannot clash with a pricing one: a clash is a
    mistake in the curation, not an override.
    """
    extra = setup_value_sets.data()
    problems = [f"{code}: a setup set clashes with an existing set"
                for code in extra["sets"] if code in data["sets"]]
    problems += [f"{key}: names {code}, which no set holds"
                 for key, code in extra["links"].items()
                 if code not in extra["sets"] and code not in data["sets"]]
    problems += [f"{key}: a setup link clashes with an existing link"
                 for key in extra["links"] if key in data["links"]]
    problems += setup_value_sets.problems()
    registry = data["catalogues"]
    if isinstance(registry, list):
        registry = {entry["code"]: entry for entry in registry}
    registry.update(extra["catalogues"])
    data["catalogues"] = registry
    for code, entry in extra["sets"].items():
        data["sets"].setdefault(code, entry)
    data["links"].update(extra["links"])
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
    problems = problems + add_conventions(data)
    problems = problems + add_curves(data)
    problems = problems + add_setup(data)
    text = curve_value_sets.text_of()
    print(f"{len(text)} curve configuration leaves take text, for example "
          f"{', '.join(text[:3])}.", file=sys.stderr)
    for name, scopes in curve_value_sets.catalogue_scopes().items():
        print(f"{name} is matched in {len(scopes)} sections: {', '.join(scopes)}.",
              file=sys.stderr)
    problems = problems + validate(data)
    if problems:
        print("\n".join(problems), file=sys.stderr)
        return 1
    content = render(data)
    vocabularies = render_vocabularies()
    for path, text in ((OUT, content), (VOCABULARIES_OUT, vocabularies)):
        if args.check:
            current = open(path, encoding="utf-8").read() if os.path.exists(path) else ""
            if current != text:
                print(f"{path} differs from the declaration; regenerate it.", file=sys.stderr)
                return 1
        else:
            with open(path, "w", encoding="utf-8") as handle:
                handle.write(text)
    if args.check:
        print(f"{len(data['sets'])} sets, {len(data['links'])} links and "
              f"{len(vocabulary_rows())} vocabularies match the scripts.")
        return 0
    print(f"Wrote {OUT}: {len(data['sets'])} sets, {len(data['links'])} links")
    print(f"Wrote {VOCABULARIES_OUT}: {len(vocabulary_rows())} vocabularies")
    return 0


if __name__ == "__main__":
    sys.exit(main())
