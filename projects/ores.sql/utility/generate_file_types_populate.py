#!/usr/bin/env python3
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
#
"""Derive the ORE file types and their canonical names.

Every Setup parameter and every analytic parameter whose value is a file name is a
file type. The canonical name of the file is the name the official ORE user guide
gives it in its listings. A file the guide does not list takes the most common name
the 416 ORE samples give it, without a date, because a date is a value of the run and
not part of a name. A name that only a few samples use, and that differs from the name
the other analytics give the same input document, is not canonical, so the file takes
that common name. The count of samples that use the name is kept.

The guide is read from file_type_guide.json, which extract_ore_guide.py writes, so this
script does not need the guide. The XSD of an XML file is the schema in external/ore/xsd
that declares the root element of the file.

    generate_file_types_populate.py          write the seed
    generate_file_types_populate.py --check  fail if the seed no longer matches
"""

import argparse
import collections
import json
import os
import re
import sys
import xml.etree.ElementTree as ET

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import file_type_descriptions as texts  # noqa: E402

ROOT = os.path.abspath(os.path.join(os.path.dirname(__file__), "..", "..", ".."))
EXAMPLES = os.path.join(ROOT, "external", "ore", "examples")
XSD_DIR = os.path.join(ROOT, "external", "ore", "xsd")
GUIDE = os.path.join(os.path.dirname(os.path.abspath(__file__)), "file_type_guide.json")
SEED = os.path.join(ROOT, "projects", "ores.sql", "populate", "reporting",
                    "reporting_file_types_populate.sql")

FILE_VALUE = re.compile(r"\.(xml|csv|txt|json|dat|gz)$")
DATED = re.compile(r"\d{8}")

# The parameters that are stored choices of the configuration, not files of the package:
# the log file and the logging files.
CHOICES = {"logFile", "progressLogFile", "structuredLogFile"}

# The configuration type whose entities fill the document, where one exists.
CONFIGURATION_TYPES = {
    ("setup", "conventionsFile"): "conventions",
    ("setup", "curveConfigFile"): "curve_configuration",
    ("setup", "marketConfigFile"): "todays_market",
    ("setup", "pricingEnginesFile"): "pricing_engines",
}

# Files that the engine writes, though the parameter name does not say Output.
WRITTEN = {
    ("simulation", "cubeFile"),
    ("simulation", "nettingSetCubeFile"),
    ("simulation", "cptyCubeFile"),
    ("simulation", "aggregationScenarioDataFileName"),
    ("simulation", "additionalScenarioDataFileName"),
    ("simulation", "scenariodump"),
    ("simulation", "aggregationScenarioDataDump"),
    ("scenarioGeneration", "scenariodump"),
    ("scenario", "scenariodump"),
    ("portfolioDetails", "marketObjectFileName"),
    ("portfolioDetails", "riskFactorFileName"),
    ("xva", "dimEvolutionFile"),
    ("xva", "dimRegressionFiles"),
    ("sacva", "saCvaNetSensitivitiesFile"),
    ("zeroToParShift", "parShiftsFile"),
}

# The analytic that writes a file another analytic reads, stated and not guessed.
WRITERS = {
    ("xva", "cubeFile"): ("simulation", "cubeFile"),
    ("pfe", "cubeFile"): ("simulation", "cubeFile"),
    ("xva", "nettingSetCubeFile"): ("simulation", "nettingSetCubeFile"),
    ("xva", "cptyCubeFile"): ("simulation", "cptyCubeFile"),
    ("xva", "scenarioFile"): ("simulation", "aggregationScenarioDataFileName"),
    ("pfe", "scenarioFile"): ("simulation", "aggregationScenarioDataFileName"),
}

SETUP_DATA_FILES = {"portfolioFile", "marketDataFile", "fixingDataFile", "dividendDataFile",
                    "marketDataMappingFile", "fixingDataMappingFile"}

DOCUMENT = "input_document"
DATA = "data_file"
OUTPUT = "output_file"
INTERMEDIATE = "intermediate_file"

# How many samples a name needs to be a convention of one analytic and not a habit of
# one example, for an input document that other analytics name too.
CONVENTION = 10


def sql_text(value):
    return "null" if value is None else "'" + value.replace("'", "''") + "'"


def is_file_parameter(name):
    """Every parameter whose value is a file name is a file parameter, whatever its name.

    The value decides, so the only parameters left out are those that never hold a file.
    """
    return bool(name) and name not in CHOICES


def read_samples():
    setup = collections.defaultdict(collections.Counter)
    analytic = collections.defaultdict(collections.Counter)
    runs = collections.Counter()
    paths = {}
    documents = 0
    for dirpath, _dirs, files in os.walk(EXAMPLES):
        for f in sorted(files):
            full = os.path.join(dirpath, f)
            if not f.endswith(".xml"):
                continue
            paths.setdefault(f, full)
            if not f.startswith("ore"):
                continue
            text = open(full, errors="ignore").read()
            m = re.search(r"<Setup>(.*?)</Setup>", text, re.S)
            if not m:
                continue
            documents += 1
            for n, v in re.findall(r'<Parameter name="(\w+)">([^<]*)</Parameter>', m.group(1)):
                v = v.strip()
                if n not in CHOICES and FILE_VALUE.search(v):
                    setup[n][os.path.basename(v)] += 1
            for t, body in re.findall(r'<Analytic type="(\w+)">(.*?)</Analytic>', text, re.S):
                runs[t] += 1
                for n, v in re.findall(r'<Parameter name="(\w+)">([^<]*)</Parameter>', body):
                    v = v.strip()
                    if "," in v or not FILE_VALUE.search(v):
                        continue
                    if is_file_parameter(n):
                        analytic[(t, n)][os.path.basename(v)] += 1
    return documents, setup, analytic, runs, paths


def read_guide():
    if not os.path.exists(GUIDE):
        raise SystemExit(f"{GUIDE} is missing; run extract_ore_guide.py.")
    return json.load(open(GUIDE))


def plain_name(counter):
    """The most common name without a date, and the count behind it."""
    plain = [(name, n) for name, n in counter.most_common() if not DATED.search(name)]
    return plain[0] if plain else None


def format_of(name):
    if name.endswith(".csv.gz"):
        return "csv_gz"
    return name.rsplit(".", 1)[-1]


def xsd_roots():
    """The root elements the schemas declare, by element name."""
    roots = {}
    for f in sorted(os.listdir(XSD_DIR)):
        if not f.endswith(".xsd"):
            continue
        schema = ET.parse(os.path.join(XSD_DIR, f)).getroot()
        for e in schema:
            if e.tag.endswith("}element") and e.get("name"):
                roots.setdefault(e.get("name"), f)
    return roots


def xml_root(path):
    try:
        return ET.parse(path).getroot().tag
    except ET.ParseError:
        return None


def classify(scope, parameter, name, produced, note):
    if scope is None:
        return DATA if parameter in SETUP_DATA_FILES or "ortfolio" in parameter else DOCUMENT
    if (scope, parameter) in WRITTEN:
        return OUTPUT
    if (scope, parameter) in WRITERS:
        return INTERMEDIATE
    if re.search(r"Output", parameter) or parameter in ("outputFile", "outputFileName"):
        return OUTPUT
    if note and re.search(r"\boutput\b", note, re.I) and not re.search(r"\binput\b", note, re.I):
        return OUTPUT
    if "ortfolio" in parameter:
        return DATA
    if name.endswith(".xml"):
        return DOCUMENT
    if name in produced or re.search(r"Input", parameter):
        return INTERMEDIATE
    return DATA


def reach(row, writer_row):
    """How this file type reaches a run."""
    where = ("the Setup block" if row["analytic_type_code"] is None
             else f"the {row['analytic_type_code']} analytic")
    p = row["ore_parameter"]
    if row["direction"] == "output":
        return (f"A run names it with the parameter {p} of {where}, so the result has one "
                "predictable name in the output folder. ORE Studio does not parse it.")
    if row["configuration_type_code"]:
        return (f"The parameter {p} of {where} names it. ORE Studio reads and parses the file "
                f"into the entities of the {row['configuration_type_code']} configuration "
                "when it imports a run, and writes it back from them when it exports one.")
    if writer_row:
        return (f"The parameter {p} of {where} names it. The "
                f"{writer_row['analytic_type_code']} analytic writes the file, so the name "
                "here is the name that analytic gives it, and the two cannot differ.")
    return (f"The parameter {p} of {where} names it. ORE Studio reads the file when it "
            "imports a run, and cannot yet parse it into entities.")


def about_one_parameter(note, guide):
    """Whether a note from the guide speaks of one parameter, not of a list of them."""
    names = {k.split("|", 1)[1] for k in guide["examples"]}
    mentioned = {n for n in names if re.search(r"(?<![A-Za-z])" + re.escape(n) + r"(?![A-Za-z])", note)}
    return len(mentioned) <= 1


def describe(row, guide, writer_row):
    key = f"{row['analytic_type_code'] or 'setup'}|{row['ore_parameter']}"
    name = row["name_pattern"]
    document = guide["documents"].get(name, {}).get("text") or texts.CONTENT.get(name)
    note = guide["notes"].get(key)
    if note and not about_one_parameter(note, guide):
        note = None
    parts = []
    if document:
        parts.append(f"What it holds: {document}")
    if row["analytic_type_code"]:
        purpose = texts.ANALYTICS.get(row["analytic_type_code"])
        if purpose:
            parts.append(f"The {row['analytic_type_code']} analytic {purpose}.")
    if note:
        parts.append(f"From the ORE user guide: {note}")
    parts.append(reach(row, writer_row))
    return " ".join(parts), bool(document or note)


def build_rows():
    documents, setup, analytic, runs, paths = read_samples()
    guide = read_guide()
    roots = xsd_roots()

    counters = {(None, p): c for p, c in setup.items()}
    counters.update({(t, p): c for (t, p), c in analytic.items()})

    # The keys: every file the samples name and every file the guide lists.
    keys = set(counters)
    for key, example in guide["examples"].items():
        scope, parameter = key.split("|", 1)
        if parameter in CHOICES or not is_file_parameter(parameter):
            continue
        if "," in example["value"] or "%" in example["value"]:
            continue
        value = os.path.basename(example["value"])
        if not FILE_VALUE.search(value):
            continue
        keys.add((None if scope == "setup" else scope, parameter))

    produced = {name for (t, p), c in counters.items()
                if t and (re.search(r"Output", p) or p in ("outputFile", "outputFileName")
                          or (t, p) in WRITTEN) for name in c}

    # The common name of an input document, across the analytics that name it.
    common = collections.defaultdict(collections.Counter)
    for (t, p), c in counters.items():
        if t and p not in ("outputFile", "outputFileName"):
            common[p].update(c)

    rows = []
    for scope, parameter in sorted(keys, key=lambda k: (k[0] or "", k[1])):
        counter = counters.get((scope, parameter), collections.Counter())
        key = f"{scope or 'setup'}|{parameter}"
        example = guide["examples"].get(key)
        note = guide["notes"].get(key)
        listed = example is not None and "," not in example["value"] and "%" not in example["value"]
        name = os.path.basename(example["value"]) if listed else None
        if name is None or DATED.search(name) or not FILE_VALUE.search(name):
            chosen = plain_name(counter)
            if chosen is None:
                continue
            name = chosen[0]
        kind = classify(scope, parameter, name, produced, note)
        if (example is None and scope and kind == DOCUMENT and
                sum(counter.values()) < CONVENTION and parameter in common):
            shared = plain_name(common[parameter])
            if shared and shared[0] != name:
                name = shared[0]
        rows.append({
            "code": f"{scope or 'setup'}.{parameter}",
            "analytic_type_code": scope,
            "ore_parameter": parameter,
            "direction": "output" if kind == OUTPUT else "input",
            "kind": kind,
            "format": format_of(name),
            "xsd_file": None,
            "configuration_type_code": CONFIGURATION_TYPES.get((scope or "setup", parameter)),
            "name_pattern": name,
            "sample_count": counter.get(name, 0),
            "sample_total": sum(counter.values()),
            "is_default": False,
            "produced_by_file_type_code": None,
            "description": "",
        })

    kind_order = [DOCUMENT, DATA, INTERMEDIATE, OUTPUT]
    rows.sort(key=lambda r: (r["analytic_type_code"] is not None, kind_order.index(r["kind"]),
                             r["analytic_type_code"] or "", r["ore_parameter"]))
    for i, r in enumerate(rows, start=1):
        r["display_order"] = i

    for r in rows:
        r["is_default"] = (r["analytic_type_code"] is not None
                           and r["kind"] in (OUTPUT, INTERMEDIATE)
                           and r["sample_total"] > 0
                           and 2 * r["sample_total"] >= runs[r["analytic_type_code"]])
        if r["format"] == "xml":
            candidates = [r["name_pattern"]] + [n for n, _c in
                          counters.get((r["analytic_type_code"], r["ore_parameter"]),
                                       collections.Counter()).most_common()]
            sample = next((paths[n] for n in candidates if n in paths), None)
            root = xml_root(sample) if sample else None
            r["xsd_file"] = roots.get(re.sub(r"^\{.*\}", "", root)) if root else None
    link_producers(rows)
    # A file that is read but that no analytic of the table writes is data the user supplies.
    for r in rows:
        if r["kind"] == INTERMEDIATE and r["produced_by_file_type_code"] is None:
            r["kind"] = DATA
            r["is_default"] = False

    by_code = {r["code"]: r for r in rows}
    missing = []
    for r in rows:
        link = r["produced_by_file_type_code"]
        r["description"], documented = describe(r, guide, by_code.get(link) if link else None)
        if not documented:
            missing.append(r["code"])
    return documents, rows, missing


def link_producers(rows):
    """An intermediate file takes the file type of the analytic that writes it."""
    by_code = {r["code"]: r for r in rows}
    outputs = collections.defaultdict(list)
    for r in rows:
        if r["kind"] == OUTPUT:
            outputs[r["name_pattern"]].append(r)
    for r in rows:
        if r["kind"] != INTERMEDIATE:
            continue
        stated = WRITERS.get((r["analytic_type_code"], r["ore_parameter"]))
        if stated:
            writer = by_code.get(f"{stated[0]}.{stated[1]}")
            if writer is None:
                raise SystemExit(f"{r['code']} reads {stated}, which no file type names.")
            r["produced_by_file_type_code"] = writer["code"]
            continue
        writers = [w for w in outputs.get(r["name_pattern"], [])
                   if w["analytic_type_code"] != r["analytic_type_code"]]
        if writers:
            writers.sort(key=lambda w: (-w["sample_count"], w["code"]))
            r["produced_by_file_type_code"] = writers[0]["code"]


COLUMNS = ("code", "analytic_type_code", "ore_parameter", "direction", "kind", "format",
           "xsd_file", "configuration_type_code", "name_pattern", "sample_count",
           "sample_total", "description", "display_order", "is_default",
           "produced_by_file_type_code")


def render(documents, rows):
    # A reader is inserted after its writer, because the insert checks the link.
    ordered = sorted(rows, key=lambda r: r["produced_by_file_type_code"] is not None)

    def value(r, c):
        v = r[c]
        if isinstance(v, bool):
            return "true" if v else "false"
        return str(v) if isinstance(v, int) else sql_text(v)

    values = ",\n".join("        (" + ", ".join(value(r, c) for c in COLUMNS) + ")" for r in ordered)
    columns = ", ".join(COLUMNS)
    insert_values = ", ".join("r." + c for c in COLUMNS)
    return f"""/* -*- sql-product: postgres; tab-width: 4; indent-tabs-mode: nil -*-
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

/**
 * File Types Population Script
 *
 * GENERATED FILE - DO NOT EDIT. Regenerate with
 * projects/ores.sql/utility/generate_file_types_populate.py.
 *
 * Seeds the file_types lookup for the system tenant with the {len(rows)} kinds of file the
 * official ORE user guide and the {documents} shipped ORE run documents name. The canonical
 * name is the one the guide gives, or else the most common name in the samples, and
 * sample_count out of sample_total records how many samples use it. This script is
 * idempotent.
 */

\\echo '--- File Types ---'

do $$
declare
    v_sys_tenant uuid := ores_utility_system_tenant_id_fn();
    r record;
begin
    for r in
        select * from (values
{values}
        ) as t ({columns})
    loop
        if not exists (
            select 1 from ores_reporting_file_types_tbl
            where tenant_id = v_sys_tenant and code = r.code
              and valid_to = ores_utility_infinity_timestamp_fn()
        ) then
            insert into ores_reporting_file_types_tbl (
                {columns},
                tenant_id, version, modified_by, change_reason_code, change_commentary
            ) values (
                {insert_values},
                v_sys_tenant, 0, current_user, 'system.initial_load', 'Seed file type: ' || r.code
            );
            raise debug 'Created file type: %', r.code;
        else
            raise debug 'File type already exists: %', r.code;
        end if;
    end loop;
end $$;
"""


def main():
    parser = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    parser.add_argument("--check", action="store_true",
                        help="fail if the committed seed differs from the guide and the samples")
    args = parser.parse_args()
    documents, rows, missing = build_rows()
    if missing:
        print("These file types have no description from the guide or the fallback text:",
              file=sys.stderr)
        for code in missing:
            print(f"  {code}", file=sys.stderr)
        return 1
    content = render(documents, rows)
    if args.check:
        current = open(SEED).read() if os.path.exists(SEED) else ""
        if current != content:
            print(f"{SEED} no longer matches the guide and the ORE samples; regenerate it.",
                  file=sys.stderr)
            return 1
        print(f"{len(rows)} file types match the guide and the {documents} ORE samples.")
        return 0
    with open(SEED, "w") as f:
        f.write(content)
    counts = collections.Counter(r["kind"] for r in rows)
    formats = collections.Counter(r["format"] for r in rows)
    print(f"Wrote {SEED}: {len(rows)} file types, {dict(counts)}, formats {dict(formats)}, "
          f"{sum(1 for r in rows if r['is_default'])} defaults, "
          f"{sum(1 for r in rows if r['produced_by_file_type_code'])} linked to a writer, "
          f"{sum(1 for r in rows if r['xsd_file'])} with an XSD")
    return 0


if __name__ == "__main__":
    sys.exit(main())
