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
import uuid
import xml.etree.ElementTree as ET

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import file_type_descriptions as texts  # noqa: E402
import ore_file_descriptions  # noqa: E402
import ore_files  # noqa: E402

ROOT = os.path.abspath(os.path.join(os.path.dirname(__file__), "..", "..", ".."))
EXAMPLES = os.path.join(ROOT, "external", "ore", "examples")
XSD_DIR = os.path.join(ROOT, "external", "ore", "xsd")
GUIDE = os.path.join(os.path.dirname(os.path.abspath(__file__)), "file_type_guide.json")
SEED = os.path.join(ROOT, "projects", "ores.sql", "populate", "reporting",
                    "reporting_file_types_populate.sql")
CATEGORY_SEED = os.path.join(ROOT, "projects", "ores.sql", "populate", "reporting",
                             "reporting_ore_file_categories_populate.sql")
ORE_FILE_SEED = os.path.join(ROOT, "projects", "ores.sql", "populate", "reporting",
                             "reporting_ore_files_populate.sql")
VERSION_SEED = os.path.join(ROOT, "projects", "ores.sql", "populate", "reporting",
                            "reporting_ore_file_versions_populate.sql")
SCHEMA_DIR = os.path.join(ROOT, "external", "ore", "xsd")

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


COLUMNS = ("code", "analytic_type_code", "ore_parameter", "direction", "kind",
           "ore_file_code", "configuration_type_code", "name_pattern", "sample_count",
           "sample_total", "description", "display_order", "is_default",
           "produced_by_file_type_code")


def link_ore_files(rows):
    """Give each file type the ORE file its name is.

    A name with a key between double braces stands for any value of the key, so the key
    is matched as text. Exactly one ORE file must match. Where the samples state an XSD
    for the file, it must be the XSD the ORE file states, so the hand-written list and the
    samples cannot drift apart.
    """
    problems = []
    for r in rows:
        name = re.sub(r"\{\{([A-Z0-9_]+)\}\}", r"\1", r["name_pattern"])
        hits = [f for f in ore_files.FILES if re.fullmatch(f["filename_regex"], name)]
        if len(hits) != 1:
            problems.append(f"{r['code']} names {r['name_pattern']}, which matches "
                            f"{[f['code'] for f in hits] or 'no ORE file'}")
            continue
        found = hits[0]
        r["ore_file_code"] = found["code"]
        stated = {code: name for name, code in ore_files.SCHEMA_OF.items()}.get(
            found["schema_ore_file_code"])
        if r["xsd_file"] is not None and r["xsd_file"] != stated:
            problems.append(f"{r['code']}: the samples give {r['name_pattern']} the XSD "
                            f"{r['xsd_file']}, the ORE file {found['code']} states {stated}")
    return problems


def check_ore_files():
    """The ORE files and categories are whole: every one described, every name unique."""
    problems = []
    categories = {c[0] for c in ore_files.CATEGORIES}
    codes = [f["code"] for f in ore_files.FILES]
    for code in sorted({c for c in codes if codes.count(c) > 1}):
        problems.append(f"the ORE file {code} is listed twice")
    for code in sorted(categories | set(codes)):
        if not re.fullmatch(r"[a-z][a-z0-9_]*", code):
            problems.append(f"{code} is not a code: lower case letters, digits and underscores, "
                            "starting with a letter")
    for f in ore_files.FILES:
        if f["category_code"] not in categories:
            problems.append(f"{f['code']} names the category {f['category_code']}, which "
                            "is not listed")
        if f["code"] not in ore_file_descriptions.FILES:
            problems.append(f"the ORE file {f['code']} has no description")
        if not re.fullmatch(r"[A-Za-z0-9_.-]+", f["typical_filename"]):
            problems.append(f"{f['code']}: {f['typical_filename']} is not a name a file can "
                            "have; give an example, and the pattern as a variant")
        if not re.fullmatch(f["filename_regex"], f["typical_filename"]):
            problems.append(f"{f['code']}: {f['typical_filename']} does not match its own "
                            "regex")
        others = [g["code"] for g in ore_files.FILES
                  if g is not f and re.fullmatch(g["filename_regex"], f["typical_filename"])]
        if others:
            problems.append(f"{f['typical_filename']} matches {f['code']} and {others}")
    for code in sorted(categories - set(ore_file_descriptions.CATEGORIES)):
        problems.append(f"the category {code} has no description")
    for code in sorted(set(ore_file_descriptions.FILES) - set(codes)):
        problems.append(f"{code} is described but is not an ORE file")
    return problems


def render_lookup(title, description, table, noun, columns, rows):
    """An idempotent seed of a lookup the system tenant owns, keyed by code."""
    def value(v):
        if v is None:
            return "null"
        if isinstance(v, bool):
            return "true" if v else "false"
        return str(v) if isinstance(v, int) else sql_text(v)

    values = ",\n".join("        (" + ", ".join(value(r[c]) for c in columns) + ")"
                         for r in rows)
    names = ", ".join(columns)
    inserted = ", ".join("r." + c for c in columns)
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
 * {title} Population Script
 *
 * GENERATED FILE - DO NOT EDIT. Regenerate with
 * projects/ores.sql/utility/generate_file_types_populate.py.
 *
 * {description} This script is idempotent.
 */

\\echo '--- {title} ---'

do $$
declare
    v_sys_tenant uuid := ores_utility_system_tenant_id_fn();
    r record;
begin
    for r in
        select * from (values
{values}
        ) as t ({names})
    loop
        if not exists (
            select 1 from {table}
            where tenant_id = v_sys_tenant and code = r.code
              and valid_to = ores_utility_infinity_timestamp_fn()
        ) then
            insert into {table} (
                {names},
                tenant_id, version, modified_by, change_reason_code, change_commentary
            ) values (
                {inserted},
                v_sys_tenant, 0, current_user, 'system.initial_load', 'Seed {noun}: ' || r.code
            );
        end if;
    end loop;
end $$;
"""


def render_categories():
    rows = [{"code": code, "name": name, "origin": origin,
             "description": ore_file_descriptions.CATEGORIES[code], "display_order": i + 1}
            for i, (code, name, origin) in enumerate(ore_files.CATEGORIES)]
    return render_lookup(
        "ORE File Categories",
        f"Seeds the {len(rows)} groups the files of an ORE run fall in, from\n"
        " * projects/ores.sql/utility/ore_files.py.",
        "ores_reporting_ore_file_categories_tbl", "ORE file category",
        ("code", "name", "origin", "description", "display_order"), rows)


def render_ore_files():
    columns = ("code", "name", "category_code", "configuration_type_code", "guide_section",
               "origin", "format", "schema_ore_file_code", "typical_filename", "filename_regex",
               "description", "display_order")
    rows = [dict(f, description=ore_file_descriptions.FILES[f["code"]], display_order=i + 1)
            for i, f in enumerate(ore_files.FILES)]
    return render_lookup(
        "ORE Files",
        f"Seeds the {len(rows)} files of an ORE run, as the ORE user guide identifies them,\n"
        " * from projects/ores.sql/utility/ore_files.py and ore_file_descriptions.py.",
        "ores_reporting_ore_files_tbl", "ORE file", columns, rows)


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


def vendored_schemas():
    """The schemas ORE ships, as vendored in external/ore/xsd, with the engine version
    their manifest states. The directory is checked against the manifest's digest of
    every file in it, so a schema edited by hand cannot reach the seed."""
    import hashlib
    manifest = json.load(open(os.path.join(SCHEMA_DIR, "manifest.json")))
    # The vendoring script orders the paths with sort, which follows the locale, so
    # the paths are ordered here by the same sort rather than by their bytes.
    import subprocess
    skip = {"manifest.json", "README.md", "methodology.txt"}
    names = [n for n in os.listdir(SCHEMA_DIR)
             if n not in skip and os.path.isfile(os.path.join(SCHEMA_DIR, n))]
    ordered = subprocess.run(["sort", "-z"], input="\0".join(names) + "\0", text=True,
                             capture_output=True, check=True).stdout.split("\0")
    lines = "".join(
        f"{hashlib.sha256(open(os.path.join(SCHEMA_DIR, n), 'rb').read()).hexdigest()}  {n}\n"
        for n in ordered if n)
    tree = hashlib.sha256(lines.encode()).hexdigest()
    if tree != manifest["tree_sha256"]:
        raise SystemExit(f"{SCHEMA_DIR} no longer matches its manifest: the tree digest is "
                         f"{tree}, the manifest states {manifest['tree_sha256']}")
    version = manifest["engine_version"]
    rows = []
    for code, _, file_name in ore_files.SCHEMAS:
        content = open(os.path.join(SCHEMA_DIR, file_name), encoding="utf-8").read()
        raw = content.encode("utf-8")
        rows.append({"id": str(uuid.uuid5(uuid.NAMESPACE_URL, f"ores:ore_file:{code}@{version}")),
                     "ore_file_code": code, "engine_version": version, "content": content,
                     "sha256": hashlib.sha256(raw).hexdigest(), "bytes": len(raw)})
    return version, rows


def render_versions():
    version, rows = vendored_schemas()
    columns = ("id", "ore_file_code", "engine_version", "content", "sha256", "bytes")

    def value(c, v):
        if c == "id":
            return f"'{v}'::uuid"
        if c == "content":
            tag = "$xsd$"
            assert tag not in v
            return tag + v + tag
        return str(v) if isinstance(v, int) else sql_text(v)

    values = ",\n".join("        (" + ", ".join(value(c, r[c]) for c in columns) + ")"
                         for r in rows)
    names = ", ".join(columns)
    inserted = ", ".join("r." + c for c in columns)
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
 * ORE File Versions Population Script
 *
 * GENERATED FILE - DO NOT EDIT. Regenerate with
 * projects/ores.sql/utility/generate_file_types_populate.py.
 *
 * Seeds the contents of the {len(rows)} XML schemas ORE {version} ships, from
 * external/ore/xsd, checked against the digest its manifest states. A later engine
 * version adds rows beside these. This script is idempotent.
 */

\\echo '--- ORE File Versions ---'

do $$
declare
    v_sys_tenant uuid := ores_utility_system_tenant_id_fn();
    r record;
begin
    for r in
        select * from (values
{values}
        ) as t ({names})
    loop
        if not exists (
            select 1 from ores_reporting_ore_file_versions_tbl
            where tenant_id = v_sys_tenant and ore_file_code = r.ore_file_code
              and engine_version = r.engine_version
              and valid_to = ores_utility_infinity_timestamp_fn()
        ) then
            insert into ores_reporting_ore_file_versions_tbl (
                {names},
                tenant_id, version, modified_by, change_reason_code, change_commentary
            ) values (
                {inserted},
                v_sys_tenant, 0, current_user, 'system.initial_load',
                'Seed ORE file version: ' || r.ore_file_code || '@' || r.engine_version
            );
        end if;
    end loop;
end $$;
"""


def main():
    parser = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    parser.add_argument("--check", action="store_true",
                        help="fail if the committed seeds differ from the guide and the samples")
    args = parser.parse_args()
    documents, rows, missing = build_rows()
    if missing:
        print("These file types have no description from the guide or the fallback text:",
              file=sys.stderr)
        for code in missing:
            print(f"  {code}", file=sys.stderr)
        return 1
    problems = check_ore_files() + link_ore_files(rows)
    if problems:
        print("The ORE files and the file types disagree:", file=sys.stderr)
        for problem in problems:
            print(f"  {problem}", file=sys.stderr)
        return 1
    seeds = ((CATEGORY_SEED, render_categories()), (ORE_FILE_SEED, render_ore_files()),
             (VERSION_SEED, render_versions()), (SEED, render(documents, rows)))
    if args.check:
        stale = [path for path, content in seeds
                 if not os.path.exists(path) or open(path).read() != content]
        for path in stale:
            print(f"{path} no longer matches its sources; regenerate it.", file=sys.stderr)
        if stale:
            return 1
        print(f"{len(rows)} file types and {len(ore_files.FILES)} ORE files match the guide "
              f"and the {documents} ORE samples.")
        return 0
    for path, content in seeds:
        with open(path, "w") as f:
            f.write(content)
    counts = collections.Counter(r["kind"] for r in rows)
    print(f"Wrote {SEED}: {len(rows)} file types, {dict(counts)}, "
          f"{sum(1 for r in rows if r['is_default'])} defaults, "
          f"{sum(1 for r in rows if r['produced_by_file_type_code'])} linked to a writer; "
          f"{len(ore_files.FILES)} ORE files in {len(ore_files.CATEGORIES)} categories")
    return 0


if __name__ == "__main__":
    sys.exit(main())
