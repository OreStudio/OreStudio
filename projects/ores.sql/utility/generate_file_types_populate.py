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
"""Derive the ORE file types and their well known names from the ORE samples.

Every Setup parameter and every analytic parameter whose value is a file name is
a file type. Its well known name is the most common name the samples give it, with
the folder dropped and the names that embed a date set aside, because a date is a
value of the run and not part of a name. The count that supports each name is kept
with it.

    generate_file_types_populate.py          write the seed
    generate_file_types_populate.py --check  fail if the seed no longer matches the samples
"""

import argparse
import collections
import os
import re
import sys

ROOT = os.path.abspath(os.path.join(os.path.dirname(__file__), "..", "..", ".."))
EXAMPLES = os.path.join(ROOT, "external", "ore", "examples")
SEED = os.path.join(ROOT, "projects", "ores.sql", "populate", "reporting",
                    "reporting_file_types_populate.sql")

FILE_VALUE = re.compile(r"\.(xml|csv|txt|json)$")
DATED = re.compile(r"\d{8}")

# The Setup parameters whose file is a stored choice of the configuration, not a
# file of the package.
SETUP_CHOICES = {"logFile"}

# The configuration type whose entities fill the document, where one exists.
CONFIGURATION_TYPES = {
    ("setup", "conventionsFile"): "conventions",
    ("setup", "curveConfigFile"): "curve_configuration",
    ("setup", "marketConfigFile"): "todays_market",
    ("setup", "pricingEnginesFile"): "pricing_engines",
}

SETUP_DATA_FILES = {"portfolioFile", "marketDataFile", "fixingDataFile",
                    "marketDataMappingFile", "fixingDataMappingFile"}

DOCUMENT = "input_document"
DATA = "data_file"
OUTPUT = "output_file"
INTERMEDIATE = "intermediate_file"


def sql_text(value):
    return "null" if value is None else "'" + value.replace("'", "''") + "'"


def read_samples():
    setup = collections.defaultdict(collections.Counter)
    analytic = collections.defaultdict(collections.Counter)
    documents = 0
    for dirpath, _dirs, files in os.walk(EXAMPLES):
        for f in sorted(files):
            if not (f.startswith("ore") and f.endswith(".xml")):
                continue
            text = open(os.path.join(dirpath, f), errors="ignore").read()
            m = re.search(r"<Setup>(.*?)</Setup>", text, re.S)
            if not m:
                continue
            documents += 1
            for n, v in re.findall(r'<Parameter name="(\w+)">([^<]*)</Parameter>', m.group(1)):
                v = v.strip()
                if n not in SETUP_CHOICES and FILE_VALUE.search(v):
                    setup[n][os.path.basename(v)] += 1
            for t, body in re.findall(r'<Analytic type="(\w+)">(.*?)</Analytic>', text, re.S):
                for n, v in re.findall(r'<Parameter name="(\w+)">([^<]*)</Parameter>', body):
                    v = v.strip()
                    if "," in v or not FILE_VALUE.search(v):
                        continue
                    if re.search(r"File|Config", n) or n.endswith("Name"):
                        analytic[(t, n)][os.path.basename(v)] += 1
    return documents, setup, analytic


def well_known(counter):
    """The most common name without a date, and the count behind it."""
    plain = [(name, n) for name, n in counter.most_common() if not DATED.search(name)]
    if not plain:
        return None
    return plain[0]


def classify(scope, parameter, name, produced):
    if scope is None:
        if parameter in SETUP_DATA_FILES or parameter == "portfolioFile":
            return DATA
        return DOCUMENT
    if re.search(r"Output", parameter) or parameter in ("outputFile", "outputFileName"):
        return OUTPUT
    if "ortfolio" in parameter:
        return DATA
    if name.endswith(".xml"):
        return DOCUMENT
    if name in produced or re.search(r"Input", parameter):
        return INTERMEDIATE
    return DATA


def describe(kind, scope, parameter):
    where = "the Setup block" if scope is None else f"the {scope} analytic"
    what = {
        DOCUMENT: "A configuration document the run reads",
        DATA: "A data file the run reads",
        OUTPUT: "A file the run writes",
        INTERMEDIATE: "A file one analytic writes and another reads",
    }[kind]
    return f"{what}, named by {parameter} in {where}."


def build_rows():
    documents, setup, analytic = read_samples()
    produced = {name for (t, p), c in analytic.items()
                if re.search(r"Output", p) or p in ("outputFile", "outputFileName")
                for name in c}
    entries = [(None, p, c) for p, c in setup.items()] + [(t, p, c) for (t, p), c in analytic.items()]
    rows = []
    for scope, parameter, counter in entries:
        chosen = well_known(counter)
        if chosen is None:
            continue
        name, count = chosen
        kind = classify(scope, parameter, name, produced)
        code = f"{scope or 'setup'}.{parameter}"
        rows.append({
            "code": code,
            "analytic_type_code": scope,
            "ore_parameter": parameter,
            "kind": kind,
            "configuration_type_code": CONFIGURATION_TYPES.get((scope or "setup", parameter)),
            "name_pattern": name,
            "sample_count": count,
            "sample_total": sum(counter.values()),
            "description": describe(kind, scope, parameter),
        })
    kind_order = [DOCUMENT, DATA, INTERMEDIATE, OUTPUT]
    rows.sort(key=lambda r: (r["analytic_type_code"] is not None, kind_order.index(r["kind"]),
                             r["analytic_type_code"] or "", r["ore_parameter"]))
    for i, r in enumerate(rows, start=1):
        r["display_order"] = i
    return documents, rows


def render(documents, rows):
    values = ",\n".join(
        "        ({code}, {atc}, {param}, {kind}, {ctc}, {pattern}, {count}, {total}, {desc}, {order})".format(
            code=sql_text(r["code"]), atc=sql_text(r["analytic_type_code"]),
            param=sql_text(r["ore_parameter"]), kind=sql_text(r["kind"]),
            ctc=sql_text(r["configuration_type_code"]), pattern=sql_text(r["name_pattern"]),
            count=r["sample_count"], total=r["sample_total"], desc=sql_text(r["description"]),
            order=r["display_order"])
        for r in rows)
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
 * {documents} shipped ORE run documents name. Each well known name is the most common
 * name the samples give the file, and sample_count out of sample_total records how
 * many of them use it. This script is idempotent.
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
        ) as t (code, analytic_type_code, ore_parameter, kind, configuration_type_code,
                name_pattern, sample_count, sample_total, description, display_order)
    loop
        if not exists (
            select 1 from ores_reporting_file_types_tbl
            where tenant_id = v_sys_tenant and code = r.code
              and valid_to = ores_utility_infinity_timestamp_fn()
        ) then
            insert into ores_reporting_file_types_tbl (
                code, tenant_id, version,
                analytic_type_code, ore_parameter, kind, configuration_type_code,
                name_pattern, sample_count, sample_total, description, display_order,
                modified_by, change_reason_code, change_commentary
            ) values (
                r.code, v_sys_tenant, 0,
                r.analytic_type_code, r.ore_parameter, r.kind, r.configuration_type_code,
                r.name_pattern, r.sample_count, r.sample_total, r.description, r.display_order,
                current_user, 'system.initial_load', 'Seed file type: ' || r.code
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
                        help="fail if the committed seed differs from the samples")
    args = parser.parse_args()
    documents, rows = build_rows()
    content = render(documents, rows)
    if args.check:
        current = open(SEED).read() if os.path.exists(SEED) else ""
        if current != content:
            print(f"{SEED} no longer matches the ORE samples; regenerate it.", file=sys.stderr)
            return 1
        print(f"{len(rows)} file types match the {documents} ORE samples.")
        return 0
    with open(SEED, "w") as f:
        f.write(content)
    counts = collections.Counter(r["kind"] for r in rows)
    print(f"Wrote {SEED}: {len(rows)} file types, {dict(counts)}")
    return 0


if __name__ == "__main__":
    sys.exit(main())
