#!/usr/bin/env python3
"""Emit the table of every output file ORE writes.

ORE publishes no list of its outputs. The engine names some files by a
parameter and some by a convention of its own, and the columns of each file
live in the writer that produces it. The one authority that states every shape
is the corpus ORE ships: the expected outputs under
=external/ore/examples/*/ExpectedOutput=. This script reads that corpus, groups
its files into families, and states for each family the file pattern, the
parameter that names it where the seed declares one, the analytics that write
it, its key columns and every column it states.

The columns are a union over the header variants a family shows, because a
family's shape follows the configuration that asked for it: =curves.csv= states
one column for each curve the run built. The count of variants is kept so a
reader knows whether it is looking at the whole shape or one variant of it.

Two things the corpus does not say and this file does:

- the analytic that writes a family no parameter names, which is read from the
  writer that produces it;
- the kind of a column, which is inferred from the values the samples state,
  because a CSV states no types. A key column is text whatever its values look
  like, because it identifies a record and is never summed.

    generate_output_catalogue.py           write the data
    generate_output_catalogue.py --check   fail if the data no longer matches
"""

import argparse
import csv
import io
import os
import re
import subprocess
import sys

ROOT = os.path.abspath(os.path.join(os.path.dirname(__file__), "..", "..", ".."))
EXAMPLES = os.path.join(ROOT, "external", "ore", "examples")
SEED = os.path.join(ROOT, "projects", "ores.sql", "populate", "reporting",
                    "reporting_file_types_populate.sql")
DATA = os.path.join(ROOT, "projects", "ores.ore", "core", "src", "results",
                    "output_catalogue_data.cpp")
DATA_HPP = os.path.join(ROOT, "projects", "ores.ore", "core", "include", "ores.ore.core",
                        "results", "output_catalogue_data.hpp")

# Files the engine names by a convention of its own, with the analytic that
# writes them. A family whose name a seed parameter states takes its analytic
# from the seed instead.
CONVENTION = {
    "bacva": "bacva",
    "colva-nettingset": "xva",
    "credit-migration": "creditcurves",
    "cva_sensitivities": "xvaSensitivity",
    "dim_evolution": "xva",
    "dim_regression": "xva",
    "eigenvalues": "calibration",
    "eigenvectors": "calibration",
    "exposure-nettingset": "xva",
    "exposure-trade": "xva",
    "historical_PnL": "historicalSimulationVar",
    "im_schedule": "imschedule",
    "im_schedule_trade": "imschedule",
    "mean-reversion": "calibration",
    "pca": "calibration",
    "regulation_breakdown_simm": "simm",
    "saccr": "saccr",
    "saccr_detail": "saccr",
    "sacva": "sacva",
    "sacva_sensitivities": "sacva",
    "sacvadetail": "sacva",
    "scenario_fullSurface": "scenario",
    "scenariostatistics": "scenario",
    "sensitivity_atmOnly": "sensitivity",
    "sensitivity_fullSurface": "sensitivity",
    "sensitivity_scenario": "sensitivity",
    "simm": "simm",
    "simm_data": "simm",
    "smrc": "smrc",
    "smrcdetail": "smrc",
    "stress": "stress",
    "stress_scenario_par_rates": "stress",
    "stress_scenarios": "scenarioGeneration",
    "stresstest": "stress",
    "stresstest_flows": "stress",
    "swaption_npv": "npv",
    "todaysmarketcalibration": "todaysMarketCalibration",
    "todaysmarketcalibration_cashflows": "todaysMarketCalibration",
    "xvaExplain": "xvaExplain",
    "xvaExplain_details": "xvaExplain",
    "xva_par_sensitivity": "xvaSensitivity",
    "xva_zero_sensitivity": "xvaSensitivity",
    "xvacg-cva-sensi-scenario": "xva",
    "xvacg-exposure": "xva",
    "scenario_par_rates": "sensitivity",
    "xva": "xva",
}

# The columns that identify a row rather than measure it. A key is text
# whatever its values look like.
KEYS = [
    "TradeId", "TradeID", "NettingSet", "NettingSetId", "NettingSetID",
    "Portfolio", "PortfolioId", "PortfolioID", "Date", "Time", "Tenor",
    "PillarDate", "Scenario", "ScenarioLabel", "LegNo", "CashflowNo",
    "RiskFactor", "Factor", "RiskFactor1", "RiskFactor2", "TradeType",
    "MarketObjectType", "MarketObjectId", "PrincipalComponent", "Sample",
    "PLDate1", "PLDate2",
]

# A family whose name a parameter states is the family of that name.
PREFIXES = [
    ("colva_nettingset_", "colva-nettingset"),
    ("exposure_nettingset_", "exposure-nettingset"),
    ("exposure_trade_", "exposure-trade"),
    ("credit_migration_", "credit-migration"),
    ("meanReversion_", "mean-reversion"),
    ("xva_par_sensitivity_", "xva_par_sensitivity"),
    ("xva_zero_sensitivity_", "xva_zero_sensitivity"),
    ("pca_", "pca"),
]

# The engine writes a value it has no figure for as its null string. It is not
# a value of the column, so it is dropped before the kind is inferred.
NULLS = {"", "#N/A", "#NA", "N/A", "null", "NULL", "NaN", "-nan"}

DATE = re.compile(r"^\d{4}-\d{2}-\d{2}$")
TIME = re.compile(r"^\d{1,2}:\d{2}(:\d{2})?$")
INTEGER = re.compile(r"^-?\d+$")
DECIMAL = re.compile(r"^-?(\d+\.?\d*|\.\d+)([eE][-+]?\d+)?$")


def family_of(file_name):
    """The family a sample file name belongs to, and the pattern it matches."""
    stem = file_name[:-4] if file_name.endswith(".csv") else file_name
    for prefix, family in PREFIXES:
        if stem.startswith(prefix):
            return family, prefix + "*.csv"
    stem = re.sub(r"_\d+$", "", stem)
    return stem, stem + ".csv"


def seed_outputs():
    """The output parameters the seed states, by the file pattern they name."""
    pattern = re.compile(
        r"\(\s*'([^']+)',\s*'([^']*)',\s*'([^']*)',\s*'output',\s*'[^']*',\s*'[^']*',"
        r"\s*(?:null|'[^']*'),\s*(?:null|'[^']*'),\s*'([^']*)',\s*(\d+),\s*(\d+),")
    text = open(SEED, encoding="utf-8").read()
    out = {}
    for code, analytic, parameter, name, samples, total in pattern.findall(text):
        out.setdefault(name, []).append((code, analytic, int(samples)))
    return out


def kind_of(values, key):
    seen = [v for v in values if v not in NULLS]
    if not seen:
        return "text"
    if all(DATE.match(v) for v in seen):
        return "date"
    if all(TIME.match(v) for v in seen):
        return "time"
    # A key that is not a date or a time identifies a record, so it stays text
    # even when its values look like numbers.
    if key:
        return "text"
    if all(INTEGER.match(v) for v in seen):
        return "integer"
    if all(DECIMAL.match(v) for v in seen):
        return "decimal"
    return "text"


def read_family(path):
    """The headers and the values a file states, column by column."""
    with open(path, encoding="utf-8", errors="ignore", newline="") as f:
        text = f.read()
    first, _, rest = text.partition("\n")
    if first.startswith("#"):
        first = first[1:]
    reader = csv.reader(io.StringIO(first + "\n" + rest))
    fields = next(reader, [])
    values = {}
    for cells in reader:
        for i, name in enumerate(fields):
            if i < len(cells):
                values.setdefault(name, []).append(cells[i])
    return fields, values


def collect():
    families = {}
    for suite in sorted(os.listdir(EXAMPLES)):
        expected = os.path.join(EXAMPLES, suite, "ExpectedOutput")
        if not os.path.isdir(expected):
            continue
        for dirpath, dirs, files in os.walk(expected):
            dirs.sort()
            for name in sorted(files):
                if not name.endswith(".csv"):
                    continue
                path = os.path.join(dirpath, name)
                family, pattern = family_of(name)
                entry = families.setdefault(family, {
                    "family": family, "pattern": pattern, "headers": [],
                    "values": {}, "sample": os.path.relpath(path, ROOT)})
                headers, values = read_family(path)
                if headers not in entry["headers"]:
                    entry["headers"].append(headers)
                for k, v in values.items():
                    entry["values"].setdefault(k, []).extend(v[:50])
    return families


def rows(families, seed):
    out = []
    for family in sorted(families):
        entry = families[family]
        headers = entry["headers"]
        columns = []
        for header in headers:
            for name in header:
                if name and name not in [c[0] for c in columns]:
                    columns.append((name, None))
        keys = [k for k in KEYS if any(k in h for h in headers)]
        named = []
        for name, _ in columns:
            named.append((name, kind_of(entry["values"].get(name, []), name in keys)))
        matches = seed.get(entry["pattern"], [])
        parameter = matches[0][0] if matches else ""
        analytic = ",".join(sorted({m[1] for m in matches})) if matches \
            else CONVENTION.get(family, "")
        out.append({
            "family": family,
            "pattern": entry["pattern"],
            "parameter": parameter,
            "analytic": analytic,
            "keys": ",".join(keys),
            "variants": len(headers),
            "sample": entry["sample"],
            "columns": named,
        })
    return out


def header():
    return """/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */
/*
 * Copyright (C) 2026 Marco Craveiro <marco.craveiro@gmail.com>
 *
 * This program is free software; you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation; either version 3 of the License, or (at your option) any later
 * version.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51
 * Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 */

/**
 * AUTO-GENERATED FILE - DO NOT EDIT MANUALLY
 * Generator: projects/ores.ore/scripts/generate_output_catalogue.py
 * To modify, update the generator and regenerate.
 */
#ifndef ORES_ORE_CORE_RESULTS_OUTPUT_CATALOGUE_DATA_HPP
#define ORES_ORE_CORE_RESULTS_OUTPUT_CATALOGUE_DATA_HPP

#include <string_view>

namespace ores::ore::results {

/**
 * @brief Every output file ORE writes, one line for each family.
 *
 * The lines are the data behind output_catalogue.hpp. Only the parser there
 * reads them.
 */
extern const std::string_view output_catalogue_text;

}

#endif
"""


def render(catalogue):
    lines = []
    lines.append("/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-")
    lines.append(" *")
    lines.append(" * Copyright (C) 2026 Marco Craveiro <marco.craveiro@gmail.com>")
    lines.append(" *")
    lines.append(" * This program is free software; you can redistribute it and/or modify it under")
    lines.append(" * the terms of the GNU General Public License as published by the Free Software")
    lines.append(" * Foundation; either version 3 of the License, or (at your option) any later")
    lines.append(" * version.")
    lines.append(" *")
    lines.append(" * You should have received a copy of the GNU General Public License along with")
    lines.append(" * this program; if not, write to the Free Software Foundation, Inc., 51")
    lines.append(" * Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.")
    lines.append(" *")
    lines.append(" */")
    lines.append("/**")
    lines.append(" * AUTO-GENERATED FILE - DO NOT EDIT MANUALLY")
    lines.append(" * Generator: projects/ores.ore/scripts/generate_output_catalogue.py")
    lines.append(" * To modify, update the generator and regenerate.")
    lines.append(" *")
    lines.append(" * Every output file ORE writes, as its samples state it. One line for each")
    lines.append(" * family, its fields separated by a tab: family, file pattern, the parameter")
    lines.append(" * that names it, the analytics that write it, its key columns, how many header")
    lines.append(" * variants the samples show, a sample file, then each column with its kind.")
    lines.append(" */")
    lines.append("")
    lines.append('#include "ores.ore.core/results/output_catalogue_data.hpp"')
    lines.append("")
    lines.append("namespace ores::ore::results {")
    lines.append("")
    lines.append("const std::string_view output_catalogue_text{")
    lines.append('R"CAT(')
    for row in catalogue:
        cols = ",".join(f"{name}:{kind}" for name, kind in row["columns"])
        lines.append("\t".join([row["family"], row["pattern"], row["parameter"],
                                row["analytic"], row["keys"], str(row["variants"]),
                                row["sample"], cols]))
    lines.append(')CAT"');
    lines.append("};")
    lines.append("")
    lines.append("}")
    lines.append("")
    return "\n".join(lines)


def formatted(text, name):
    """The text as the project's formatter states it.

    The data files are formatted like the rest of the tree, so the check that
    they match the generator cannot fail on whitespace alone.
    """
    try:
        p = subprocess.run(["clang-format", f"--assume-filename={name}"],
                           input=text, capture_output=True, text=True)
    except FileNotFoundError:
        return text
    return p.stdout if p.returncode == 0 and p.stdout else text


def main(argv):
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument("--check", action="store_true",
                    help="Fail if the committed data no longer matches the corpus.")
    args = ap.parse_args(argv)

    families = collect()
    catalogue = rows(families, seed_outputs())
    text = render(catalogue)

    text = formatted(text, DATA)
    data_header = formatted(header(), DATA_HPP)

    if args.check:
        for path, want in ((DATA, text), (DATA_HPP, data_header)):
            try:
                current = open(path, encoding="utf-8").read()
            except OSError:
                print(f"the catalogue data is missing: {path}", file=sys.stderr)
                return 1
            if current != want:
                print(f"the catalogue no longer matches the corpus: {path}",
                      file=sys.stderr)
                return 1
        print(f"the catalogue matches the corpus: {len(catalogue)} families")
        return 0

    os.makedirs(os.path.dirname(DATA), exist_ok=True)
    with open(DATA, "w", encoding="utf-8") as f:
        f.write(text)
    with open(DATA_HPP, "w", encoding="utf-8") as f:
        f.write(data_header)
    named = sum(1 for r in catalogue if r["parameter"])
    print(f"wrote {len(catalogue)} families, {named} named by a parameter, "
          f"{len(catalogue) - named} by an engine convention")
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
