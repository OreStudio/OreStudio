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
import fnmatch
import io
import os
import re
import subprocess
import sys

ROOT = os.path.abspath(os.path.join(os.path.dirname(__file__), "..", "..", ".."))
EXAMPLES = os.path.join(ROOT, "external", "ore", "examples")
SEED = os.path.join(ROOT, "projects", "ores.sql", "populate", "reporting",
                    "reporting_file_types_populate.sql")
PAGE = os.path.join(ROOT, "doc", "knowledge", "external", "ore_output_formats.org")
PAGE_ID = "34C68112-7A34-4557-835C-BDCAA7BB9C2A"

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

# Numeric columns that state the context of a figure rather than the figure:
# the size of a shift, the time to a maturity, the index of a point.
NUMERIC_QUALIFIERS = {
    "ShiftSize_1", "ShiftSize_2", "MaturityTime", "DateIndex", "Depth", "TimeStep",
}

# The families whose figures are the risk factors the run simulated. Their
# columns come from the configuration even where the corpus holds one sample.
RISK_FACTOR_FAMILIES = {"scenariostatistics", "stress_scenarios", "scenariodump"}

# The families the suite corpus holds no sample of, read from the notebook
# examples instead. Only these are read from there, because the notebooks also
# hold copies of families the suites already state.
NOTEBOOK_FAMILIES = {"marketdata", "fixings", "dividends", "runtimes", "scenariodump",
                     "additional_results"}
NOTEBOOKS = os.path.join("ORE-Python", "Notebooks")

# The families only the legacy examples hold a sample of. Only these are read
# from there, because the legacy examples repeat the suites with older headers.
LEGACY_FAMILIES = {"timeaveragedexposure", "marketObjects", "riskFactors", "counterparties",
                   "netting_sets", "swap_indices", "trade_types", "underlying_indices"}
LEGACY = "Legacy"

# The samples ORE did not ship: files a run here wrote, and files we wrote from
# the writer's columns. A family whose only sample is synthetic is marked so.
SAMPLES = os.path.join(ROOT, "projects", "ores.ore", "data", "samples")

# The other names a family is written under. A report the run configuration
# does not name is written under the engine's own report name, and the seed
# names the initial margin files with a .txt extension.
ALIASES = {
    "flows": ["cashflow.csv"],
    "saccr_detail": ["saccrdetail.csv"],
    "dim_evolution": ["dim_evolution.txt"],
    "dim_regression": ["dim_regression*.txt"],
}

# The output files the seed names that are not read into rows, and why. The
# generator refuses a seed output that is neither a family nor stated here. The
# reasons that cite a version hold for that version: re-check them when the
# vendored ORE moves.
OUT_OF_SCOPE = {
    "amcBuffer.dat": "binary path data the AMC engine reads back, not a report",
    "scenariodata.dat": "binary scenario data the engine reads back, not a report",
    "cube.csv.gz": "a compressed copy of the NPV cube; rawcube states it as text",
    "cptyCube.csv.gz": "a compressed counterparty cube the engine reads back",
    "nettingSetCube.csv.gz": "a compressed copy of the netting set cube; netcube states it as text",
    "scenariodata.csv.gz": "a compressed copy of the aggregation scenario data the engine reads back",
    "crossgamma.csv": "no writer reads sensitivity.crossGammaOutputFile at v1.8.17.0",
    "productclass_mapping.csv": "no writer reads sensitivity.productClassMappingOutputFile at v1.8.17.0",
    "creditcurves.csv": "no writer reads creditcurves.outputFileName at v1.8.17.0",
    "parsensi.csv": "no writer reads sensitivity.parRateSensitivityOutputFile at v1.8.17.0",
    "scenariodata.csv": "no writer reads simulation.aggregationScenarioDataDump at v1.8.17.0",
    "xva_sensitivity_jacobi.csv": "the xvaSensitivity analytic writes its jacobi to jacobi.csv at v1.8.17.0",
    "xva_sensitivity_jacobi_inverse.csv": "the xvaSensitivity analytic writes its jacobi to jacobi_inverse.csv at v1.8.17.0",
    "pca.csv": "a stem: the analytic writes pca_<currency>.csv, the pca family",
    "meanReversion.csv": "a stem: the analytic writes meanReversion_<currency>.csv, the mean-reversion family",
}

# The kinds a sample cannot show, because every sample of the column is empty.
KIND_OVERRIDES = {
    ("crif", "Bucket"): "text",
    ("capital_crif", "Bucket"): "text",
    ("dividends", "dividendExDate"): "date",
    ("dividends", "dividendRate"): "decimal",
    ("dividends", "dividendPaymentDate"): "date",
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

# The keys of one family only, because another family states the same name as
# a column of its own: a cashflow states the fixing date of its coupon.
FAMILY_KEYS = {
    "marketdata": ["datumDate", "datumId"],
    "fixings": ["fixingDate", "fixingId"],
    "dividends": ["dividendExDate", "equityId"],
    "jacobi": ["ParFactor", "RawFactor"],
    "jacobi_inverse": ["RawFactor(z)", "ParFactor(c)"],
    "runtimes": ["Key"],
    "additional_results": ["TradeId", "ResultId"],
    "counterparties": ["Counterparty"],
    "netting_sets": ["NettingSets"],
    "trade_types": ["TradeTypes"],
    "marketObjects": ["MarketObjectType"],
    "riskFactors": ["RiskFactorType"],
    "underlying_indices": ["AssetType"],
}

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


def expected_dirs():
    """Each suite's expected output, the notebooks', the legacy examples', then ours.

    Each source states the families it may contribute, None for any, and whether
    its files are synthetic.
    """
    for suite in sorted(os.listdir(EXAMPLES)):
        yield os.path.join(EXAMPLES, suite, "ExpectedOutput"), None, False
    for folder, only in ((NOTEBOOKS, NOTEBOOK_FAMILIES), (LEGACY, LEGACY_FAMILIES)):
        parent = os.path.join(EXAMPLES, folder)
        for example in sorted(os.listdir(parent)):
            yield os.path.join(parent, example, "ExpectedOutput"), only, False
    yield os.path.join(SAMPLES, "captured"), None, False
    yield os.path.join(SAMPLES, "synthetic"), None, True


def collect():
    families = {}
    for expected, only, synthetic in expected_dirs():
        if not os.path.isdir(expected):
            continue
        for dirpath, dirs, files in os.walk(expected):
            dirs.sort()
            for name in sorted(files):
                if not name.endswith(".csv"):
                    continue
                path = os.path.join(dirpath, name)
                family, pattern = family_of(name)
                if only is not None and family not in only:
                    continue
                # Our samples are read last, so a family already seen has one ORE
                # shipped, and is not proven from ours.
                if expected.startswith(SAMPLES) and family in families:
                    continue
                entry = families.setdefault(family, {
                    "family": family, "pattern": pattern, "headers": [],
                    "values": {}, "sample": os.path.relpath(path, ROOT),
                    "synthetic": synthetic})
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

        # A column every variant states is required; the rest are the shape of
        # one configuration. A key is required by definition, so a name that
        # only some variants state is not a key.
        required = {name for name, _ in columns if all(name in h for h in headers)}
        keys = [k for k in dict.fromkeys(KEYS + FAMILY_KEYS.get(family, [])) if k in required]

        named = []
        qualifiers = []
        measures = []
        for name, _ in columns:
            kind = KIND_OVERRIDES.get((family, name)) or \
                kind_of(entry["values"].get(name, []), name in keys)
            named.append((name, kind))
            if name in keys:
                continue
            if kind in ("text", "date", "time") or name in NUMERIC_QUALIFIERS:
                qualifiers.append(name)
            else:
                measures.append(name)

        # A family whose variants disagree on their measures gains columns from
        # the configuration: a curve, a factor or a quantile the run asked for.
        measure_sets = [
            {n for n in h if n not in keys and
             n not in [q for q, k in named if k in ("text", "date", "time")]
             and n not in NUMERIC_QUALIFIERS}
            for h in headers
        ]
        is_open = len({tuple(sorted(m)) for m in measure_sets}) > 1 \
            or family in RISK_FACTOR_FAMILIES

        # A configuration-driven family states the figures the run asked for, so
        # no sample can say which of them every run states.
        if is_open:
            required -= set(measures)

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
            "required": ",".join(sorted(required)),
            "qualifiers": ",".join(qualifiers),
            "measures": ",".join(measures),
            "open": "1" if is_open else "0",
            "variants": len(headers),
            "sample": entry["sample"],
            "aliases": ",".join(ALIASES.get(family, [])),
            "synthetic": "1" if entry["synthetic"] else "0",
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
    lines.append(" * variants the samples show, a sample file, the other names the family is")
    lines.append(" * written under, whether its only sample is synthetic, then each column with")
    lines.append(" * its kind.")
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
                                row["analytic"], row["keys"], row["required"],
                                row["qualifiers"], row["measures"], row["open"],
                                str(row["variants"]), row["sample"], row["aliases"],
                                row["synthetic"], cols]))
    lines.append(')CAT"');
    lines.append("};")
    lines.append("")
    lines.append("}")
    lines.append("")
    return "\n".join(lines)


def page(catalogue):
    """The output formats page: every family, rendered from the catalogue."""
    from output_family_docs import FAMILY_DOCS

    missing = sorted(r["family"] for r in catalogue if r["family"] not in FAMILY_DOCS)
    if missing:
        raise SystemExit("the output formats page states nothing for: " + ", ".join(missing))

    def cell(text):
        return (text or "").replace("|", "\\vert{}")

    out = [
        ":PROPERTIES:",
        f":ID: {PAGE_ID}",
        ":END:",
        "#+title: ORE output formats",
        "#+description: Every output file ORE writes, as ORE Studio reads it: the file, the "
        "parameter that names it, the analytic that writes it, its key and every column with its "
        "kind, what each figure measures and its unit, and a sample that proves it.",
        "#+type: knowledge",
        "#+level: cross",
        "#+filetags: :ore:results:external:knowledge:",
        "#+created: 2026-11-02",
        "#+updated: 2026-11-02",
        "",
        "This page is generated by =projects/ores.ore/scripts/generate_output_catalogue.py= from "
        "the output catalogue and =output_family_docs.py=; edit those and regenerate, never the "
        "page. The generator's =--check= fails when the page and the catalogue disagree.",
        "",
        "* Summary",
        "",
        f"ORE publishes no list of what its output files hold. ORE Studio's catalogue names "
        f"{len(catalogue)} families, derived from the samples ORE ships and checked against the "
        "writers of the vendored version. Each family below states the file it is written to, "
        "the parameter that names it, the analytic that writes it, the columns that identify a "
        "row, every column with its kind, what its figures measure and their unit, and the sample "
        "that proves the readers read it. How the files are read and where the rows are stored is "
        "in [[id:8BF75172-CD91-461A-A3BA-2B4E913D63A8][ORE output processing]].",
        "",
        "* Detail",
        "",
        "** How to read a family",
        "",
        "- *Key* columns identify a row; a reader refuses a row that leaves its subject empty.",
        "- *Qualifier* columns state the context of a figure and travel with it.",
        "- *Measure* columns are the figures; each becomes one stored value named by its heading.",
        "- An *open* family gains a measure column for each curve, factor or quantile the run "
        "asks for, so its measures here are a sample of what a run states, and none is required.",
        "- *Other names* are the names a run writes the family under when its configuration "
        "names none.",
        "- A *synthetic* sample is one ORE Studio wrote from the writer's columns because ORE "
        "ships none; it proves the columns, not real figures.",
        "",
        "** Outputs not read into rows",
        "",
        "| File | Why |",
        "|------+-----|",
    ]
    for name, why in sorted(OUT_OF_SCOPE.items()):
        out.append(f"| ={name}= | {cell(why)} |")
    out += ["", "** Families", ""]
    for r in catalogue:
        meaning, units = FAMILY_DOCS[r["family"]]
        out.append(f"*** {r['family']}")
        out.append("")
        out.append(meaning)
        out.append("")
        out.append("| Field | Value |")
        out.append("|-------+-------|")
        out.append(f"| File | ={r['pattern']}= |")
        if r["aliases"]:
            out.append("| Other names | " + ", ".join(f"={a}=" for a in r["aliases"].split(",")) + " |")
        out.append(f"| Parameter | {('=' + r['parameter'] + '=') if r['parameter'] else 'none; the engine names it'} |")
        out.append(f"| Analytics | {cell(r['analytic'].replace(',', ', ')) or 'none stated'} |")
        out.append(f"| Key | {cell(r['keys'].replace(',', ', ')) or 'none: one row for each line'} |")
        out.append(f"| Measures | {'open: the run decides them' if r['open'] == '1' else 'closed'} |")
        out.append(f"| Units | {cell(units)} |")
        out.append(f"| Header variants | {r['variants']} |")
        sample = f"={r['sample']}=" + (" (synthetic)" if r["synthetic"] == "1" else "")
        out.append(f"| Sample | {sample} |")
        out.append("")
        keys = set(r["keys"].split(",")) if r["keys"] else set()
        qualifiers = set(r["qualifiers"].split(",")) if r["qualifiers"] else set()
        measures = set(r["measures"].split(",")) if r["measures"] else set()
        out.append("| Column | Kind | Role |")
        out.append("|--------+------+------|")
        for name, kind in r["columns"]:
            role = ("key" if name in keys else "qualifier" if name in qualifiers
                    else "measure" if name in measures else "")
            out.append(f"| {cell(name)} | {kind} | {role} |")
        out.append("")
    out += [
        "* See also",
        "",
        "- [[id:8BF75172-CD91-461A-A3BA-2B4E913D63A8][ORE output processing]] — how the files are "
        "read, and why their shape follows the configuration.",
        "- [[id:D697A04C-387C-451E-AC90-1989FCDFCAB9][Report post-processing]] — which store takes "
        "which shape.",
        "- [[id:969C14D2-4280-4D7C-8F51-1D395B521C83][Journey: read what a run produced]] — the "
        "reader's view of the same files.",
        "- [[id:CF2C4F3A-9C5C-4E7A-9E16-2A3A8C1D4C11][External knowledge]]",
        "",
    ]
    return "\n".join(out)


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
    seed = seed_outputs()
    catalogue = rows(families, seed)

    patterns = [r["pattern"] for r in catalogue]
    patterns += [a for r in catalogue for a in r["aliases"].split(",") if a]
    uncovered = sorted(name for name in seed
                       if not any(fnmatch.fnmatch(name, p) for p in patterns)
                       and name not in OUT_OF_SCOPE)
    if uncovered:
        print("the seed names outputs no family reads and no reason excuses: " +
              ", ".join(uncovered), file=sys.stderr)
        return 1
    text = render(catalogue)

    text = formatted(text, DATA)
    data_header = formatted(header(), DATA_HPP)
    formats_page = page(catalogue)

    if args.check:
        for path, want in ((DATA, text), (DATA_HPP, data_header), (PAGE, formats_page)):
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
    with open(PAGE, "w", encoding="utf-8") as f:
        f.write(formats_page)
    named = sum(1 for r in catalogue if r["parameter"])
    print(f"wrote {len(catalogue)} families, {named} named by a parameter, "
          f"{len(catalogue) - named} by an engine convention")
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
