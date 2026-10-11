"""
Generates the report source data for the ACME Corporation dataset:
report_definitions.json, read by generate_sql.py in this same directory.

Each legal entity runs the reports it would run in a real group, and the
four entities do not share one list. The rules below follow the design in the
investigation "Which reports each ACME party runs" (sprint 27). A rule is
named by its number in that page so a reviewer can find the reason for it.

- D1: an entity runs a report when it is accountable for the output.
- D2: the holding company runs consolidation, group oversight and the
  consolidated ratios. It has no desk, so it runs no trading analytics.
- D3: a trading entity runs the analytics its desks need, scaled to its
  depth. London runs the full set. New York and Hong Kong run the core set.
- D4: the jurisdiction reports follow the entity.
- D5: the platform health report runs once, at the holding company.
- D6: a report runs only with the reports it depends on. The core set keeps
  every prerequisite of every report in it.
- D7: every trading entity runs Stressed VaR and P&L Attribution.
- D8: only London runs the Vega and Volatility Surfaces reports.

The report texts, schedules and concurrency policies come from
report_catalogue.json, which holds the report definitions the platform seeds.

Usage:
    python3 generate_report_data.py
"""
import json
import os

import generate_configuration_data as documents

SCRIPT_DIR = os.path.dirname(os.path.abspath(__file__))

# The legal entities and what each one is.
ENTITIES = {
    "acme_group": {"role": "holding", "jurisdiction": None, "depth": None},
    "acme_uk": {"role": "trading", "jurisdiction": "GB", "depth": "full"},
    "acme_us": {"role": "trading", "jurisdiction": "US", "depth": "core"},
    "acme_hk": {"role": "trading", "jurisdiction": "HK", "depth": "core"},
}

# D5: the platform runs one health report, for the group.
PLATFORM_REPORTS = {"System Health"}

# D2: the capital and liquidity ratios the holding company files on a
# consolidated basis. The entities that hold the positions file their own.
CONSOLIDATED_REPORTS = {"Leverage Ratio", "Liquidity Coverage", "Large Exposures"}

# D8: the option risk reports, which only the full-depth entity runs.
OPTION_REPORTS = {"Vega", "Volatility Surfaces"}

# D6: what a report needs in the same entity, from the prerequisites its
# catalogue text states. Stressed VaR is left out on purpose: its text names
# the volatility surface as a prerequisite elsewhere, but it also states that it
# uses full revaluation, and the core set runs it without a surface (D7, D8).
PREREQUISITES = {
    "Exposure": {"Model Calibration", "Yield Curves"},
    "CVA/DVA/FVA": {"Exposure", "Credit Curves"},
    "NPV": {"Yield Curves", "FX Spot Rates"},
    "Delta and Gamma": {"NPV"},
    "Bucketed DV01": {"NPV"},
    "Vega": {"NPV", "Volatility Surfaces"},
}


def load(name):
    with open(os.path.join(SCRIPT_DIR, f"{name}.json")) as f:
        return json.load(f)


def write(name, records):
    path = os.path.join(SCRIPT_DIR, f"{name}.json")
    with open(path, "w") as f:
        json.dump(records, f, indent=2, ensure_ascii=False)
        f.write("\n")
    print(f"Wrote {path} ({len(records)} records)")


def runs(entity, report):
    tier = report["tier"]
    name = report["name"]

    if entity["role"] == "holding":
        # D2: oversight, consolidation and the consolidated ratios. D5: the
        # platform report. The common tier is the reports every entity runs.
        return tier in ("strategic", "common") or name in CONSOLIDATED_REPORTS

    if name in PLATFORM_REPORTS:
        return False
    if tier == "strategic":
        return False
    jurisdiction = report["applicable_jurisdiction"]
    if jurisdiction is not None and jurisdiction != entity["jurisdiction"]:
        return False
    if name in OPTION_REPORTS and entity["depth"] != "full":
        return False
    return True


def check_prerequisites(company, records):
    names = {r["name"] for r in records}
    for name in names & PREREQUISITES.keys():
        missing = PREREQUISITES[name] - names
        if missing:
            raise SystemExit(f"{company} runs {name} without {sorted(missing)} (D6)")


def bindings(company, records):
    """Every definition binds the document of each type that exists for its entity. The
    risk report type requires four documents. A type is bound once its document is
    published, so the list grows with the documents."""
    return [{
        "company_code": company,
        "version": 0,
        "report_name": r["name"],
        "configuration_type_code": type_code,
        "configuration_name": documents.document_name(company, type_code),
    } for r in records for type_code in documents.DOCUMENT_NAMES]


def report_definitions():
    catalogue = load("report_catalogue")
    records = []
    for company, entity in ENTITIES.items():
        chosen = [{"company_code": company, **r} for r in catalogue if runs(entity, r)]
        check_prerequisites(company, chosen)
        records.extend(chosen)
    return records


def main():
    definitions = report_definitions()
    write("report_definitions", definitions)
    write("report_configurations", [
        b for company in ENTITIES
        for b in bindings(company, [r for r in definitions if r["company_code"] == company])])


if __name__ == "__main__":
    main()
