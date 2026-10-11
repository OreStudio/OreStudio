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
        if tier == "strategic" or name in PLATFORM_REPORTS:
            return True
        if tier == "common":
            return True
        return name in CONSOLIDATED_REPORTS

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


def report_definitions():
    catalogue = load("report_catalogue")
    records = []
    for company, entity in ENTITIES.items():
        for report in catalogue:
            if runs(entity, report):
                records.append({"company_code": company, **report})
    return records


def main():
    write("report_definitions", report_definitions())


if __name__ == "__main__":
    main()
