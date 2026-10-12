"""
Generates the risk report configurations of the ACME Corporation dataset:
risk_report_configs.json, read by generate_sql.py in this same directory.

A risk report configuration holds the run settings of a report: the analytics it
switches on, the currency it reports in, and the portfolio whose subtree it reads.
Reports that run the same analytics over the same scope in the same currency share
one configuration, so an entity holds far fewer configurations than reports. A
definition binds a configuration by name, as it binds its pricing engines.

The analytics of each report follow the analysis "What each ACME report needs to
run" (sprint 27). The currency and the scope follow rules D9 and D10 of the sprint 27
investigation "Which reports each ACME party runs". The entity's portfolio tree has
one root and two branches (D13):

- D9: a report runs in the functional currency of its entity, which is the
  aggregation currency of the entity's root portfolio.
- D10: a report on trading positions reads the Trading Book branch. A liquidity,
  leverage, regulatory or data report reads the whole entity. The holding company
  has only its Group Treasury portfolio.

Usage:
    python3 generate_risk_config_data.py
"""
import json
import os

import generate_configuration_data as documents

SCRIPT_DIR = os.path.dirname(os.path.abspath(__file__))

# The analytics each report turns on. A report that inspects data rather than
# prices it turns none on.
ANALYTICS = {
    "Data Quality Completeness": [],
    "System Health": [],
    "Audit Trail Summary": [],
    "SA-CCR Exposure": ["pfe"],
    "Leverage Ratio": ["pfe"],
    "FRTB SA Capital": ["sensitivity"],
    "Liquidity Coverage": ["cashflow"],
    "Large Exposures": ["pfe"],
    "MIFID II Transaction Reporting": [],
    "CFTC Swap Data Reporting": [],
    "HKMA Trade Repository": [],
    "Group Consolidated Exposure": ["pfe"],
    "Board Risk Dashboard": ["npv", "pfe", "parametric_var"],
    "Cross-Entity Counterparty Concentration": ["pfe"],
    "Intercompany Exposure Matrix": ["pfe"],
    "Model Calibration": ["simulation"],
    "Yield Curves": ["curves"],
    "FX Spot Rates": [],
    "Volatility Surfaces": ["curves"],
    "Credit Curves": ["curves"],
    "NPV": ["npv"],
    "Cashflows": ["cashflow"],
    "Delta and Gamma": ["sensitivity"],
    "Vega": ["sensitivity"],
    "Bucketed DV01": ["sensitivity"],
    "Exposure": ["simulation", "pfe"],
    "CVA/DVA/FVA": ["simulation", "xva"],
    "Stressed VaR": ["simulation", "stress"],
    "P&L Attribution": ["npv", "sensitivity"],
    "Intraday Risk Monitor": ["npv", "sensitivity", "parametric_var"],
    "Headline Position": ["npv", "sensitivity"],
}

# The name of each set of analytics, for the name of the configuration.
PROFILES = {
    frozenset(): "Data checks",
    frozenset(["pfe"]): "Counterparty exposure",
    frozenset(["sensitivity"]): "Sensitivities",
    frozenset(["cashflow"]): "Cashflows",
    frozenset(["curves"]): "Curves",
    frozenset(["npv"]): "Valuation",
    frozenset(["simulation"]): "Model calibration",
    frozenset(["simulation", "pfe"]): "Exposure simulation",
    frozenset(["simulation", "xva"]): "Valuation adjustments",
    frozenset(["simulation", "stress"]): "Stressed VaR",
    frozenset(["npv", "sensitivity"]): "Valuation and sensitivities",
    frozenset(["npv", "sensitivity", "parametric_var"]): "Intraday risk",
    frozenset(["npv", "pfe", "parametric_var"]): "Board risk",
}

# D10: the reports on trading positions.
TRADING_REPORTS = {
    "FRTB SA Capital", "NPV", "Cashflows", "Delta and Gamma", "Vega", "Bucketed DV01",
    "Exposure", "CVA/DVA/FVA", "Stressed VaR", "P&L Attribution", "Intraday Risk Monitor",
    "Headline Position",
}

ROOT_PORTFOLIO = {
    "acme_group": "Group Treasury Portfolio",
    "acme_uk": "Global Portfolio",
    "acme_us": "Global Portfolio",
    "acme_hk": "Global Portfolio",
}

# The switch columns of the configuration, by analytic.
SWITCH_COLUMNS = {
    "npv": "npv_enabled",
    "cashflow": "cashflow_enabled",
    "curves": "curves_enabled",
    "sensitivity": "sensitivity_enabled",
    "simulation": "simulation_enabled",
    "xva": "xva_enabled",
    "stress": "stress_enabled",
    "parametric_var": "parametric_var_enabled",
    "initial_margin": "initial_margin_enabled",
    "pfe": "pfe_enabled",
}

# The quantile exposure and value-at-risk report at, and the threads a simulation
# of the exposure profiles needs.
EXPOSURE_QUANTILE = 0.95
VAR_QUANTILE = 0.99
SIMULATION_THREADS = 4


def load(name):
    with open(os.path.join(SCRIPT_DIR, f"{name}.json")) as f:
        return json.load(f)


def write(name, records):
    path = os.path.join(SCRIPT_DIR, f"{name}.json")
    with open(path, "w") as f:
        json.dump(records, f, indent=2, ensure_ascii=False)
        f.write("\n")
    print(f"Wrote {path} ({len(records)} records)")


def analytics_of(report_name):
    if report_name not in ANALYTICS:
        raise SystemExit(f"No analytics are decided for the report {report_name}")
    return frozenset(ANALYTICS[report_name])


def scope_of(company, report_name):
    """The portfolio whose subtree the report reads (D10)."""
    if company == "acme_group":
        return ROOT_PORTFOLIO[company]
    if report_name in TRADING_REPORTS:
        return "Trading Book"
    return ROOT_PORTFOLIO[company]


def configuration_name(company, report_name):
    analytics = analytics_of(report_name)
    if analytics not in PROFILES:
        raise SystemExit(f"No profile is named for the analytics of {report_name}: {sorted(analytics)}")
    return f"{PROFILES[analytics]} - {scope_of(company, report_name)}"


def base_currency(company, portfolios):
    """D9: the aggregation currency of the entity's root portfolio."""
    root = [p for p in portfolios if p["company_code"] == company
            and p["name"] == ROOT_PORTFOLIO[company]]
    if len(root) != 1:
        raise SystemExit(f"{company} has {len(root)} portfolios named {ROOT_PORTFOLIO[company]}")
    return root[0]["aggregation_ccy"]


def settings(analytics):
    """The columns that follow from the analytics alone."""
    s = {column: 0 for column in SWITCH_COLUMNS.values()}
    for analytic in analytics:
        s[SWITCH_COLUMNS[analytic]] = 1
    exposure = "pfe" in analytics or "xva" in analytics
    s.update({
        "n_threads": SIMULATION_THREADS if "simulation" in analytics else 1,
        "xva_quantile": EXPOSURE_QUANTILE if exposure else None,
        "xva_cva_enabled": 1 if "xva" in analytics else 0,
        "xva_dva_enabled": 1 if "xva" in analytics else 0,
        "xva_fva_enabled": 1 if "xva" in analytics else 0,
        "xva_colva_enabled": 0,
        "xva_dim_enabled": 0,
        "xva_dim_quantile": None,
        "xva_dim_horizon_calendar_days": None,
        "xva_dim_regression_order": None,
        "var_quantiles": [VAR_QUANTILE] if "parametric_var" in analytics else None,
        "var_method": "delta" if "parametric_var" in analytics else None,
        "simm_version": None,
        "simm_calculation_currency": None,
    })
    return s


def check_scopes(company, names, portfolios):
    held = {p["name"] for p in portfolios if p["company_code"] == company}
    for name in names:
        if name not in held:
            raise SystemExit(f"{company} scopes a configuration to {name}, which it does not hold")


def risk_report_configs(definitions, portfolios):
    """One configuration for each distinct (analytics, scope) of an entity's reports."""
    records = []
    for company in documents.LABEL:
        used = {}
        for d in definitions:
            if d["company_code"] != company:
                continue
            name = configuration_name(company, d["name"])
            used.setdefault(name, (d["name"], []))[1].append(d["name"])
        check_scopes(company, {scope_of(company, reports[0]) for _first, reports in used.values()},
                     portfolios)
        currency = base_currency(company, portfolios)
        for name in sorted(used):
            first, reports = used[name]
            analytics = analytics_of(first)
            records.append({
                "company_code": company,
                "id": documents.new_uuid(f"risk_report_config:{company}:{name}"),
                "version": 0,
                "party_id": documents.NO_PARTY,
                "name": name,
                "description": f"Run settings shared by {', '.join(sorted(reports))}.",
                "configuration_id": documents.new_uuid(f"risk_report_config_header:{company}:{name}"),
                "base_currency": currency,
                "observation_model": "disable",
                "market_data_type": "eod",
                "market_data_date": None,
                **settings(analytics),
                "scope_portfolio": scope_of(company, first),
            })
    return [with_staging_order(r) for r in records]


# The columns in the order of the staging table.
STAGING_ORDER = [
    "company_code", "id", "version", "party_id", "name", "description", "configuration_id",
    "base_currency", "observation_model", "n_threads", "market_data_type", "market_data_date",
    "npv_enabled", "cashflow_enabled", "curves_enabled", "sensitivity_enabled",
    "simulation_enabled", "xva_enabled", "stress_enabled", "parametric_var_enabled",
    "initial_margin_enabled", "pfe_enabled", "xva_quantile", "xva_cva_enabled",
    "xva_dva_enabled", "xva_fva_enabled", "xva_colva_enabled", "xva_dim_enabled",
    "xva_dim_quantile", "xva_dim_horizon_calendar_days", "xva_dim_regression_order",
    "var_quantiles", "var_method", "simm_version", "simm_calculation_currency",
    "scope_portfolio",
]


def with_staging_order(record):
    missing = set(STAGING_ORDER) ^ set(record)
    if missing:
        raise SystemExit(f"A risk report configuration differs from the staging table in {sorted(missing)}")
    return {c: record[c] for c in STAGING_ORDER}


def main():
    definitions = load("report_definitions")
    write("risk_report_configs", risk_report_configs(definitions, load("portfolios")))


if __name__ == "__main__":
    main()
