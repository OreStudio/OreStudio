"""
Generates the risk report configurations of the ACME Corporation dataset, read by
generate_sql.py in this same directory: risk_report_configs.json holds the
configurations, configuration_analytics.json the analytics each one runs, and
configuration_analytic_parameters.json the values of their parameters.

A risk report configuration holds the run settings of a report: the analytics it
runs with their parameters, the currency it reports in, and the portfolio whose
subtree it reads.
Reports that run the same analytics over the same scope in the same currency share
one configuration, so an entity holds far fewer configurations than reports. A
definition binds a configuration by name, as it binds its pricing engines.

The analytics of each report follow the analysis "What each ACME report needs to
run" (sprint 27). The currency and the scope follow rules D9 and D10 of the sprint 27
investigation "Which reports each ACME party runs". The entity's portfolio tree has
one root and two branches (D13):

- D9: a report runs in the functional currency of its entity, which is the
  aggregation currency of the entity's root portfolio.
- D10: every report reads the whole entity, because a report may be run on any
  book, and the banking book holds positions a valuation report prices. The
  holding company has only its Group Treasury portfolio.

Usage:
    python3 generate_risk_config_data.py
"""
import json
import os

import generate_configuration_data as documents

SCRIPT_DIR = os.path.dirname(os.path.abspath(__file__))

# The analytics each report turns on. An ORE run needs at least one, so a report that
# inspects data rather than prices it asks for the portfolio details: the market
# objects and risk factors the portfolio needs. FX Spot Rates asks for the curves
# analytic, which writes the calibration of the market the rates belong to. The
# platform has one report workflow and it runs the engine, so these seven reports
# run it too.
ANALYTICS = {
    "Data Quality Completeness": ["portfolio_details"],
    "System Health": ["portfolio_details"],
    "Audit Trail Summary": ["portfolio_details"],
    "SA-CCR Exposure": ["npv", "saccr"],
    "Leverage Ratio": ["npv", "saccr"],
    "FRTB SA Capital": ["sensitivity"],
    "Liquidity Coverage": ["cashflow"],
    "Large Exposures": ["npv", "saccr"],
    "MIFID II Transaction Reporting": ["portfolio_details"],
    "CFTC Swap Data Reporting": ["portfolio_details"],
    "HKMA Trade Repository": ["portfolio_details"],
    "Group Consolidated Exposure": ["npv", "saccr"],
    "Board Risk Dashboard": ["npv", "saccr", "parametric_var"],
    "Cross-Entity Counterparty Concentration": ["npv", "saccr"],
    "Intercompany Exposure Matrix": ["npv", "saccr"],
    "Model Calibration": ["calibration"],
    "Yield Curves": ["curves"],
    "FX Spot Rates": ["curves"],
    "Volatility Surfaces": ["curves"],
    "Credit Curves": ["curves", "credit_curves"],
    "NPV": ["npv"],
    "Cashflows": ["cashflow"],
    "Delta and Gamma": ["sensitivity"],
    "Vega": ["sensitivity"],
    "Bucketed DV01": ["sensitivity"],
    "Exposure": ["simulation", "pfe"],
    "CVA/DVA/FVA": ["simulation", "xva"],
    "Stressed VaR": ["historical_var"],
    "P&L Attribution": ["npv", "pnl_explain"],
    "Intraday Risk Monitor": ["npv", "sensitivity", "parametric_var"],
    "Headline Position": ["npv", "sensitivity"],
}

# The name of each set of analytics, for the name of the configuration.
PROFILES = {
    frozenset(["portfolio_details"]): "Portfolio details",
    frozenset(["npv", "saccr"]): "Standardised counterparty exposure",
    frozenset(["sensitivity"]): "Sensitivities",
    frozenset(["cashflow"]): "Cashflows",
    frozenset(["curves"]): "Curves",
    frozenset(["curves", "credit_curves"]): "Credit curves",
    frozenset(["npv"]): "Valuation",
    frozenset(["calibration"]): "Model calibration",
    frozenset(["simulation", "pfe"]): "Exposure simulation",
    frozenset(["simulation", "xva"]): "Valuation adjustments",
    frozenset(["historical_var"]): "Stressed VaR",
    frozenset(["npv", "pnl_explain"]): "Profit and loss explain",
    frozenset(["npv", "sensitivity"]): "Valuation and sensitivities",
    frozenset(["npv", "sensitivity", "parametric_var"]): "Intraday risk",
    frozenset(["npv", "saccr", "parametric_var"]): "Board risk",
}

ROOT_PORTFOLIO = {
    "acme_group": "Group Treasury Portfolio",
    "acme_uk": "Global Portfolio",
    "acme_us": "Global Portfolio",
    "acme_hk": "Global Portfolio",
}

# The analytic types of the ORE run document that each analytic key stands for, in
# the order a run executes them.
ORE_TYPES = {
    "portfolio_details": "portfolioDetails",
    "npv": "npv",
    "cashflow": "cashflow",
    "curves": "curves",
    "credit_curves": "creditcurves",
    "calibration": "calibration",
    "sensitivity": "sensitivity",
    "stress": "stress",
    "simulation": "simulation",
    "xva": "xva",
    "pfe": "pfe",
    "saccr": "saccr",
    "historical_var": "historicalSimulationVar",
    "pnl_explain": "pnlExplain",
    "initial_margin": "initialMargin",
    "parametric_var": "parametricVar",
}

# The window a stressed VaR looks back over. It is the financial crisis of 2008 and
# 2009. This is an ACME choice that the shipped samples do not give, and a reviewer
# who knows the regime confirms it.
STRESS_WINDOW = "2008-09-01,2009-03-31"

# The choices each analytic stores, as (parameter, value). A value that names a
# currency is written {ccy}. The parameters that name a file are not stored: the
# export resolves an input document through the bindings and takes an output name
# from the file types table. The values are the most common ones in the ORE samples,
# counted in the investigation "The analytics of a configuration and their
# parameters".
PARAMETERS = {
    "portfolioDetails": [],
    "npv": [("baseCurrency", "{ccy}")],
    "cashflow": [],
    "curves": [("configuration", "default"), ("grid", "240,1M")],
    "creditcurves": [("configuration", "default"), ("grid", "240,1M")],
    "calibration": [],
    "sensitivity": [
        ("recalibrateModels", "Y"),
        ("outputSensitivityThreshold", "0.000001"),
        ("parSensitivity", "Y"),
    ],
    "stress": [],
    "simulation": [("baseCurrency", "{ccy}"), ("storeFlows", "Y"), ("amc", "N")],
    "xva": [
        ("exposureProfiles", "Y"),
        ("cva", "Y"),
        ("dva", "Y"),
        ("fva", "Y"),
        ("quantile", "0.95"),
        ("calculationType", "Symmetric"),
        ("allocationMethod", "None"),
        ("exposureProfilesByTrade", "Y"),
        ("exerciseNextBreak", "N"),
        ("baseCurrency", "{ccy}"),
    ],
    "pfe": [
        ("exposureProfiles", "Y"),
        ("exposureProfilesByTrade", "N"),
        ("quantile", "0.95"),
        ("calculationType", "Symmetric"),
        ("allocationMethod", "None"),
        ("marginalAllocationLimit", "1.0"),
        ("exerciseNextBreak", "N"),
        ("baseCurrency", "{ccy}"),
    ],
    "saccr": [("simmVersion", "2.1")],
    "historicalSimulationVar": [
        ("historicalPeriod", STRESS_WINDOW),
        ("mporDays", "10"),
        ("mporCalendar", "{ccy}"),
        ("mporOverlappingPeriods", "true"),
        ("quantiles", "0.99"),
        ("includeExpectedShortfall", "Y"),
        ("breakdown", "Y"),
    ],
    "pnlExplain": [("parSensitivity", "Y")],
    "initialMargin": [],
    "parametricVar": [
        ("quantiles", "0.99"),
        ("method", "DeltaGammaNormal"),
        ("breakdown", "Y"),
    ],
}

# The threads a simulation of the exposure profiles needs.
SIMULATION_THREADS = 4

# The choices of an ORE run that are no data file, as ORE spells them. Every
# shipped sample states the folders and the log file, and almost all state the
# log mask and the fixings and error flags.
SETUP_CHOICES = {
    "input_path": "Input",
    "output_path": "Output",
    "log_file": "log.txt",
}

# The engine settings every ACME run states. A report that asks for a simulation
# of the exposure profiles runs on the simulation settings, which add threads.
ENGINE_FIXINGS = {"imply_todays_fixings": False}
ENGINE_BUILDING = {"observation_model": "Disable", "continue_on_error": False}
ENGINE_EXECUTION = {"log_mask": 31}

# The market roles a run prices against and the today's market configuration each
# one reads. Every ACME document names one configuration, default.
MARKETS = [("pricing", "default")]


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
    if not ANALYTICS[report_name]:
        raise SystemExit(f"The report {report_name} switches no analytic on, and an ORE run needs one")
    return frozenset(ANALYTICS[report_name])


def scope_of(company, report_name):
    """The portfolio whose subtree the report reads (D10)."""
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


def check_exposure_needs_simulation(name, analytics):
    """The pfe analytic reads the cube a simulation writes."""
    if "pfe" in analytics and "simulation" not in analytics:
        raise SystemExit(f"{name} switches pfe on without a simulation")


def check_scopes(company, names, portfolios):
    held = {p["name"] for p in portfolios if p["company_code"] == company}
    for name in names:
        if name not in held:
            raise SystemExit(f"{company} scopes a configuration to {name}, which it does not hold")


def analytic_rows(company, name, configuration_id, analytics, currency):
    """The analytics of one configuration and the values of their parameters."""
    rows, values = [], []
    for key, ore_type in ORE_TYPES.items():
        if key not in analytics:
            continue
        row_id = documents.new_uuid(f"configuration_analytic:{company}:{name}:{ore_type}")
        rows.append({
            "company_code": company,
            "id": row_id,
            "version": 0,
            "party_id": documents.NO_PARTY,
            "configuration_id": configuration_id,
            "analytic_type_code": ore_type,
            "display_order": len(rows) + 1,
            "active": "Y",
        })
        for position, (parameter, value) in enumerate(PARAMETERS[ore_type], start=1):
            values.append({
                "company_code": company,
                "id": documents.new_uuid(
                    f"configuration_analytic_parameter:{company}:{name}:{ore_type}:{parameter}"),
                "version": 0,
                "party_id": documents.NO_PARTY,
                "configuration_analytic_id": row_id,
                "parameter_name": parameter,
                "value": value.format(ccy=currency),
                "position": position,
            })
    return rows, values


def risk_report_configs(definitions, portfolios):
    """One configuration for each distinct (analytics, scope) of an entity's reports,
    with its analytics and their parameters."""
    configs, analytics_rows, parameter_rows, market_rows = [], [], [], []
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
            check_exposure_needs_simulation(name, analytics)
            configuration_id = documents.new_uuid(f"risk_report_config_header:{company}:{name}")
            configs.append({
                "company_code": company,
                "id": documents.new_uuid(f"risk_report_config:{company}:{name}"),
                "version": 0,
                "party_id": documents.NO_PARTY,
                "name": name,
                "description": f"Run settings shared by {', '.join(sorted(reports))}.",
                "configuration_id": configuration_id,
                "base_currency": currency,
                "market_data_type": "eod",
                "market_data_date": None,
                **SETUP_CHOICES,
                "scope_portfolio": scope_of(company, first),
            })
            rows, values = analytic_rows(company, name, configuration_id, analytics, currency)
            analytics_rows.extend(rows)
            parameter_rows.extend(values)
            for position, (role, market) in enumerate(MARKETS, start=1):
                market_rows.append({
                    "company_code": company,
                    "id": documents.new_uuid(f"configuration_market_binding:{company}:{name}:{role}"),
                    "version": 0,
                    "party_id": documents.NO_PARTY,
                    "configuration_id": configuration_id,
                    "role": role,
                    "market_configuration_name": market,
                    "position": position,
                })
    return configs, analytics_rows, parameter_rows, market_rows


def engine_settings_name(company, report_name):
    kind = "Simulation" if "simulation" in analytics_of(report_name) else "Standard"
    return f"{documents.LABEL[company]} - {kind} engine settings"


def engine_settings(definitions):
    """One engine settings configuration for each distinct kind of run an entity has,
    as the central row and its three parts."""
    central, fixings, building, execution = [], [], [], []
    for company in documents.LABEL:
        used = {}
        for d in definitions:
            if d["company_code"] != company:
                continue
            name = engine_settings_name(company, d["name"])
            used.setdefault(name, (d["name"], []))[1].append(d["name"])
        for name in sorted(used):
            first, reports = used[name]
            simulation = "simulation" in analytics_of(first)
            settings_id = documents.new_uuid(f"engine_settings:{company}:{name}")
            central.append({
                "company_code": company,
                "id": settings_id,
                "version": 0,
                "party_id": documents.NO_PARTY,
                "name": name,
                "description": f"Engine settings shared by {', '.join(sorted(reports))}.",
                "configuration_id": documents.new_uuid(f"engine_settings_header:{company}:{name}"),
            })
            for rows, part, values in (
                    (fixings, "fixings", ENGINE_FIXINGS),
                    (building, "building", ENGINE_BUILDING),
                    (execution, "execution",
                     {**ENGINE_EXECUTION, "n_threads": SIMULATION_THREADS if simulation else 1})):
                rows.append({
                    "company_code": company,
                    "id": documents.new_uuid(f"engine_settings_{part}:{company}:{name}"),
                    "version": 0,
                    "party_id": documents.NO_PARTY,
                    "engine_settings_id": settings_id,
                    **values,
                })
    return central, fixings, building, execution


def main():
    definitions = load("report_definitions")
    configs, analytics_rows, parameter_rows, market_rows = risk_report_configs(
        definitions, load("portfolios"))
    write("risk_report_configs", configs)
    write("configuration_analytics", analytics_rows)
    write("configuration_analytic_parameters", parameter_rows)
    write("configuration_market_bindings", market_rows)
    for table, rows in zip(("engine_settings", "engine_settings_fixings", "engine_settings_building",
                            "engine_settings_execution"), engine_settings(definitions)):
        write(table, rows)


if __name__ == "__main__":
    main()
