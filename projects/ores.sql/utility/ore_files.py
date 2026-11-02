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
"""The files of an ORE run, as the ORE user guide identifies them.

Each entry is one file: its code, the name the guide gives it, its category, the
configuration type of a configuration document, the label of the guide section
that describes it, whether a run is given it or ORE writes it, its format and
XSD, the name ORE gives it when nothing names it otherwise, and the names a run
may give it. What each file is for is written in ore_file_descriptions.py.

The code is the guide's name for the file where the guide gives one: the NPV
cube, the market data, the fixing history. A configuration document takes the
code of its configuration type. An output the guide calls a report is the report
of what it states: npv_report, cashflow_report. Any other output is named after
the file ORE writes. The variants of one file are one entry: the pricing engine
configuration a run gives for AMC is still the pricing engines.

The names are matched whole, so no name a run writes may match two entries. The
file type of each parameter names its file by this code, and each output family
of the ores.ore catalogue is keyed by it.
"""

import re

SUPPLIED = "supplied"
PRODUCED = "produced"
PACKAGED = "packaged"

# The groups the files fall in. The supplied groups follow the chapters of the
# guide; the produced groups follow the three steps of its ORE Data Flow chapter,
# with the reports kept apart from the data files one analytic writes for another.
# The packaging group is ORE Studio's own: the archives a run travels in.
CATEGORIES = [
    ("master_input", "Master Input", SUPPLIED),
    ("configuration", "Configuration", SUPPLIED),
    ("trade_data", "Trade Data", SUPPLIED),
    ("market_data", "Market Data", SUPPLIED),
    ("calibration_report", "Calibration Reports", PRODUCED),
    ("valuation_report", "Valuation Reports", PRODUCED),
    ("risk_report", "Risk Reports", PRODUCED),
    ("capital_report", "Capital and Margin Reports", PRODUCED),
    ("xva_report", "XVA and Exposure Reports", PRODUCED),
    ("portfolio_details", "Portfolio Details", PRODUCED),
    ("intermediate_data", "Intermediate Data", PRODUCED),
    ("run_diagnostics", "Run Diagnostics", PRODUCED),
    ("packaging", "ORE Studio Packaging", PACKAGED),
]


def _rx(name):
    """A name as a regular expression; a * stands for a key, which may be empty."""
    return re.escape(name).replace(r"\*", ".*")


def entry(code, name, category, origin, fmt, typical, *variants, section=None, xsd=None,
          configuration_type=None, patterns=()):
    """One file, with the names beyond the typical one that a run may give it, and
    any names no * can state, as regular expressions."""
    alternatives = [_rx(typical)] + [_rx(n) for n in variants] + list(patterns)
    regex = "^" + (alternatives[0] if len(alternatives) == 1
                   else "(" + "|".join(alternatives) + ")") + "$"
    return {"code": code, "name": name, "category_code": category,
            "configuration_type_code": configuration_type, "guide_section": section,
            "origin": origin, "format": fmt, "xsd_file": xsd, "typical_filename": typical,
            "filename_regex": regex}


S, P, K = SUPPLIED, PRODUCED, PACKAGED

# The archive the compute node uploads is named by the id of its result.
RESULT_ARCHIVE = "[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}\\.tar\\.gz"

FILES = [
    entry("master_input", "Analytics", "master_input", S, "xml", "ore.xml",
          section="sec:master_input", xsd="ore.xsd"),

    entry("todays_market", "Market", "configuration", S, "xml", "todaysmarket.xml",
          section="sec:market", xsd="todaysmarket.xsd", configuration_type="todays_market"),
    entry("pricing_engines", "Pricing Engines", "configuration", S, "xml", "pricingengine.xml",
          "pricingengine_amc.xml", "pricingengine_amccg.xml", "pricingengine_dg.xml",
          xsd="pricingengines.xsd", configuration_type="pricing_engines"),
    entry("simulation", "Simulation", "configuration", S, "xml", "simulation.xml",
          "simulationmarket.xml", "simulationmodels.xml", "simulationparams.xml",
          "simulation_B.xml", "simulation_ccs.xml", "sensimarket.xml", "xvasensimarket.xml",
          "stressmarket.xml", "sensitivitystressmarket.xml",
          section="sec:simulation", xsd="simulation.xsd", configuration_type="simulation"),
    entry("sensitivity", "Sensitivity Analysis", "configuration", S, "xml", "sensitivity.xml",
          "sensitivity_stress.xml", "sensitivitystress.xml", "xvasensiconfig.xml",
          section="sec:sensitivity", xsd="sensitivity.xsd", configuration_type="sensitivity"),
    entry("stress_test", "Stress Scenario Analysis", "configuration", S, "xml",
          "stresstest.xml", "stressconfig.xml", "parstresstest.xml", "stresstest_sensi.xml",
          section="sec:stress", xsd="stress.xsd", configuration_type="stress_test"),
    entry("calendar_adjustment", "Calendar Adjustment", "configuration", S, "xml",
          "calendaradjustment.xml", section="sec:calendaradjustment",
          xsd="calendaradjustment.xsd", configuration_type="calendar_adjustment"),
    entry("curve_configuration", "Curves", "configuration", S, "xml", "curveconfig.xml",
          section="sec:curveconfig", xsd="curveconfig.xsd",
          configuration_type="curve_configuration"),
    entry("reference_data", "Reference Data", "configuration", S, "xml", "referencedata.xml",
          xsd="referencedata.xsd", configuration_type="reference_data"),
    entry("ibor_fallback", "Ibor Fallback Config", "configuration", S, "xml",
          "iborFallbackConfig.xml", "ibor_fallback.xml", xsd="iborfallbackconfig.xsd",
          configuration_type="ibor_fallback"),
    entry("simm_calibration", "SIMM Calibration", "configuration", S, "xml",
          "simm_calibration.xml", section="sec:simmcalibration", xsd="simmcalibration.xsd",
          configuration_type="simm_calibration"),
    entry("conventions", "Conventions", "configuration", S, "xml", "conventions.xml",
          xsd="conventions.xsd", configuration_type="conventions"),
    entry("currency_configuration", "Currencies", "configuration", S, "xml", "currencies.xml",
          xsd="currencyconfig.xsd", configuration_type="currency_configuration"),
    entry("credit_simulation", "Credit Simulation", "configuration", S, "xml",
          "creditsimulation.xml", xsd="creditsimulation.xsd",
          configuration_type="credit_simulation"),
    entry("crif_market", "CRIF Market", "configuration", S, "xml", "crifmarket.xml",
          xsd="simulation.xsd"),
    entry("script_library", "Script Library", "configuration", S, "xml", "scriptlibrary.xml",
          xsd="scriptlibrary.xsd"),

    entry("trade_data", "Trade Data", "trade_data", S, "xml", "portfolio.xml",
          "mporportfolio.xml", section="sec:portfolio_data", xsd="input.xsd"),
    entry("netting_set_definitions", "Netting Set Definitions", "trade_data", S, "xml",
          "netting.xml", section="sec:nettingsetinput", xsd="nettingsetdefinitions.xsd",
          configuration_type="netting_set_definitions"),
    entry("counterparty_information", "Counterparty Information", "trade_data", S, "xml",
          "counterparty.xml", section="sec:counterpartyinformation", xsd="counterparty.xsd",
          configuration_type="counterparty_information"),
    entry("collateral_balances", "Collateral Balances", "trade_data", S, "xml",
          "collateralbalances.xml", section="sec:collateralbalances",
          xsd="collateralbalance.xsd"),
    entry("crif_schedule", "CRIF Schedule", "trade_data", S, "csv", "crif_schedule.csv"),

    entry("market_data", "Market Data", "market_data", S, "txt", "market.txt",
          section="sec:market_data"),
    entry("fixing_history", "Fixing History", "market_data", S, "txt", "fixings.txt",
          section="sec:fixings"),
    entry("covariance", "Covariance", "market_data", S, "csv", "covariance.csv"),
    entry("historical_scenarios", "Historical Scenarios", "market_data", S, "csv",
          "scenarios.csv"),
    entry("zero_stress_scenario_data", "Zero Stress Scenario Data", "market_data", S, "xml",
          "zeroStressScenarioData.xml"),

    entry("curves_report", "Curves Report", "calibration_report", P, "csv", "curves.csv"),
    entry("credit_curves", "Credit Curves", "calibration_report", P, "csv",
          "creditcurves.csv"),
    entry("todays_market_calibration", "Today's Market Calibration", "calibration_report", P,
          "csv", "todaysmarketcalibration.csv"),
    entry("todays_market_calibration_cashflows", "Today's Market Calibration Cashflows",
          "calibration_report", P, "csv", "todaysmarketcalibration_cashflows.csv"),
    entry("calibration_report", "Calibration Report", "calibration_report", P, "csv",
          "calibration.csv"),
    entry("mean_reversion", "Mean Reversion", "calibration_report", P, "csv",
          "meanReversion.csv", "meanReversion_*.csv"),
    entry("pca", "Principal Components", "calibration_report", P, "csv", "pca.csv",
          "pca_*.csv"),
    entry("eigenvalues", "Eigenvalues", "calibration_report", P, "csv", "eigenvalues.csv"),
    entry("eigenvectors", "Eigenvectors", "calibration_report", P, "csv", "eigenvectors.csv"),
    entry("correlations", "Risk Factor Correlations", "calibration_report", P, "csv",
          "correlation.csv"),

    entry("npv_report", "NPV Report", "valuation_report", P, "csv", "npv.csv"),
    entry("swaption_npv", "Swaption NPV Report", "valuation_report", P, "csv",
          "swaption_npv.csv"),
    entry("cashflow_report", "Cashflow Report", "valuation_report", P, "csv", "flows.csv",
          "cashflow.csv"),
    entry("additional_results_report", "Additional Results Report", "valuation_report", P,
          "csv", "additional_results.csv"),

    entry("sensitivity_report", "Sensitivity Report", "risk_report", P, "csv",
          "sensitivity.csv"),
    entry("sensitivity_atm_only", "Sensitivity Report, ATM Only", "risk_report", P, "csv",
          "sensitivity_atmOnly.csv"),
    entry("sensitivity_full_surface", "Sensitivity Report, Full Surface", "risk_report", P,
          "csv", "sensitivity_fullSurface.csv"),
    entry("sensitivity_scenario", "Sensitivity Scenario Report", "risk_report", P, "csv",
          "sensitivity_scenario.csv"),
    entry("scenario", "Scenario Report", "risk_report", P, "csv", "scenario.csv"),
    entry("scenario_full_surface", "Scenario Report, Full Surface", "risk_report", P, "csv",
          "scenario_fullSurface.csv"),
    entry("scenario_par_rates", "Scenario Par Rates", "risk_report", P, "csv",
          "scenario_par_rates.csv"),
    entry("par_sensitivity", "Par Sensitivity Report", "risk_report", P, "csv",
          "parsensitivity.csv", "parsensi.csv"),
    entry("par_conversion_sensitivity", "Par Conversion Sensitivity Report", "risk_report", P,
          "csv", "parconversion_sensitivity.csv"),
    entry("par_shifts", "Par Shifts", "risk_report", P, "csv", "parshifts.csv"),
    entry("cross_gamma", "Cross Gamma Report", "risk_report", P, "csv", "crossgamma.csv"),
    entry("product_class_mapping", "Product Class Mapping", "risk_report", P, "csv",
          "productclass_mapping.csv"),
    entry("stress_test_report", "Stress Test Report", "risk_report", P, "csv",
          "stresstest.csv", "stress.csv"),
    entry("stress_test_flows", "Stress Test Cashflow Report", "risk_report", P, "csv",
          "stresstest_flows.csv"),
    entry("stress_scenario_par_rates", "Stress Scenario Par Rates", "risk_report", P, "csv",
          "stress_scenario_par_rates.csv"),
    entry("var_report", "VaR Report", "risk_report", P, "csv", "var.csv"),
    entry("historical_pnl", "Historical P&L", "risk_report", P, "csv", "historical_PnL.csv"),
    entry("pnl_report", "P&L Report", "risk_report", P, "csv", "pnl.csv"),
    entry("pnl_explain_report", "P&L Explain Report", "risk_report", P, "csv",
          "pnl_explain.csv"),

    entry("simm_report", "SIMM Report", "capital_report", P, "csv", "simm.csv"),
    entry("simm_data", "SIMM Data", "capital_report", P, "csv", "simm_data.csv"),
    entry("simm_regulation_breakdown", "SIMM Regulation Breakdown", "capital_report", P, "csv",
          "regulation_breakdown_simm.csv"),
    entry("crif", "CRIF", "capital_report", P, "csv", "crif.csv"),
    entry("capital_crif", "Capital CRIF", "capital_report", P, "csv", "capital_crif.csv"),
    entry("im_schedule", "IM Schedule Report", "capital_report", P, "csv", "im_schedule.csv"),
    entry("im_schedule_trade", "IM Schedule Report by Trade", "capital_report", P, "csv",
          "im_schedule_trade.csv"),
    entry("saccr_report", "SA-CCR Report", "capital_report", P, "csv", "saccr.csv"),
    entry("saccr_detail", "SA-CCR Detail Report", "capital_report", P, "csv",
          "saccr_detail.csv", "saccrdetail.csv"),
    entry("sacva_report", "SA-CVA Report", "capital_report", P, "csv", "sacva.csv"),
    entry("sacva_detail", "SA-CVA Detail Report", "capital_report", P, "csv",
          "sacvadetail.csv"),
    entry("sacva_sensitivity", "SA-CVA Net Sensitivities", "capital_report", P, "csv",
          "sacva_sensitivity.csv"),
    entry("sacva_sensitivities", "SA-CVA Sensitivities", "capital_report", P, "csv",
          "sacva_sensitivities.csv"),
    entry("bacva_report", "BA-CVA Report", "capital_report", P, "csv", "bacva.csv"),
    entry("smrc_report", "SMRC Report", "capital_report", P, "csv", "smrc.csv"),
    entry("smrc_detail", "SMRC Detail Report", "capital_report", P, "csv", "smrcdetail.csv"),
    entry("credit_migration", "Credit Migration Report", "capital_report", P, "csv",
          "credit_migration_*.csv"),

    entry("xva_report", "XVA Report", "xva_report", P, "csv", "xva.csv"),
    entry("trade_exposure_report", "Trade Exposure Report", "xva_report", P, "csv",
          "exposure_trade_*.csv"),
    entry("netting_set_exposure_report", "Netting Set Exposure Report", "xva_report", P, "csv",
          "exposure_nettingset_*.csv"),
    entry("netting_set_colva_report", "Netting Set COLVA Report", "xva_report", P, "csv",
          "colva_nettingset_*.csv"),
    entry("time_averaged_exposure", "Time Averaged Exposure Report", "xva_report", P, "csv",
          "timeaveragedexposure.csv"),
    entry("dim_evolution", "DIM Evolution Report", "xva_report", P, "csv",
          "dim_evolution.csv", "dim_evolution.txt"),
    entry("dim_regression", "DIM Regression Report", "xva_report", P, "csv",
          "dim_regression.csv", "dim_regression_*.csv", "dim_regression*.txt"),
    entry("xva_explain_report", "XVA Explain Report", "xva_report", P, "csv",
          "xvaExplain.csv"),
    entry("xva_explain_details", "XVA Explain Details", "xva_report", P, "csv",
          "xvaExplain_details.csv"),
    entry("xva_par_sensitivity", "XVA Par Sensitivity Report", "xva_report", P, "csv",
          "xva_par_sensitivity_*.csv"),
    entry("xva_zero_sensitivity", "XVA Zero Sensitivity Report", "xva_report", P, "csv",
          "xva_zero_sensitivity_*.csv"),
    entry("xva_sensitivity_jacobi", "XVA Sensitivity Jacobian", "xva_report", P, "csv",
          "xva_sensitivity_jacobi.csv"),
    entry("xva_sensitivity_jacobi_inverse", "XVA Sensitivity Inverse Jacobian", "xva_report",
          P, "csv", "xva_sensitivity_jacobi_inverse.csv"),
    entry("cva_sensitivities", "CVA Sensitivities", "xva_report", P, "csv",
          "cva_sensitivities.csv"),
    entry("xva_cg_exposure", "XVA Exposure, Computation Graph", "xva_report", P, "csv",
          "xvacg-exposure.csv"),
    entry("xva_cg_cva_sensitivity_scenario", "CVA Sensitivity Scenarios, Computation Graph",
          "xva_report", P, "csv", "xvacg-cva-sensi-scenario.csv"),

    entry("market_objects", "Market Objects", "portfolio_details", P, "csv",
          "marketObjects.csv"),
    entry("risk_factors", "Risk Factors", "portfolio_details", P, "csv", "riskFactors.csv"),
    entry("counterparties", "Counterparties", "portfolio_details", P, "csv",
          "counterparties.csv"),
    entry("netting_sets", "Netting Sets", "portfolio_details", P, "csv", "netting_sets.csv"),
    entry("trade_types", "Trade Types", "portfolio_details", P, "csv", "trade_types.csv"),
    entry("swap_indices", "Swap Indices", "portfolio_details", P, "csv", "swap_indices.csv"),
    entry("underlying_indices", "Underlying Indices", "portfolio_details", P, "csv",
          "underlying_indices.csv"),

    entry("npv_cube", "NPV Cube", "intermediate_data", P, "csv_gz", "cube.csv.gz"),
    entry("npv_cube_binary", "NPV Cube, Binary", "intermediate_data", P, "dat", "cube.dat"),
    entry("raw_cube", "Raw Cube", "intermediate_data", P, "csv", "rawcube.csv"),
    entry("netting_set_cube", "Netting Set Cube", "intermediate_data", P, "csv_gz",
          "nettingSetCube.csv.gz"),
    entry("net_cube", "Net Cube", "intermediate_data", P, "csv", "netcube.csv"),
    entry("counterparty_cube", "Counterparty Cube", "intermediate_data", P, "csv_gz",
          "cptyCube.csv.gz"),
    entry("aggregation_scenario_data", "Aggregation Scenario Data", "intermediate_data", P,
          "csv_gz", "scenariodata.csv.gz", "scenariodata.csv"),
    entry("aggregation_scenario_data_binary", "Aggregation Scenario Data, Binary",
          "intermediate_data", P, "dat", "scenariodata.dat"),
    entry("scenario_dump", "Scenario Dump", "intermediate_data", P, "csv", "scenariodump.csv"),
    entry("scenario_statistics", "Scenario Statistics", "intermediate_data", P, "csv",
          "scenariostatistics.csv"),
    entry("stress_scenarios", "Stress Scenarios", "intermediate_data", P, "csv",
          "stress_scenarios.csv"),
    entry("zero_stress_scenario_results", "Zero Stress Scenario Results", "intermediate_data",
          P, "xml", "results.xml"),
    entry("jacobi", "Jacobian", "intermediate_data", P, "csv", "jacobi.csv"),
    entry("jacobi_inverse", "Inverse Jacobian", "intermediate_data", P, "csv",
          "jacobi_inverse.csv"),
    entry("amc_buffer", "AMC Path Data", "intermediate_data", P, "dat", "amcBuffer.dat"),

    entry("market_data_report", "Market Data Report", "run_diagnostics", P, "csv",
          "marketdata.csv"),
    entry("fixings_report", "Fixings Report", "run_diagnostics", P, "csv", "fixings.csv"),
    entry("dividends_report", "Dividends Report", "run_diagnostics", P, "csv",
          "dividends.csv"),
    entry("pricing_stats", "Pricing Statistics", "run_diagnostics", P, "csv",
          "pricingstats.csv"),
    entry("runtimes", "Run Times", "run_diagnostics", P, "csv", "runtimes.csv"),
    entry("log", "Log", "run_diagnostics", P, "txt", "log.txt",
          section="sec:master_input_logging"),
    entry("structured_log", "Structured Log", "run_diagnostics", P, "json",
          "log_structured.json", section="sec:structured_messages"),

    entry("input_package", "Input Package", "packaging", K, "tar_gz", "ore_package.tar.gz"),
    entry("output_archive", "Output Archive", "packaging", K, "tar_gz", "output.tar.gz",
          patterns=(RESULT_ARCHIVE,)),
    entry("output_manifest", "Output Manifest", "packaging", K, "json_gz",
          "manifest.json.gz"),
]
