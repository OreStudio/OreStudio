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
"""The initial margin, CRIF and portfolio details analytics, and the analytic types that ORE does not implement."""

from ._entry import FILE, FLAG, INPUT_FILE, entry

IGNORED = "ORE ignores the parameter, so a value has no effect."
SIMM_VERSIONS = (
    "ORE supports the SIMM versions 1.0, 1.1, 1.2, 1.3, 1.3.38, 2.0, 2.1, 2.2, 2.3, 2.4, 2.5, 2.5A, 2.6 and 2.6.5."
)
SIMM_FORMAT = "A SIMM version such as 2.6."
MAPPING_ORDER = "ORE reads it from the first of the blocks crif, saccr, simm, npv and Setup that has a value."
MATCH = " When the SIMM and the IM schedule analytic are both active, the two values must be the same, or the run stops."


def currency(label, what, extra=""):
    return entry(
        label,
        "A currency code, for example USD.",
        what + extra,
        "The calculation currency, or the base currency when that is missing too.",
        "A currency code of three letters, for example USD.")


SIMM = {
    "active": entry(
        "Run the SIMM analytic",
        "A flag. Y runs the analytic.",
        "Switches the ISDA Standard Initial Margin Model (SIMM) on or off. The analytic computes the initial "
        "margin from sensitivities in the Common Risk Interchange Format (CRIF): it applies the risk weights "
        "and the correlations of the chosen SIMM version to the sensitivities of each product class and adds "
        "the results. Use it to estimate the margin that a bilateral agreement asks for.",
        "No. The analytic does not run.",
        FLAG, required=True),
    "version": entry(
        "SIMM version",
        "A SIMM version, for example 2.6.",
        "Selects the version of the SIMM. ISDA publishes a new version about once a year, and the firm and its "
        "counterparties must use the same one. ORE supports the SIMM versions 1.0, 1.1, 1.2, 1.3, 1.3.38, 2.0, "
        "2.1, 2.2, 2.3, 2.4, 2.5, 2.5A, 2.6 and 2.6.5. For example, 2.6 applies the risk weights of that "
        "version. The run stops if no version is set.",
        "None. The parameter is mandatory.",
        SIMM_FORMAT),
    "crif": entry(
        "CRIF file",
        "A file name.",
        "Names the CRIF file with the sensitivities. The SIMM analytic can process a full CRIF with interest "
        "rate, FX, inflation, equity, credit and commodity risk. When the parameter is missing, ORE uses the CRIF "
        "analytic to generate the CRIF, and that generation covers interest rate and FX risk only. For "
        "example, crif.csv.",
        "None. ORE generates the CRIF with the CRIF analytic.",
        INPUT_FILE),
    "calculationCurrency": entry(
        "Calculation currency",
        "A currency code, for example USD.",
        "Sets the currency of the calculation. The SIMM ignores the Risk_FX entries of the CRIF in this "
        "currency. It is also the currency to call and to post, unless the next two parameters override it. "
        "For example, USD." + MATCH,
        "The base currency. The run stops if neither is set.",
        "A currency code of three letters, for example USD."),
    "calculationCurrencyCall": currency(
        "Calculation currency to call",
        "Sets a separate calculation currency for the margin that we call. A credit support annex can set one "
        "currency for what we call and another for what we post." + MATCH, ""),
    "calculationCurrencyPost": currency(
        "Calculation currency to post",
        "Sets a separate calculation currency for the margin that we post. A credit support annex can set one "
        "currency for what we call and another for what we post." + MATCH, ""),
    "resultCurrency": entry(
        "Result currency",
        "A currency code, for example USD.",
        "Sets the currency of the SIMM amounts in the report. ORE converts the margin from the calculation "
        "currency into this currency at the spot rate of the valuation date. For example, EUR reports the "
        "margin in euro. When the SIMM and the IM schedule analytic are both active, the two values must be the "
        "same, or the run stops.",
        "The calculation currency to call.",
        "A currency code of three letters, for example USD."),
    "reportingCurrency": entry(
        "Reporting currency",
        "A currency code, for example USD.",
        "Adds two columns to the SIMM report: the reporting currency and the SIMM amount converted into it."
        + MATCH,
        "None. The report has no extra columns.",
        "A currency code of three letters, for example USD."),
    "enforceIMRegulations": entry(
        "Enforce the IM regulations",
        "A flag. Y calculates the SIMM for each regulation.",
        "Decides whether ORE calculates the SIMM for each post and collect regulation that the records of the "
        "CRIF name, and then reports the worst case. When it is N, ORE ignores the regulations of the CRIF."
        + MATCH,
        "No. ORE ignores the post and collect regulations of the CRIF.",
        FLAG),
    "mporDays": entry(
        "Margin period of risk in days",
        "1 or 10.",
        "Sets the margin period of risk of the SIMM, in days. The SIMM scales the risk weights to the period "
        "over which the portfolio can move before a defaulted counterparty is closed out. ORE supports 1 and 10 "
        "for the versions from 2.2 onwards and only 10 for the earlier versions. The default is 10, the period "
        "for bilateral margin.",
        "10",
        "1 or 10."),
    "simmCalibration": entry(
        "SIMM calibration file",
        "A file name.",
        "Names a file with the calibration of the SIMM, that is the risk weights and the correlations. When the "
        "file exists, it replaces the calibration of the SIMM version above. Use it to test a calibration that "
        "ISDA has proposed but not yet released, or a calibration of your own. Without it ORE uses the "
        "calibration of the version.",
        "None. ORE uses the calibration of the version.",
        INPUT_FILE),
    "writeIntermediateReports": entry(
        "Write the intermediate reports",
        "A flag. Y writes the reports.",
        "Decides whether the SIMM analytic writes its intermediate reports, which show the steps from the CRIF "
        "to the margin: the sensitivities by risk class, the weighted sensitivities, and the margin of each "
        "bucket. Use them to follow how the margin builds up, or to find which bucket drives it. The default is "
        "Yes.",
        "Yes.",
        FLAG),
    "nameMappingInputFile": entry(
        "Name mapping file",
        "A file name.",
        "Names the file that maps the names in the portfolio to the SIMM names. Without it ORE uses the "
        "names as they are. " + MAPPING_ORDER,
        "None. ORE uses the names as they are.",
        INPUT_FILE),
    "bucketMappingInputFile": entry(
        "Bucket mapping file",
        "A file name.",
        "Names the file that maps risk factors to the SIMM buckets. The bucket decides the risk weight and the "
        "correlation that a sensitivity gets. Without it ORE uses the standard buckets. ORE reads it from the "
        "first of the blocks crif, saccr, simm, npv and Setup that has a value.",
        "None. ORE uses the standard SIMM buckets.",
        INPUT_FILE),
    "computeTheta": entry(
        "Compute theta",
        "A flag. Y computes the theta.",
        "Decides whether the sensitivity calculation behind the CRIF also computes the theta. ORE reads the "
        "parameter from the sensitivity block, the simm block or the crif block, in this order, and one value "
        "serves all three analytics.",
        "No.",
        FLAG),
}

CRIF = {
    "active": entry(
        "Run the CRIF generation",
        "A flag. Y runs the analytic.",
        "Switches the CRIF generation on or off. The analytic writes a CRIF file, the input for the SIMM. It "
        "is based on a par sensitivity analysis with a SIMM-specific sensitivity configuration, and then "
        "labels the sensitivities so that the file has the CRIF format of ISDA. The generation covers interest "
        "rate and FX risk only.",
        "No. The analytic does not run.",
        FLAG, required=True),
    "marketConfigFile": entry(
        "Simulation market file",
        "A file name.",
        "Names the XML file with the simulation market for the sensitivity analysis. The analytic uses the "
        "same market and sensitivity settings as the sensitivity analytic and overwrites its settings. For "
        "example, crifmarket.xml.",
        "None. The analytic needs the file.",
        INPUT_FILE),
    "sensitivityConfigFile": entry(
        "Sensitivity configuration file",
        "A file name.",
        "Names the XML file with the SIMM sensitivity configuration: the risk factors, the shift types and "
        "sizes. The CRIF generation computes the sensitivities with this setup and then writes them in the CRIF "
        "format. For example, sensitivity.xml. The analytic needs this file.",
        "None. The analytic needs the file.",
        INPUT_FILE),
    "simmVersion": entry(
        "SIMM version",
        "A SIMM version, for example 2.7.",
        "Selects the SIMM version for the CRIF. ORE reads it from the first of the blocks crif, saccr, simm "
        "(parameter version) and portfolioDetails (parameter version) that has a value. The run stops if "
        "none of them has one.",
        "None. The parameter is mandatory.",
        SIMM_FORMAT),
    "nameMappingInputFile": entry(
        "Name mapping file",
        "A file name.",
        "Names the file that maps the names in the portfolio to the names of the CRIF. Without it ORE uses "
        "the names as they are. " + MAPPING_ORDER,
        "None. ORE uses the names as they are.",
        INPUT_FILE),
    "bucketMappingInputFile": entry(
        "Bucket mapping file",
        "A file name.",
        "Names the file that maps risk factors to the SIMM buckets of the CRIF. Without it ORE uses the "
        "standard buckets. " + MAPPING_ORDER,
        "None. ORE uses the standard SIMM buckets.",
        INPUT_FILE),
    "applySimmExemptions": entry(
        "Apply SIMM exemptions",
        "A flag. Y applies the exemptions.",
        "Decides whether the CRIF generation applies the SIMM exemptions and writes a report of what it "
        "exempted. Some products or risk factors are exempt from the SIMM under the rules of ISDA, for example "
        "certain FX products. With Yes, the CRIF leaves out the exempt sensitivities. The default is No.",
        "No.",
        FLAG),
    "computeTheta": entry(
        "Compute theta",
        "A flag. Y computes the theta.",
        "Decides whether the sensitivity calculation behind the CRIF also computes the theta. ORE reads the "
        "parameter from the sensitivity block, the simm block or the crif block, in this order, and one value "
        "serves all three analytics.",
        "No.",
        FLAG),
    "crifOutputFile": entry(
        "CRIF output file",
        "A file name. ORE ignores it.",
        "ORE does not read this parameter in any release. Example files carry it. The CRIF analytic writes "
        "its result as a report with a fixed name.",
        "None. " + IGNORED,
        FILE),
    "baseCurrency": entry(
        "Base currency",
        "A currency code. ORE ignores it in the crif block.",
        "ORE does not read this parameter in the crif block. The base currency of the Setup block applies.",
        "None. " + IGNORED,
        "A currency code of three letters, for example EUR."),
}

IMSCHEDULE = {
    "active": entry(
        "Run the IM schedule analytic",
        "A flag. Y runs the analytic.",
        "Switches the IM schedule analytic on or off. It computes the basic initial margin with the IM "
        "schedule method from minimal trade information: the NPV, the notional and the end date. The CRIF "
        "file gives the information.",
        "No. The analytic does not run.",
        FLAG, required=True),
    "crif": entry(
        "CRIF file",
        "A file name.",
        "Names the CRIF file for the IM schedule. It needs two lines for each trade, one with the risk class PV "
        "and one with the risk class Notional, which ORE reads as the NPV and the notional. It also needs the "
        "product class and the end date. The product class must be one of Rates, FX, Equity, Credit and "
        "Commodity: the SIMM uses the combined class RatesFX, but the IM schedule does not." + MATCH
        + " For example, crif_schedule.csv.",
        "None. The analytic needs the file.",
        INPUT_FILE),
    "version": entry(
        "SIMM version",
        "A SIMM version, for example 2.6.",
        "Selects the version for the CRIF. When the SIMM and the IM schedule analytic are both active, the two "
        "values must be the same, or the run stops. Use the version that the firm has adopted. The default is "
        "the version of the simm block.",
        "The version of the simm block.",
        SIMM_FORMAT),
    "calculationCurrency": entry(
        "Calculation currency",
        "A currency code, for example USD.",
        "Sets the currency of the calculation. The schedule approach states the margin in this currency, and "
        "the credit support annex usually names it. When the SIMM and the IM schedule analytic are both active, "
        "the two values must be the same, or the run stops. For example, USD.",
        "The base currency. The run stops if neither is set.",
        "A currency code of three letters, for example USD."),
    "calculationCurrencyCall": currency(
        "Calculation currency to call",
        "Sets a separate calculation currency for the margin that we call. A credit support annex can set one "
        "currency for what we call and another for what we post." + MATCH, ""),
    "calculationCurrencyPost": currency(
        "Calculation currency to post",
        "Sets a separate calculation currency for the margin that we post. A credit support annex can set one "
        "currency for what we call and another for what we post." + MATCH, ""),
    "resultCurrency": entry(
        "Result currency",
        "A currency code, for example USD.",
        "Sets the currency of the amounts in the report. ORE converts the margin from the calculation currency "
        "into this currency at the spot rate of the valuation date. For example, EUR reports the margin in "
        "euro. When the SIMM and the IM schedule analytic are both active, the two values must be the same, or "
        "the run stops.",
        "The calculation currency to call.",
        "A currency code of three letters, for example USD."),
    "reportingCurrency": entry(
        "Reporting currency",
        "A currency code, for example USD.",
        "Adds two columns to the report: the reporting currency and the amount converted into it. Use it to see "
        "the margin in a second currency next to the calculation currency, for example the currency of the "
        "group accounts. When the SIMM and the IM schedule analytic are both active, the two values must be the "
        "same, or the run stops.",
        "None. The report has no extra columns.",
        "A currency code of three letters, for example USD."),
    "enforceIMRegulations": entry(
        "Enforce the IM regulations",
        "A flag. Y calculates for each regulation.",
        "Decides whether ORE calculates the margin for each post and collect regulation that the records of "
        "the CRIF name, and then reports the worst case." + MATCH,
        "No.",
        FLAG),
}

PORTFOLIO_DETAILS = {
    "active": entry(
        "Run the portfolio details analytic",
        "A flag. Y runs the analytic.",
        "Switches the portfolio details analytic on or off. It analyses the portfolio and writes reports of "
        "its risk factors, its market objects, its swap indices, the number of trades for each counterparty, "
        "netting set and trade type, and its underlying indices.",
        "No. The analytic does not run.",
        FLAG, required=True),
    "riskFactorFileName": entry(
        "Risk factor report file",
        "A file name.",
        "Names the file with the report of the risk factors of the portfolio: one row for each risk factor that "
        "the trades depend on. Use it to find the market data that a run needs before you load it. The default "
        "is risk_factors.csv, written in the output path.",
        "risk_factors.csv",
        FILE),
    "marketObjectFileName": entry(
        "Market object report file",
        "A file name.",
        "Names the file with the report of the market objects that the portfolio needs, for example the yield "
        "curves, the volatility surfaces and the FX spots. Use it to check that todays market holds everything "
        "the trades use. The default is market_objects.csv, written in the output path.",
        "market_objects.csv",
        FILE),
    "version": entry(
        "SIMM version",
        "A SIMM version, for example 2.6.",
        "Names a SIMM version. The CRIF generation uses it as its last choice, when the crif block, the saccr "
        "block and the simm block have no version. The portfolio details analytic itself computes no margin. "
        "For example, 2.6.",
        "None.",
        SIMM_FORMAT),
    "portfolio_details_configuration": entry(
        "Details configuration",
        "A name.",
        "Names the configuration of the portfolio details. ORE reads the value, and no analytic of release "
        "1.8.17.0 uses it. The default is BASIC. Leave it as it is.",
        "BASIC",
        "A name such as BASIC."),
}

STRESSED_SIMM = {
    "active": entry(
        "Run the stressed SIMM analytic",
        "A flag. Y runs the analytic.",
        "Switches the stressed SIMM analytic on or off. It computes the SIMM under stress scenarios, as the "
        "margin that the firm would owe in a stressed market. It reads the other SIMM settings from the simm "
        "block, so the version and the calibration are those of the plain SIMM. Use N to keep the entry in the "
        "document without running it.",
        "No. The analytic does not run.",
        FLAG, required=True),
    "stressedSimmScenarioData": entry(
        "Stress scenario file",
        "A file name.",
        "Names the XML file with the stress scenarios for the stressed SIMM. Each scenario shifts the market, "
        "and the analytic recomputes the sensitivities and the margin under it. The run stops if the file is "
        "missing. For example, stressedsimm.xml.",
        "None. The parameter is mandatory.",
        INPUT_FILE),
    "version": entry(
        "SIMM version",
        "A SIMM version, for example 2.6.",
        "Selects the SIMM version for the stressed SIMM. Use the same version as for the plain SIMM, so that "
        "the two margins are comparable. The run stops if no version is set. ORE supports the SIMM versions "
        "1.0, 1.1, 1.2, 1.3, 1.3.38, 2.0, 2.1, 2.2, 2.3, 2.4, 2.5, 2.5A, 2.6 and 2.6.5.",
        "None. The parameter is mandatory.",
        SIMM_FORMAT),
}

SIMM_BACKTEST = {
    "active": entry(
        "Run the SIMM backtest",
        "A flag. Y runs the analytic.",
        "Switches the SIMM backtest on or off. In release 1.8.17.0 ORE reads the switch, but the open source "
        "release has no analytic of this name, so a run with Y fails with the error Invalid analytic name, "
            "unless an extension library provides the analytic.",
        "No. The analytic does not run.",
        FLAG, required=True),
    "crif": entry(
        "CRIF file",
        "A file name.",
        "Names the CRIF file for the SIMM backtest. ORE reads the parameter, and the open source release has no "
        "analytic of this type, so a value has no effect there. Another build can use the file to compare the "
        "margin that the SIMM implies with the realised moves.",
        "None.",
        INPUT_FILE),
}


def not_implemented(label):
    return {
        "active": entry(
            f"Run the {label} analytic",
            "A flag. Y runs the analytic.",
            f"Switches the {label} analytic on or off. ORE 1.8.17.0 reads the switch, but the open source "
            "release has no analytic of this name, so a run with Y fails with the error Invalid analytic name, "
            "unless an extension library provides the analytic.",
            "No. The analytic does not run.",
            FLAG, required=True),
    }


def ignored_block(label, what):
    return entry(
        label,
        "ORE ignores this block.",
        "ORE does not read analytics of this type, and the block has no effect. Example files of an older "
        "layout carry it. " + what,
        "None. " + IGNORED,
        FLAG)


INITIAL_MARGIN = {
    "active": ignored_block("Initial margin block", "To compute an initial margin, use the simm analytic or the imschedule analytic."),
    "method": entry(
        "Initial margin method",
        "A name. ORE ignores it.",
        "ORE does not read analytics of the type initialMargin, so this parameter has no effect. Use the simm "
        "analytic or the imschedule analytic.",
        "None. " + IGNORED,
        "A name."),
}

CREDITCURVES = {
    "active": ignored_block("Credit curves block", "To export curves, use the curves analytic."),
    "configuration": entry(
        "Market configuration",
        "A name. ORE ignores it.",
        "ORE does not read analytics of the type creditcurves, so this parameter has no effect. The curves "
        "analytic exports the curves of a market configuration.",
        "None. " + IGNORED,
        "A name."),
    "grid": entry(
        "Time grid",
        "A time grid. ORE ignores it.",
        "ORE does not read analytics of the type creditcurves, so this parameter has no effect.",
        "None. " + IGNORED,
        "A time grid."),
    "outputFileName": entry(
        "Output file",
        "A file name. ORE ignores it.",
        "ORE does not read analytics of the type creditcurves, so this parameter has no effect.",
        "None. " + IGNORED,
        FILE),
}

ENTRIES = {
    "simm": SIMM,
    "crif": CRIF,
    "imschedule": IMSCHEDULE,
    "portfolioDetails": PORTFOLIO_DETAILS,
    "stressedSimm": STRESSED_SIMM,
    "simmBacktest": SIMM_BACKTEST,
    "simmOptimization": not_implemented("SIMM optimisation"),
    "totalIM": not_implemented("total initial margin"),
    "imImpact": not_implemented("initial margin impact"),
    "crifToTrade": not_implemented("CRIF to trade"),
    "frtb": not_implemented("FRTB"),
    "frtbCrif": not_implemented("FRTB CRIF"),
    "npvLagged": not_implemented("lagged NPV"),
    "fixingEstimate": not_implemented("fixing estimate"),
    "initialMargin": INITIAL_MARGIN,
    "creditcurves": CREDITCURVES,
}
