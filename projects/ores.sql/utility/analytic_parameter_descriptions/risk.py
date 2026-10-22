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
"""The value at risk, P&L, scenario and correlation analytics."""

from ._entry import COUNT, DATE, FILE, FLAG, INPUT_FILE, REAL, entry

IGNORED = "ORE ignores the parameter, so a value has no effect."
QUANTILES = "A comma-separated list of numbers between 0 and 1, for example 0.01,0.05,0.95,0.99."
PERIODS = (
    "A comma-separated list of dates with an even number of entries, for example 2017-01-17,2019-12-30. Each "
    "pair of dates is the start and the end of one observation period."
)


def var_shared(kind, label):
    """The VaR parameters that the parametric and the historical simulation analytics share."""
    other = (
        "The historical simulation VaR analytic has the same parameter." if kind == "parametric"
        else "The parametric VaR analytic has the same parameter."
    )
    return {
        "active": entry(
            f"Run the {label}",
            "A flag. Y runs the analytic.",
            f"Switches the {label} on or off. Use N to keep the entry in the document without running it.",
            "No. The analytic does not run.",
            FLAG, required=True),
        "quantiles": entry(
            "Quantiles",
            "A comma-separated list of quantiles.",
            "Lists the quantiles of the profit and loss distribution that the report shows. Each quantile "
            "gives one column in the output file. The 1 percent quantile is the lower tail of the P&L "
            "distribution, which is the VaR, and the 99 percent quantile is the upper tail. For example, "
            "0.01,0.05,0.95,0.99. " + other,
            "None. The analytic needs the list.",
            QUANTILES),
        "breakdown": entry(
            "Break down the VaR",
            "A flag. Y breaks down the VaR.",
            "Decides whether the analytic computes the VaR for each portfolio, risk class (all, interest rate, "
            "FX, inflation, equity, credit) and risk type (all, delta and gamma, vega), and not only for the "
            "whole portfolio. " + other,
            "No.",
            FLAG),
        "portfolioFilter": entry(
            "Portfolio filter",
            "A regular expression.",
            "Limits the VaR to the trades whose portfolio id matches the regular expression. Without it, the "
            "analytic uses the whole portfolio. For example, PF1|PF2 selects the portfolios PF1 and PF2. "
            + other,
            "None. The analytic uses the whole portfolio.",
            "A regular expression."),
        "outputFile": entry(
            "VaR report file",
            "A file name.",
            "Names the file with the main VaR report. The parametric and the historical simulation analytic "
            "share this name: ORE reads it from the parametric block first. For example, var.csv.",
            "var.csv",
            FILE),
    }


PARAMETRIC = var_shared("parametric", "parametric VaR analytic")
PARAMETRIC.update({
    "sensitivityInputFile": entry(
        "Sensitivities file",
        "A comma-separated list of file names.",
        "Lists the files with the delta, vega and gamma sensitivities and the cross gamma, as ORE's "
        "sensitivity analytic writes them. For example, ../Output/sensitivity.csv,../Output/crossgamma.csv.",
        "None. The analytic needs the sensitivities.",
        "A comma-separated list of file names."),
    "covarianceInputFile": entry(
        "Covariance file",
        "A file name.",
        "Names the file with the covariances of the risk factors. ORE does not compute them, so you must "
        "supply them. The file has three columns: the first factor, the second factor and the covariance. "
        "The factor names follow the naming in the sensitivity and cross gamma files, and the covariances must "
        "agree with the sensitivities: if a sensitivity is for an absolute shift in basis points, the "
        "covariances are for absolute shifts in basis points. The covariances must include the holding "
        "period, because ORE does not scale them with the square root of time. For example, covariance.csv.",
        "None. The analytic needs the covariances.",
        INPUT_FILE),
    "method": entry(
        "VaR method",
        "Delta, DeltaGammaNormal, MonteCarlo, Cornish-Fisher or Saddlepoint.",
        "Selects the method for the VaR. Delta uses the delta sensitivities only. DeltaGammaNormal adds the "
        "gamma and uses a normal approximation. MonteCarlo simulates the risk factors. Cornish-Fisher and "
        "Saddlepoint are two other approximations of the distribution. The name varMethod is another name for "
        "the same parameter.",
        "DeltaGammaNormal",
        "One of Delta, DeltaGammaNormal, MonteCarlo, Cornish-Fisher or Saddlepoint."),
    "SalvagingAlgorithm": entry(
        "Salvaging algorithm",
        "None, Spectral, Hypersphere, LowerDiagonal, Higham or Principal.",
        "Selects the algorithm that repairs a covariance matrix that is not positive semi-definite. None "
        "does not repair it. The other five names are repair methods.",
        "Spectral",
        "One of None, Spectral, Hypersphere, LowerDiagonal, Higham or Principal."),
    "mcSamples": entry(
        "Monte Carlo samples",
        "A whole number.",
        "Sets the number of samples of the Monte Carlo method. A higher number gives a steadier result and "
        "takes longer. The name varMcSamples is another name for the same parameter. ORE uses it only when "
        "the method is MonteCarlo.",
        "1000000",
        COUNT),
    "mcSeed": entry(
        "Monte Carlo seed",
        "A whole number.",
        "Sets the seed of the random number generator of the Monte Carlo method. The same seed gives the "
        "same result. The name varMcSeed is another name for the same parameter. ORE uses it only when the "
        "method is MonteCarlo.",
        "42",
        COUNT),
})

HISTORICAL = var_shared("historical", "historical simulation VaR analytic")
HISTORICAL.update({
    "historicalScenarioFile": entry(
        "Historical scenario file",
        "A file name.",
        "Names the CSV file with the market scenarios for every date of the observation periods. The "
        "granularity of the scenarios must match the simulation market. The name scenarioFile is another "
        "name for the same parameter. For example, scenarios.csv.",
        "None. The analytic needs the file.",
        INPUT_FILE),
    "simulationConfigFile": entry(
        "Simulation configuration file",
        "A file name.",
        "Names the XML file that defines the structure of the simulation market for the P&L calculation: the "
        "discount and index curves, the tenor points of the yield curves, the FX pairs and so on. For "
        "example, simulation.xml.",
        "None. The analytic needs the file.",
        INPUT_FILE),
    "historicalPeriod": entry(
        "Historical period",
        "An even number of dates.",
        "Sets the observation periods of the history. " + PERIODS + " The name lookbackPeriod is another "
        "name for the same parameter. The run needs a period that is one contiguous part.",
        "None. The analytic needs the period.",
        "A comma-separated list of dates with an even number of entries."),
    "mporDays": entry(
        "Holding period in days",
        "A whole number of days.",
        "Sets the number of days between two scenarios that the analytic uses to compute a P&L. Typical "
        "values are 1 and 10. The name horizonDays is another name for the same parameter.",
        "10",
        COUNT),
    "mporCalendar": entry(
        "Holding period calendar",
        "A calendar name.",
        "Names the calendar that ORE uses to compute the dates of the scenarios. The name horizonCalendar is "
        "another name for the same parameter.",
        "The calendar of the base currency.",
        "A calendar name such as TARGET, USD or UK."),
    "mporOverlappingPeriods": entry(
        "Overlapping holding periods",
        "A flag. Y uses overlapping periods.",
        "Decides whether the holding periods overlap. With Y, a period of 10 days runs from t to t+10, the "
        "next from t+1 to t+11, and so on. With N, the periods follow each other without overlap. The "
        "name horizonOverlappingPeriods is another name for the same parameter.",
        "Yes.",
        FLAG),
    "includeExpectedShortfall": entry(
        "Include the expected shortfall",
        "A flag. Y adds the expected shortfall columns.",
        "Decides whether the report also shows the expected shortfall for each quantile, in extra columns. "
        "The expected shortfall at level p is the average P&L of all scenarios at or below the VaR for p.",
        "No.",
        FLAG),
    "tradePnl": entry(
        "Report the P&L by trade",
        "A flag. Y adds the report.",
        "Decides whether the analytic writes a second report with the P&L for each trade, risk class and risk "
        "type.",
        "No.",
        FLAG),
    "riskFactorBreakdown": entry(
        "Report the P&L by risk factor",
        "A flag. Y adds the report.",
        "Decides whether the analytic writes a third report with the P&L for each risk factor.",
        "No.",
        FLAG),
    "riskClassBreakdown": entry(
        "Break down by risk class",
        "A flag. Y computes the VaR for each risk class.",
        "Decides whether the analytic computes the VaR for each risk class and risk type (interest rate, FX, "
        "inflation, equity, credit, commodity) besides the row for the whole portfolio. Set it to N to compute "
        "only the row for the whole portfolio, which cuts the run time a lot.",
        "Yes.",
        FLAG),
    "outputHistoricalScenarios": entry(
        "Write the historical scenarios",
        "A flag. Y writes the scenarios.",
        "Decides whether the analytic writes the historical scenarios that it used to a report.",
        "No.",
        FLAG),
    "includeTheta": entry(
        "Include the theta",
        "A flag. Y includes the theta in the P&L.",
        "Decides whether the P&L of each scenario also includes the change of the NPV that the passing of "
        "the holding period causes.",
        "No.",
        FLAG),
    "includePeriodCashflow": entry(
        "Include the cash flows of the period",
        "A flag. Y includes the cash flows.",
        "Decides whether the P&L of each scenario also includes the cash flows that pay during the "
        "holding period.",
        "No.",
        FLAG),
    "sensitivityInputFile": entry(
        "Sensitivities file",
        "A file name.",
        "Names a CSV file with the delta, gamma and cross gamma sensitivities, in the same format as for the "
        "parametric VaR analytic. When this and the sensitivity configuration file are both set, ORE computes "
        "the historical P&Ls from the sensitivities and the historical scenario shifts and not by full "
        "revaluation.",
        "None. The analytic revalues in full.",
        INPUT_FILE),
    "sensitivityConfigFile": entry(
        "Sensitivity configuration file",
        "A file name.",
        "Names the XML file with the sensitivity scenario setup that maps the historical moves to the risk "
        "factors of the sensitivities file. If either of the two sensitivity parameters is missing, the "
        "analytic revalues in full.",
        "None. The analytic revalues in full.",
        INPUT_FILE),
    "returnConfigFile": entry(
        "Return configuration file",
        "A file name.",
        "Names the XML file with the return configuration, which sets how ORE derives the shift of each risk "
        "factor from the historical scenarios.",
        "None. ORE uses its standard return calculation.",
        INPUT_FILE),
})
HISTORICAL["portfolioFilter"] = entry(
    "Portfolio filter",
    "A regular expression.",
    "Limits the VaR to the trades whose portfolio id matches the regular expression. Without it, the "
    "analytic uses the whole portfolio. For example, PF1 selects the portfolio PF1.",
    "None. The analytic uses the whole portfolio.",
    "A regular expression.")


def pnl_shared(kind, label):
    """The parameters that the P&L and the P&L explain analytic share."""
    other = (
        "The P&L explain analytic has the same parameter." if kind == "pnl"
        else "The P&L analytic has the same parameter."
    )
    return {
        "mporDate": entry(
            "Second valuation date",
            "A date.",
            "Sets the later of the two valuation dates of the P&L calculation. The first date is the valuation "
            "date of the Setup block. The market data must be available for both dates. The name pnlDate is "
            "another name for the same parameter. " + other + " For example, 2023-02-14.",
            "None. ORE derives the date from the number of days.",
            DATE),
        "mporDays": entry(
            "Days to the second date",
            "A whole number of days.",
            "Sets the second valuation date as a number of days after the valuation date, counted on the "
            "calendar. ORE uses it when the second date is missing. The name horizonDays is another name for "
            "the same parameter. " + other + " For example, 10.",
            "10",
            COUNT),
        "mporCalendar": entry(
            "Calendar of the second date",
            "A calendar name.",
            "Names the calendar that ORE uses to compute the second valuation date from the valuation date "
            "and the number of days. The name horizonCalendar is another name for the same parameter. "
            + other,
            "None. The analytic needs a calendar when it derives the date from the days.",
            "A calendar name such as TARGET, US or UK."),
        "mporOverlappingPeriods": entry(
            "Overlapping periods",
            "A flag. Y uses overlapping periods.",
            "Sets whether holding periods overlap. The name horizonOverlappingPeriods is another name for the "
            "same parameter. " + other,
            "Yes.",
            FLAG),
        "simulationConfigFile": entry(
            "Simulation configuration file",
            "A file name.",
            "Names the XML file with the simulation market. It decides which market factors ORE moves from "
            "the first to the second valuation date to compute the P&L. When it is missing, ORE uses the "
            "simulation market of the scenario analytic. For example, simulation.xml. " + other,
            "The simulation market of the scenario analytic.",
            INPUT_FILE),
        "curveConfigMporFile": entry(
            "Curve configuration at the second date",
            "A file name.",
            "Names the XML file with the curve configuration for the second valuation date. It can differ "
            "from the configuration of the first date, which the Setup block gives. The name "
            "curveConfigPnlFile is another name for the same parameter. " + other,
            "The curve configuration of the Setup block.",
            INPUT_FILE),
        "curveConfigMporOverride": entry(
            "Curve configuration override at the second date",
            "A file name.",
            "Names an XML file with curve settings that replace the matching settings of the curve "
            "configuration at the second date. The name curveConfigPnlOverride is another name for the same "
            "parameter. " + other,
            "None.",
            INPUT_FILE),
        "conventionsMporFile": entry(
            "Conventions at the second date",
            "A file name.",
            "Names the XML file with the conventions for the second valuation date. The name "
            "conventionsPnlFile is another name for the same parameter. " + other,
            "The conventions of the Setup block.",
            INPUT_FILE),
        "conventionsPnlOverride": entry(
            "Conventions override at the second date",
            "A file name.",
            "Names an XML file with convention settings that replace the matching settings of the "
            "conventions at the second date. " + other,
            "None.",
            INPUT_FILE),
        "portfolioMporFile": entry(
            "Portfolio at the second date",
            "A file name.",
            "Names the XML file with the portfolio at the second valuation date. It is usually different from the "
            "portfolio of the first date. The name portfolioPnlFile is another name for the same parameter. "
            + other,
            "None. The analytic values the portfolio of the valuation date at the second date too.",
            INPUT_FILE),
        "dateAdjustedRiskFactors": entry(
            "Date adjusted risk factors",
            "A comma-separated list of risk factor types.",
            "Lists the types of risk factor for which ORE adjusts the scenario dates at the second valuation "
            "date relative to the first, when it computes the P&L explain. The adjustment matters for "
            "commodity curves, so that the dates refer to the same future contracts at both dates. For example, "
            "CommodityCurve. " + other,
            "None. ORE makes no adjustment.",
            "A comma-separated list of risk factor key types such as CommodityCurve."),
    }


PNL = pnl_shared("pnl", "P&L")
PNL["active"] = entry(
    "Run the P&L analytic",
    "A flag. Y runs the analytic.",
    "Switches the P&L analytic on or off. When it is on, the run computes the single-period P&L between "
    "the valuation date and a second valuation date. It needs market data for both dates.",
    "No. The analytic does not run.",
    FLAG, required=True)
PNL["outputFileName"] = entry(
    "P&L report file",
    "A file name.",
    "Names the file with the P&L report. For example, pnl.csv.",
    "pnl.csv",
    FILE)

PNL_EXPLAIN = pnl_shared("explain", "P&L explain")
PNL_EXPLAIN["active"] = entry(
    "Run the P&L explain analytic",
    "A flag. Y runs the analytic.",
    "Switches the P&L explain analytic on or off. When it is on, the run explains the P&L between two "
    "valuation dates by the raw or par sensitivities and the moves of the risk factors.",
    "No. The analytic does not run.",
    FLAG, required=True)
PNL_EXPLAIN.update({
    "outputFileName": entry(
        "P&L explain report file",
        "A file name.",
        "Names the file with the P&L explain report. For example, pnl_explain.csv.",
        "pnl_explain.csv",
        FILE),
    "sensitivityConfigFile": entry(
        "Sensitivity configuration file",
        "A file name.",
        "Names the XML file with the sensitivity setup for the sensitivity analysis at the valuation date. "
        "When it is missing, ORE uses the sensitivity setup of the sensitivity analytic. For example, "
        "sensitivity.xml.",
        "The sensitivity setup of the sensitivity analytic.",
        INPUT_FILE),
    "parSensitivity": entry(
        "Use par sensitivities",
        "A flag. Y explains the P&L with par sensitivities.",
        "Decides whether the explanation uses par sensitivities. With N, it uses the raw sensitivities. "
        "Par rate shifts between the two dates come from the zero to par shift analytic.",
        "No. The explanation uses raw sensitivities.",
        FLAG),
    "riskFactorLevelReporting": entry(
        "Report by risk factor",
        "A flag. Y breaks the explanation down by risk factor.",
        "Decides whether the explanation shows the P&L for each risk factor.",
        "No.",
        FLAG),
    "portfolioFilter": entry(
        "Portfolio filter",
        "A regular expression.",
        "Limits the explanation to the trades whose portfolio id matches the regular expression. Without "
        "it, the analytic uses the whole portfolio.",
        "None. The analytic uses the whole portfolio.",
        "A regular expression."),
    "returnConfigFile": entry(
        "Return configuration file",
        "A file name.",
        "Names the XML file with the return configuration, which sets how ORE derives the shift of each risk "
        "factor between the two dates.",
        "None. ORE uses its standard return calculation.",
        INPUT_FILE),
    "historicalScenarioFile": entry(
        "Historical scenario file",
        "A file name.",
        "Names the CSV file with the market scenarios of the two dates, if the explanation reads them from a "
        "file.",
        "None.",
        INPUT_FILE),
})

SCENARIO = {
    "active": entry(
        "Run the scenario analytic",
        "A flag. Y runs the analytic.",
        "Switches the scenario analytic on or off. It is a utility that exports the base scenario of the "
        "simulation market to a file. The P&L calculation uses it too.",
        "No. The analytic does not run.",
        FLAG, required=True),
    "simulationConfigFile": entry(
        "Simulation configuration file",
        "A file name.",
        "Names the XML file with the simulation market whose base scenario the analytic exports. For "
        "example, simulation.xml.",
        "None. The analytic needs the file.",
        INPUT_FILE),
    "scenarioOutputFile": entry(
        "Scenario output file",
        "A file name.",
        "Names the file that receives the base scenario. For example, scenario.csv.",
        "scenario.csv",
        FILE),
}

CORRELATION = {
    "active": entry(
        "Run the correlation analytic",
        "A flag. Y runs the analytic.",
        "Switches the correlation analytic on or off. It computes a correlation matrix of the risk factors "
        "from historical scenarios. The XVA analytic can run it to get the correlations of its model.",
        "No. The analytic does not run.",
        FLAG, required=True),
    "correlation_method": entry(
        "Correlation method",
        "Pearson or Kendall-Rank.",
        "Selects the method for the correlation matrix. Pearson is the standard linear correlation. "
        "Kendall-Rank is a rank correlation. The name correlationMethod is another name for the same "
        "parameter.",
        "Pearson",
        "Pearson or Kendall-Rank."),
    "scenarioCorrSimulation": entry(
        "Simulation market file",
        "A file name.",
        "Names the XML file with the simulation market for the correlation calculation. The name "
        "scenarioSimulationUri is another name for the same parameter. For example, simulation.xml.",
        "None. The analytic needs the file.",
        INPUT_FILE),
    "historicalScenarioFile": entry(
        "Historical scenario file",
        "A file name.",
        "Names the CSV file with the market scenarios for every date of the observation period. The "
        "granularity of the scenarios must match the simulation market, and each yield curve tenor is a "
        "discount factor. The name scenarioFile is another name for the same parameter. For example, "
        "scenarios.csv.",
        "None. The analytic needs the file.",
        INPUT_FILE),
    "sensitivityConfigFile": entry(
        "Sensitivity configuration file",
        "A file name.",
        "Names the XML file with the sensitivity setup for the analysis at the valuation date. For example, "
        "sensitivity.xml.",
        "None.",
        INPUT_FILE),
    "historicalPeriod": entry(
        "Historical period",
        "Two dates.",
        PERIODS + " The correlation analytic needs one contiguous period. The names lookbackPeriod, "
        "benchmarkVarPeriod and benchmarkPeriod are other names for the same parameter. The run stops if "
        "the period is missing.",
        "None. The parameter is mandatory.",
        "A comma-separated list of dates."),
    "mporDays": entry(
        "Horizon in days",
        "A whole number of days.",
        "Sets the length of the horizon over which the analytic measures the moves of the risk factors, in "
        "days on the calendar. The name horizonDays is another name for the same parameter.",
        "10",
        COUNT),
    "mporCalendar": entry(
        "Horizon calendar",
        "A calendar name.",
        "Names the calendar that ORE uses to count the days of the horizon. The name horizonCalendar is "
        "another name for the same parameter.",
        "The calendar of the base currency.",
        "A calendar name such as TARGET, USD or UK."),
    "mporOverlappingPeriods": entry(
        "Overlapping horizons",
        "A flag. Y uses overlapping horizons.",
        "Decides whether the horizons overlap. The name horizonOverlappingPeriods is another name for the "
        "same parameter.",
        "Yes.",
        FLAG),
    "allowPartialScenarios": entry(
        "Allow partial scenarios",
        "A flag. Y accepts scenarios that touch only part of the market.",
        "Decides whether the simulated market accepts scenarios that do not set every risk factor.",
        "No.",
        FLAG),
    "filterCamCorrelationScenarioTenor": entry(
        "Tenor filter for the output",
        "A JSON object.",
        "Limits the tenors of the correlation output. The value is a JSON object that maps regular "
        "expressions to tenors. ORE matches the expressions against the risk factor keys, written as "
        "KeyType/Name, in reverse order, so the last entry has the highest priority. A risk factor without a "
        "match keeps all tenors. For example, {\"DiscountCurve/.*\":\"5Y\", \"DiscountCurve/USD/.*\":\"3Y\", "
        "\"IndexCurve/USD-FedFunds\":\"10Y\"}.",
        "None. All tenors are included.",
        "A JSON object of regular expressions and tenors."),
    "outputFileName": entry(
        "Correlation report file",
        "A file name.",
        "Names the file with the correlation matrix. For example, correlation.csv.",
        "correlation.csv",
        FILE),
    "outputFile": entry(
        "Correlation report file (older name)",
        "A file name. ORE ignores it.",
        "ORE reads the file name from the parameter outputFileName and does not read this one. The user guide "
        "and the examples use this name, so a document with this name still writes correlation.csv.",
        "None. " + IGNORED,
        FILE),
}

ENTRIES = {
    "parametricVar": PARAMETRIC,
    "historicalSimulationVar": HISTORICAL,
    "pnl": PNL,
    "pnlExplain": PNL_EXPLAIN,
    "scenario": SCENARIO,
    "correlation": CORRELATION,
}
