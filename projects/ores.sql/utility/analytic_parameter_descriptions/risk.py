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


WHAT = {
    "parametric": (
        "When it is on, the run computes the value at risk from the sensitivities and a covariance matrix of "
        "the risk factors, under the assumption that the returns follow a normal distribution. It is fast, "
        "because it revalues nothing."
    ),
    "historical": (
        "When it is on, the run revalues the portfolio under shifts of the risk factors taken from a history "
        "of market moves, and reads the VaR as a quantile of the resulting P&L. It needs a file of historical "
        "scenarios and the return configuration."
    ),
}


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
            f"Switches the {label} on or off. {WHAT[kind]} Use N to keep the entry in the document without running it.",
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
            "analytic uses the whole portfolio. Use it to compute the VaR of one desk or one book from a "
            "portfolio file that holds many. For example, PF1|PF2 selects the portfolios PF1 and PF2. "
            + other,
            "None. The analytic uses the whole portfolio.",
            "A regular expression."),
        "outputFile": entry(
            "VaR report file",
            "A file name.",
            "Names the file with the main VaR report: one row for each portfolio, risk class and risk type with "
            "the VaR at each quantile. The parametric and the historical simulation analytic share this name: "
            "ORE reads it from the parametric block first, so set it there when you run both. For example, "
            "var.csv.",
            "var.csv",
            FILE),
    }


PARAMETRIC = var_shared("parametric", "parametric VaR analytic")
PARAMETRIC.update({
    "sensitivityInputFile": entry(
        "Sensitivities file",
        "A comma-separated list of file names.",
        "Lists the files with the delta, vega and gamma sensitivities and the cross gamma, as the sensitivity "
        "analytic of ORE writes them. The analytic reads the sensitivities from these files and does not "
        "compute them, so run the sensitivity analytic first or supply the files from another system. Separate "
        "the names with a comma. For example, ../Output/sensitivity.csv,../Output/crossgamma.csv.",
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
        "Selects the algorithm that repairs a covariance matrix that is not positive semi-definite. Such a "
        "matrix arises when the correlations come from different sources or from too few observations, and the "
        "VaR needs a valid matrix. None does not repair it, and the run then stops on an invalid matrix. The "
        "other five names are repair methods. The default is Spectral, which replaces the negative eigenvalues "
        "by zero.",
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
        "Sets the number of days between two scenarios that the analytic uses to compute a P&L. This is the "
        "holding period of the VaR: the time in which the portfolio cannot be changed. Typical values are 1 and "
        "10. For example, 10 gives a ten-day VaR, which is the period of the Basel market risk rules. The name "
        "horizonDays is another name for the same parameter.",
        "10",
        COUNT),
    "mporCalendar": entry(
        "Holding period calendar",
        "A calendar name.",
        "Names the calendar that ORE uses to compute the dates of the scenarios. The holding period counts "
        "business days of this calendar, so a ten-day period over a holiday spans more calendar days. Use the "
        "calendar of the market of the portfolio. For example, TARGET. The name horizonCalendar is another name "
        "for the same parameter.",
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
        "Decides whether the report also shows the expected shortfall for each quantile, in extra columns. The "
        "expected shortfall at level p is the average P&L of all scenarios at or below the VaR for p, so it "
        "shows how bad the tail is beyond the VaR. The Basel market risk rules use the expected shortfall. The "
        "default is No.",
        "No.",
        FLAG),
    "tradePnl": entry(
        "Report the P&L by trade",
        "A flag. Y adds the report.",
        "Decides whether the analytic writes a second report with the P&L for each trade, risk class and risk "
        "type. Use it to find which trades drive the VaR. The report is large for a big portfolio and many "
        "scenarios. The default is No.",
        "No.",
        FLAG),
    "riskFactorBreakdown": entry(
        "Report the P&L by risk factor",
        "A flag. Y adds the report.",
        "Decides whether the analytic writes a third report with the P&L for each risk factor. Use it to find "
        "which risk factors drive the VaR, for example a single curve or a single FX rate. The report is large "
        "for many risk factors and scenarios. The default is No.",
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
        "Decides whether the analytic writes the historical scenarios that it used to a report. Use it to check "
        "which dates and which shifts went into the VaR, or to rerun the scenarios in another system. The "
        "report is large, because it holds every risk factor for every scenario. The default is No.",
        "No.",
        FLAG),
    "includeTheta": entry(
        "Include the theta",
        "A flag. Y includes the theta in the P&L.",
        "Decides whether the P&L of each scenario also includes the change of the NPV that the passing of the "
        "holding period causes, the theta. Without it the P&L shows the effect of the market move only. Switch "
        "it on when the VaR must cover the time decay too. The default is No.",
        "No.",
        FLAG),
    "includePeriodCashflow": entry(
        "Include the cash flows of the period",
        "A flag. Y includes the cash flows.",
        "Decides whether the P&L of each scenario also includes the cash flows that pay during the holding "
        "period. Without it a coupon that pays in the period is lost from the P&L. Switch it on for a VaR that "
        "measures the full change of wealth. The default is No.",
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
        "factor from the historical scenarios, for example as an absolute difference or as a relative or log "
        "return. Use it when a risk factor needs another return type than the standard one. Without the file "
        "ORE uses its standard return calculation.",
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
            "same parameter. With overlapping periods each scenario starts one day after the one before, which "
            "gives many scenarios from a short history. " + other,
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
            "Names the XML file with the conventions for the second valuation date. Use it when the market "
            "conventions changed between the two dates, for example after a move to a new reference rate. The "
            "name conventionsPnlFile is another name for the same parameter. Without it ORE uses the "
            "conventions of the Setup block. " + other,
            "The conventions of the Setup block.",
            INPUT_FILE),
        "conventionsPnlOverride": entry(
            "Conventions override at the second date",
            "A file name.",
            "Names an XML file with convention settings that replace the matching settings of the "
            "conventions at the second date. Use it to change only the conventions that differ between the two "
            "dates and keep the rest. The file needs only the entries that change. " + other,
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
    "the valuation date and a second valuation date: the change of the NPV of each trade, split into the "
    "effect of the market move and the time effect. It needs market data for both dates.",
    "No. The analytic does not run.",
    FLAG, required=True)
PNL["outputFileName"] = entry(
    "P&L report file",
    "A file name.",
    "Names the file with the P&L report: one row for each trade with the NPV at both dates and the P&L. "
    "Change it when one run writes several reports and you need to tell the files apart. The default is "
    "pnl.csv, written in the output path. For example, pnl.csv.",
    "pnl.csv",
    FILE)

PNL_EXPLAIN = pnl_shared("explain", "P&L explain")
PNL_EXPLAIN["active"] = entry(
    "Run the P&L explain analytic",
    "A flag. Y runs the analytic.",
    "Switches the P&L explain analytic on or off. When it is on, the run explains the P&L between two "
    "valuation dates by the raw or par sensitivities and the moves of the risk factors: the delta effect, "
    "the gamma effect and what remains unexplained. Use it to check that the risk numbers account for the "
    "P&L.",
    "No. The analytic does not run.",
    FLAG, required=True)
PNL_EXPLAIN.update({
    "outputFileName": entry(
        "P&L explain report file",
        "A file name.",
        "Names the file with the P&L explain report: one row for each trade, or for each risk factor when the "
        "risk factor level report is on, with the explained parts and the remainder. The default is "
        "pnl_explain.csv, written in the output path. For example, pnl_explain.csv.",
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
        "Decides whether the explanation uses par sensitivities. With N, it uses the raw sensitivities and the "
        "moves of the zero rates. With Y, it uses the par sensitivities and the par rate shifts between the two "
        "dates, which come from the zero to par shift analytic. Use Y when the desk thinks in par rates. The "
        "default is No.",
        "No. The explanation uses raw sensitivities.",
        FLAG),
    "riskFactorLevelReporting": entry(
        "Report by risk factor",
        "A flag. Y breaks the explanation down by risk factor.",
        "Decides whether the explanation shows the P&L for each risk factor. Use it to see which move of the "
        "market caused which part of the P&L. The report is larger, because it has a row for each trade and "
        "risk factor. The default is No.",
        "No.",
        FLAG),
    "portfolioFilter": entry(
        "Portfolio filter",
        "A regular expression.",
        "Limits the explanation to the trades whose portfolio id matches the regular expression. Use it to "
        "explain the P&L of one desk or one book from a portfolio file that holds many. Without it, the "
        "analytic uses the whole portfolio. For example, PF1 selects the portfolio PF1.",
        "None. The analytic uses the whole portfolio.",
        "A regular expression."),
    "returnConfigFile": entry(
        "Return configuration file",
        "A file name.",
        "Names the XML file with the return configuration, which sets how ORE derives the shift of each risk "
        "factor between the two dates, for example as an absolute difference or as a relative or log return. "
        "Without the file ORE uses its standard return calculation.",
        "None. ORE uses its standard return calculation.",
        INPUT_FILE),
    "historicalScenarioFile": entry(
        "Historical scenario file",
        "A file name.",
        "Names the CSV file with the market scenarios of the two dates, if the explanation reads them from a "
        "file. The explanation then takes the moves of the risk factors from the file and does not build them "
        "from market data of the two dates. Without it the analytic uses the market data of the two dates.",
        "None.",
        INPUT_FILE),
})

SCENARIO = {
    "active": entry(
        "Run the scenario analytic",
        "A flag. Y runs the analytic.",
        "Switches the scenario analytic on or off. It is a utility that exports the base scenario of the "
        "simulation market to a file: the value of every risk factor on the valuation date. The P&L calculation "
        "uses it too, to get the scenario of each date. Use it to check what the simulation market holds.",
        "No. The analytic does not run.",
        FLAG, required=True),
    "simulationConfigFile": entry(
        "Simulation configuration file",
        "A file name.",
        "Names the XML file with the simulation market whose base scenario the analytic exports. The market "
        "lists the risk factors, so the file decides which values the scenario holds. For example, "
        "simulation.xml. The analytic needs this file.",
        "None. The analytic needs the file.",
        INPUT_FILE),
    "scenarioOutputFile": entry(
        "Scenario output file",
        "A file name.",
        "Names the file that receives the base scenario: one row for each risk factor with its value on the "
        "valuation date. The default is scenario.csv, written in the output path. For example, scenario.csv.",
        "scenario.csv",
        FILE),
}

CORRELATION = {
    "active": entry(
        "Run the correlation analytic",
        "A flag. Y runs the analytic.",
        "Switches the correlation analytic on or off. It computes a correlation matrix of the risk factors from "
        "historical scenarios, as the correlation of the moves over the horizon. The XVA analytic can run it to "
        "get the correlations of its model. Use N to keep the entry in the document without running it.",
        "No. The analytic does not run.",
        FLAG, required=True),
    "correlation_method": entry(
        "Correlation method",
        "Pearson or Kendall-Rank.",
        "Selects the method for the correlation matrix. Pearson is the standard linear correlation, which "
        "reacts strongly to outliers. Kendall-Rank is a rank correlation, which depends on the order of the "
        "moves and not on their size, so it is steadier when the data has outliers. The default is Pearson. The "
        "name correlationMethod is another name for the same parameter.",
        "Pearson",
        "Pearson or Kendall-Rank."),
    "scenarioCorrSimulation": entry(
        "Simulation market file",
        "A file name.",
        "Names the XML file with the simulation market for the correlation calculation. The market lists the "
        "risk factors, so the file decides which factors the matrix covers. The name scenarioSimulationUri is "
        "another name for the same parameter. For example, simulation.xml. The analytic needs this file.",
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
        "Names the XML file with the sensitivity setup for the analysis at the valuation date. The file lists "
        "the risk factors and the shift sizes. For example, sensitivity.xml. It is optional, and the analytic "
        "uses it only for the analysis at the valuation date.",
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
        "Sets the length of the horizon over which the analytic measures the moves of the risk factors, in days "
        "on the calendar. The correlation is the correlation of the moves over this horizon, so a longer "
        "horizon smooths daily noise and gives fewer observations. The default is 10. The name horizonDays is "
        "another name for the same parameter.",
        "10",
        COUNT),
    "mporCalendar": entry(
        "Horizon calendar",
        "A calendar name.",
        "Names the calendar that ORE uses to count the days of the horizon. Use the calendar of the market of "
        "the risk factors. For example, TARGET. The name horizonCalendar is another name for the same "
        "parameter. Without a value ORE takes the calendar of the base currency.",
        "The calendar of the base currency.",
        "A calendar name such as TARGET, USD or UK."),
    "mporOverlappingPeriods": entry(
        "Overlapping horizons",
        "A flag. Y uses overlapping horizons.",
        "Decides whether the horizons overlap. The name horizonOverlappingPeriods is another name for the same "
        "parameter. With overlapping horizons each observation starts one day after the one before, which gives "
        "many observations from a short history. The observations are then not independent. The default is Yes.",
        "Yes.",
        FLAG),
    "allowPartialScenarios": entry(
        "Allow partial scenarios",
        "A flag. Y accepts scenarios that touch only part of the market.",
        "Decides whether the simulated market accepts scenarios that do not set every risk factor. Historical "
        "data often lacks a few risk factors on some dates. With Yes, the market keeps the value of a missing "
        "factor from the base scenario. With No, a scenario with a gap stops the run. The default is No.",
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
        "Names the file with the correlation matrix: one row for each pair of risk factors with their "
        "correlation. The default is correlation.csv, written in the output path. For example, correlation.csv.",
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
