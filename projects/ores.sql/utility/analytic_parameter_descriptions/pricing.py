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
"""The pricing analytics: NPV, cash flows, curves and additional results."""

from ._entry import entry

FLAG = "A flag, written Y or N, or true or false."
FILE = "A file name, written without a directory. ORE writes the file in the output path of the Setup block."
DATE = "A date, written as YYYY-MM-DD or YYYYMMDD."

ENTRIES = {
    "npv": {
        "active": entry(
            "Run the NPV analytic",
            "A flag. Y runs the analytic.",
            "Switches the NPV analytic on or off. When it is on, the run prices every trade of the "
            "portfolio at the valuation date and writes the NPV of each trade to the NPV report. "
            "Use N to keep the entry in the document without running it. Every analytic has this parameter.",
            "No. The analytic does not run.",
            FLAG, required=True),
        "baseCurrency": entry(
            "Base currency",
            "A currency code, for example EUR.",
            "Names the currency in which the NPV report states every NPV. ORE converts the NPV of a trade "
            "in another currency at the spot rate of the valuation date. The Setup block has a parameter of the "
            "same name, and ORE reads that one first. This parameter is the older place for the same choice and "
            "ORE reads it only when the Setup block has none. For example, EUR states all NPVs in euro.",
            "The base currency of the Setup block. When that is also missing, the currency of the discounting index.",
            "A currency code of three letters, for example EUR."),
        "outputFileName": entry(
            "NPV report file",
            "A file name.",
            "Names the file that holds the NPV report: one row for each trade with its NPV, its currency and "
            "its NPV in the base currency. Change it when one run writes several reports and you need to tell the "
            "files apart. For example, npv.csv writes the report to npv.csv in the output path.",
            "npv.csv",
            FILE),
        "extendedResults": entry(
            "Extended results",
            "A flag. ORE ignores it.",
            "ORE does not read this parameter in any release. Example files carry it with the value N, so it "
            "appears in many run documents. Leave it as the document has it, because a value has no effect on the "
            "run. To get more output from the NPV analytic, use Write additional results.",
            "No. ORE ignores the parameter, so nothing changes with either value.",
            FLAG),
        "additionalResults": entry(
            "Write additional results",
            "A flag. Y writes the report.",
            "Decides whether the run writes the additional results of the pricing engines. These are values that "
            "an engine computes on the way to the NPV, for example the annuity, the forward rate and the standard "
            "deviation of a swaption. Switch it on to check how an engine reached an NPV. "
            "This is the older place for the choice. The newer way is the additional results analytic with its "
            "own Active parameter, and ORE reads this parameter only when that analytic does not set it.",
            "No. The run does not write the additional results.",
            FLAG),
        "additionalResultsReportPrecision": entry(
            "Additional results precision",
            "A whole number of decimal places.",
            "Sets the number of decimal places of the numbers in the additional results report. A higher value "
            "shows more of each result and makes the file larger. For example, 12 writes a result such as "
            "0.016641350123. This is the older place for the choice. The additional results analytic has a "
            "parameter of the same name, and ORE reads this one only when that analytic does not set it.",
            "6",
            "A whole number of zero or more."),
        "outputCurves": entry(
            "Write curves with the NPV",
            "A flag. Y also runs the curves analytic.",
            "Is an older way to switch on the curves report from inside the NPV analytic. When it is Y, the "
            "run writes the yield curves as the curves analytic does. ORE reads it only when the curves "
            "analytic is not active. For a new document, set Active in the curves analytic instead.",
            "No. The NPV analytic does not write the curves report.",
            FLAG),
        "nameMappingInputFile": entry(
            "Name mapping file",
            "A file name.",
            "Names the file that maps the names in the portfolio to the names that the CRIF report uses. The CRIF "
            "report needs this mapping. ORE reads the file name from the first of these blocks that has one: "
            "crif, saccr, simm, npv and Setup. This is the fourth place. The Setup block is the best place, "
            "because it serves all of the analytics.",
            "None. When no block names a file, ORE uses the names as they are.",
            FILE),
        "bucketMappingInputFile": entry(
            "Bucket mapping file",
            "A file name.",
            "Names the file that maps risk factors to the SIMM buckets of the CRIF report. ORE reads the file name "
            "from the first of these blocks that has one: crif, saccr, simm, npv and Setup. This is the fourth "
            "place. The Setup block is the best place, because it serves all of the analytics.",
            "None. When no block names a file, ORE uses the standard SIMM buckets.",
            FILE),
    },
    "cashflow": {
        "active": entry(
            "Run the cash flow analytic",
            "A flag. Y runs the analytic.",
            "Switches the cash flow analytic on or off. When it is on, the run writes a report with every future "
            "cash flow of every trade: the pay date, the amount, the currency, the coupon, the accrual and the "
            "fixing. Use it to check the cash flows that sit behind an NPV.",
            "No. The analytic does not run.",
            FLAG, required=True),
        "outputFileName": entry(
            "Cash flow report file",
            "A file name.",
            "Names the file that holds the cash flow report. Change it when one run writes several reports and "
            "you need to tell the files apart. For example, flows.csv writes the report to flows.csv in the "
            "output path.",
            "cashflow.csv",
            FILE),
        "includePastCashflows": entry(
            "Include past cash flows",
            "A flag. Y adds the cash flows before the valuation date.",
            "Decides whether the report also lists cash flows with a pay date before the valuation date. "
            "Past cash flows are useful to check fixings and accruals of the current period. They make the "
            "report longer. This is the older place for the choice. The Setup block has a parameter of the same "
            "name, and ORE reads that one first.",
            "No. The report lists only the cash flows from the valuation date on.",
            FLAG),
        "cashFlowHorizon": entry(
            "Cash flow horizon",
            "A date.",
            "Sets the last pay date that the cash flow NPV analytic counts. The analytic adds up the present value "
            "of the cash flows of each trade that pay after the valuation date and on or before this date. Use "
            "it to see how much of an NPV falls due in the near term. For example, 2027-12-31 counts the cash "
            "flows up to the end of 2027. The cash flow NPV analytic has a parameter of the same name.",
            "None. Without a horizon the analytic counts no cash flow, so every present value is 0.",
            DATE),
        "portfolioFilterDate": entry(
            "Portfolio filter date",
            "A date.",
            "Sets the date that decides which trades the run keeps. ORE removes every trade that matures before "
            "this date. This applies to all analytics of the run, not only to the cash flow analytic. For "
            "example, 2027-01-01 removes all trades that mature before the start of 2027.",
            "The valuation date. ORE removes the trades that have matured.",
            DATE),
    },
    "cashflownpv": {
        "active": entry(
            "Run the cash flow NPV analytic",
            "A flag. Y runs the analytic.",
            "Switches the cash flow NPV analytic on or off. When it is on, the run builds the cash flow report "
            "and adds up, for each trade, the present value of the cash flows up to the horizon. The result is "
            "one row for each trade with its NPV in the base currency.",
            "No. The analytic does not run.",
            FLAG, required=True),
        "cashFlowHorizon": entry(
            "Cash flow horizon",
            "A date.",
            "Sets the last pay date that the report counts. ORE adds up the present value of the cash flows "
            "of each trade that pay after the valuation date and on or before this date. For example, "
            "2027-12-31 counts the cash flows up to the end of 2027. The cash flow analytic has a parameter of "
            "the same name.",
            "None. Without a horizon the report counts no cash flow, so every present value is 0.",
            DATE),
    },
    "curves": {
        "active": entry(
            "Run the curves analytic",
            "A flag. Y runs the analytic.",
            "Switches the curves analytic on or off. When it is on, the run exports the discount factors of the "
            "yield curves that it builds from the market data, as set in the todaysmarket file. Use it to check "
            "the curves behind an NPV.",
            "No. The analytic does not run.",
            FLAG, required=True),
        "configuration": entry(
            "Market configuration",
            "The name of a market configuration.",
            "Selects the set of curves that the analytic exports. The todaysmarket file groups the curves in "
            "named configurations, and this parameter picks one. For example, default exports the curves of the "
            "default configuration.",
            "default",
            "The name of a configuration in the todaysmarket file."),
        "grid": entry(
            "Time grid",
            "A number of steps and a step length, or a list of tenors.",
            "Sets the dates at which the analytic exports the discount factors. Write it as a number of steps "
            "followed by the length of a step, or as a list of tenors. ORE builds the grid from the valuation "
            "date and rolls it with the calendar of this analytic. For example, 240,1M gives 240 monthly "
            "steps from the valuation date, and 1W,1M,2M,3M gives the four dates one week, one month, "
            "two months and three months out.",
            "240,1M. That is 240 monthly steps, 20 years.",
            "Either a number of steps, a comma and a period such as 1M, or a comma-separated list of periods."),
        "outputFileName": entry(
            "Curves report file",
            "A file name.",
            "Names the file that holds the exported discount factors. The file has one row for each date of "
            "the grid and one column for each curve. For example, curves.csv writes the report to curves.csv "
            "in the output path.",
            "curves.csv",
            FILE),
        "outputTodaysMarketCalibration": entry(
            "Write the market calibration report",
            "A flag. Y writes the calibration report.",
            "Decides whether the run also writes a report on how it built the market objects of the "
            "valuation date. The report shows the calibration of each curve and surface. Switch it on to find "
            "out why a curve has the shape it has, or to find a build that failed.",
            "No. The run does not write the calibration report.",
            FLAG),
        "calendar": entry(
            "Grid calendar",
            "A calendar name.",
            "Names the calendar that ORE uses to roll the dates of the grid to business days. Change it when "
            "the curves belong to a market with other holidays than the euro area. For example, UK "
            "rolls the grid dates to business days of the United Kingdom.",
            "TARGET",
            "A calendar name such as TARGET or UK."),
        "todaysMarketCalibrationPrecision": entry(
            "Calibration report precision",
            "A whole number of decimal places.",
            "Sets the number of decimal places of the numbers in the market calibration report. A higher value "
            "shows more of each number and makes the file larger. It has an effect only when the market "
            "calibration report is on. The NPV analytic has a similar parameter for its additional results.",
            "8",
            "A whole number of zero or more."),
    },
    "additionalResults": {
        "active": entry(
            "Run the additional results analytic",
            "A flag. Y runs the analytic.",
            "Switches the additional results analytic on or off. When it is on, the run writes a report with "
            "the results that the pricing engines compute besides the NPV, for example the annuity, the forward "
            "rate and the standard deviation of a swaption. What a trade returns depends on its pricing engine. "
            "Use it to check how an engine reached an NPV.",
            "No. The analytic does not run.",
            FLAG, required=True),
        "outputFileName": entry(
            "Additional results report file",
            "A file name.",
            "Is meant to name the file that holds the additional results. ORE does not read it. The report always "
            "has the name additional_results.csv, whatever this parameter says, and has one row for each result "
            "of each trade with the trade, the name of the result, its type and its value. Example files carry "
            "the parameter.",
            "additional_results.csv. ORE ignores the parameter, so the name is always this one.",
            FILE),
        "additionalResultsReportPrecision": entry(
            "Additional results precision",
            "A whole number of decimal places.",
            "Sets the number of decimal places of the numbers in the additional results report. A higher value "
            "shows more of each result and makes the file larger. For example, 12 writes a result such as "
            "0.016641350123. ORE reads this parameter first. The NPV analytic has a parameter of the same name "
            "that ORE reads only when this one is missing.",
            "6",
            "A whole number of zero or more."),
    },
}
