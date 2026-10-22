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
"""The Setup parameters that name the date, the currency, the paths and the market data."""

from analytic_parameter_descriptions._entry import DATE, FLAG, entry

PATH = "A path. ORE reads it relative to the directory the program starts in, or as an absolute path."
INPUT_FILE = "A file name or a path. ORE looks for the file in the input path of the Setup block."
OUTPUT_FILE = "A file name or a path. ORE writes the file in the output path of the Setup block."
CODE = "A text, written as in the example."

ENTRIES = {
    "asofDate": entry(
        "Valuation date",
        "A date. The date the run values the portfolio at.",
        "Sets the valuation date of the run, also called the as of date. ORE sets the evaluation date of QuantLib to this date, "
        "reads the market data and the fixings for it and values every trade at it. Every analytic uses it. "
        "For example, 2016-02-05 values the portfolio with the market of 5 February 2016. "
        "Use the date of the market data file.",
        "None. The document must carry a date.", DATE, required=True),
    "baseCurrency": entry(
        "Base currency",
        "A currency code, for example EUR.",
        "Names the currency in which results are stated when an analytic converts them to one currency, for example the NPV "
        "report and the exposure reports. ORE converts a result in another currency at the spot rate of the valuation date. "
        "For example, EUR states all results in euro. Analytics that carry their own base currency use that one for their own report.",
        "None. ORE takes the currency of the discounting index when that one is set.",
        "A currency code of three letters, for example EUR or USD."),
    "discountingIndex": entry(
        "Discounting index",
        "An index name, for example EUR-EONIA.",
        "Names the main discounting index of the run. When the base currency is not set, ORE takes the currency of this index "
        "as the base currency. For example, EUR-EONIA sets the base currency to EUR. Set it only when the base currency "
        "is not given in the document.",
        "None.", "An index name written as currency, dash and index, for example EUR-EONIA or USD-FedFunds."),
    "dryRun": entry(
        "Dry run",
        "A flag. Y builds the run and does not value.",
        "Decides whether the valuation engine does all of its work. When it is Yes, the engine prices the trades for the first "
        "scenario sample only. It fills the other samples with the value at the valuation date plus a small made-up shift. "
        "Use Yes to test a document and its input files in a short time before a long run. The results of a dry run are not real "
        "and are not for use.",
        "No. The engines value the portfolio.", FLAG),
    "inputPath": entry(
        "Input path",
        "A path to the input directory.",
        "Names the directory that ORE reads input files from. Every file name in the Setup block, for example the portfolio "
        "file and the curve configuration file, is relative to it. A relative path counts from the directory the program starts in. "
        "For example, Input reads the files from the directory Input next to the program.",
        "None. The document must carry a path.", PATH, required=True),
    "outputPath": entry(
        "Output path",
        "A path to the output directory.",
        "Names the directory that ORE writes the reports, the log and the cubes to. Every output file name in the document "
        "is relative to it. A relative path counts from the directory the program starts in. "
        "For example, Output writes the results to the directory Output next to the program.",
        "None. The document must carry a path.", PATH, required=True),
    "inputPathMarket": entry(
        "Market input path",
        "A path. No release of ORE reads it.",
        "No release of the ORE sources reads this parameter. Some example documents carry it to name a separate directory "
        "for the market data files. ORE ignores it, so the market data files follow the input path. Do not rely on it.",
        "None. ORE ignores the parameter.", PATH),
    "inputPathPortfolio": entry(
        "Portfolio input path",
        "A path. No release of ORE reads it.",
        "No release of the ORE sources reads this parameter. Some example documents carry it to name a separate directory "
        "for the portfolio files. ORE ignores it, so the portfolio file follows the input path. Do not rely on it.",
        "None. ORE ignores the parameter.", PATH),
    "logFile": entry(
        "Log file",
        "A file name for the log.",
        "Names the file that ORE writes its log to. The path is relative to the output path. The Logging block has a "
        "parameter of the same name, and the Logging block wins when both are present. For example, log.txt writes the log to "
        "the file log.txt in the output directory.",
        "None. ORE writes the log to the file given in the Logging block, or to no file.", OUTPUT_FILE),
    "marketDataFile": entry(
        "Market data file",
        "A file name of the market quotes.",
        "Names the file that holds the market quotes for the valuation date: one line for each quote with the date, the quote "
        "name and the value. ORE builds the curves and surfaces of today's market from these quotes. "
        "You can name more than one file, separated by a comma or a semicolon. For example, market_20160205.txt holds the quotes of 5 February 2016.",
        "None. The document must name the file, unless the run loads the market data some other way.", INPUT_FILE, required=True),
    "fixingDataFile": entry(
        "Fixing data file",
        "A file name of the index fixings.",
        "Names the file that holds the historic fixings of the indices: one line for each fixing with the date, the index name and "
        "the value. Coupons that have already fixed use these values. "
        "You can name more than one file, separated by a comma or a semicolon. For example, fixings_20160205.txt holds the fixings known on 5 February 2016. "
        "A missing fixing makes the valuation of a trade that needs it fail, unless the run implies today's fixing or fills gaps.",
        "None. The document must name the file, unless the run loads the fixings some other way.", INPUT_FILE, required=True),
    "dividendDataFile": entry(
        "Dividend data file",
        "A file name of the equity dividends.",
        "Names the file that holds the historic dividends of equities. ORE needs them to value equity options and "
        "forwards that depend on dividends. The file is optional and the run needs none when the portfolio holds no such trade. "
        "For example, dividends_20160205.txt.",
        "None. The run loads no dividends.", INPUT_FILE),
    "fixingCutoff": entry(
        "Fixing cut-off",
        "A date. The ORE application reads it, with the spelling fixingCutoff.",
        "Sets the date after which the ORE application drops fixings from the fixing file. A fixing after the cut-off date is ignored, so the "
        "run does not use data from the future. For example, 2016-02-04 ignores a fixing dated 5 February 2016. "
        "The input parameters of the library read the same choice under the name fixingCutOffDate, with a capital O. When the date is not set, "
        "the application cuts at the valuation date, or the day before it when today's fixings are implied.",
        "None. The application cuts at the valuation date.", DATE),
    "fixingCutOffDate": entry(
        "Fixing cut-off date",
        "A date. The library reads it, with the spelling fixingCutOffDate.",
        "Sets the date after which the library drops fixings. A fixing after the cut-off date is ignored. For example, 2016-02-04 "
        "ignores a fixing dated 5 February 2016. The ORE application reads the same choice under the name fixingCutoff. "
        "Use this spelling when the document goes to a program built on the library.",
        "None. No fixing is dropped.", DATE),
    "marketDataLoaderInput": entry(
        "Market data loader input",
        "A name of the loader for the market data.",
        "Names the input of the market data loader. A program built on the library sets the loader to read market data and "
        "fixings. The value tells the loader where to read them from. Leave it empty when the market data comes from the "
        "market data file.",
        "None. The run reads the market data and fixings from the files.", CODE),
    "marketDataLoaderOutput": entry(
        "Market data loader output",
        "A name for the output of the market data loader.",
        "Names the output of the market data loader. A program built on the library sets the loader to store the market data "
        "and the fixings of a run, so that another run can load the same data again. The value tells the loader where to "
        "store them. Leave it empty to store nothing.",
        "None. The run stores nothing.", CODE),
    "entireMarket": entry(
        "Load the entire market",
        "A flag. Y loads every quote of the file.",
        "Decides how much of the market data file the run loads. When it is Yes, ORE loads every quote of the file. When it is "
        "No, ORE loads only the quotes that the curve configuration asks for, which is faster. "
        "The library defaults to No. The ORE application sets Yes when it reads ore.xml, so a document run by the application loads everything.",
        "No in the library. The ORE application sets Yes.", FLAG),
    "allFixings": entry(
        "Load all fixings",
        "A flag. Y loads every fixing of the file.",
        "Decides how much of the fixing file the run loads. When it is Yes, ORE loads every fixing of the file. When it is "
        "No, ORE loads only the fixings of the indices that the portfolio and the market need. "
        "The library defaults to No. The ORE application sets Yes when it reads ore.xml. Use No to load faster with a large file.",
        "No in the library. The ORE application sets Yes.", FLAG),
    "eomInflationFixings": entry(
        "End-of-month inflation fixings",
        "A flag. Y loads inflation fixings at the end of the month.",
        "Decides how ORE stores the fixing of an inflation index. When it is Yes, ORE stores the fixing at the end of the "
        "month it belongs to, which is how most inflation indices publish. When it is No, ORE stores the fixing at the start "
        "of the month. Change it only when the fixing file dates its inflation fixings at the start of the month.",
        "Yes. ORE stores an inflation fixing at the end of its month.", FLAG),
    "useMarketDataFixings": entry(
        "Use fixings of the market data",
        "A flag. ORE reads it and does not use it.",
        "The input parameters read this parameter and keep it. No code of the ORE sources uses the value, so the choice has no effect on a run. "
        "The intent is to take the fixings from the market data file, where the file holds them, and not from the fixing file.",
        "Yes. The value has no effect.", FLAG),
    "curveConfigFile": entry(
        "Curve configuration file",
        "A file name of the curve configuration.",
        "Names the file that says how ORE builds each curve and surface: which quotes it uses, how it interpolates and which "
        "conventions it takes. Today's market refers to the curves in this file by identifier. "
        "For example, curveconfig.xml. You can name more than one file, separated by a comma or a semicolon, and a later file adds to an earlier one.",
        "None. The document must name the file.", INPUT_FILE, required=True),
    "curveConfigOverride": entry(
        "Curve configuration override",
        "Curve configuration XML. It overrides the file.",
        "Holds a curve configuration inside the document. A curve in this block replaces the curve of the same identifier in "
        "the curve configuration file. Use it to change one curve for a run without a copy of the whole file.",
        "None. The run uses the file only.", "An XML element, the curve configuration, written inside the parameter."),
    "conventionsFile": entry(
        "Conventions file",
        "A file name of the market conventions.",
        "Names the file that holds the market conventions: the day count, the calendar, the settlement lag and the business day "
        "rule of each instrument and index. The curves and the trades look their conventions up by identifier. "
        "For example, conventions.xml.",
        "None. The document must name the file.", INPUT_FILE, required=True),
    "conventionsOverride": entry(
        "Conventions override",
        "Conventions XML. It overrides the file.",
        "Holds conventions inside the document. A convention in this block replaces the convention of the same identifier in "
        "the conventions file. Use it to change one convention for a run without a copy of the whole file.",
        "None. The run uses the file only.", "An XML element, the conventions, written inside the parameter."),
    "marketConfigFile": entry(
        "Today's market file",
        "A file name of today's market configuration.",
        "Names the file that defines today's market: which curve, surface or quote each market object uses, grouped into "
        "named market configurations. The Markets block picks a configuration from this file for each purpose. "
        "For example, todaysmarket.xml.",
        "None. The document must name the file.", INPUT_FILE, required=True),
    "pricingEnginesFile": entry(
        "Pricing engines file",
        "A file name of the pricing engines.",
        "Names the file that says which pricing engine and which model prices each product, and with which engine settings. "
        "Every trade looks up its engine here by product type. "
        "For example, pricingengine.xml.",
        "None. The document must name the file.", INPUT_FILE, required=True),
    "pricingEnginesOverride": entry(
        "Pricing engines override",
        "Pricing engine XML. It overrides the file.",
        "Holds pricing engine settings inside the document. An entry in this block replaces the entry of the same product in the "
        "pricing engines file. Use it to change one engine for a run without a copy of the whole file.",
        "None. The run uses the file only.", "An XML element, the pricing engines, written inside the parameter."),
    "pricingEnginesFileScenario": entry(
        "Scenario pricing engines file",
        "A file name. No release of ORE reads it.",
        "No release of the ORE sources reads this parameter. Some example documents carry it to name a second pricing engines "
        "file for scenario runs. ORE ignores it, so the scenario runs use the file of the pricing engines parameter.",
        "None. ORE ignores the parameter.", INPUT_FILE),
    "portfolioFile": entry(
        "Portfolio file",
        "A file name of the portfolio.",
        "Names the file that holds the trades the run values. You can name more than one file, separated by a comma or a semicolon, "
        "and ORE joins their trades. A name that is a directory loads the files inside it. "
        "For example, portfolio.xml.",
        "None. The document must name the file.", INPUT_FILE, required=True),
    "portfolioRecurseIntoSubDirectories": entry(
        "Portfolio sub-directories",
        "A flag. No release of ORE reads it.",
        "No release of the ORE sources reads this parameter. Some example documents carry it to load the portfolio files of "
        "the sub-directories of a directory. ORE ignores it, so name each directory or file yourself.",
        "No. ORE ignores the parameter.", FLAG),
    "counterpartyFile": entry(
        "Counterparty file",
        "A file name of the counterparty information.",
        "Names the file that holds the counterparty data some capital analytics need, for example the credit quality and the "
        "SA-CCR and CVA parameters of each counterparty. "
        "For example, counterparty.xml. Leave it out when no analytic of the run uses counterparty data.",
        "None. The run loads no counterparty data.", INPUT_FILE),
    "calendarAdjustment": entry(
        "Calendar adjustment file",
        "A file name of the calendar adjustments.",
        "Names the file that lists holidays and business days to add to named calendars. Use it to correct a calendar "
        "of QuantLib, for example to add a closure the library does not know. "
        "For example, calendaradjustment.xml.",
        "None. ORE uses the calendars as they are.", INPUT_FILE),
    "currencyConfiguration": entry(
        "Currency configuration file",
        "A file name of extra currencies.",
        "Names the file that adds currencies to the ones ORE knows. The file adds only currencies that are not yet in "
        "QuantLib or QuantExt, and it never changes one that is. The example file lists all ISO currencies and a few unofficial codes. "
        "For example, currencies.xml.",
        "None. ORE knows its built-in currencies only.", INPUT_FILE),
    "referenceDataFile": entry(
        "Reference data file",
        "A file name of the reference data.",
        "Names the file that holds reference data: the details of the bonds, the credit names and the other objects a trade "
        "refers to by identifier. A trade that holds only an identifier needs this file. "
        "For example, referencedata.xml.",
        "None. The run loads no reference data.", INPUT_FILE),
    "referenceDataOverride": entry(
        "Reference data override",
        "Reference data XML. It overrides the file.",
        "Holds reference data inside the document. An entry in this block replaces the entry of the same identifier in the "
        "reference data file. Use it to change one object for a run without a copy of the whole file.",
        "None. The run uses the file only.", "An XML element, the reference data, written inside the parameter."),
    "iborFallbackConfig": entry(
        "Ibor fallback file",
        "A file name of the Ibor fallback rules.",
        "Names the file that says how a discontinued Ibor index falls back to a risk free rate: the replacement index, the "
        "spread and the switch date. Trades on a discontinued index use it. "
        "For example, iborFallbackConfig.xml.",
        "None. ORE uses the fallback rules built into the program.", INPUT_FILE),
    "iborFallbackOverride": entry(
        "Ibor fallback override",
        "A flag. Y switches the Ibor fallbacks on.",
        "Decides whether ORE applies the Ibor fallback rules before their switch dates. "
        "When it is Yes, ORE moves every switch date to one year before the valuation date, so every index that has a fallback "
        "has already switched. When it is No, ORE applies a fallback from its own switch date.",
        "No. ORE applies a fallback from its switch date.", FLAG),
    "baselTrafficLightConfig": entry(
        "Basel traffic light file",
        "A file name of the traffic light zones.",
        "Names the file that sets the Basel traffic light zones for backtests: how many exceptions place a result in the green, "
        "yellow and red zones. The market risk backtest analytic reads it. "
        "Leave it out when no run does a backtest.",
        "None. The backtest uses no zones.", INPUT_FILE),
    "scriptLibrary": entry(
        "Script library file",
        "A file name of the script library.",
        "Names the file that holds the scripts of scripted trades. A scripted trade picks its payoff script by name from this "
        "library. Leave it out when the portfolio has no scripted trade.",
        "None. The run loads no scripts.", INPUT_FILE),
    "nameMappingInputFile": entry(
        "Name mapping file",
        "A file name of the SIMM name mapping.",
        "Names the file that maps the names of issuers and underlyings to the names the SIMM expects. The CRIF analytics read it. "
        "The same parameter in the block of an analytic wins, and this one is the last place ORE looks.",
        "None. The run maps no names.", INPUT_FILE),
    "bucketMappingInputFile": entry(
        "Bucket mapping file",
        "A file name of the SIMM bucket mapping.",
        "Names the file that maps each issuer and underlying to its SIMM bucket. The CRIF analytics read it. "
        "The same parameter in the block of an analytic wins, and this one is the last place ORE looks.",
        "None. The run maps no buckets.", INPUT_FILE),
    "allowModelBuilderFallbacks": entry(
        "Allow model builder fallbacks",
        "A flag. ORE reads it and does not use it.",
        "The input parameters read this parameter and keep it. No code of the ORE sources uses the value, so the choice has no effect on a run. "
        "The intent is to let a model builder use a fallback when its calibration fails.",
        "Yes. The value has no effect.", FLAG),
    "enableScenarioInformation": entry(
        "Scenario information",
        "A flag. Y keeps the information on scenarios.",
        "Decides whether ORE records extra information on each scenario while it runs the valuation engine, for example "
        "which risk factor each scenario shifts. The run takes more memory and time when it is Yes. Switch it on only when "
        "a report needs that information.",
        "No. ORE records nothing.", FLAG),
    "includePastCashflows": entry(
        "Include past cash flows",
        "A flag. Y keeps cash flows before the valuation date.",
        "Decides whether the cash flow report lists the cash flows that paid before the valuation date. "
        "When it is Yes, the report lists them. When it is No, the report lists only the cash flows from the valuation date on. "
        "The cash flow analytic has a parameter of the same name, and the Setup block wins when both are present.",
        "No. The report holds the future cash flows only.", FLAG),
    "reportNaString": entry(
        "Report not-available text",
        "A text for a value that is not available.",
        "Sets the text that a report writes in a field that has no value. For example, #N/A is the text a spreadsheet shows "
        "for a missing value. Set another text when a program that reads the report needs a different mark.",
        "#N/A", "A text."),
    "csvSeparator": entry(
        "CSV separator",
        "A single character that separates fields.",
        "Sets the character that separates the fields of a CSV report. A semicolon helps when a spreadsheet uses the "
        "comma as the decimal sign. The text must be one character. For example, ; writes semicolon separated reports.",
        "A comma.", "One character."),
    "csvCommentReportHeader": entry(
        "CSV comment character",
        "A single character. It starts the header line of a report.",
        "Sets the character at the start of the header line of a CSV report. A reader that skips lines that start "
        "with this character skips the header, which is the choice for a file with no header. The text must be one character. "
        "For example, # writes #Trade,NPV and so on.",
        "A hash mark, #.", "One character."),
    "reportBufferSize": entry(
        "Report buffer size",
        "A number of rows kept in memory.",
        "Sets how many rows ORE keeps in memory for each block of an in-memory report. A buffer keeps the memory "
        "use low for a large report. A value of 0 keeps the whole report in one block. For example, 100000 stores a "
        "report in blocks of 100000 rows.",
        "0. The whole report stays in memory.", "A whole number of zero or more."),
    "additionalResults": entry(
        "Write additional results",
        "A flag. Y writes the additional results report.",
        "Decides whether the NPV analytic writes the additional results of each trade, for example the legs, the curves and the model "
        "values that the pricing engine reports. The additionalResults block is the place for the same choice. "
        "The Setup spelling is the older one. Use Yes to look inside a price.",
        "No. ORE writes no additional results.", FLAG),
    "accrualDate": entry(
        "Accrual date",
        "A date. No release of ORE reads it.",
        "No release of the ORE sources reads this parameter. Some example documents carry it. ORE ignores it. Do not rely on it.",
        "None. ORE ignores the parameter.", DATE),
    "dateCalendar": entry(
        "Date calendar",
        "A calendar name. No release of ORE reads it.",
        "No release of the ORE sources reads this parameter. Some example documents carry it. ORE ignores it. Do not rely on it.",
        "None. ORE ignores the parameter.", "A calendar name, for example TARGET."),
    "dateConvention": entry(
        "Date convention",
        "A convention name. No release of ORE reads it.",
        "No release of the ORE sources reads this parameter. Some example documents carry it. ORE ignores it. Do not rely on it.",
        "None. ORE ignores the parameter.", "A business day convention, for example Following."),
    "defaultMappingToIdentity": entry(
        "Default mapping to identity",
        "A flag. No release of ORE reads it.",
        "No release of the ORE sources reads this parameter. Some example documents carry it. ORE ignores it. Do not rely on it.",
        "None. ORE ignores the parameter.", FLAG),
    "useAnalytics": entry(
        "Use analytics",
        "A flag. The latest release no longer reads it.",
        "Earlier releases read this parameter to switch the analytics on. Later releases switch an analytic on with its own "
        "active parameter, and they do not read this one. A document that carries it runs the same.",
        "None. The latest release ignores the parameter.", FLAG),
}
