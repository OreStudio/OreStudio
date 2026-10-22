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
"""The parameters of the Setup, Logging and Markets blocks of ore.xml, apart from the 15 engine settings.

Each row is the block, the parameter name and the domain of its value. The engine settings are in
setup_parameter_descriptions.py, and the analytic parameters are in analytic_parameter_catalogue.py. The
texts are in the block_parameter_descriptions package. ORE reads no Restore block, so it has no row.
"""

BLOCK_CATALOGUE = [
    ("Setup", "asofDate", "date"),
    ("Setup", "baseCurrency", "string"),
    ("Setup", "discountingIndex", "string"),
    ("Setup", "dryRun", "boolean"),
    ("Setup", "inputPath", "string"),
    ("Setup", "outputPath", "string"),
    ("Setup", "inputPathMarket", "string"),
    ("Setup", "inputPathPortfolio", "string"),
    ("Setup", "logFile", "string"),
    ("Setup", "marketDataFile", "string"),
    ("Setup", "fixingDataFile", "string"),
    ("Setup", "dividendDataFile", "string"),
    ("Setup", "fixingCutoff", "date"),
    ("Setup", "fixingCutOffDate", "date"),
    ("Setup", "marketDataLoaderInput", "string"),
    ("Setup", "marketDataLoaderOutput", "string"),
    ("Setup", "entireMarket", "boolean"),
    ("Setup", "allFixings", "boolean"),
    ("Setup", "eomInflationFixings", "boolean"),
    ("Setup", "useMarketDataFixings", "boolean"),
    ("Setup", "curveConfigFile", "string"),
    ("Setup", "curveConfigOverride", "string"),
    ("Setup", "conventionsFile", "string"),
    ("Setup", "conventionsOverride", "string"),
    ("Setup", "marketConfigFile", "string"),
    ("Setup", "pricingEnginesFile", "string"),
    ("Setup", "pricingEnginesOverride", "string"),
    ("Setup", "pricingEnginesFileScenario", "string"),
    ("Setup", "portfolioFile", "string"),
    ("Setup", "portfolioRecurseIntoSubDirectories", "boolean"),
    ("Setup", "counterpartyFile", "string"),
    ("Setup", "calendarAdjustment", "string"),
    ("Setup", "currencyConfiguration", "string"),
    ("Setup", "referenceDataFile", "string"),
    ("Setup", "referenceDataOverride", "string"),
    ("Setup", "iborFallbackConfig", "string"),
    ("Setup", "iborFallbackOverride", "boolean"),
    ("Setup", "baselTrafficLightConfig", "string"),
    ("Setup", "scriptLibrary", "string"),
    ("Setup", "nameMappingInputFile", "string"),
    ("Setup", "bucketMappingInputFile", "string"),
    ("Setup", "allowModelBuilderFallbacks", "boolean"),
    ("Setup", "enableScenarioInformation", "boolean"),
    ("Setup", "includePastCashflows", "boolean"),
    ("Setup", "reportNaString", "string"),
    ("Setup", "csvSeparator", "string"),
    ("Setup", "csvCommentReportHeader", "string"),
    ("Setup", "reportBufferSize", "integer"),
    ("Setup", "additionalResults", "boolean"),
    ("Setup", "accrualDate", "string"),
    ("Setup", "dateCalendar", "string"),
    ("Setup", "dateConvention", "string"),
    ("Setup", "defaultMappingToIdentity", "boolean"),
    ("Setup", "useAnalytics", "boolean"),
    ("Logging", "logFile", "string"),
    ("Logging", "logMask", "integer"),
    ("Logging", "progressLogFile", "string"),
    ("Logging", "progressLogRotationSize", "integer"),
    ("Logging", "progressLogToConsole", "boolean"),
    ("Logging", "structuredLogFile", "string"),
    ("Logging", "structuredLogRotationSize", "integer"),
    ("Markets", "pricing", "string"),
    ("Markets", "simulation", "string"),
    ("Markets", "lgmcalibration", "string"),
    ("Markets", "fxcalibration", "string"),
    ("Markets", "eqcalibration", "string"),
    ("Markets", "infcalibration", "string"),
    ("Markets", "crcalibration", "string"),
    ("Markets", "sensitivity", "string"),
    ("Markets", "bondpricing", "string"),
]
