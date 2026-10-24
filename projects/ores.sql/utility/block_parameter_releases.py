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
"""The ORE release that introduced each parameter of the Setup, Logging and Markets blocks.

Written by measure_ore_parameter_releases.py. The key is the analytic type or the block
and the parameter name. The oldest tag of the checkout is 1.8.0.7, so a date equal
to it reads "1.8.0.7 or earlier".
"""

INTRODUCED_IN = {
    "Logging|logFile": "1.8.0.7 or earlier",
    "Logging|logMask": "1.8.2.0",
    "Logging|progressLogFile": "1.8.11.0",
    "Logging|progressLogRotationSize": "1.8.11.0",
    "Logging|progressLogToConsole": "1.8.11.0",
    "Logging|structuredLogFile": "1.8.11.0",
    "Logging|structuredLogRotationSize": "1.8.11.0",
    "Markets|bondpricing": "1.8.10.0",
    "Markets|crcalibration": "1.8.6.0",
    "Markets|eqcalibration": "1.8.2.0",
    "Markets|fxcalibration": "1.8.0.7 or earlier",
    "Markets|infcalibration": "1.8.3.0",
    "Markets|lgmcalibration": "1.8.0.7 or earlier",
    "Markets|pricing": "1.8.0.7 or earlier",
    "Markets|sensitivity": "1.8.2.0",
    "Markets|simulation": "1.8.0.7 or earlier",
    "Setup|accrualDate": "1.8.10.0",
    "Setup|additionalResults": "1.8.6.0",
    "Setup|allFixings": "1.8.16.0",
    "Setup|allowModelBuilderFallbacks": "1.8.14.0",
    "Setup|asofDate": "1.8.0.7 or earlier",
    "Setup|baseCurrency": "1.8.0.7 or earlier",
    "Setup|baselTrafficLightConfig": "1.8.13.1",
    "Setup|bucketMappingInputFile": "1.8.13.0",
    "Setup|calendarAdjustment": "1.8.5.0",
    "Setup|conventionsFile": "1.8.0.7 or earlier",
    "Setup|conventionsOverride": "1.8.15.0",
    "Setup|counterpartyFile": "1.8.13.0",
    "Setup|csvCommentReportHeader": "1.8.12.0",
    "Setup|csvSeparator": "1.8.12.0",
    "Setup|currencyConfiguration": "1.8.6.0",
    "Setup|curveConfigFile": "1.8.0.7 or earlier",
    "Setup|curveConfigOverride": "1.8.15.0",
    "Setup|dateCalendar": "1.8.10.0",
    "Setup|dateConvention": "1.8.10.0",
    "Setup|defaultMappingToIdentity": "1.8.10.0",
    "Setup|discountingIndex": "1.8.16.0",
    "Setup|dividendDataFile": "1.8.5.0",
    "Setup|dryRun": "1.8.9.0",
    "Setup|enableScenarioInformation": "1.8.14.0",
    "Setup|entireMarket": "1.8.9.0",
    "Setup|eomInflationFixings": "1.8.9.0",
    "Setup|fixingCutOffDate": "1.8.15.0",
    "Setup|fixingCutoff": "1.8.12.0",
    "Setup|fixingDataFile": "1.8.0.7 or earlier",
    "Setup|iborFallbackConfig": "1.8.6.0",
    "Setup|iborFallbackOverride": "1.8.9.0",
    "Setup|includePastCashflows": "1.8.6.0",
    "Setup|inputPath": "1.8.0.7 or earlier",
    "Setup|inputPathMarket": "1.8.10.0",
    "Setup|inputPathPortfolio": "1.8.10.0",
    "Setup|logFile": "1.8.0.7 or earlier",
    "Setup|marketConfigFile": "1.8.0.7 or earlier",
    "Setup|marketDataFile": "1.8.0.7 or earlier",
    "Setup|marketDataLoaderInput": "1.8.13.0",
    "Setup|marketDataLoaderOutput": "1.8.13.0",
    "Setup|nameMappingInputFile": "1.8.13.0",
    "Setup|outputPath": "1.8.0.7 or earlier",
    "Setup|portfolioFile": "1.8.0.7 or earlier",
    "Setup|portfolioRecurseIntoSubDirectories": "1.8.10.0",
    "Setup|pricingEnginesFile": "1.8.0.7 or earlier",
    "Setup|pricingEnginesFileScenario": "1.8.10.0",
    "Setup|pricingEnginesOverride": "1.8.17.0",
    "Setup|referenceDataFile": "1.8.5.0",
    "Setup|referenceDataOverride": "1.8.15.0",
    "Setup|reportBufferSize": "1.8.13.0",
    "Setup|reportNaString": "1.8.9.0",
    "Setup|scriptLibrary": "1.8.11.0",
    "Setup|useAnalytics": "1.8.9.0",
    "Setup|useMarketDataFixings": "1.8.9.0",
}

# The first release whose C++ sources no longer read the name.
REMOVED_IN = {
    "Setup|useAnalytics": "1.8.9.1",
}

# Names that no release of the C++ sources ever read. Examples carry them.
NEVER_READ = [
    "Markets|bondpricing",
    "Setup|accrualDate",
    "Setup|dateCalendar",
    "Setup|dateConvention",
    "Setup|defaultMappingToIdentity",
    "Setup|inputPathMarket",
    "Setup|inputPathPortfolio",
    "Setup|portfolioRecurseIntoSubDirectories",
    "Setup|pricingEnginesFileScenario",
]
