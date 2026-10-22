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
"""The model calibration and scenario generation analytics."""

from ._entry import COUNT, DATE, FILE, FLAG, INPUT_FILE, REAL, entry

IGNORED = "ORE ignores the parameter, so a value has no effect."
HW_ONLY = "ORE uses it only when the model is HW and the mode is historical."
PCA_ONLY = "ORE uses it only for the HW model with historical mode and PCA calibration on."
MR_ONLY = "ORE uses it only for the HW model with historical mode and mean reversion calibration on."

ENTRIES = {
    "calibration": {
        "active": entry(
            "Run the calibration",
            "A flag. Y runs the calibration.",
            "Switches the model calibration on or off. When it is on, the run calibrates the simulation "
            "model and writes the calibrated model data, so that a later simulation can start from it "
            "without a new calibration. The calibration runs a part of what the simulation does.",
            "No. The analytic does not run.",
            FLAG, required=True),
        "model": entry(
            "Calibration model",
            "CAM or HW.",
            "Selects the model to calibrate. CAM is the cross asset model of the simulation, calibrated to "
            "market instruments. HW is the Hull-White model, calibrated to historical data. The HW parameters "
            "of this analytic have an effect only for HW.",
            "CAM",
            "CAM or HW."),
        "configFile": entry(
            "Model configuration file",
            "A file name.",
            "Names the XML file with the cross asset model data to calibrate: the currencies, the model "
            "parametrisations and the calibration instruments. This is usually the simulation configuration "
            "file. The output of the run replaces the initial values in this data with the calibrated values. "
            "For example, simulation.xml. ORE uses the file only for the CAM model.",
            "None. A CAM calibration needs this file, or the parameter Cross asset model file.",
            INPUT_FILE),
        "crossAssetModelData": entry(
            "Cross asset model file",
            "A file name.",
            "Names the XML file with the cross asset model data, if it sits in another file than the "
            "model configuration file. When it is set, ORE reads the model data from this file and not from "
            "the configuration file.",
            "The model configuration file.",
            INPUT_FILE),
        "pricingEnginesFile": entry(
            "Pricing engines file for the calibration",
            "A file name.",
            "Names the XML file that sets the pricing engines for the calibration instruments, for example "
            "the swaptions that the model is calibrated to. For example, pricingengine.xml.",
            "The pricing engines file of the Setup block.",
            INPUT_FILE),
        "outputFile": entry(
            "Calibration report file",
            "A file name.",
            "Names the file of the calibration report. The report holds the calibrated cross asset model as "
            "one XML string. The run also writes the model data to calibration.xml in the output path, and "
            "you can paste its model section into the simulation configuration file to run without "
            "a calibration. For example, calibration.csv.",
            "calibration.csv",
            FILE),
        "mode": entry(
            "HW calibration mode",
            "historical",
            "Selects how ORE calibrates the Hull-White model. Only historical is supported: ORE fits the model "
            "to historical discount factors and rates. The mode riskNeutral is a stub that does nothing in "
            "release 1.8.17.0. " + HW_ONLY,
            "None. With the HW model the parameter is needed.",
            "historical"),
        "foreignCurrencies": entry(
            "Foreign currencies",
            "A comma-separated list of currency codes.",
            "Lists the foreign currencies that the calibration covers, besides the base currency. For example, "
            "EUR,GBP calibrates the model for euro and sterling. " + HW_ONLY,
            "None. The calibration covers the base currency only.",
            "A comma-separated list of currency codes."),
        "curveTenors": entry(
            "Curve tenors",
            "A comma-separated list of periods.",
            "Lists the tenors of each interest rate curve. They are the points of the historical discount "
            "factors, and the points of each eigenvector in the files that give them. For example, "
            "1Y,2Y,3Y,5Y,10Y,15Y,20Y,30Y. " + HW_ONLY + " The run stops if the list is missing.",
            "None. The parameter is mandatory.",
            "A comma-separated list of periods such as 1Y,5Y."),
        "useForwardOrZeroRate": entry(
            "Zero or forward rates",
            "zero or forward.",
            "Selects the rates of the calibration. With PCA calibration on, it says whether the covariance "
            "matrix comes from changes in zero rates or in forward rates. With PCA calibration off, it says "
            "how ORE reads the eigenvectors of the input files for the mean reversion calibration. The run stops "
            "if the value is neither zero nor forward. " + HW_ONLY,
            "None. The parameter is needed.",
            "zero or forward."),
        "pcaCalibration": entry(
            "PCA calibration",
            "A flag. Y runs the principal component analysis.",
            "Decides whether ORE reads the historical discount factors and runs a principal component analysis "
            "(PCA) to find the factors of the interest rate curves. Switch it off to supply the eigenvectors "
            "from files, with PCA input files. " + HW_ONLY,
            "No.",
            FLAG),
        "scenarioInputFile": entry(
            "Historical scenario file",
            "A file name.",
            "Names the file with the historical discount factors and FX spot rates. The file must be in the "
            "scenario.csv format of ORE. " + PCA_ONLY + " The run stops if the file is missing.",
            "None. The parameter is mandatory.",
            INPUT_FILE),
        "startDate": entry(
            "History start date",
            "A date.",
            "Sets the first date of the historical data that the calibration uses. For example, 2019-09-29. "
            + PCA_ONLY + " The run stops if the date is missing.",
            "None. The parameter is mandatory.",
            DATE),
        "endDate": entry(
            "History end date",
            "A date.",
            "Sets the last date of the historical data that the calibration uses. For example, 2022-09-27. "
            + PCA_ONLY + " The run stops if the date is missing.",
            "None. The parameter is mandatory.",
            DATE),
        "lambda": entry(
            "Decay factor",
            "A number between 0 and 1.",
            "Sets the weight decay of the exponentially weighted differences in the historical rates, when ORE "
            "computes the covariance matrix. A value of 1 gives all dates the same weight. A smaller value "
            "puts more weight on recent dates. " + PCA_ONLY,
            "1. All dates have the same weight.",
            REAL),
        "varianceRetained": entry(
            "Variance retained",
            "A number above 0 and up to 1.",
            "Sets the share of the total variance that the retained principal components must explain. It "
            "decides how many principal components ORE keeps. For example, 0.90 keeps as many components as "
            "needed to explain 90 percent of the variance. " + PCA_ONLY + " The run stops if the value is "
            "missing, zero or above 1.",
            "None. The parameter is mandatory.",
            REAL),
        "pcaOutputFileName": entry(
            "PCA output file",
            "A file name.",
            "Names the file that receives the eigenvalues and eigenvectors that ORE retains for each "
            "currency. ORE writes it in the output path. For example, pca.csv. " + PCA_ONLY,
            "None. ORE does not write the file.",
            FILE),
        "pcaInputFileName": entry(
            "PCA input files",
            "A comma-separated list of file names.",
            "Lists the files with the eigenvalues and eigenvectors for the mean reversion calibration, one "
            "curve for each file. Use it when PCA calibration is off. For example, "
            "pca_USD.csv,pca_EUR.csv,pca_GBP.csv. " + MR_ONLY,
            "None.",
            "A comma-separated list of file names."),
        "meanReversionCalibration": entry(
            "Mean reversion calibration",
            "A flag. Y calibrates the mean reversion.",
            "Decides whether ORE calibrates the mean reversion of the principal components that it keeps. "
            "The components come from the PCA step or from the PCA input files. " + HW_ONLY,
            "No.",
            FLAG),
        "basisFunctionNumber": entry(
            "Number of basis functions",
            "A whole number.",
            "Sets the number of basis functions in the mean reversion calibration. For example, 2. " + MR_ONLY,
            "0. A run with mean reversion calibration needs a value.",
            COUNT),
        "kappaUpperBound": entry(
            "Upper bound of kappa",
            "A number.",
            "Sets the upper bound of the mean reversion speed (kappa) in the mean reversion calibration. For "
            "example, 5.0. " + MR_ONLY,
            "0. A run with mean reversion calibration needs a value.",
            REAL),
        "haltonMaxGuess": entry(
            "Maximum number of guesses",
            "A whole number.",
            "Sets the largest number of starting points that the optimisation of the mean reversion "
            "calibration tries. The points come from a Halton sequence. A higher number can find a better "
            "fit and takes longer. For example, 500. " + MR_ONLY,
            "0. A run with mean reversion calibration needs a value.",
            COUNT),
        "meanReversionOutputFileName": entry(
            "Mean reversion output file",
            "A file name.",
            "Names the file that receives the calibrated mean reversion speed (kappa) and the volatility "
            "(v) of each curve. ORE writes it in the output path. For example, meanReversion.csv. " + MR_ONLY,
            "None. ORE does not write the file.",
            FILE),
    },
    "scenarioGeneration": {
        "active": entry(
            "Run scenario generation",
            "A flag. Y runs the analytic.",
            "Switches the scenario generation on or off. When it is on, the run generates market scenarios "
            "and writes statistics and distributions of them. Use it to check the scenarios of a simulation "
            "market, a stress test or a sensitivity run.",
            "No. The analytic does not run.",
            FLAG, required=True),
        "scenarioType": entry(
            "Scenario type",
            "stress, sensitivity or exposure.",
            "Selects which scenarios the analytic generates. exposure takes them from the simulation model. "
            "stress takes them from a stress test configuration. sensitivity is accepted, but release "
            "1.8.17.0 has no scenario generation for it. The run stops if the value is none of the three.",
            "exposure",
            "stress, sensitivity or exposure."),
        "simulationConfigFile": entry(
            "Simulation configuration file",
            "A file name.",
            "Names the XML file with the simulation market that the scenarios are for. For an exposure run the "
            "file also supplies the model and the scenario generator, unless the parameters Cross asset model "
            "file and Scenario generator file name another file. For example, simulation.xml.",
            "None. The analytic needs this file.",
            INPUT_FILE),
        "stressConfigFile": entry(
            "Stress configuration file",
            "A file name.",
            "Names the XML file with the stress scenarios to generate. ORE uses it only when the scenario "
            "type is stress.",
            "None. A stress run needs this file.",
            INPUT_FILE),
        "crossAssetModelData": entry(
            "Cross asset model file",
            "A file name.",
            "Names the XML file with the cross asset model, if it sits in another file than the simulation "
            "configuration file. ORE uses it only when the scenario type is exposure.",
            "The simulation configuration file.",
            INPUT_FILE),
        "scenarioGeneratorData": entry(
            "Scenario generator file",
            "A file name.",
            "Names the XML file with the scenario generator, if it sits in another file than the simulation "
            "configuration file. ORE uses it only when the scenario type is exposure.",
            "The simulation configuration file.",
            INPUT_FILE),
        "scenarioOutputFile": entry(
            "Scenario output file",
            "A file name.",
            "Names the file that holds all scenarios that the analytic generates through the simulation "
            "market. For example, scenariodump.csv.",
            "scenario.csv",
            FILE),
        "amcPathDataOutput": entry(
            "AMC path data output file",
            "A file name.",
            "Names a file that receives the path data of the generated exposure scenarios, so that an AMC run "
            "can read it later with its path data input. ORE uses it only for the scenario type exposure.",
            "None. The analytic does not write path data.",
            FILE),
        "filterRiskKeys": entry(
            "Risk factor types in the output",
            "A comma-separated list of risk factor types.",
            "Limits the scenario output to the listed types of risk factor. Without it the output holds every "
            "risk factor of the simulation market and can be very large. For example, DiscountCurve.",
            "None. The output holds all risk factors.",
            "A comma-separated list of risk factor key types such as DiscountCurve."),
        "distributionBuckets": entry(
            "Distribution buckets",
            "A whole number.",
            "Sets the number of buckets of the distribution histogram of the generated scenarios. A higher "
            "number gives a finer histogram and needs more scenarios to be stable. For example, 20.",
            "20",
            COUNT),
        "outputZeroRate": entry(
            "Report zero rates",
            "A flag. Y reports zero rates, N reports discount factors.",
            "Decides whether the statistics report and the distribution report show zero rates or discount "
            "factors for the yield curves. Zero rates are easier to compare between dates.",
            "No. The reports show discount factors.",
            FLAG),
        "outputStatistics": entry(
            "Write the statistics report",
            "A flag. Y writes the report.",
            "Decides whether the analytic writes a report with statistics of the generated scenarios for each "
            "risk factor and date: the minimum, the mean, the maximum, the standard deviation, the skewness "
            "and the kurtosis.",
            "Yes.",
            FLAG),
        "outputDistributions": entry(
            "Write the distribution report",
            "A flag. Y writes the report.",
            "Decides whether the analytic writes a report with the distribution histogram of the generated "
            "scenarios for each risk factor and date. The parameter Distribution buckets sets its resolution.",
            "Yes.",
            FLAG),
        "scenarioPrecision": entry(
            "Scenario precision",
            "A whole number of decimal places.",
            "Sets the number of decimal places of the numbers in the scenario output. A higher number shows "
            "more of each value and makes the file larger.",
            "8",
            COUNT),
    },
    "amc": {
        "active": entry(
            "AMC block active",
            "A flag. ORE ignores the amc block.",
            "ORE has no analytic of the type amc and does not read this block. Example files of an older layout "
            "carry it. The switch for American Monte Carlo is the parameter AMC in the simulation analytic.",
            "None. " + IGNORED,
            FLAG),
        "pricingEnginesFile": entry(
            "Pricing engines file in the amc block",
            "A file name. ORE ignores the amc block.",
            "ORE has no analytic of the type amc and does not read this block. The pricing engines for American "
            "Monte Carlo are set in the simulation analytic, with the parameter Pricing engines file for AMC.",
            "None. " + IGNORED,
            INPUT_FILE),
        "amcTradeTypes": entry(
            "Trade types in the amc block",
            "A comma-separated list of trade types. ORE ignores the amc block.",
            "ORE has no analytic of the type amc and does not read this block. The trade types that American "
            "Monte Carlo covers are set in the simulation analytic, with the parameter AMC trade types.",
            "None. " + IGNORED,
            "A comma-separated list of trade type names."),
    },
}
