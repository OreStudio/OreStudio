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
"""The sensitivity, stress and par conversion analytics."""

from ._entry import COUNT, FILE, FLAG, INPUT_FILE, REAL, entry

IGNORED = "ORE ignores the parameter, so a value has no effect."
SIMMARKET = (
    "Names the XML file with the simulation market under which the analytic computes its results. The "
    "simulation market lists the risk factors that ORE can shift, for example the discount curves, the FX spots "
    "and the volatility surfaces, and the tenors of each. The analytic needs only the part under Market and "
    "ignores the model and the scenario generator. A risk factor that this file leaves out cannot be shifted. "
    "For example, simulation.xml."
)
ENGINES = (
    "Names the XML file that sets the pricing engines for the analytic. The file says which model and which "
    "engine price each product, so a change here changes the NPVs behind the results. When it is missing, ORE "
    "uses the pricing engines file of the Setup block. Use it to run the analytic with other engines than the "
    "rest of the run. For example, pricingengine.xml."
)
WHAT = {
    "sensitivity": "it shifts each risk factor of the sensitivity configuration file in turn, revalues the portfolio and writes the change of the NPV for each trade",
    "stress": "it revalues the portfolio under each scenario of the stress configuration file and writes the change of the NPV for each trade",
    "stressed sensitivity": "it computes the sensitivities of the portfolio under each stress scenario, so you see how a stress changes the risk",
    "par stress conversion": "it converts a stress test given as shifts of par rates into the equivalent stress test on the zero rates of the model",
    "zero to par shift": "it converts the zero rate shifts of a stress test into the par rate shifts that give the same market, and writes them to a file",
    "par conversion": "it converts raw (zero) sensitivities that you supply into sensitivities to par rates",
}


def shared(kind):
    """Texts that several of the analytics share, worded for one analytic."""
    return {
        "active": entry(
            f"Run the {kind} analytic",
            "A flag. Y runs the analytic.",
            f"Switches the {kind} analytic on or off. When it is on, {WHAT[kind]}. Use N to keep the entry "
            "in the document without running it.",
            "No. The analytic does not run.",
            FLAG, required=True),
        "marketConfigFile": entry(
            "Simulation market file",
            "A file name.",
            SIMMARKET,
            "None. The analytic needs this file.",
            INPUT_FILE),
        "pricingEnginesFile": entry(
            "Pricing engines file",
            "A file name.",
            ENGINES,
            "The pricing engines file of the Setup block.",
            INPUT_FILE),
        "sensitivityConfigFile": entry(
            "Sensitivity configuration file",
            "A file name.",
            "Names the XML file that sets the sensitivity calculation: the risk factors, the shift types "
            "and sizes, and the par conversion. A risk factor that the file does not list gets no "
            "sensitivity, and the shift size decides the finite difference. The par conversion sections tell "
            "ORE which instruments to hedge with. For example, sensitivity.xml.",
            "None.",
            INPUT_FILE),
        "stressConfigFile": entry(
            "Stress configuration file",
            "A file name.",
            "Names the XML file with the stress scenarios. Each scenario lists the shifts of the risk factors, "
            "for example a parallel move of a yield curve by 100 basis points. The same file also decides "
            "whether the stress is in the raw domain, which shifts the zero rates, or in the par domain, which "
            "shifts the par rates. For example, stresstest.xml.",
            "None. The analytic needs this file.",
            INPUT_FILE),
    }


def conversion_bounds(kind):
    """The bounds and the accuracy of the par stress conversion, worded for one analytic."""
    note = (
        f" ORE reads the value for the {kind} analytic. The conversion of release 1.8.17.0 uses its own "
        "fixed bounds and does not take the value."
    )
    return {
        "lowerBoundCapVols": entry(
            "Lower bound of cap volatilities",
            "A number.",
            "Is meant to set the lowest cap and floor volatility that the par to raw conversion tries. The "
            "conversion uses the bound 1e-8." + note,
            "1e-8",
            REAL),
        "upperBoundCapVols": entry(
            "Upper bound of cap volatilities",
            "A number.",
            "Is meant to set the highest cap and floor volatility that the par to raw conversion tries, as a "
            "multiple of the base volatility. The conversion uses the bound 10." + note,
            "10",
            REAL),
        "lowerBoundDiscountFactors": entry(
            "Lower bound of discount factors",
            "A number.",
            "Is meant to set the lowest discount factor that the par to raw conversion tries. The "
            "conversion uses the bound 1e-8." + note,
            "1e-8",
            REAL),
        "upperBoundDiscountFactors": entry(
            "Upper bound of discount factors",
            "A number.",
            "Is meant to set the highest discount factor that the par to raw conversion tries. The "
            "conversion uses the bound 10." + note,
            "10",
            REAL),
        "lowerBoundSurvivalProb": entry(
            "Lower bound of survival probabilities",
            "A number.",
            "Is meant to set the lowest survival probability that the par to raw conversion tries. The "
            "conversion treats survival probabilities like discount factors and uses the bound 1e-8." + note,
            "1e-8",
            REAL),
        "upperBoundSurvivalProb": entry(
            "Upper bound of survival probabilities",
            "A number.",
            "Is meant to set the highest survival probability that the par to raw conversion tries. The "
            "conversion treats survival probabilities like discount factors and uses the bound 10." + note,
            "10",
            REAL),
        "accuracy": entry(
            "Accuracy of the conversion",
            "A number.",
            "Is meant to set the accuracy of the root search that turns a par shift into a raw shift. The "
            "conversion uses the accuracy 1e-8." + note,
            "1e-8",
            REAL),
    }


SENSITIVITY = shared("sensitivity")
SENSITIVITY.pop("stressConfigFile")
SENSITIVITY.update({
    "marketConfigFile": entry(
        "Simulation market file",
        "A file name.",
        SIMMARKET,
        "None. The analytic needs this file.",
        INPUT_FILE),
    "sensitivityConfigFile": entry(
        "Sensitivity configuration file",
        "A file name.",
        "Names the XML file that sets the sensitivity calculation: which risk factors ORE bumps, the shift "
        "types and sizes, and the par conversion. A risk factor that the file does not list gets no "
        "sensitivity. The file is the main input of the analytic, and the shift sizes decide the finite "
        "differences. For example, sensitivity.xml.",
        "None. The analytic needs this file.",
        INPUT_FILE),
    "scenarioOutputFile": entry(
        "Scenario output file",
        "A file name.",
        "Names the file with the result of each scenario: the NPV of the base scenario, the NPV of the shifted "
        "scenario and their difference. Use it to check a sensitivity by hand, because each row is one bump of "
        "one risk factor. The default is sensitivity_scenario.csv, written in the output path.",
        "sensitivity_scenario.csv",
        FILE),
    "sensitivityOutputFile": entry(
        "Sensitivity output file",
        "A file name.",
        "Names the file with the sensitivities: the base NPV, the shift size with its risk factor, and the "
        "first and the pure second order finite differences. It also holds a second set with the mixed second "
        "order finite differences of the cross gamma.",
        "sensitivity.csv",
        FILE),
    "outputSensitivityThreshold": entry(
        "Sensitivity output threshold",
        "A number.",
        "Sets the smallest absolute value that the analytic writes. It drops every finite difference below the "
        "number from the output files, so a bump that moves no trade leaves no row. A larger value gives "
        "smaller files and hides numerical noise. The default is 0.000001. For example, 0.01 keeps only the "
        "sensitivities of one cent or more.",
        "0.000001",
        REAL),
    "outputPrecision": entry(
        "Sensitivity output precision",
        "A whole number of decimal places.",
        "Sets the number of decimal places of the sensitivities in the output files. A higher number shows more "
        "of each value and makes the files larger. The default is 2. For example, 6 shows a small sensitivity "
        "such as 0.001234 in full. Raise it when you compare two runs to find a small difference.",
        "2",
        COUNT),
    "recalibrateModels": entry(
        "Recalibrate the models",
        "A flag. Y recalibrates after each shift.",
        "Decides whether ORE recalibrates the pricing models after it shifts a relevant term structure. "
        "Recalibration gives sensitivities that include the model reaction, and it takes more time. When it is "
        "N, the models keep their calibration of the base scenario.",
        "Yes.",
        FLAG),
    "parSensitivity": entry(
        "Par sensitivities",
        "A flag. Y also computes par sensitivities.",
        "Decides whether ORE converts the raw sensitivities into sensitivities to par rates after the raw "
        "sensitivity analysis. Raw sensitivities are to the zero rates of the curves, and par sensitivities are "
        "to the quotes that a desk trades, such as the par rate of a swap, so they are the numbers a hedge "
        "uses. The sensitivity configuration file must have par conversion sections. The default is No.",
        "No.",
        FLAG),
    "parSensitivityOutputFile": entry(
        "Par sensitivity output file",
        "A file name.",
        "Names the file with the par sensitivities. It has an effect only when Par sensitivities is on. The "
        "default is par_sensitivity.csv, written in the output path. Change it when one run writes more than "
        "one set of reports.",
        "par_sensitivity.csv",
        FILE),
    "outputJacobi": entry(
        "Write the Jacobi matrices",
        "A flag. Y writes the matrices.",
        "Decides whether ORE writes the Jacobi matrix and its inverse, which convert between raw and par "
        "sensitivities, to files. Use the files to check the par conversion, or to convert other raw "
        "sensitivities yourself. The two file name parameters set the names. The default is No.",
        "No.",
        FLAG),
    "jacobiOutputFile": entry(
        "Jacobi matrix file",
        "A file name.",
        "Names the file for the Jacobi matrix, which converts a raw shift into the par shifts of the quotes. It "
        "has an effect only when Write the Jacobi matrices is on. The default is jacobi.csv, written in the "
        "output path.",
        "jacobi.csv",
        FILE),
    "jacobiInverseOutputFile": entry(
        "Inverse Jacobi matrix file",
        "A file name.",
        "Names the file for the inverse Jacobi matrix, which converts a par shift into the raw shifts of the "
        "market. It has an effect only when Write the Jacobi matrices is on. The default is jacobi_inverse.csv, "
        "written in the output path.",
        "jacobi_inverse.csv",
        FILE),
    "optimiseRiskFactors": entry(
        "Optimise the risk factors",
        "A flag. Y limits the par sensitivities to the risk factors that matter.",
        "Decides whether the par sensitivity calculation uses only the risk factors that the raw sensitivity "
        "analysis found relevant for the portfolio. It makes the par conversion faster and smaller. It has an "
        "effect only when Par sensitivities is on.",
        "No.",
        FLAG),
    "alignPillars": entry(
        "Align the pillars",
        "A flag. Y aligns the pillars with the par instruments.",
        "Decides whether ORE aligns the pillars of the simulation market with those of the par instruments, so "
        "that the raw and the par sensitivities refer to the same points. Without alignment a par instrument "
        "can fall between two pillars, and the conversion then interpolates. The default follows Par "
        "sensitivities: ORE aligns the pillars when par sensitivities are on.",
        "The value of Par sensitivities. Pillars are aligned when par sensitivities are on.",
        FLAG),
    "laxFxConversion": entry(
        "Lax FX conversion",
        "A flag. Y uses the spot quote of the currency pair directly.",
        "Decides how ORE finds the FX rate that converts a sensitivity into the base currency. With Y, ORE uses "
        "the spot quote of the pair directly. With N, ORE uses the stricter rate lookup. Switch it on if "
        "the strict lookup fails for a currency pair that the market does not quote.",
        "No.",
        FLAG),
    "decomposeIndexSensitivities": entry(
        "Decompose index sensitivities",
        "A flag. Y splits index sensitivities into their constituents.",
        "Decides whether ORE decomposes the sensitivities of credit, equity and commodity indices into the "
        "sensitivities of the constituents. Credit index decomposition also needs SensitivityDecomposition in "
        "the pricing engine settings. Equity and commodity decomposition need index_decomposition in the "
        "additional fields of the trade, and reference data for the indices.",
        "No.",
        FLAG),
    "computeTheta": entry(
        "Compute theta",
        "A flag. Y computes the theta.",
        "Decides whether the sensitivity calculation also computes the theta, the change of the NPV when the "
        "valuation date moves. ORE reads the parameter from the sensitivity block, the simm block or the crif "
        "block, in this order, and one value serves all three analytics.",
        "No.",
        FLAG),
    "thetaPeriod": entry(
        "Theta period",
        "A period, for example 1D.",
        "Sets the length of the time step for the theta. The theta is the change of the NPV when the valuation "
        "date moves by this period, with the market unchanged. For example, 1D moves the valuation date by one "
        "day, and 1W by one week. ORE reads it only when Compute theta is on. The default is 1D.",
        "1D",
        "A period such as 1D or 1W."),
    "crossGammaOutputFile": entry(
        "Cross gamma output file",
        "A file name. ORE no longer reads it.",
        "Named the file for the cross gamma. ORE stopped reading it in 1.8.4.0. The cross gamma is now part of the "
        "sensitivity output file.",
        "None. " + IGNORED,
        FILE),
    "applySimmExemptions": entry(
        "Apply SIMM exemptions",
        "A flag. ORE ignores it in the sensitivity block.",
        "ORE reads this parameter in the crif analytic only. A value in the sensitivity block has no effect.",
        "No. " + IGNORED,
        FLAG),
    "outputProductClassMapping": entry(
        "Output the product class mapping",
        "A flag. ORE ignores it.",
        "ORE does not read this parameter in any release. Example files carry it.",
        "No. " + IGNORED,
        FLAG),
    "productClassMappingOutputFile": entry(
        "Product class mapping output file",
        "A file name. ORE ignores it.",
        "ORE does not read this parameter in any release. Example files carry it.",
        "None. " + IGNORED,
        FILE),
    "parRateSensitivityOutputFile": entry(
        "Par rate sensitivity output file",
        "A file name. ORE ignores it.",
        "ORE does not read this parameter in any release. Example files carry it. Use the par sensitivity "
        "output file.",
        "None. " + IGNORED,
        FILE),
})

STRESS = shared("stress")
STRESS.update({
    "marketConfigFile": entry(
        "Simulation market file",
        "A file name.",
        SIMMARKET + " The analytic replaces the market of the valuation date with this simulation market "
        "and applies the stress scenarios to it.",
        "None. The analytic needs this file.",
        INPUT_FILE),
    "stressConfigFile": entry(
        "Stress configuration file",
        "A file name.",
        "Names the XML file with the stress scenarios. The same file also decides whether the stress is in "
        "the raw domain, as shifts of the zero rates, or in the par domain, as shifts of the par rates. For "
        "example, stresstest.xml.",
        "None. The analytic needs this file.",
        INPUT_FILE),
    "sensitivityConfigFile": entry(
        "Sensitivity configuration file",
        "A file name.",
        "Names the XML file with the sensitivity setup, which a par stress test needs for the conversion of the "
        "par shifts. The conversion turns a shift of par rates into the equivalent shift of the model rates. "
        "Without it ORE supports no par stress test and the stress file must be in the raw domain.",
        "None. Par stress tests are not supported.",
        INPUT_FILE),
    "scenarioFile": entry(
        "Scenario input file",
        "A file name.",
        "Names a file with market scenarios that ORE reads as stress scenarios, for example scenarios that an "
        "earlier run wrote or a historical event that you saved. For example, "
        "StressScenarios/stress_scenarios.csv. The scenarios come in addition to those of the stress "
        "configuration file.",
        "None.",
        INPUT_FILE),
    "scenarioOutputFile": entry(
        "Stress result file",
        "A file name.",
        "Names the file with the result of each stress scenario: the NPV of the base scenario, the NPV of the "
        "stressed scenario and their difference. Each row is one trade under one scenario. The default is "
        "stress.csv, written in the output path. For example, stresstest.csv.",
        "stress.csv",
        FILE),
    "outputThreshold": entry(
        "Output threshold",
        "A number.",
        "Sets the smallest absolute NPV change that the analytic writes. It drops every change below the number "
        "from the output, so a scenario that moves a trade by less than the number leaves no row. The default "
        "is 0, so the analytic writes every change. For example, 0.000001.",
        "0. The analytic writes every change.",
        REAL),
    "precision": entry(
        "Output precision",
        "A whole number of decimal places.",
        "Sets the number of decimal places of the numbers in the stress result file. A higher number shows more "
        "of each value and makes the file larger. The default is 2. For example, 6 shows a small change such as "
        "0.001234 in full.",
        "2",
        COUNT),
    "stressZeroScenarioDataFile": entry(
        "Raw domain stress file",
        "A file name.",
        "Names the file that receives the stress test definition in the raw domain, when the stress test file "
        "is in the par domain. The converted file can be used again without a new conversion, which saves the "
        "cost of the par conversion. For example, zeroStressScenarioData.xml.",
        "stress_ZeroStressData.xml. The analytic writes the file only for a stress test in the par domain.",
        FILE),
    "generateCashflows": entry(
        "Generate cash flows",
        "A flag. Y writes the cash flows under each scenario.",
        "Decides whether the analytic also writes the cash flows of the trades under each stress scenario, to "
        "the cash flow output file. Use it to see how a scenario changes the amounts and the dates of the "
        "flows, for example when a floating rate fixes higher. The file is large for a big portfolio. The "
        "default is No.",
        "No.",
        FLAG),
    "scenarioCashflowOutputFile": entry(
        "Stress cash flow file",
        "A file name.",
        "Names the file with the cash flows under each stress scenario. It has an effect only when Generate "
        "cash flows is on. The default is stress_cashflows.csv, written in the output path. For example, "
        "stresstest_flows.csv.",
        "stress_cashflows.csv",
        FILE),
    "optimiseRiskFactors": entry(
        "Optimise the risk factors",
        "A flag. Y limits the par conversion to the risk factors that matter.",
        "Decides whether the par stress conversion limits itself to the risk factors that the stress scenarios "
        "shift. A par stress test converts par shifts into raw shifts, and the conversion handles every risk "
        "factor of the market unless this is on. It makes the conversion faster. The default is No.",
        "No.",
        FLAG),
})
STRESS.update(conversion_bounds("stress"))

SENSITIVITY_STRESS = shared("stressed sensitivity")
SENSITIVITY_STRESS.pop("pricingEnginesFile")
SENSITIVITY_STRESS.update({
    "marketConfigFile": entry(
        "Simulation market file",
        "A file name.",
        SIMMARKET + " For each stress scenario the analytic replaces the market of the valuation date with this "
        "simulation market, shifted by the scenario, and runs the sensitivity calculation on it.",
        "None. The analytic needs this file.",
        INPUT_FILE),
    "sensitivityConfigFile": entry(
        "Sensitivity configuration file",
        "A file name.",
        "Names the XML file that sets the sensitivity calculation, as for the sensitivity analytic. The "
        "analytic runs the calculation once for each stress scenario, so the cost is the cost of one "
        "sensitivity run times the number of scenarios. For example, sensitivity.xml.",
        "None. The analytic needs this file.",
        INPUT_FILE),
    "calcBaseScenario": entry(
        "Compute the base scenario",
        "A flag. Y also computes the unshifted scenario.",
        "Decides whether the analytic also computes the sensitivities of the unshifted base scenario, next to "
        "those of the stress scenarios. Use it to compare the sensitivities before and after a stress in one "
        "file. The default is No.",
        "No.",
        FLAG),
    "scenarioOutputFile": entry(
        "Stressed sensitivity result file",
        "A file name.",
        "Names the file with the result of the stressed sensitivity calculation: the sensitivities of each "
        "trade under each stress scenario. The default is sensitivity_stress.csv, written in the output path.",
        "sensitivity_stress.csv",
        FILE),
})
SENSITIVITY_STRESS["active"] = entry(
    "Run the stressed sensitivity analytic",
    "A flag. Y runs the analytic.",
    "Switches the stressed sensitivity analytic on or off. It combines the stress test and the sensitivity "
    "analysis. For each stress scenario, it replaces the market of the valuation date with the simulation "
    "market shifted by the scenario, and runs the sensitivity calculation on it.",
    "No. The analytic does not run.",
    FLAG, required=True)

PAR_STRESS = shared("par stress conversion")
PAR_STRESS["active"] = entry(
    "Run the par stress conversion",
    "A flag. Y runs the analytic.",
    "Switches the par stress conversion on or off. The analytic does not run a stress test. It turns a "
    "stress test definition in the par domain into one in the raw domain, so that you can apply the result "
    "again and again.",
    "No. The analytic does not run.",
    FLAG, required=True)
PAR_STRESS.update({
    "marketConfigFile": entry("Simulation market file", "A file name.", SIMMARKET, "None. The analytic needs this file.", INPUT_FILE),
    "sensitivityConfigFile": entry(
        "Sensitivity configuration file",
        "A file name.",
        "Names the XML file with the sensitivity setup, in particular the par conversion sections that the "
        "conversion needs. The sections list the par instruments that define each curve, so the conversion can "
        "map a par shift to the zero rates. For example, sensitivity.xml.",
        "None. The analytic needs this file.",
        INPUT_FILE),
    "scenarioOutputFile": entry(
        "Scenario output file",
        "A file name. ORE ignores it.",
        "ORE does not read this parameter for the par stress conversion. Example files carry it.",
        "None. " + IGNORED,
        FILE),
    "outputThreshold": entry(
        "Output threshold",
        "A number. ORE ignores it.",
        "ORE does not read this parameter for the par stress conversion. Example files carry it.",
        "None. " + IGNORED,
        REAL),
    "stressZeroScenarioDataFile": entry(
        "Raw domain stress file",
        "A file name.",
        "Names the file that receives the stress test definition in the raw domain, that is the result of the "
        "conversion. A later stress analytic can read the file and run the stress test in the zero domain. The "
        "default is parStress_ZeroStressData.xml, written in the output path. For example, results.xml.",
        "parStress_ZeroStressData.xml",
        FILE),
})
PAR_STRESS.update(conversion_bounds("par stress conversion"))

ZERO_TO_PAR_SHIFT = shared("zero to par shift")
ZERO_TO_PAR_SHIFT["active"] = entry(
    "Run the zero to par shift conversion",
    "A flag. Y runs the analytic.",
    "Switches the zero to par shift conversion on or off. The analytic converts the zero rate shifts of a "
    "stress test into the par rate shifts that give the same market, and writes them to a file.",
    "No. The analytic does not run.",
    FLAG, required=True)
ZERO_TO_PAR_SHIFT.update({
    "marketConfigFile": entry("Simulation market file", "A file name.", SIMMARKET, "None. The analytic needs this file.", INPUT_FILE),
    "sensitivityConfigFile": entry(
        "Sensitivity configuration file",
        "A file name.",
        "Names the XML file with the sensitivity setup, in particular the par conversion sections. The sections "
        "list the par instruments that define each curve, so the conversion can map a zero shift to par shifts. "
        "For example, sensitivity.xml.",
        "None. The analytic needs this file.",
        INPUT_FILE),
    "parShiftsFile": entry(
        "Par shifts file",
        "A file name.",
        "Names the file that receives the par rate shifts that the conversion finds. Each row is a par "
        "instrument with its shift. The default is parshifts.csv, written in the output path. For example, "
        "parshifts.csv.",
        "parshifts.csv",
        FILE),
    "scenarioOutputFile": entry(
        "Scenario output file",
        "A file name. ORE ignores it.",
        "ORE does not read this parameter for the zero to par shift conversion. Example files carry it. The "
        "par shifts file names the output.",
        "None. " + IGNORED,
        FILE),
})

ZERO_TO_PAR_SENSI = shared("par conversion")
ZERO_TO_PAR_SENSI.pop("stressConfigFile")
ZERO_TO_PAR_SENSI["active"] = entry(
    "Run the par conversion of sensitivities",
    "A flag. Y runs the analytic.",
    "Switches the conversion of raw sensitivities into par sensitivities on or off, as a separate step. "
    "The raw sensitivities come from a file. The conversion is the same as the embedded par conversion of "
    "the sensitivity analytic.",
    "No. The analytic does not run.",
    FLAG, required=True)
ZERO_TO_PAR_SENSI.update({
    "marketConfigFile": entry("Simulation market file", "A file name.", SIMMARKET, "None. The analytic needs this file.", INPUT_FILE),
    "sensitivityConfigFile": entry(
        "Sensitivity configuration file",
        "A file name.",
        "Names the XML file with the sensitivity setup, in particular the par conversion sections. The sections "
        "list the par instruments that define each curve, so the conversion can map a raw sensitivity to par "
        "sensitivities. For example, sensitivity.xml.",
        "None. The analytic needs this file.",
        INPUT_FILE),
    "sensitivityInputFile": entry(
        "Raw sensitivities file",
        "A file name.",
        "Names the CSV file with the raw (zero) sensitivities to convert, for example the result of an earlier "
        "sensitivity run or the sensitivities of another system. The file needs six columns, and the column "
        "name parameters set their names. For example, sensitivity.csv. The analytic needs this file.",
        "None. The analytic needs this file.",
        INPUT_FILE),
    "outputThreshold": entry(
        "Output threshold",
        "A number.",
        "Sets the smallest absolute value that the analytic writes. It drops every par sensitivity below the "
        "number from the output. A larger value gives a smaller file and hides numerical noise. The default is "
        "0.000001. For example, 0.01.",
        "0.000001",
        REAL),
    "outputFile": entry(
        "Par sensitivities file",
        "A file name.",
        "Names the file that receives the par sensitivities, one row for each trade, netting set or portfolio "
        "and each par instrument. The default is parConversionSensitivity.csv, written in the output path. For "
        "example, parconversion_sensitivity.csv.",
        "parConversionSensitivity.csv",
        FILE),
    "outputJacobi": entry(
        "Write the Jacobi matrices",
        "A flag. Y writes the matrices.",
        "Decides whether the analytic writes the Jacobi matrix and its inverse to files. The matrices convert "
        "between raw and par sensitivities, so you can check the conversion or use them elsewhere. The default "
        "is No.",
        "No.",
        FLAG),
    "jacobiOutputFile": entry(
        "Jacobi matrix file",
        "A file name.",
        "Names the file for the Jacobi matrix, which converts a raw shift into the par shifts of the quotes. It "
        "has an effect only when Write the Jacobi matrices is on. The default is parConversionJacobi.csv, "
        "written in the output path.",
        "parConversionJacobi.csv",
        FILE),
    "jacobiInverseOutputFile": entry(
        "Inverse Jacobi matrix file",
        "A file name.",
        "Names the file for the inverse Jacobi matrix, which converts a par shift into the raw shifts of the "
        "market. It has an effect only when Write the Jacobi matrices is on. The default is "
        "parConversionJacobi_inverse.csv, written in the output path.",
        "parConversionJacobi_inverse.csv",
        FILE),
    "idColumn": entry(
        "Id column",
        "A column name.",
        "Names the column of the input file with the unique identifier of the trade, netting set or portfolio. "
        "Change it when the file comes from a system that calls the column something else. The default is "
        "TradeId. For example, NettingSetId for a file of netting set sensitivities.",
        "TradeId",
        "A column name."),
    "riskFactorColumn": entry(
        "Risk factor column",
        "A column name.",
        "Names the column of the input file with the identifier of the raw sensitivity. The name must follow "
        "the ORE naming convention, for example DiscountCurve/EUR/5/1Y. Change the parameter when the file "
        "calls the column something else. The default is Factor_1.",
        "Factor_1",
        "A column name."),
    "deltaColumn": entry(
        "Sensitivity column",
        "A column name.",
        "Names the column of the input file with the raw sensitivity of the trade, netting set or portfolio to "
        "the risk factor. Change it when the file calls the column something else. The default is Delta.",
        "Delta",
        "A column name."),
    "currencyColumn": entry(
        "Currency column",
        "A column name.",
        "Names the column of the input file with the currency of the raw sensitivity. The currency must equal "
        "the base currency of the simulation settings. Change the parameter when the file calls the column "
        "something else. The default is Currency.",
        "Currency",
        "A column name."),
    "shiftSizeColumn": entry(
        "Shift size column",
        "A column name.",
        "Names the column of the input file with the shift size that ORE applied to compute the raw "
        "sensitivity. The value must agree with the sensitivity configuration, because the conversion rescales "
        "by it. Change the parameter when the file calls the column something else. The default is ShiftSize_1.",
        "ShiftSize_1",
        "A column name."),
    "baseNpvColumn": entry(
        "Base NPV column",
        "A column name.",
        "Names the column of the input file with the base NPV of the trade, netting set or portfolio. Change it "
        "when the file calls the column something else. The default is Base NPV.",
        "Base NPV",
        "A column name."),
})

ENTRIES = {
    "sensitivity": SENSITIVITY,
    "stress": STRESS,
    "sensitivityStress": SENSITIVITY_STRESS,
    "parStressConversion": PAR_STRESS,
    "zeroToParShift": ZERO_TO_PAR_SHIFT,
    "zeroToParSensiConversion": ZERO_TO_PAR_SENSI,
}
