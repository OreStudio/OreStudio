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
    "analytic needs only the part under Market. For example, simulation.xml."
)
ENGINES = (
    "Names the XML file that sets the pricing engines for the analytic. When it is missing, ORE uses the "
    "pricing engines file of the Setup block. For example, pricingengine.xml."
)


def shared(kind):
    """Texts that several of the analytics share, worded for one analytic."""
    return {
        "active": entry(
            f"Run the {kind} analytic",
            "A flag. Y runs the analytic.",
            f"Switches the {kind} analytic on or off. Use N to keep the entry in the document without "
            "running it.",
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
            "and sizes, and the par conversion. For example, sensitivity.xml.",
            "None.",
            INPUT_FILE),
        "stressConfigFile": entry(
            "Stress configuration file",
            "A file name.",
            "Names the XML file with the stress scenarios. The same file also decides whether the stress is in "
            "the raw domain or in the par domain. For example, stresstest.xml.",
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
        "types and sizes, and the par conversion. For example, sensitivity.xml.",
        "None. The analytic needs this file.",
        INPUT_FILE),
    "scenarioOutputFile": entry(
        "Scenario output file",
        "A file name.",
        "Names the file with the result of each scenario: the NPV of the base scenario, the NPV of the shifted "
        "scenario and their difference.",
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
        "Sets the smallest absolute value that the analytic writes. It drops every finite difference below "
        "the number from the output files. For example, 0.000001.",
        "0.000001",
        REAL),
    "outputPrecision": entry(
        "Sensitivity output precision",
        "A whole number of decimal places.",
        "Sets the number of decimal places of the sensitivities in the output files. A higher number shows "
        "more of each value and makes the files larger.",
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
        "sensitivity analysis. The sensitivity configuration file must have par conversion sections.",
        "No.",
        FLAG),
    "parSensitivityOutputFile": entry(
        "Par sensitivity output file",
        "A file name.",
        "Names the file with the par sensitivities. It has an effect only when Par sensitivities is on.",
        "par_sensitivity.csv",
        FILE),
    "outputJacobi": entry(
        "Write the Jacobi matrices",
        "A flag. Y writes the matrices.",
        "Decides whether ORE writes the Jacobi matrix and its inverse, which convert between raw and par "
        "sensitivities, to files.",
        "No.",
        FLAG),
    "jacobiOutputFile": entry(
        "Jacobi matrix file",
        "A file name.",
        "Names the file for the Jacobi matrix. It has an effect only when Write the Jacobi matrices is on.",
        "jacobi.csv",
        FILE),
    "jacobiInverseOutputFile": entry(
        "Inverse Jacobi matrix file",
        "A file name.",
        "Names the file for the inverse Jacobi matrix. It has an effect only when Write the Jacobi matrices "
        "is on.",
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
        "that the raw and the par sensitivities refer to the same points.",
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
        "Sets the length of the time step for the theta. For example, 1D moves the valuation date by one day. "
        "ORE reads it only when Compute theta is on.",
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
        "par shifts. Without it ORE supports no par stress test.",
        "None. Par stress tests are not supported.",
        INPUT_FILE),
    "scenarioFile": entry(
        "Scenario input file",
        "A file name.",
        "Names a file with market scenarios that ORE reads as stress scenarios. For example, "
        "StressScenarios/stress_scenarios.csv.",
        "None.",
        INPUT_FILE),
    "scenarioOutputFile": entry(
        "Stress result file",
        "A file name.",
        "Names the file with the result of each stress scenario: the NPV of the base scenario, the NPV of "
        "the stressed scenario and their difference. For example, stresstest.csv.",
        "stress.csv",
        FILE),
    "outputThreshold": entry(
        "Output threshold",
        "A number.",
        "Sets the smallest absolute NPV change that the analytic writes. It drops every change below the "
        "number from the output. For example, 0.000001.",
        "0. The analytic writes every change.",
        REAL),
    "precision": entry(
        "Output precision",
        "A whole number of decimal places.",
        "Sets the number of decimal places of the numbers in the stress result file. For example, 6.",
        "2",
        COUNT),
    "stressZeroScenarioDataFile": entry(
        "Raw domain stress file",
        "A file name.",
        "Names the file that receives the stress test definition in the raw domain, when the stress test "
        "file is in the par domain. The converted file can be used again without a new conversion. For "
        "example, zeroStressScenarioData.xml.",
        "stress_ZeroStressData.xml. The analytic writes the file only for a stress test in the par domain.",
        FILE),
    "generateCashflows": entry(
        "Generate cash flows",
        "A flag. Y writes the cash flows under each scenario.",
        "Decides whether the analytic also writes the cash flows of the trades under each stress scenario, to "
        "the cash flow output file.",
        "No.",
        FLAG),
    "scenarioCashflowOutputFile": entry(
        "Stress cash flow file",
        "A file name.",
        "Names the file with the cash flows under each stress scenario. It has an effect only when Generate "
        "cash flows is on. For example, stresstest_flows.csv.",
        "stress_cashflows.csv",
        FILE),
    "optimiseRiskFactors": entry(
        "Optimise the risk factors",
        "A flag. Y limits the par conversion to the risk factors that matter.",
        "Decides whether the par stress conversion limits itself to the risk factors that the stress "
        "scenarios shift. It makes the conversion faster.",
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
        "analytic runs the calculation once for each stress scenario.",
        "None. The analytic needs this file.",
        INPUT_FILE),
    "calcBaseScenario": entry(
        "Compute the base scenario",
        "A flag. Y also computes the unshifted scenario.",
        "Decides whether the analytic also computes the sensitivities of the unshifted base scenario, next to "
        "those of the stress scenarios.",
        "No.",
        FLAG),
    "scenarioOutputFile": entry(
        "Stressed sensitivity result file",
        "A file name.",
        "Names the file with the result of the stressed sensitivity calculation.",
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
        "conversion needs. For example, sensitivity.xml.",
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
        "Names the file that receives the stress test definition in the raw domain. For example, results.xml.",
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
        "Names the XML file with the sensitivity setup, in particular the par conversion sections. For "
        "example, sensitivity.xml.",
        "None. The analytic needs this file.",
        INPUT_FILE),
    "parShiftsFile": entry(
        "Par shifts file",
        "A file name.",
        "Names the file that receives the par rate shifts that the conversion finds. For example, "
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
        "Names the XML file with the sensitivity setup, in particular the par conversion sections. For "
        "example, sensitivity.xml.",
        "None. The analytic needs this file.",
        INPUT_FILE),
    "sensitivityInputFile": entry(
        "Raw sensitivities file",
        "A file name.",
        "Names the CSV file with the raw (zero) sensitivities to convert. The file needs six columns, and the "
        "column name parameters set their names. For example, sensitivity.csv.",
        "None. The analytic needs this file.",
        INPUT_FILE),
    "outputThreshold": entry(
        "Output threshold",
        "A number.",
        "Sets the smallest absolute value that the analytic writes. It drops every par sensitivity below the "
        "number from the output. For example, 0.000001.",
        "0.000001",
        REAL),
    "outputFile": entry(
        "Par sensitivities file",
        "A file name.",
        "Names the file that receives the par sensitivities. For example, parconversion_sensitivity.csv.",
        "parConversionSensitivity.csv",
        FILE),
    "outputJacobi": entry(
        "Write the Jacobi matrices",
        "A flag. Y writes the matrices.",
        "Decides whether the analytic writes the Jacobi matrix and its inverse to files.",
        "No.",
        FLAG),
    "jacobiOutputFile": entry(
        "Jacobi matrix file",
        "A file name.",
        "Names the file for the Jacobi matrix. It has an effect only when Write the Jacobi matrices is on.",
        "parConversionJacobi.csv",
        FILE),
    "jacobiInverseOutputFile": entry(
        "Inverse Jacobi matrix file",
        "A file name.",
        "Names the file for the inverse Jacobi matrix. It has an effect only when Write the Jacobi matrices "
        "is on.",
        "parConversionJacobi_inverse.csv",
        FILE),
    "idColumn": entry(
        "Id column",
        "A column name.",
        "Names the column of the input file with the unique identifier of the trade, netting set or "
        "portfolio.",
        "TradeId",
        "A column name."),
    "riskFactorColumn": entry(
        "Risk factor column",
        "A column name.",
        "Names the column of the input file with the identifier of the raw sensitivity. The name must follow "
        "the ORE naming convention, for example DiscountCurve/EUR/5/1Y.",
        "Factor_1",
        "A column name."),
    "deltaColumn": entry(
        "Sensitivity column",
        "A column name.",
        "Names the column of the input file with the raw sensitivity of the trade, netting set or portfolio "
        "to the risk factor.",
        "Delta",
        "A column name."),
    "currencyColumn": entry(
        "Currency column",
        "A column name.",
        "Names the column of the input file with the currency of the raw sensitivity. It must equal the base "
        "currency of the simulation settings.",
        "Currency",
        "A column name."),
    "shiftSizeColumn": entry(
        "Shift size column",
        "A column name.",
        "Names the column of the input file with the shift size that ORE applied to compute the raw "
        "sensitivity. It must agree with the sensitivity configuration.",
        "ShiftSize_1",
        "A column name."),
    "baseNpvColumn": entry(
        "Base NPV column",
        "A column name.",
        "Names the column of the input file with the base NPV of the trade, netting set or portfolio.",
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
