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
"""The XVA sensitivity, explain and stress analytics, and the capital analytics."""

from ._entry import COUNT, DATE, FILE, FLAG, INPUT_FILE, REAL, entry

IGNORED = "ORE ignores the parameter, so a value has no effect."
REUSE = (
    " The analytic has no settings of its own for the exposure and the XVA. It reuses those of the simulation "
    "and XVA analytics, which you can switch off."
)
MARKET = (
    "Names the XML file with the simulation market under which the analytic computes its results. The "
    "analytic needs only the part under Market. The analytic replaces the market of the valuation date in the "
    "exposure simulation with this simulation market. For example, simulation.xml."
)
SPREAD = (
    " The simulation market behaves differently from the market of the valuation date for some risk factors, "
    "for example it can use other curve tenors or only the at-the-money swaption volatilities. To reduce the "
    "difference, switch on UseSpreadedTermStructures in the scenario setup and simulate the swaption volatility."
)
NAMES = "ORE reads it from the first of the blocks crif, saccr, simm, npv and Setup that has a value."
SIM_FWD = " The SA-CVA analytic passes the value on to the simulation analytic that runs under it."

ENTRIES = {
    "xvaSensitivity": {
        "active": entry(
            "Run the XVA sensitivity analytic",
            "A flag. Y runs the analytic.",
            "Switches the XVA sensitivity analytic on or off. When it is on, the run computes the "
            "sensitivities of the value adjustments and the exposures by bump and revalue: it shifts each "
            "risk factor and runs the XVA calculation again." + REUSE,
            "No. The analytic does not run.",
            FLAG, required=True),
        "marketConfigFile": entry(
            "Simulation market file",
            "A file name.",
            MARKET + SPREAD,
            "None. The analytic needs this file.",
            INPUT_FILE),
        "sensitivityConfigFile": entry(
            "Sensitivity configuration file",
            "A file name.",
            "Names the XML file that sets the sensitivity calculation: the risk factors, the shift types and "
            "sizes, and the par conversion. The analytic uses the same file format as the sensitivity analytic. "
            "For example, sensitivity.xml.",
            "None. The analytic needs this file.",
            INPUT_FILE),
        "parSensitivity": entry(
            "Par sensitivities",
            "A flag. Y also writes par sensitivities.",
            "Decides whether the analytic converts the raw sensitivities into sensitivities to par rates and "
            "writes them in an extra report. The sensitivity configuration file must have par conversion "
            "sections.",
            "Yes.",
            FLAG),
        "outputJacobi": entry(
            "Write the Jacobi matrices",
            "A flag. Y writes the matrices.",
            "Decides whether the analytic writes the Jacobi matrix and its inverse, which convert between raw "
            "and par sensitivities, to files. The two file name parameters set the names.",
            "No.",
            FLAG),
        "outputSensitivityThreshold": entry(
            "Sensitivity output threshold",
            "A number.",
            "Sets the smallest absolute value that the analytic writes. It drops every finite difference "
            "below the number from the output files. A larger value gives smaller files. For example, 0.000001.",
            "0.000001",
            REAL),
        "outputPrecision": entry(
            "Sensitivity output precision",
            "A whole number of decimal places.",
            "Sets the number of decimal places of the numbers in the XVA sensitivity reports. A higher number "
            "shows more of each value and makes the files larger.",
            "4",
            COUNT),
        "jacobiOutputFile": entry(
            "Jacobi matrix file",
            "A file name.",
            "Names the file for the Jacobi matrix. It has an effect only when Write the Jacobi matrices is on.",
            "xva_sensi_jacobi.csv",
            FILE),
        "jacobiInverseOutputFile": entry(
            "Inverse Jacobi matrix file",
            "A file name.",
            "Names the file for the inverse Jacobi matrix. It has an effect only when Write the Jacobi "
            "matrices is on.",
            "xva_sensi_jacobi_inverse.csv",
            FILE),
    },
    "xvaExplain": {
        "active": entry(
            "Run the XVA explain analytic",
            "A flag. Y runs the analytic.",
            "Switches the XVA explain analytic on or off. When it is on, the run computes the change of the "
            "value adjustments that the market moves between two dates imply: the XVA with the market data of "
            "the second date minus the XVA with the market data of the first, both at the exposure of the "
            "first date. The output has the value adjustments under each shifted risk factor and a CVA "
            "explain report." + REUSE,
            "No. The analytic does not run.",
            FLAG, required=True),
        "marketConfigFile": entry(
            "Simulation market file",
            "A file name.",
            MARKET + SPREAD,
            "None. The analytic needs this file.",
            INPUT_FILE),
        "sensitivityConfigFile": entry(
            "Sensitivity configuration file",
            "A file name.",
            "Names the XML file that lists the risk factors to explain. For each risk factor of the file, "
            "ORE computes the change of its par rate between the first date and the second date. It builds a "
            "shift scenario from the change and computes the CVA change that the shift implies. For example, "
            "sensitivity_stress.xml.",
            "None. The analytic needs this file.",
            INPUT_FILE),
        "shiftThreshold": entry(
            "Par rate shift threshold",
            "A number.",
            "Sets the smallest par rate shift that the analytic uses. It ignores every shift below this "
            "number. For example, 1e-4 ignores shifts smaller than one basis point.",
            "0. The analytic ignores no shift.",
            REAL),
        "mporDays": entry(
            "Days to the second date",
            "A whole number of days.",
            "Sets the number of days from the valuation date to the second evaluation date. The analytic "
            "derives the second date from the valuation date plus this number of days, and rolls it to a "
            "business day with the calendar. The parameter MPoR date sets the date directly. For "
            "example, 1 explains the move of one day.",
            "10",
            COUNT),
        "mporDate": entry(
            "Second evaluation date",
            "A date.",
            "Sets the second evaluation date directly, instead of deriving it from the number of days. For "
            "example, 2026-03-02.",
            "None. ORE derives the date from the number of days.",
            DATE),
        "mporCalendar": entry(
            "Calendar of the second date",
            "A calendar name.",
            "Names the calendar that ORE uses to make sure that the second evaluation date is a business day. "
            "For example, EUR.",
            "The calendar of the base currency.",
            "A calendar name such as TARGET, EUR or UK."),
        "writeCubes": entry(
            "Write the cubes",
            "A flag. ORE ignores it in the xvaExplain block.",
            "The user guide lists this parameter for the XVA explain analytic, but ORE reads it only for the "
            "XVA stress analytic. A value here has no effect in release 1.8.17.0.",
            "No. " + IGNORED,
            FLAG),
        "stressConfigFile": entry(
            "Stress configuration file",
            "A file name. ORE ignores it in the xvaExplain block.",
            "The user guide lists this parameter for the XVA explain analytic, but the analytic reads the "
            "risk factors from the sensitivity configuration file. A value here has no effect in release 1.8.17.0.",
            "None. " + IGNORED,
            INPUT_FILE),
    },
    "xvaStress": {
        "active": entry(
            "Run the XVA stress analytic",
            "A flag. Y runs the analytic.",
            "Switches the XVA stress analytic on or off. When it is on, the run computes the value "
            "adjustments and exposures under each stress scenario and writes them next to the base results. "
            "Use it to see how the XVA reacts to a market shock." + REUSE
            + " For speed, use AMC in the simulation.",
            "No. The analytic does not run.",
            FLAG, required=True),
        "marketConfigFile": entry(
            "Simulation market file",
            "A file name.",
            MARKET + SPREAD,
            "None. The analytic needs this file.",
            INPUT_FILE),
        "stressConfigFile": entry(
            "Stress configuration file",
            "A file name.",
            "Names the XML file with the stress scenarios in the format of the NPV stress test. For example, "
            "stresstest.xml.",
            "None. The analytic needs this file.",
            INPUT_FILE),
        "sensitivityConfigFile": entry(
            "Sensitivity configuration file",
            "A file name.",
            "Names the XML file that sets the sensitivity calculation, in particular the par conversion that a "
            "par stress test needs. Without it, ORE supports no par stress test. For example, "
            "sensitivity_stress.xml.",
            "None. Par stress tests are not supported.",
            INPUT_FILE),
        "writeCubes": entry(
            "Write the cubes under each scenario",
            "A flag. Y writes the cubes.",
            "Decides whether the analytic writes the raw cube and the net cube for each stress scenario. The "
            "files can be very large.",
            "No.",
            FLAG),
        "scenarioOutputFile": entry(
            "Stress scenario output file",
            "A file name.",
            "Names the file for the report of the XVA stress scenarios.",
            "xva_stress.csv",
            FILE),
    },
    "sacva": {
        "active": entry(
            "Run the SA-CVA analytic",
            "A flag. Y runs the analytic.",
            "Switches the standardised approach for the CVA capital (SA-CVA) on or off. The calculation is based "
            "on CVA sensitivities. ORE can compute them on the fly, with the XVA sensitivity analytic and the "
            "simulation and XVA analytics under it, or read them from a file. The run stops if no "
            "sensitivity file is given and the XVA sensitivity analytic is missing.",
            "No. The analytic does not run.",
            FLAG, required=True),
        "saCvaNetSensitivitiesFile": entry(
            "Net sensitivities file",
            "A file name.",
            "Names a file with net sensitivities that ORE passes directly to the SA-CVA calculator. When it "
            "is set, ORE computes no sensitivities. For example, sacva_sensitivity.csv.",
            "None. ORE computes the sensitivities or reads the CVA sensitivities file.",
            INPUT_FILE),
        "cvaSensitivitiesFile": entry(
            "CVA sensitivities file",
            "A file name.",
            "Names a file with CVA sensitivities, the output of the XVA sensitivity analytic. ORE maps and "
            "aggregates them before the calculation. ORE reads it only when no net sensitivities file is "
            "given. For example, xva_par_sensitivity_cva.csv.",
            "None. ORE computes the sensitivities.",
            INPUT_FILE),
        "csaFile": entry(
            "Netting set definitions file",
            "A file name.",
            "Names the XML file with the netting set definitions. ORE looks for the file name in the sacva block, "
            "then in the bacva block and last in the Setup block." + SIM_FWD,
            "The netting set definitions file of the Setup block.",
            INPUT_FILE),
        "counterpartyFile": entry(
            "Counterparty file",
            "A file name.",
            "Names the XML file with the counterparty information that the SA-CVA calculator needs. ORE looks for the file name in the sacva block, then in the bacva "
            "block and last in the Setup block.",
            "The counterparty file of the Setup block.",
            INPUT_FILE),
        "simulationConfigFile": entry(
            "Simulation configuration file",
            "A file name.",
            "Names the XML file with the simulation configuration for the exposure simulation under the "
            "analytic." + SIM_FWD,
            "None.",
            INPUT_FILE),
        "scenarioGeneratorData": entry(
            "Scenario generator file",
            "A file name.",
            "Names the XML file with the scenario generator for the simulation under the analytic. Without "
            "it the simulation uses its own defaults, for example 1000 samples." + SIM_FWD,
            "None.",
            INPUT_FILE),
        "crossAssetModelData": entry(
            "Cross asset model file",
            "A file name.",
            "Names the XML file with the cross asset model for the simulation under the analytic." + SIM_FWD,
            "None.",
            INPUT_FILE),
        "dimModel": entry(
            "DIM model",
            "A DIM model name, for example DeltaVaR.",
            "Selects the dynamic initial margin model for the XVA analytic under the SA-CVA analytic, and switches "
            "DIM on. The names are those of the DIM model parameter of the XVA analytic.",
            "None. The XVA analytic under SA-CVA uses no DIM.",
            "One of the DIM models of the XVA analytic."),
        "dimScaling": entry(
            "DIM scaling",
            "A number.",
            "Sets the scaling factor for the DIM values of the XVA analytic under the SA-CVA analytic. When "
            "it is missing, the XVA analytic derives the factor from the initial margin of the collateral "
            "balances file.",
            "None.",
            REAL),
        "collateralBalancesFile": entry(
            "Collateral balances file",
            "A file name.",
            "Names the XML file with the current variation margin and initial margin balances. ORE passes it "
            "on to the XVA analytic under the SA-CVA analytic, which derives the DIM scaling from it when "
            "DIM scaling is missing.",
            "None.",
            INPUT_FILE),
        "storeSensis": entry(
            "Store sensitivities along the paths",
            "A flag. Y stores the sensitivities.",
            "Decides whether the simulation under the SA-CVA analytic stores sensitivities along the paths, "
            "which the DIM model needs." + SIM_FWD,
            "No.",
            FLAG),
        "curveSensiGrid": entry(
            "Curve sensitivity grid",
            "A comma-separated list of numbers of years.",
            "Lists the points on the yield curves for which the simulation stores sensitivities." + SIM_FWD
            + " For example, 1,2,3,5,7,10,15,20.",
            "None.",
            "A comma-separated list of numbers of years."),
        "vegaSensiGrid": entry(
            "Vega sensitivity grid",
            "A comma-separated list of numbers of years.",
            "Lists the option expiries for which the simulation stores vega sensitivities." + SIM_FWD
            + " For example, 1,2,3,5,7,10,15,20.",
            "None.",
            "A comma-separated list of numbers of years."),
        "xvaSensiSimMarketParams": entry(
            "XVA sensitivity simulation market",
            "A file name.",
            "Names the XML file with the simulation market for the XVA sensitivity analytic that runs under "
            "the SA-CVA analytic.",
            "None.",
            INPUT_FILE),
        "xvaSensiScenarioData": entry(
            "XVA sensitivity scenario file",
            "A file name.",
            "Names the XML file with the sensitivity scenarios for the XVA sensitivity analytic that runs "
            "under the SA-CVA analytic.",
            "None.",
            INPUT_FILE),
        "nameMappingInputFile": entry(
            "Name mapping file",
            "A file name.",
            "Names the file that maps the names in the portfolio to the names that the SA-CVA calculator "
            "uses. Without it ORE uses the names as they are.",
            "None. ORE uses the names as they are.",
            INPUT_FILE),
    },
    "bacva": {
        "active": entry(
            "Run the BA-CVA analytic",
            "A flag. Y runs the analytic.",
            "Switches the basic approach for the CVA capital (BA-CVA) on or off. The calculation uses the "
            "SA-CCR analytic under it to compute the exposures at default.",
            "No. The analytic does not run.",
            FLAG, required=True),
        "csaFile": entry(
            "Netting set definitions file",
            "A file name.",
            "Names the XML file with the netting set definitions, as in the XVA analytics. For example, "
            "netting.xml.",
            "None. The analytic needs this file, here or in the Setup block.",
            INPUT_FILE),
        "collateralBalancesFile": entry(
            "Collateral balances file",
            "A file name.",
            "Names the XML file with the variation margin and initial margin balances of the valuation date. "
            "The SA-CCR analytic under the analytic reads the file. For example, collateralbalances.xml.",
            "None.",
            INPUT_FILE),
        "simmVersion": entry(
            "SIMM version",
            "A SIMM version, for example 2.6.",
            "Names the version of the SIMM that the calculation uses for the name and bucket mapping of "
            "commodity hedging sets. The run stops if the version is empty.",
            "None. The parameter is mandatory.",
            "A SIMM version such as 2.4, 2.5A, 2.6 or 2.7."),
        "nameMappingInputFile": entry(
            "Name mapping file",
            "A file name.",
            "Names the file that maps the names in the portfolio to the SIMM names for the hedging sets. "
            "Without it ORE uses the names as they are.",
            "None. ORE uses the names as they are.",
            INPUT_FILE),
        "bucketMappingInputFile": entry(
            "Bucket mapping file",
            "A file name.",
            "Names the file that maps risk factors to the SIMM buckets for the hedging sets. Without it ORE "
            "uses the standard buckets.",
            "None. ORE uses the standard SIMM buckets.",
            INPUT_FILE),
    },
    "saccr": {
        "active": entry(
            "Run the SA-CCR analytic",
            "A flag. Y runs the analytic.",
            "Switches the standardised approach for the counterparty credit risk capital (SA-CCR) on or off. "
            "The calculation needs the netting set definitions and the collateral balances.",
            "No. The analytic does not run.",
            FLAG, required=True),
        "csaFile": entry(
            "Netting set definitions file",
            "A file name.",
            "Names the XML file with the netting set definitions, as in the XVA analytics. ORE looks for the "
            "file name in the saccr block, then in the bacva block. For example, netting.xml.",
            "None. The analytic needs this file.",
            INPUT_FILE),
        "collateralBalancesFile": entry(
            "Collateral balances file",
            "A file name.",
            "Names the XML file with the variation margin and initial margin balances of the valuation date. "
            "ORE looks for the file name in the saccr block, then in the bacva block. For example, "
            "collateralbalances.xml.",
            "None. The analytic needs this file.",
            INPUT_FILE),
        "counterpartyFile": entry(
            "Counterparty file",
            "A file name.",
            "Names the XML file with the counterparty information for the SA-CCR calculation. A value here "
            "replaces the file of the Setup block.",
            "The counterparty file of the Setup block.",
            INPUT_FILE),
        "simmVersion": entry(
            "SIMM version",
            "A SIMM version, for example 2.6.",
            "Names the version of the SIMM that the calculation uses for the name and bucket mapping of "
            "commodity hedging sets. The run stops if the version is empty.",
            "None. The parameter is mandatory.",
            "A SIMM version such as 2.4, 2.5A, 2.6 or 2.7."),
        "nameMappingInputFile": entry(
            "Name mapping file",
            "A file name.",
            "Names the file that maps the names in the portfolio to the SIMM names for the commodity hedging "
            "sets. Without it ORE uses the names as they are.",
            "None. ORE uses the names as they are.",
            INPUT_FILE),
        "bucketMappingInputFile": entry(
            "Bucket mapping file",
            "A file name.",
            "Names the file that maps risk factors to the SIMM buckets for the commodity hedging sets. "
            "Without it ORE uses the standard buckets.",
            "None. ORE uses the standard SIMM buckets.",
            INPUT_FILE),
    },
    "smrc": {
        "active": entry(
            "Run the SMRC analytic",
            "A flag. Y runs the analytic.",
            "Switches the basic standardised market risk capital calculation (SMRC) on or off. It needs no "
            "inputs besides the Setup block.",
            "No. The analytic does not run.",
            FLAG, required=True),
    },
}
