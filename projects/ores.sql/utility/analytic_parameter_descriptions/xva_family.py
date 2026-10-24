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
            "sizes, and the par conversion. The analytic uses the same file format as the sensitivity analytic, "
            "so a file can serve both. A risk factor that the file does not list gets no XVA sensitivity. For "
            "example, sensitivity.xml.",
            "None. The analytic needs this file.",
            INPUT_FILE),
        "parSensitivity": entry(
            "Par sensitivities",
            "A flag. Y also writes par sensitivities.",
            "Decides whether the analytic converts the raw sensitivities of the XVA into sensitivities to par "
            "rates and writes them in an extra report. Par sensitivities relate to the quotes that a desk "
            "hedges with, for example the par rate of a swap, so they are the numbers a trader uses. The "
            "sensitivity configuration file must have par conversion sections. The default is Yes.",
            "Yes.",
            FLAG),
        "outputJacobi": entry(
            "Write the Jacobi matrices",
            "A flag. Y writes the matrices.",
            "Decides whether the analytic writes the Jacobi matrix and its inverse, which convert between raw "
            "and par sensitivities, to files. Use the files to check the par conversion, or to convert other "
            "raw sensitivities yourself. The two file name parameters set the names. The default is No.",
            "No.",
            FLAG),
        "outputSensitivityThreshold": entry(
            "Sensitivity output threshold",
            "A number.",
            "Sets the smallest absolute value that the analytic writes. It drops every finite difference below "
            "the number from the output files. A larger value gives smaller files and hides the noise of the "
            "finite differences. The default is 0.000001. For example, 0.01 keeps only the sensitivities that "
            "change the XVA by one cent or more.",
            "0.000001",
            REAL),
        "outputPrecision": entry(
            "Sensitivity output precision",
            "A whole number of decimal places.",
            "Sets the number of decimal places of the numbers in the XVA sensitivity reports. A higher number "
            "shows more of each value and makes the files larger. The default is 4. For example, 8 shows small "
            "sensitivities such as 0.00001234 in full.",
            "4",
            COUNT),
        "jacobiOutputFile": entry(
            "Jacobi matrix file",
            "A file name.",
            "Names the file for the Jacobi matrix, which converts a raw shift into the par shifts of the "
            "quotes. It has an effect only when Write the Jacobi matrices is on. The default is "
            "xva_sensi_jacobi.csv, written in the output path. Change it when one run writes more than one set "
            "of reports.",
            "xva_sensi_jacobi.csv",
            FILE),
        "jacobiInverseOutputFile": entry(
            "Inverse Jacobi matrix file",
            "A file name.",
            "Names the file for the inverse Jacobi matrix, which converts a par shift into the raw shifts of "
            "the market. It has an effect only when Write the Jacobi matrices is on. The default is "
            "xva_sensi_jacobi_inverse.csv, written in the output path. Change it when one run writes more than "
            "one set of reports.",
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
            "Sets the smallest par rate shift that the analytic uses. It ignores every shift below this number, "
            "so a small move of the market does not enter the explanation. The default is 0, so the analytic "
            "ignores no shift. For example, 1e-4 ignores shifts smaller than one basis point.",
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
            "Sets the second evaluation date directly, instead of deriving it from the number of days. The "
            "analytic explains the change of the XVA between the valuation date and this date. For example, "
            "2026-03-02. When it is missing, ORE derives the date from the number of days and the calendar.",
            "None. ORE derives the date from the number of days.",
            DATE),
        "mporCalendar": entry(
            "Calendar of the second date",
            "A calendar name.",
            "Names the calendar that ORE uses to make sure that the second evaluation date is a business day. "
            "If the date falls on a holiday of the calendar, ORE moves it. Use the calendar of the market of "
            "the portfolio. For example, EUR. When it is missing, ORE takes the calendar of the base currency.",
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
            "Names the XML file with the stress scenarios in the format of the NPV stress test. The analytic "
            "runs the XVA calculation once for each scenario in the file and reports the XVA of each. For "
            "example, stresstest.xml. The analytic needs this file.",
            "None. The analytic needs this file.",
            INPUT_FILE),
        "sensitivityConfigFile": entry(
            "Sensitivity configuration file",
            "A file name.",
            "Names the XML file that sets the sensitivity calculation, in particular the par conversion that a "
            "par stress test needs. The conversion turns a shift of par rates into the equivalent shift of the "
            "model. Without it, ORE supports no par stress test and the stress file must be in the raw domain. "
            "For example, sensitivity_stress.xml.",
            "None. Par stress tests are not supported.",
            INPUT_FILE),
        "writeCubes": entry(
            "Write the cubes under each scenario",
            "A flag. Y writes the cubes.",
            "Decides whether the analytic writes the raw cube and the net cube for each stress scenario. The "
            "files can be very large, because a cube is written for each scenario. Use it to inspect the "
            "exposures of one scenario. The default is No.",
            "No.",
            FLAG),
        "scenarioOutputFile": entry(
            "Stress scenario output file",
            "A file name.",
            "Names the file for the report of the XVA stress scenarios: the XVA under the base market and under "
            "each stress scenario, and their differences. The default is xva_stress.csv, written in the output "
            "path.",
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
            "Names a file with net sensitivities that ORE passes directly to the SA-CVA calculator. When it is "
            "set, ORE computes no sensitivities, so the run is fast. Use it when another system supplies the "
            "CVA sensitivities. For example, sacva_sensitivity.csv.",
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
            "Names the XML file with the counterparty information that the SA-CVA calculator needs, such as the "
            "credit quality and the sector of each counterparty. ORE looks for the file name in the sacva "
            "block, then in the bacva block and last in the Setup block. A value here replaces the others.",
            "The counterparty file of the Setup block.",
            INPUT_FILE),
        "simulationConfigFile": entry(
            "Simulation configuration file",
            "A file name.",
            "Names the XML file with the simulation configuration for the exposure simulation under the "
            "analytic. The SA-CVA analytic passes the value on to the simulation analytic that runs under it, "
            "so the file sets the model, the grid and the number of samples of that simulation.",
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
            "Names the XML file with the cross asset model for the simulation under the analytic. The SA-CVA "
            "analytic passes the value on to the simulation analytic that runs under it. Use it when the model "
            "sits in another file than the simulation configuration file.",
            "None.",
            INPUT_FILE),
        "dimModel": entry(
            "DIM model",
            "A DIM model name, for example DeltaVaR.",
            "Selects the dynamic initial margin model for the XVA analytic under the SA-CVA analytic, and "
            "switches DIM on. The names are those of the DIM model parameter of the XVA analytic. Without a "
            "value the XVA analytic under SA-CVA uses no DIM, and the run is faster.",
            "None. The XVA analytic under SA-CVA uses no DIM.",
            "One of the DIM models of the XVA analytic."),
        "dimScaling": entry(
            "DIM scaling",
            "A number.",
            "Sets the scaling factor for the DIM values of the XVA analytic under the SA-CVA analytic. The "
            "factor scales the projected margin so that it agrees with the margin that the firm posts today. "
            "When it is missing, the XVA analytic derives the factor from the initial margin of the collateral "
            "balances file.",
            "None.",
            REAL),
        "collateralBalancesFile": entry(
            "Collateral balances file",
            "A file name.",
            "Names the XML file with the current variation margin and initial margin balances. ORE passes it on "
            "to the XVA analytic under the SA-CVA analytic, which derives the DIM scaling from it when DIM "
            "scaling is missing. For example, collateralbalances.xml.",
            "None.",
            INPUT_FILE),
        "storeSensis": entry(
            "Store sensitivities along the paths",
            "A flag. Y stores the sensitivities.",
            "Decides whether the simulation under the SA-CVA analytic stores sensitivities along the paths, "
            "which the DIM model needs. The SA-CVA analytic passes the value on to the simulation analytic that "
            "runs under it. Storing makes the cube larger and the run longer. Switch it on only when a DIM "
            "model is set.",
            "No.",
            FLAG),
        "curveSensiGrid": entry(
            "Curve sensitivity grid",
            "A comma-separated list of numbers of years.",
            "Lists the points on the yield curves for which the simulation stores sensitivities. Each number is "
            "a tenor in years. The SA-CVA analytic passes the value on to the simulation analytic that runs "
            "under it. For example, 1,2,3,5,7,10,15,20 stores the sensitivity to eight points of each curve.",
            "None.",
            "A comma-separated list of numbers of years."),
        "vegaSensiGrid": entry(
            "Vega sensitivity grid",
            "A comma-separated list of numbers of years.",
            "Lists the option expiries for which the simulation stores vega sensitivities. The SA-CVA analytic "
            "passes the value on to the simulation analytic that runs under it. Each number is an expiry in "
            "years. For example, 1,2,3,5,7,10,15,20.",
            "None.",
            "A comma-separated list of numbers of years."),
        "xvaSensiSimMarketParams": entry(
            "XVA sensitivity simulation market",
            "A file name.",
            "Names the XML file with the simulation market for the XVA sensitivity analytic that runs under the "
            "SA-CVA analytic. The file says which risk factors the market holds and on which tenors, so it "
            "decides the points of the CVA sensitivities.",
            "None.",
            INPUT_FILE),
        "xvaSensiScenarioData": entry(
            "XVA sensitivity scenario file",
            "A file name.",
            "Names the XML file with the sensitivity scenarios for the XVA sensitivity analytic that runs under "
            "the SA-CVA analytic. The file sets the shift of each risk factor, so it decides the size of the "
            "bumps behind the CVA sensitivities.",
            "None.",
            INPUT_FILE),
        "nameMappingInputFile": entry(
            "Name mapping file",
            "A file name.",
            "Names the file that maps the names in the portfolio to the names that the SA-CVA calculator uses, "
            "for example the name of an issuer to its sector and bucket. Use it when the names of the portfolio "
            "differ from the names of the calculator. Without it ORE uses the names as they are.",
            "None. ORE uses the names as they are.",
            INPUT_FILE),
    },
    "bacva": {
        "active": entry(
            "Run the BA-CVA analytic",
            "A flag. Y runs the analytic.",
            "Switches the basic approach for the CVA capital (BA-CVA) on or off. The calculation uses the "
            "SA-CCR analytic under it to compute the exposures at default, and then applies the supervisory "
            "formula of the basic approach. It needs the netting set definitions and the collateral balances.",
            "No. The analytic does not run.",
            FLAG, required=True),
        "csaFile": entry(
            "Netting set definitions file",
            "A file name.",
            "Names the XML file with the netting set definitions, as in the XVA analytics. The file holds the "
            "margin agreements, such as the threshold and the minimum transfer amount, that the exposure at "
            "default depends on. For example, netting.xml. The analytic needs this file here or in the Setup "
            "block.",
            "None. The analytic needs this file, here or in the Setup block.",
            INPUT_FILE),
        "collateralBalancesFile": entry(
            "Collateral balances file",
            "A file name.",
            "Names the XML file with the variation margin and initial margin balances of the valuation date. "
            "The SA-CCR analytic under the analytic reads the file. The balances reduce the exposure at default "
            "of each netting set. For example, collateralbalances.xml.",
            "None.",
            INPUT_FILE),
        "simmVersion": entry(
            "SIMM version",
            "A SIMM version, for example 2.6.",
            "Names the version of the SIMM that the calculation uses for the name and bucket mapping of "
            "commodity hedging sets. Use the version that the firm has adopted, for example 2.6. The run stops "
            "if the version is empty. The parameter is mandatory.",
            "None. The parameter is mandatory.",
            "A SIMM version such as 2.4, 2.5A, 2.6 or 2.7."),
        "nameMappingInputFile": entry(
            "Name mapping file",
            "A file name.",
            "Names the file that maps the names in the portfolio to the SIMM names for the hedging sets, for "
            "example a trade name to an issuer. Use it when the names of the portfolio differ from the names of "
            "the SIMM. Without it ORE uses the names as they are.",
            "None. ORE uses the names as they are.",
            INPUT_FILE),
        "bucketMappingInputFile": entry(
            "Bucket mapping file",
            "A file name.",
            "Names the file that maps risk factors to the SIMM buckets for the hedging sets. The bucket decides "
            "the risk weight that the hedging set gets. Without it ORE uses the standard buckets.",
            "None. ORE uses the standard SIMM buckets.",
            INPUT_FILE),
    },
    "saccr": {
        "active": entry(
            "Run the SA-CCR analytic",
            "A flag. Y runs the analytic.",
            "Switches the standardised approach for the counterparty credit risk capital (SA-CCR) on or off. "
            "The calculation gives the exposure at default of each netting set, as the replacement cost plus "
            "the potential future exposure, times a factor. It needs the netting set definitions and the "
            "collateral balances.",
            "No. The analytic does not run.",
            FLAG, required=True),
        "csaFile": entry(
            "Netting set definitions file",
            "A file name.",
            "Names the XML file with the netting set definitions, as in the XVA analytics. The file holds the "
            "margin agreements, such as the threshold and the minimum transfer amount, that the exposure at "
            "default depends on. ORE looks for the file name in the saccr block, then in the bacva block. For "
            "example, netting.xml.",
            "None. The analytic needs this file.",
            INPUT_FILE),
        "collateralBalancesFile": entry(
            "Collateral balances file",
            "A file name.",
            "Names the XML file with the variation margin and initial margin balances of the valuation date. "
            "The balances reduce the replacement cost and the exposure at default of each netting set. ORE "
            "looks for the file name in the saccr block, then in the bacva block. For example, "
            "collateralbalances.xml.",
            "None. The analytic needs this file.",
            INPUT_FILE),
        "counterpartyFile": entry(
            "Counterparty file",
            "A file name.",
            "Names the XML file with the counterparty information for the SA-CCR calculation, such as the "
            "credit quality and the sector of each counterparty. A value here replaces the file of the Setup "
            "block, so one run can use a different list of counterparties for the capital calculation.",
            "The counterparty file of the Setup block.",
            INPUT_FILE),
        "simmVersion": entry(
            "SIMM version",
            "A SIMM version, for example 2.6.",
            "Names the version of the SIMM that the calculation uses for the name and bucket mapping of "
            "commodity hedging sets. Use the version that the firm has adopted, for example 2.6. The run stops "
            "if the version is empty. The parameter is mandatory.",
            "None. The parameter is mandatory.",
            "A SIMM version such as 2.4, 2.5A, 2.6 or 2.7."),
        "nameMappingInputFile": entry(
            "Name mapping file",
            "A file name.",
            "Names the file that maps the names in the portfolio to the SIMM names for the commodity hedging "
            "sets. Use it when the names of the portfolio differ from the names of the SIMM. Without it ORE "
            "uses the names as they are.",
            "None. ORE uses the names as they are.",
            INPUT_FILE),
        "bucketMappingInputFile": entry(
            "Bucket mapping file",
            "A file name.",
            "Names the file that maps risk factors to the SIMM buckets for the commodity hedging sets. The "
            "bucket decides the supervisory factor that the hedging set gets. Without it ORE uses the standard "
            "buckets.",
            "None. ORE uses the standard SIMM buckets.",
            INPUT_FILE),
    },
    "smrc": {
        "active": entry(
            "Run the SMRC analytic",
            "A flag. Y runs the analytic.",
            "Switches the basic standardised market risk capital calculation (SMRC) on or off. The calculation "
            "applies the standardised formula to the sensitivities of the portfolio and gives the capital "
            "charge for market risk. It needs no inputs besides the Setup block.",
            "No. The analytic does not run.",
            FLAG, required=True),
    },
}
