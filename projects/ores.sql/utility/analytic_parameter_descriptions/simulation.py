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
"""The exposure simulation analytic."""

from ._entry import COUNT, FILE, FLAG, INPUT_FILE, REAL, entry

ENGINE_FILE = "A file name. ORE looks for the file in the input path of the Setup block."
IGNORED = "ORE ignores the parameter, so a value has no effect."
GRID = "A comma-separated list of numbers of years, for example 1,2,3,5,7,10."

ENTRIES = {
    "simulation": {
        "active": entry(
            "Run the exposure simulation",
            "A flag. Y runs the simulation.",
            "Switches the exposure simulation on or off. When it is on, the run evolves the market with a "
            "Monte Carlo simulation and values every trade of the portfolio at each date of the simulation "
            "grid on each path. The result is an NPV cube. The XVA, PFE and other exposure analytics use "
            "this cube. Use N to keep the entry in the document without running it.",
            "No. The simulation does not run.",
            FLAG, required=True),
        "simulationConfigFile": entry(
            "Simulation configuration file",
            "A file name.",
            "Names the XML file that sets up the simulation. The file holds three things: the simulated market "
            "(which curves, volatilities and rates evolve), the model (the cross asset model and its "
            "calibration) and the scenario generator (the date grid, the number of samples and the random "
            "number generator). ORE reads all three from this one file, unless the parameters "
            "Cross asset model data or Scenario generator data name another file. For example, simulation.xml.",
            "None. The simulation needs this file, so a run without it fails.",
            INPUT_FILE),
        "crossAssetModelData": entry(
            "Cross asset model file",
            "A file name.",
            "Names an XML file for the cross asset model, if the model sits in another file than the rest "
            "of the simulation setup. Use it to share one calibrated model between several runs. When the "
            "parameter is missing, ORE reads the model from the simulation configuration file.",
            "The simulation configuration file.",
            INPUT_FILE),
        "scenarioGeneratorData": entry(
            "Scenario generator file",
            "A file name.",
            "Names an XML file for the scenario generator, if it sits in another file than the rest of "
            "the simulation setup. The scenario generator sets the date grid, the number of samples and the "
            "random number generator. Use it to run the same model on a different grid or sample size. When "
            "the parameter is missing, ORE reads the scenario generator from the simulation configuration file.",
            "The simulation configuration file.",
            INPUT_FILE),
        "maxScenario": entry(
            "Maximum number of scenarios",
            "A whole number of paths.",
            "Caps the number of Monte Carlo paths. When the scenario generator asks for more samples than "
            "this number, ORE uses this number. It never raises the number of samples. Use a small value for a "
            "quick test run. For example, 100 runs at most 100 paths.",
            "None. The scenario generator decides.",
            COUNT),
        "pricingEnginesFile": entry(
            "Pricing engines file for the simulation",
            "A file name.",
            "Names the XML file that sets the pricing engines for the valuations on the simulated scenarios. "
            "These can differ from the engines of the valuation date, for example faster approximations, "
            "because the simulation values every trade on every date and path. The cost of an engine matters "
            "much more here than in the NPV run. For example, pricingengine.xml.",
            "The pricing engines file of the Setup block.",
            INPUT_FILE),
        "pricingEnginesOverride": entry(
            "Pricing engines override",
            "A file name.",
            "Names an XML file with pricing engine settings that replace the matching settings of the "
            "pricing engines file for the simulation. Use it to change a few engines and keep the rest. ORE "
            "applies the file on top of the pricing engines that the simulation uses.",
            "None. The engines stay as the pricing engines file sets them.",
            INPUT_FILE),
        "baseCurrency": entry(
            "Base currency for the cube",
            "A currency code, for example EUR.",
            "Names the currency in which ORE converts all NPVs before it writes them to the trade-level cube "
            "and the netting set cube. Exposures and XVA are then stated in this currency. Choose the currency "
            "of the reports that you will produce, because the cube cannot change it later. For example, EUR "
            "stores all cube values in euro. The Setup block has a parameter of the same name.",
            "The base currency of the Setup block.",
            "A currency code of three letters, for example EUR."),
        "observationModel": entry(
            "Observation model for the simulation",
            "The name of an observation model, for example Disable.",
            "Controls how many QuantLib observer notifications the simulation uses when market data and fixings "
            "are updated and when the evaluation date moves along a path. Notifications cost time in a "
            "simulation. None is the normal QuantLib behaviour. Disable switches the notifications off and lets "
            "the engine trigger the recalculations. Defer holds the updates back until notifications are on "
            "again. Unregister keeps notifications on but unregisters floating rate coupons from their indices. "
            "Disable and Unregister give a similar speed up. The Setup block has a parameter of the same name, "
            "and this one overrides it for the simulation.",
            "The observation model of the Setup block, which is None by default.",
            "One of None, Disable, Defer or Unregister."),
        "includeTodaysCashFlows": entry(
            "Include today's cash flows in the simulation",
            "A flag. Y counts the cash flows of the valuation date.",
            "Decides whether the simulation counts a cash flow that pays on the valuation date itself, or treats "
            "it as matured. Set it to Y when a payment of the day must be part of the exposure. The Setup "
            "block has a parameter of the same name, and this one overrides it for the simulation.",
            "The choice of the Setup block. When that is missing too, No: a cash flow of the valuation date is treated as matured.",
            FLAG),
        "includeReferenceDateEvents": entry(
            "Include reference date events in the simulation",
            "A flag. Y counts the events of the valuation date.",
            "Decides whether the simulation counts an event that falls on the valuation date, for example an "
            "exercise, a fixing or a payment. The Setup block has a parameter of the same name, and this one "
            "overrides it for the simulation. Use it when the exposure on the first date must agree with the "
            "NPV, because the two must treat the same-day cash flow in the same way.",
            "The choice of the Setup block. When that is missing too, No: an event of the valuation date is treated as matured.",
            FLAG),
        "scenarioFile": entry(
            "Scenario input file",
            "A file name.",
            "Names a file with ready-made market scenarios. When it is set, the simulation reads its scenarios "
            "from the file and does not generate them with the model. Use it to value a portfolio on "
            "scenarios that an earlier run wrote. For example, ../Output/scenarios.csv.",
            "None. The simulation generates its own scenarios.",
            INPUT_FILE),
        "allowPartialScenarios": entry(
            "Allow partial scenarios",
            "A flag. Y accepts scenarios that touch only part of the market.",
            "Decides whether the simulated market accepts scenarios that do not set every risk factor of the "
            "simulation market. When it is N, ORE checks that each scenario covers all of the market and "
            "stops if a risk factor is missing. Switch it on when you read scenarios from a file that covers "
            "only part of the market.",
            "No. ORE checks that scenarios cover the whole market.",
            FLAG),
        "amc": entry(
            "Use American Monte Carlo",
            "A flag. Y uses American Monte Carlo for the trade types listed.",
            "Decides whether ORE values some trade types with American Monte Carlo (AMC) instead of "
            "the classic simulation. AMC regresses the trade value on the simulated state and is much faster "
            "for products with early exercise. The parameter AMC trade types lists the trade types that AMC "
            "covers. The classic simulation values the other trades, and ORE joins the two cubes.",
            "No. The classic simulation values all trades.",
            FLAG),
        "amcTradeTypes": entry(
            "AMC trade types",
            "A comma-separated list of trade types.",
            "Lists the trade types that ORE values with American Monte Carlo when AMC is on. The allowed types "
            "are Swap, Swaption, FxForward, FxOption, ForwardBond, FxTaRF, ScriptedTrade and CompositeTrade. "
            "The classic simulation values every other trade of the portfolio. For example, "
            "Swap,Swaption,FxOption.",
            "None. With AMC on and no list, no trade type is covered by AMC.",
            "A comma-separated list of trade type names."),
        "amcPricingEnginesFile": entry(
            "Pricing engines file for AMC",
            "A file name.",
            "Names the XML file that sets the pricing engines for the trades that American Monte Carlo (AMC) "
            "values. These engines differ from the classic ones, because they are the AMC pricers: they value a "
            "trade on the simulated paths with a regression and not with a full revaluation on each path. A "
            "trade type that this file does not cover stays with the classic engine. For example, "
            "pricingengine_amc.xml.",
            "The pricing engines file of the Setup block.",
            INPUT_FILE),
        "amcPricingEnginesOverride": entry(
            "Pricing engines override for AMC",
            "A file name.",
            "Names an XML file with pricing engine settings that replace the matching settings of the AMC "
            "pricing engines file. Use it to change a few engines for one run, for example the number of "
            "training paths of one product, and keep the rest of the file as it is. The file has the same "
            "layout as the pricing engines file, and it needs only the entries that change.",
            "None. The engines stay as the AMC pricing engines file sets them.",
            INPUT_FILE),
        "amcPathDataInput": entry(
            "AMC path data input file",
            "A file name.",
            "Names a file with the path data of an earlier AMC run, that is the simulated states of the model "
            "on each path and date. The run reads the path data from the file and does not simulate again. Use "
            "it together with the output parameter to repeat an AMC valuation on the same paths, for example "
            "for a second portfolio. The paths must come from the same model and the same grid.",
            "None. The run computes the path data.",
            INPUT_FILE),
        "amcPathDataOutput": entry(
            "AMC path data output file",
            "A file name.",
            "Names the file that receives the path data of the AMC run, that is the simulated states of the "
            "model on each path and date. A later run can read the file with the AMC path data input parameter "
            "and so skip the simulation. Use it when you run the same simulation for several portfolios. For "
            "example, amcBuffer.dat writes the data to amcBuffer.dat in the output path.",
            "None. The run does not write the path data.",
            FILE),
        "amcIndividualTrainingInput": entry(
            "AMC individual training input",
            "A flag. ORE does not read this setting from the simulation block.",
            "Is meant to make the AMC run read the trained regression of each trade from a file. ORE reads "
            "the parameter from a block with the wrong name, xsimulationva, so a value in the simulation block "
            "has no effect in release 1.8.17.0.",
            "No. The value in the simulation block has no effect.",
            FLAG),
        "amcIndividualTrainingOutput": entry(
            "AMC individual training output",
            "A flag. Y writes the trained regression of each trade.",
            "Decides whether the AMC run writes the trained regression of each trade to a file, so that a later "
            "run can reuse it. Training is the costly step of AMC, so reuse saves time when the same trades are "
            "valued again on the same paths. The files take disk space for each trade. Switch it on for a "
            "stable portfolio that runs every day.",
            "No. The run does not write the regressions.",
            FLAG),
        "amcUseGpuRegression": entry(
            "Use the GPU for the AMC regression",
            "A flag. Y runs the regression on the GPU.",
            "Decides whether the AMC regression runs its QR step on a GPU through cuSOLVER. It is an opt-in for "
            "runs with a large portfolio on a machine with a supported GPU, where the regression is the main "
            "cost. The results do not change, so you can compare a run with the GPU and a run without it. A "
            "machine without a supported GPU fails when this is on.",
            "No. The regression runs on the CPU.",
            FLAG),
        "amcCg": entry(
            "Computation graph engine mode",
            "One of Disabled, CubeGeneration or Full.",
            "Selects the use of the computation graph (CG) engine for AMC. Disabled keeps the normal AMC. "
            "CubeGeneration builds the NPV cube with the CG engine and leaves the XVA post processing to "
            "the normal code. Full also computes the XVA with the CG engine. The CG engine allows "
            "sensitivities by algorithmic differentiation and dynamic initial margin.",
            "Disabled",
            "One of Disabled, CubeGeneration or Full."),
        "amcCgPricingEnginesFile": entry(
            "Pricing engines file for the CG engine",
            "A file name.",
            "Names the XML file that sets the pricing engines for the computation graph (CG) engine. The CG "
            "engine builds a graph of the calculation of each trade and then evaluates it on all paths, so it "
            "needs engines that can produce such a graph. A trade type without such an engine cannot run in the "
            "CG engine. For example, pricingengine_amccg.xml.",
            "The pricing engines file of the Setup block.",
            INPUT_FILE),
        "amcCgPricingEnginesOverride": entry(
            "Pricing engines override for the CG engine",
            "A file name.",
            "Names an XML file with pricing engine settings that replace the matching settings of the CG "
            "pricing engines file. Use it to change a few engines for one run and keep the rest of the file as "
            "it is. The file has the same layout as the pricing engines file, and it needs only the entries "
            "that change.",
            "None. The engines stay as the CG pricing engines file sets them.",
            INPUT_FILE),
        "xvaCgSensitivityConfigFile": entry(
            "CG sensitivity configuration file",
            "A file name.",
            "Names the XML file that sets which sensitivities the CG engine computes: the risk factors and the "
            "shifts. The CG engine computes the sensitivities from the graph, so the cost of each extra risk "
            "factor is small. Without the file the CG engine computes no sensitivities. For example, "
            "xvasensiconfig.xml. The file has the layout of the sensitivity configuration file.",
            "None. The CG engine computes no sensitivities.",
            INPUT_FILE),
        "xvaCgBumpSensis": entry(
            "CG bump and revalue sensitivities",
            "A flag. Y bumps and revalues instead of using differentiation.",
            "Decides how the CG engine computes the CVA sensitivities. When it is Y, the engine bumps each risk "
            "factor and revalues, which is slow and easy to check. When it is N, the engine uses algorithmic "
            "differentiation, which is fast and gives all sensitivities in one pass. Use Y to check the "
            "differentiated sensitivities against bumped ones.",
            "No. The engine uses algorithmic differentiation.",
            FLAG),
        "xvaCgDynamicIM": entry(
            "CG dynamic initial margin",
            "A flag. Y computes dynamic initial margin with the CG engine.",
            "Decides whether the CG engine also computes the initial margin along each path. The DIM model "
            "DynamicIM of the XVA analytic needs this, because that model reads the margin from the CG run. It "
            "makes the run longer and the memory use higher. Leave it off when the XVA analytic uses another "
            "DIM model or does not compute MVA.",
            "No.",
            FLAG),
        "xvaCgDynamicIMStepSize": entry(
            "CG dynamic IM step size",
            "A whole number of time steps.",
            "Sets how often the CG engine computes dynamic initial margin: on every n-th time step of the "
            "grid. A value of 1 computes it on every step. With a larger value the run is faster, and ORE "
            "copies the margin of the last computed step to the steps in between. For example, 4 computes it "
            "on every fourth step.",
            "1",
            "A whole number of one or more."),
        "xvaCgRegressionOrder": entry(
            "CG regression order",
            "A whole number.",
            "Sets the order of the polynomial that the CG engine uses in the regression for the exposures. A "
            "higher order fits the value of a trade better on each date and needs more paths to stay stable, "
            "because the fit has more coefficients. The default is 4. For example, 2 gives a quadratic fit that "
            "is steady with few paths. Raise it for trades with a strong non-linear payoff.",
            "4",
            "A whole number of one or more."),
        "xvaCgRegressionVarianceCutoff": entry(
            "CG regression variance cutoff",
            "A number between 0 and 1.",
            "Switches on a principal component analysis of the regressors in the CG regression and sets its "
            "variance cutoff. The analysis rotates the regressors, and the regression drops the directions with "
            "a variance below the cutoff. Use it when the regressors are strongly correlated. For example, 0.01.",
            "None. The regression uses the regressors as they are.",
            REAL),
        "xvaCgRegressionOrderDynamicIm": entry(
            "CG regression order for dynamic IM",
            "A whole number.",
            "Sets the order of the regression polynomial that the CG engine uses for dynamic initial margin. It "
            "works like the CG regression order and applies to the margin only, so you can fit the margin with "
            "another order than the exposure. The default is 4. A lower order is steadier with few paths, and a "
            "higher order follows a non-linear margin better.",
            "4",
            "A whole number of one or more."),
        "xvaCgRegressionVarianceCutoffDynamicIm": entry(
            "CG regression variance cutoff for dynamic IM",
            "A number between 0 and 1.",
            "Sets the variance cutoff of the regression that the CG engine uses for dynamic initial margin. It "
            "works like the CG regression variance cutoff and applies to the margin only. A regressor whose "
            "variance is below the cutoff adds noise and no information, so the regression drops it. Without a "
            "value the regression uses the regressors as they are.",
            "None. The regression uses the regressors as they are.",
            REAL),
        "xvaCgRegressionReportTimeStepsDynamicIM": entry(
            "CG regression report time steps",
            "A comma-separated list of time step numbers.",
            "Lists the time steps for which the CG engine writes a report of the dynamic initial margin "
            "regression. Use it to check the quality of the fit on a few dates, because a report on every date "
            "would be very large. For example, 1,2,3 writes the report for the first three steps. Without a "
            "value the engine writes no regression report.",
            "None. The engine writes no regression report.",
            "A comma-separated list of whole numbers."),
        "xvaCgTradeLevelBreakDown": entry(
            "CG trade level breakdown",
            "A flag. Y keeps the results for each trade.",
            "Decides whether the CG engine produces the exposure results for each trade as well as for each "
            "netting set. The default is Yes. Switch it off when you need only the netting set results, because "
            "the trade level results take memory and time for a large portfolio. Reports that need the trade "
            "level, such as the allocation of the XVA to trades, then have no data.",
            "Yes. The engine keeps the results for each trade.",
            FLAG),
        "xvaCgTradeLevelBreakdown": entry(
            "CG trade level breakdown (misspelt)",
            "A flag. ORE ignores this spelling.",
            "Is a spelling of the CG trade level breakdown parameter that ORE does not read. ORE reads only "
            "the spelling with a capital D in Break Down: xvaCgTradeLevelBreakDown. Example files "
            "use both spellings, so a document with this spelling does not change the run.",
            "Not applicable. " + IGNORED,
            FLAG),
        "xvaCgUseRedBlocks": entry(
            "CG use red blocks",
            "A flag. Y uses red blocks.",
            "Decides whether the CG engine puts the part of the graph that belongs to one trade into a red "
            "block while it builds the graph. It is on by default. Switch it off to build the graph "
            "without red blocks, for example to check a result.",
            "Yes.",
            FLAG),
        "xvaCgUseExternalComputeDevice": entry(
            "CG use an external compute device",
            "A flag. Y runs the graph on an external device.",
            "Decides whether the CG engine runs the computation graph on an external compute device, for "
            "example a GPU, instead of the CPU. A device pays off for a graph that is evaluated on many paths. "
            "The parameter CG external compute device names the device. The default is No, so the engine runs "
            "on the CPU.",
            "No. The engine runs on the CPU.",
            FLAG),
        "xvaCgExternalComputeDevice": entry(
            "CG external compute device",
            "The name of a compute device.",
            "Names the device that the CG engine uses when it runs on an external compute device. The name "
            "has the form framework, vendor and device, separated by slashes. For example, "
            "OpenCL/Apple/Apple M2 Max, or BasicCpu/Default/Default for a plain CPU back end.",
            "None. Without a name, ORE uses no external device.",
            "Three names separated by slashes, for example OpenCL/Apple/Apple M2 Max."),
        "xvaCgExternalDeviceCompatibilityMode": entry(
            "CG device compatibility mode",
            "A flag. Y uses the compatibility mode.",
            "Decides whether the CG engine runs the external device in its compatibility mode. Switch it on "
            "when the device does not run the normal mode correctly, for example when a driver rejects the "
            "kernels of the normal mode. The compatibility mode can be slower. Example files set it together "
            "with the name of the device.",
            "No.",
            FLAG),
        "xvaCgUseDoublePrecisionForExternalCalculation": entry(
            "CG double precision on the device",
            "A flag. Y uses double precision.",
            "Decides whether the external device calculates with double precision numbers instead of single "
            "precision. Double precision is more exact and slower, and some devices do not support it. Single "
            "precision is enough for many exposure profiles, and double precision is the safe choice when you "
            "compare results with a CPU run. ORE uses it only when the CG engine uses an external device.",
            "No. The device uses single precision.",
            FLAG),
        "xvaCgUsePythonIntegration": entry(
            "CG Python integration",
            "A flag. Y uses the Python integration.",
            "Decides whether the CG regression calls Python code for the conditional expectations, through the "
            "Python integration of ORE. Use it to try a regression method that you write in Python, for example "
            "a machine learning model. Switch it on only when that integration is installed, because a run "
            "without it fails.",
            "No.",
            FLAG),
        "xvaCgUsePythonIntegrationDynamicIm": entry(
            "CG Python integration for dynamic IM",
            "A flag. Y uses the Python integration for the margin.",
            "Decides whether the CG engine calls Python code for the dynamic initial margin. It works like the "
            "CG Python integration parameter and applies to the margin only. Switch it on only when that "
            "integration is installed, because a run without it fails.",
            "No.",
            FLAG),
        "xvaCgRegressionCacheSize": entry(
            "CG regression cache size",
            "A whole number.",
            "Sets the size of the cache of regression results that the CG engine uses. A larger cache can save "
            "repeated regressions when many trades share the same regressors, and it uses more memory. The "
            "default is 512. Lower it on a machine with little memory, and raise it for a large portfolio with "
            "many similar trades.",
            "512",
            "A whole number of one or more."),
        "xvaCgEnableCgOptimization": entry(
            "CG graph optimisation",
            "A flag. Y optimises the graph.",
            "Decides whether the CG engine optimises the computation graph while it builds the graph. The "
            "optimisation removes steps that do not change the result, so the graph is smaller and the "
            "evaluation is faster, at the cost of a longer build. Switch it on to try it on a large portfolio. "
            "The default is No.",
            "No.",
            FLAG),
        "cubeFile": entry(
            "Cube file",
            "A file name.",
            "Names the file that holds the NPV cube of the simulation: the value of every trade on every "
            "date and every path. Setting it also switches the cube output on. The XVA and PFE analytics can "
            "read the file back with their own Cube file parameter, so a later run does not need to "
            "simulate again. Example files use cube.csv.gz and cube.dat.",
            "None. When Write cube is on, the name is cube.csv.gz.",
            FILE),
        "writeCube": entry(
            "Write the cube",
            "A flag. Y writes the cube files.",
            "Decides whether the run writes the NPV cube, the scenario data, the netting set cube and the "
            "counterparty cube to files. A value in Cube file switches the output on by itself. Use this "
            "flag to write the files with the standard names. The files are cube.csv.gz, "
            "scenariodata.csv.gz, nettingsetcube.csv.gz and cptycube.csv.gz.",
            "No. The run writes no cube unless Cube file is set.",
            FLAG),
        "aggregationScenarioDataFileName": entry(
            "Aggregation scenario data file",
            "A file name.",
            "Names the file that holds the market data that the XVA post processing needs, for example the "
            "simulated FX rates and index fixings that the variation margin calculation uses. The simulation "
            "configuration file lists the data to store. For example, scenariodata.csv.gz.",
            "None. When Write cube is on, the name is scenariodata.csv.gz.",
            FILE),
        "aggregationScenarioDataDump": entry(
            "Aggregation scenario data dump",
            "A file name. ORE no longer reads it.",
            "Named a CSV file for a dump of the aggregation scenario data. ORE stopped reading it in 1.8.9.1. "
            "Use Aggregation scenario data file to name the file that holds this data.",
            "None. ORE ignores the parameter.",
            FILE),
        "additionalScenarioDataFileName": entry(
            "Additional scenario data file",
            "A file name. ORE no longer reads it.",
            "Named the file for the additional market data of the simulation. ORE stopped reading it in "
            "1.8.3.0. Use Aggregation scenario data file to name the file that holds this data.",
            "None. ORE ignores the parameter.",
            FILE),
        "scenariodump": entry(
            "Scenario dump file",
            "A file name.",
            "Names the file that receives the raw market scenarios of the simulation: the discount factors "
            "of the yield curves and the other simulated values, for each date and path. Setting it also "
            "switches the output on. Use it to inspect the scenarios or to feed them to another tool. "
            "Filter risk keys limits the content. For example, scenariodump.csv.",
            "None. When Write scenarios is on, the name is scenario.csv.",
            FILE),
        "writeScenarios": entry(
            "Write the scenarios",
            "A flag. Y writes the scenario dump.",
            "Decides whether the run writes the raw scenarios to a file with the standard name scenario.csv. A "
            "scenario file holds the value of each risk factor on each date and path, so it is large. Use it to "
            "check the simulation on a small test case. A value in Scenario dump file switches the output on by "
            "itself and sets the name.",
            "No. The run writes no scenarios unless Scenario dump file is set.",
            FLAG),
        "filterRiskKeys": entry(
            "Risk factor types in the dump",
            "A comma-separated list of risk factor types.",
            "Limits the scenario dump to the listed types of risk factor. Without it the dump holds every risk "
            "factor of the simulation market and can be very large. Write the types as a comma-separated list. "
            "For example, DiscountCurve writes the discount factors only, and DiscountCurve,FXSpot adds the "
            "spot rates of the currencies.",
            "None. The dump holds all risk factors.",
            "A comma-separated list of risk factor key types such as DiscountCurve."),
        "storeFlows": entry(
            "Store cash flows in the cube",
            "A flag. Y stores the cash flows.",
            "Decides whether the cube also stores the cumulative cash flows between the simulation dates. Some "
            "dynamic initial margin and variation margin models need them in the XVA post processing, because "
            "the exposure depends on the cash that a trade pays between two dates. The cube gets larger when "
            "this is on. Leave it off when no model of the run needs the flows.",
            "No.",
            FLAG),
        "storeExerciseValues": entry(
            "Store exercise values in the cube",
            "A flag. Y stores the exercise values.",
            "Decides whether the cube also stores the exercise value of each trade on each date. Switch it on "
            "when a later analytic needs the value of an option if it is exercised, for example to check the "
            "exercise decision of a Bermudan swaption. The cube gets larger when this is on.",
            "No.",
            FLAG),
        "storeSurvivalProbabilities": entry(
            "Store survival probabilities",
            "A flag. Y stores the survival probabilities.",
            "Decides whether the cube stores the survival probabilities of the counterparties on each "
            "simulation date. The dynamic credit XVA calculation needs them, because the default probability "
            "then changes along the path. The cube gets larger when this is on. Leave it off when the credit "
            "curves are static.",
            "No.",
            FLAG),
        "storeCreditStateNPVs": entry(
            "Store NPVs by credit state",
            "A whole number of credit states.",
            "Sets the number of credit states for which the cube also stores the NPV of each trade. A "
            "value of zero stores none. The credit migration and the dynamic credit XVA calculations "
            "need these values. For example, 8 stores the NPV for 8 credit states.",
            "0. The cube stores no NPVs by credit state.",
            COUNT),
        "storeSensis": entry(
            "Store sensitivities along the paths",
            "A flag. Y stores the sensitivities.",
            "Decides whether the simulation stores the analytical sensitivities of the trades along each "
            "path, in the netting set cube. Some dynamic initial margin models need them. The parameters Curve "
            "sensitivity grid and Vega sensitivity grid set the points. The run stops if it stores "
            "sensitivities and neither grid is set.",
            "No.",
            FLAG),
        "curveSensiGrid": entry(
            "Curve sensitivity grid",
            "A comma-separated list of numbers of years.",
            "Lists the points on the yield curves for which the simulation stores sensitivities when Store "
            "sensitivities along the paths is on. Each number is a tenor in years. For example, "
            "1,2,3,5,7,10,15,20 stores the sensitivity to eight points of each curve. More points give a finer "
            "hedge and a larger cube.",
            "None.",
            GRID),
        "vegaSensiGrid": entry(
            "Vega sensitivity grid",
            "A comma-separated list of numbers of years.",
            "Lists the option expiries for which the simulation stores vega sensitivities when Store "
            "sensitivities along the paths is on. Each number is an expiry in years. For example, "
            "1,2,3,5,7,10,15,20 stores the vega at eight expiries. More points give a finer hedge and a larger "
            "cube.",
            "None.",
            GRID),
        "nettingSetId": entry(
            "Netting set",
            "A netting set id. ORE reads it and does not use it.",
            "Is read by ORE in release 1.8.17.0 and not used by any later step. A value has no effect.",
            "None. " + IGNORED,
            "A netting set id."),
        "cubeNpvOverlay": entry(
            "Correct the cube with the first NPV",
            "A flag. Y corrects the cube entries.",
            "Decides whether ORE corrects every raw NPV of the cube by the difference between the NPV of the "
            "valuation date from the pricing analytic and the NPV of the valuation date from the simulation. "
            "Use it when the simulation engines give a slightly different NPV today than the pricing engines.",
            "No.",
            FLAG),
        "storeScenarios": entry(
            "Store scenarios",
            "A flag. ORE ignores it.",
            "ORE does not read this parameter in any release. Example files carry it with the value N. To "
            "write the scenarios, use Scenario dump file or Write scenarios.",
            "No. " + IGNORED,
            FLAG),
        "simulationModelFile": entry(
            "Simulation model file",
            "A file name. ORE ignores it.",
            "ORE does not read this parameter in any release. Example files of an older layout carry it. ORE "
            "reads the model from the simulation configuration file.",
            "None. " + IGNORED,
            INPUT_FILE),
        "simulationMarketFile": entry(
            "Simulation market file",
            "A file name. ORE ignores it.",
            "ORE does not read this parameter in any release. Example files of an older layout carry it. ORE "
            "reads the simulated market from the simulation configuration file.",
            "None. " + IGNORED,
            INPUT_FILE),
        "simulationParamFile": entry(
            "Simulation parameter file",
            "A file name. ORE ignores it.",
            "ORE does not read this parameter in any release. Example files of an older layout carry it. ORE "
            "reads the scenario generator from the simulation configuration file.",
            "None. " + IGNORED,
            INPUT_FILE),
        "computeCube": entry(
            "Compute the cube",
            "A flag. ORE ignores it.",
            "ORE does not read this parameter in any release. Example files carry it with the value Y. The "
            "simulation always computes the cube when it runs.",
            "Yes. " + IGNORED,
            FLAG),
        "disableEvaluationDateObservation": entry(
            "Disable evaluation date observation",
            "A flag. ORE ignores it.",
            "ORE does not read this parameter in any release. Example files carry it with the value Y. To "
            "reduce the cost of notifications, use the observation model.",
            "None. " + IGNORED,
            FLAG),
    },
}
