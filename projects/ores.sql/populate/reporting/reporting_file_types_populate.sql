/* -*- sql-product: postgres; tab-width: 4; indent-tabs-mode: nil -*-
 *
 * Copyright (C) 2026 Marco Craveiro <marco.craveiro@gmail.com>
 *
 * This program is free software; you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation; either version 3 of the License, or (at your option) any later
 * version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
 * details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51
 * Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */

/**
 * File Types Population Script
 *
 * GENERATED FILE - DO NOT EDIT. Regenerate with
 * projects/ores.sql/utility/generate_file_types_populate.py.
 *
 * Seeds the file_types lookup for the system tenant with the 142 kinds of file the
 * 416 shipped ORE run documents name. Each well known name is the most common
 * name the samples give the file, and sample_count out of sample_total records how
 * many of them use it. This script is idempotent.
 */

\echo '--- File Types ---'

do $$
declare
    v_sys_tenant uuid := ores_utility_system_tenant_id_fn();
    r record;
begin
    for r in
        select * from (values
        ('setup.calendarAdjustment', null, 'calendarAdjustment', 'input_document', null, 'calendaradjustment.xml', 41, 42, 'A configuration document the run reads, named by calendarAdjustment in the Setup block.', 1),
        ('setup.conventionsFile', null, 'conventionsFile', 'input_document', 'conventions', 'conventions.xml', 404, 414, 'A configuration document the run reads, named by conventionsFile in the Setup block.', 2),
        ('setup.counterpartyFile', null, 'counterpartyFile', 'input_document', null, 'counterparty.xml', 7, 7, 'A configuration document the run reads, named by counterpartyFile in the Setup block.', 3),
        ('setup.currencyConfiguration', null, 'currencyConfiguration', 'input_document', null, 'currencies.xml', 31, 31, 'A configuration document the run reads, named by currencyConfiguration in the Setup block.', 4),
        ('setup.curveConfigFile', null, 'curveConfigFile', 'input_document', 'curve_configuration', 'curveconfig.xml', 396, 415, 'A configuration document the run reads, named by curveConfigFile in the Setup block.', 5),
        ('setup.iborFallbackConfig', null, 'iborFallbackConfig', 'input_document', null, 'ibor_fallback.xml', 1, 1, 'A configuration document the run reads, named by iborFallbackConfig in the Setup block.', 6),
        ('setup.marketConfigFile', null, 'marketConfigFile', 'input_document', 'todays_market', 'todaysmarket.xml', 385, 413, 'A configuration document the run reads, named by marketConfigFile in the Setup block.', 7),
        ('setup.pricingEnginesFile', null, 'pricingEnginesFile', 'input_document', 'pricing_engines', 'pricingengine.xml', 402, 416, 'A configuration document the run reads, named by pricingEnginesFile in the Setup block.', 8),
        ('setup.pricingEnginesFileScenario', null, 'pricingEnginesFileScenario', 'input_document', null, 'pricingengine.xml', 3, 3, 'A configuration document the run reads, named by pricingEnginesFileScenario in the Setup block.', 9),
        ('setup.referenceDataFile', null, 'referenceDataFile', 'input_document', null, 'referencedata.xml', 6, 13, 'A configuration document the run reads, named by referenceDataFile in the Setup block.', 10),
        ('setup.scriptLibrary', null, 'scriptLibrary', 'input_document', null, 'scriptlibrary.xml', 45, 46, 'A configuration document the run reads, named by scriptLibrary in the Setup block.', 11),
        ('setup.fixingDataFile', null, 'fixingDataFile', 'data_file', null, 'fixings.txt', 62, 395, 'A data file the run reads, named by fixingDataFile in the Setup block.', 12),
        ('setup.marketDataFile', null, 'marketDataFile', 'data_file', null, 'market.txt', 75, 413, 'A data file the run reads, named by marketDataFile in the Setup block.', 13),
        ('setup.portfolioFile', null, 'portfolioFile', 'data_file', null, 'portfolio.xml', 173, 388, 'A data file the run reads, named by portfolioFile in the Setup block.', 14),
        ('amc.pricingEnginesFile', 'amc', 'pricingEnginesFile', 'input_document', null, 'pricingengine.xml', 1, 1, 'A configuration document the run reads, named by pricingEnginesFile in the amc analytic.', 15),
        ('bacva.collateralBalancesFile', 'bacva', 'collateralBalancesFile', 'input_document', null, 'collateralbalances.xml', 4, 4, 'A configuration document the run reads, named by collateralBalancesFile in the bacva analytic.', 16),
        ('bacva.csaFile', 'bacva', 'csaFile', 'input_document', null, 'netting.xml', 4, 4, 'A configuration document the run reads, named by csaFile in the bacva analytic.', 17),
        ('calibration.configFile', 'calibration', 'configFile', 'input_document', null, 'simulation.xml', 8, 12, 'A configuration document the run reads, named by configFile in the calibration analytic.', 18),
        ('correlation.sensitivityConfigFile', 'correlation', 'sensitivityConfigFile', 'input_document', null, 'sensitivity.xml', 1, 1, 'A configuration document the run reads, named by sensitivityConfigFile in the correlation analytic.', 19),
        ('crif.marketConfigFile', 'crif', 'marketConfigFile', 'input_document', null, 'crifmarket.xml', 2, 2, 'A configuration document the run reads, named by marketConfigFile in the crif analytic.', 20),
        ('crif.sensitivityConfigFile', 'crif', 'sensitivityConfigFile', 'input_document', null, 'sensitivity.xml', 2, 2, 'A configuration document the run reads, named by sensitivityConfigFile in the crif analytic.', 21),
        ('historicalSimulationVar.sensitivityConfigFile', 'historicalSimulationVar', 'sensitivityConfigFile', 'input_document', null, 'sensitivity.xml', 1, 1, 'A configuration document the run reads, named by sensitivityConfigFile in the historicalSimulationVar analytic.', 22),
        ('historicalSimulationVar.simulationConfigFile', 'historicalSimulationVar', 'simulationConfigFile', 'input_document', null, 'simulation.xml', 4, 5, 'A configuration document the run reads, named by simulationConfigFile in the historicalSimulationVar analytic.', 23),
        ('parStressConversion.marketConfigFile', 'parStressConversion', 'marketConfigFile', 'input_document', null, 'simulation.xml', 1, 1, 'A configuration document the run reads, named by marketConfigFile in the parStressConversion analytic.', 24),
        ('parStressConversion.pricingEnginesFile', 'parStressConversion', 'pricingEnginesFile', 'input_document', null, 'pricingengine.xml', 1, 1, 'A configuration document the run reads, named by pricingEnginesFile in the parStressConversion analytic.', 25),
        ('parStressConversion.sensitivityConfigFile', 'parStressConversion', 'sensitivityConfigFile', 'input_document', null, 'sensitivity.xml', 1, 1, 'A configuration document the run reads, named by sensitivityConfigFile in the parStressConversion analytic.', 26),
        ('parStressConversion.stressConfigFile', 'parStressConversion', 'stressConfigFile', 'input_document', null, 'parstresstest.xml', 1, 1, 'A configuration document the run reads, named by stressConfigFile in the parStressConversion analytic.', 27),
        ('parStressConversion.stressZeroScenarioDataFile', 'parStressConversion', 'stressZeroScenarioDataFile', 'input_document', null, 'results.xml', 1, 1, 'A configuration document the run reads, named by stressZeroScenarioDataFile in the parStressConversion analytic.', 28),
        ('pfe.csaFile', 'pfe', 'csaFile', 'input_document', null, 'netting.xml', 1, 1, 'A configuration document the run reads, named by csaFile in the pfe analytic.', 29),
        ('pnl.conventionsMporFile', 'pnl', 'conventionsMporFile', 'input_document', null, 'conventions.xml', 3, 3, 'A configuration document the run reads, named by conventionsMporFile in the pnl analytic.', 30),
        ('pnl.curveConfigMporFile', 'pnl', 'curveConfigMporFile', 'input_document', null, 'curveconfig.xml', 3, 3, 'A configuration document the run reads, named by curveConfigMporFile in the pnl analytic.', 31),
        ('pnl.simulationConfigFile', 'pnl', 'simulationConfigFile', 'input_document', null, 'simulation.xml', 3, 3, 'A configuration document the run reads, named by simulationConfigFile in the pnl analytic.', 32),
        ('pnlExplain.conventionsMporFile', 'pnlExplain', 'conventionsMporFile', 'input_document', null, 'conventions.xml', 5, 5, 'A configuration document the run reads, named by conventionsMporFile in the pnlExplain analytic.', 33),
        ('pnlExplain.curveConfigMporFile', 'pnlExplain', 'curveConfigMporFile', 'input_document', null, 'curveconfig.xml', 5, 5, 'A configuration document the run reads, named by curveConfigMporFile in the pnlExplain analytic.', 34),
        ('pnlExplain.sensitivityConfigFile', 'pnlExplain', 'sensitivityConfigFile', 'input_document', null, 'sensitivity.xml', 5, 5, 'A configuration document the run reads, named by sensitivityConfigFile in the pnlExplain analytic.', 35),
        ('pnlExplain.simulationConfigFile', 'pnlExplain', 'simulationConfigFile', 'input_document', null, 'simulation.xml', 5, 5, 'A configuration document the run reads, named by simulationConfigFile in the pnlExplain analytic.', 36),
        ('saccr.collateralBalancesFile', 'saccr', 'collateralBalancesFile', 'input_document', null, 'collateralbalances.xml', 3, 3, 'A configuration document the run reads, named by collateralBalancesFile in the saccr analytic.', 37),
        ('saccr.csaFile', 'saccr', 'csaFile', 'input_document', null, 'netting.xml', 3, 3, 'A configuration document the run reads, named by csaFile in the saccr analytic.', 38),
        ('scenario.simulationConfigFile', 'scenario', 'simulationConfigFile', 'input_document', null, 'simulation.xml', 3, 3, 'A configuration document the run reads, named by simulationConfigFile in the scenario analytic.', 39),
        ('scenarioGeneration.simulationConfigFile', 'scenarioGeneration', 'simulationConfigFile', 'input_document', null, 'stressmarket.xml', 1, 3, 'A configuration document the run reads, named by simulationConfigFile in the scenarioGeneration analytic.', 40),
        ('scenarioGeneration.stressConfigFile', 'scenarioGeneration', 'stressConfigFile', 'input_document', null, 'stresstest.xml', 1, 1, 'A configuration document the run reads, named by stressConfigFile in the scenarioGeneration analytic.', 41),
        ('sensitivity.marketConfigFile', 'sensitivity', 'marketConfigFile', 'input_document', null, 'simulation.xml', 63, 76, 'A configuration document the run reads, named by marketConfigFile in the sensitivity analytic.', 42),
        ('sensitivity.pricingEnginesFile', 'sensitivity', 'pricingEnginesFile', 'input_document', null, 'pricingengine.xml', 69, 76, 'A configuration document the run reads, named by pricingEnginesFile in the sensitivity analytic.', 43),
        ('sensitivity.sensitivityConfigFile', 'sensitivity', 'sensitivityConfigFile', 'input_document', null, 'sensitivity.xml', 70, 76, 'A configuration document the run reads, named by sensitivityConfigFile in the sensitivity analytic.', 44),
        ('sensitivity.stressConfigFile', 'sensitivity', 'stressConfigFile', 'input_document', null, 'stresstest.xml', 1, 4, 'A configuration document the run reads, named by stressConfigFile in the sensitivity analytic.', 45),
        ('sensitivity.stressZeroScenarioDataFile', 'sensitivity', 'stressZeroScenarioDataFile', 'input_document', null, 'zeroStressScenarioData.xml', 1, 1, 'A configuration document the run reads, named by stressZeroScenarioDataFile in the sensitivity analytic.', 46),
        ('sensitivityStress.marketConfigFile', 'sensitivityStress', 'marketConfigFile', 'input_document', null, 'sensitivitystressmarket.xml', 1, 1, 'A configuration document the run reads, named by marketConfigFile in the sensitivityStress analytic.', 47),
        ('sensitivityStress.sensitivityConfigFile', 'sensitivityStress', 'sensitivityConfigFile', 'input_document', null, 'sensitivitystress.xml', 1, 1, 'A configuration document the run reads, named by sensitivityConfigFile in the sensitivityStress analytic.', 48),
        ('sensitivityStress.stressConfigFile', 'sensitivityStress', 'stressConfigFile', 'input_document', null, 'stresstest_sensi.xml', 1, 1, 'A configuration document the run reads, named by stressConfigFile in the sensitivityStress analytic.', 49),
        ('simulation.amcCgPricingEnginesFile', 'simulation', 'amcCgPricingEnginesFile', 'input_document', null, 'pricingengine_amccg.xml', 19, 19, 'A configuration document the run reads, named by amcCgPricingEnginesFile in the simulation analytic.', 50),
        ('simulation.amcPricingEnginesFile', 'simulation', 'amcPricingEnginesFile', 'input_document', null, 'pricingengine_amc.xml', 53, 65, 'A configuration document the run reads, named by amcPricingEnginesFile in the simulation analytic.', 51),
        ('simulation.pricingEnginesFile', 'simulation', 'pricingEnginesFile', 'input_document', null, 'pricingengine.xml', 248, 261, 'A configuration document the run reads, named by pricingEnginesFile in the simulation analytic.', 52),
        ('simulation.simulationConfigFile', 'simulation', 'simulationConfigFile', 'input_document', null, 'simulation.xml', 128, 257, 'A configuration document the run reads, named by simulationConfigFile in the simulation analytic.', 53),
        ('simulation.simulationMarketFile', 'simulation', 'simulationMarketFile', 'input_document', null, 'simulationmarket.xml', 5, 5, 'A configuration document the run reads, named by simulationMarketFile in the simulation analytic.', 54),
        ('simulation.simulationModelFile', 'simulation', 'simulationModelFile', 'input_document', null, 'simulationmodels.xml', 5, 5, 'A configuration document the run reads, named by simulationModelFile in the simulation analytic.', 55),
        ('simulation.simulationParamFile', 'simulation', 'simulationParamFile', 'input_document', null, 'simulationparams.xml', 5, 5, 'A configuration document the run reads, named by simulationParamFile in the simulation analytic.', 56),
        ('simulation.xvaCgSensitivityConfigFile', 'simulation', 'xvaCgSensitivityConfigFile', 'input_document', null, 'xvasensiconfig.xml', 6, 6, 'A configuration document the run reads, named by xvaCgSensitivityConfigFile in the simulation analytic.', 57),
        ('stress.marketConfigFile', 'stress', 'marketConfigFile', 'input_document', null, 'simulation.xml', 14, 20, 'A configuration document the run reads, named by marketConfigFile in the stress analytic.', 58),
        ('stress.pricingEnginesFile', 'stress', 'pricingEnginesFile', 'input_document', null, 'pricingengine.xml', 17, 17, 'A configuration document the run reads, named by pricingEnginesFile in the stress analytic.', 59),
        ('stress.sensitivityConfigFile', 'stress', 'sensitivityConfigFile', 'input_document', null, 'sensitivity.xml', 7, 8, 'A configuration document the run reads, named by sensitivityConfigFile in the stress analytic.', 60),
        ('stress.stressConfigFile', 'stress', 'stressConfigFile', 'input_document', null, 'stresstest.xml', 15, 19, 'A configuration document the run reads, named by stressConfigFile in the stress analytic.', 61),
        ('stress.stressZeroScenarioDataFile', 'stress', 'stressZeroScenarioDataFile', 'input_document', null, 'zeroStressScenarioData.xml', 3, 4, 'A configuration document the run reads, named by stressZeroScenarioDataFile in the stress analytic.', 62),
        ('xva.collateralBalancesFile', 'xva', 'collateralBalancesFile', 'input_document', null, 'collateralbalances.xml', 23, 23, 'A configuration document the run reads, named by collateralBalancesFile in the xva analytic.', 63),
        ('xva.creditMigrationConfig', 'xva', 'creditMigrationConfig', 'input_document', null, 'creditsimulation.xml', 2, 14, 'A configuration document the run reads, named by creditMigrationConfig in the xva analytic.', 64),
        ('xva.csaFile', 'xva', 'csaFile', 'input_document', null, 'netting.xml', 213, 252, 'A configuration document the run reads, named by csaFile in the xva analytic.', 65),
        ('xva.sensitivityConfigFile', 'xva', 'sensitivityConfigFile', 'input_document', null, 'sensitivity.xml', 1, 1, 'A configuration document the run reads, named by sensitivityConfigFile in the xva analytic.', 66),
        ('xva.simulationConfigFile', 'xva', 'simulationConfigFile', 'input_document', null, 'simulation_ccs.xml', 1, 1, 'A configuration document the run reads, named by simulationConfigFile in the xva analytic.', 67),
        ('xvaExplain.marketConfigFile', 'xvaExplain', 'marketConfigFile', 'input_document', null, 'simulation.xml', 1, 2, 'A configuration document the run reads, named by marketConfigFile in the xvaExplain analytic.', 68),
        ('xvaExplain.sensitivityConfigFile', 'xvaExplain', 'sensitivityConfigFile', 'input_document', null, 'sensitivity_stress.xml', 1, 2, 'A configuration document the run reads, named by sensitivityConfigFile in the xvaExplain analytic.', 69),
        ('xvaExplain.stressConfigFile', 'xvaExplain', 'stressConfigFile', 'input_document', null, 'stresstest.xml', 1, 1, 'A configuration document the run reads, named by stressConfigFile in the xvaExplain analytic.', 70),
        ('xvaSensitivity.marketConfigFile', 'xvaSensitivity', 'marketConfigFile', 'input_document', null, 'xvasensimarket.xml', 5, 5, 'A configuration document the run reads, named by marketConfigFile in the xvaSensitivity analytic.', 71),
        ('xvaSensitivity.sensitivityConfigFile', 'xvaSensitivity', 'sensitivityConfigFile', 'input_document', null, 'sensitivity.xml', 5, 5, 'A configuration document the run reads, named by sensitivityConfigFile in the xvaSensitivity analytic.', 72),
        ('xvaStress.marketConfigFile', 'xvaStress', 'marketConfigFile', 'input_document', null, 'simulation.xml', 3, 7, 'A configuration document the run reads, named by marketConfigFile in the xvaStress analytic.', 73),
        ('xvaStress.sensitivityConfigFile', 'xvaStress', 'sensitivityConfigFile', 'input_document', null, 'sensitivity_stress.xml', 3, 3, 'A configuration document the run reads, named by sensitivityConfigFile in the xvaStress analytic.', 74),
        ('xvaStress.stressConfigFile', 'xvaStress', 'stressConfigFile', 'input_document', null, 'stresstest.xml', 7, 7, 'A configuration document the run reads, named by stressConfigFile in the xvaStress analytic.', 75),
        ('zeroToParSensiConversion.marketConfigFile', 'zeroToParSensiConversion', 'marketConfigFile', 'input_document', null, 'simulation.xml', 3, 3, 'A configuration document the run reads, named by marketConfigFile in the zeroToParSensiConversion analytic.', 76),
        ('zeroToParSensiConversion.pricingEnginesFile', 'zeroToParSensiConversion', 'pricingEnginesFile', 'input_document', null, 'pricingengine.xml', 3, 3, 'A configuration document the run reads, named by pricingEnginesFile in the zeroToParSensiConversion analytic.', 77),
        ('zeroToParSensiConversion.sensitivityConfigFile', 'zeroToParSensiConversion', 'sensitivityConfigFile', 'input_document', null, 'sensitivity.xml', 3, 3, 'A configuration document the run reads, named by sensitivityConfigFile in the zeroToParSensiConversion analytic.', 78),
        ('zeroToParShift.marketConfigFile', 'zeroToParShift', 'marketConfigFile', 'input_document', null, 'simulation.xml', 2, 2, 'A configuration document the run reads, named by marketConfigFile in the zeroToParShift analytic.', 79),
        ('zeroToParShift.pricingEnginesFile', 'zeroToParShift', 'pricingEnginesFile', 'input_document', null, 'pricingengine.xml', 2, 2, 'A configuration document the run reads, named by pricingEnginesFile in the zeroToParShift analytic.', 80),
        ('zeroToParShift.sensitivityConfigFile', 'zeroToParShift', 'sensitivityConfigFile', 'input_document', null, 'sensitivity.xml', 2, 2, 'A configuration document the run reads, named by sensitivityConfigFile in the zeroToParShift analytic.', 81),
        ('zeroToParShift.stressConfigFile', 'zeroToParShift', 'stressConfigFile', 'input_document', null, 'stresstest.xml', 2, 2, 'A configuration document the run reads, named by stressConfigFile in the zeroToParShift analytic.', 82),
        ('pnl.portfolioMporFile', 'pnl', 'portfolioMporFile', 'data_file', null, 'mporportfolio.xml', 3, 3, 'A data file the run reads, named by portfolioMporFile in the pnl analytic.', 83),
        ('pnlExplain.portfolioMporFile', 'pnlExplain', 'portfolioMporFile', 'data_file', null, 'mporportfolio.xml', 5, 5, 'A data file the run reads, named by portfolioMporFile in the pnlExplain analytic.', 84),
        ('portfolioDetails.marketObjectFileName', 'portfolioDetails', 'marketObjectFileName', 'data_file', null, 'marketObjects.csv', 4, 4, 'A data file the run reads, named by marketObjectFileName in the portfolioDetails analytic.', 85),
        ('portfolioDetails.riskFactorFileName', 'portfolioDetails', 'riskFactorFileName', 'data_file', null, 'riskFactors.csv', 4, 4, 'A data file the run reads, named by riskFactorFileName in the portfolioDetails analytic.', 86),
        ('sacva.cvaSensitivitiesFile', 'sacva', 'cvaSensitivitiesFile', 'data_file', null, 'xva_par_sensitivity_cva.csv', 3, 3, 'A data file the run reads, named by cvaSensitivitiesFile in the sacva analytic.', 87),
        ('sacva.saCvaNetSensitivitiesFile', 'sacva', 'saCvaNetSensitivitiesFile', 'data_file', null, 'sacva_sensitivity.csv', 3, 3, 'A data file the run reads, named by saCvaNetSensitivitiesFile in the sacva analytic.', 88),
        ('simulation.aggregationScenarioDataFileName', 'simulation', 'aggregationScenarioDataFileName', 'data_file', null, 'scenariodata.csv', 1, 1, 'A data file the run reads, named by aggregationScenarioDataFileName in the simulation analytic.', 89),
        ('xva.deterministicInitialMarginFile', 'xva', 'deterministicInitialMarginFile', 'data_file', null, 'dim_evolution.csv', 1, 1, 'A data file the run reads, named by deterministicInitialMarginFile in the xva analytic.', 90),
        ('xva.dimEvolutionFile', 'xva', 'dimEvolutionFile', 'data_file', null, 'dim_evolution.csv', 71, 93, 'A data file the run reads, named by dimEvolutionFile in the xva analytic.', 91),
        ('xva.dimRegressionFiles', 'xva', 'dimRegressionFiles', 'data_file', null, 'dim_regression.csv', 58, 87, 'A data file the run reads, named by dimRegressionFiles in the xva analytic.', 92),
        ('zeroToParShift.parShiftsFile', 'zeroToParShift', 'parShiftsFile', 'data_file', null, 'parshifts.csv', 2, 2, 'A data file the run reads, named by parShiftsFile in the zeroToParShift analytic.', 93),
        ('calibration.scenarioInputFile', 'calibration', 'scenarioInputFile', 'intermediate_file', null, 'scenario.csv', 1, 1, 'A file one analytic writes and another reads, named by scenarioInputFile in the calibration analytic.', 94),
        ('correlation.historicalScenarioFile', 'correlation', 'historicalScenarioFile', 'intermediate_file', null, 'scenarios.csv', 1, 1, 'A file one analytic writes and another reads, named by historicalScenarioFile in the correlation analytic.', 95),
        ('historicalSimulationVar.historicalScenarioFile', 'historicalSimulationVar', 'historicalScenarioFile', 'intermediate_file', null, 'scenarios.csv', 4, 5, 'A file one analytic writes and another reads, named by historicalScenarioFile in the historicalSimulationVar analytic.', 96),
        ('historicalSimulationVar.sensitivityInputFile', 'historicalSimulationVar', 'sensitivityInputFile', 'intermediate_file', null, 'sensitivity.csv', 1, 1, 'A file one analytic writes and another reads, named by sensitivityInputFile in the historicalSimulationVar analytic.', 97),
        ('parametricVar.covarianceInputFile', 'parametricVar', 'covarianceInputFile', 'intermediate_file', null, 'covariance.csv', 3, 3, 'A file one analytic writes and another reads, named by covarianceInputFile in the parametricVar analytic.', 98),
        ('parametricVar.sensitivityInputFile', 'parametricVar', 'sensitivityInputFile', 'intermediate_file', null, 'sensitivity.csv', 3, 3, 'A file one analytic writes and another reads, named by sensitivityInputFile in the parametricVar analytic.', 99),
        ('simulation.scenarioFile', 'simulation', 'scenarioFile', 'intermediate_file', null, 'scenarios.csv', 1, 1, 'A file one analytic writes and another reads, named by scenarioFile in the simulation analytic.', 100),
        ('stress.scenarioFile', 'stress', 'scenarioFile', 'intermediate_file', null, 'stress_scenarios.csv', 1, 1, 'A file one analytic writes and another reads, named by scenarioFile in the stress analytic.', 101),
        ('xva.correlationInputFile', 'xva', 'correlationInputFile', 'intermediate_file', null, 'correlation.csv', 1, 1, 'A file one analytic writes and another reads, named by correlationInputFile in the xva analytic.', 102),
        ('xva.historicalScenarioFile', 'xva', 'historicalScenarioFile', 'intermediate_file', null, 'scenarios.csv', 1, 1, 'A file one analytic writes and another reads, named by historicalScenarioFile in the xva analytic.', 103),
        ('zeroToParSensiConversion.sensitivityInputFile', 'zeroToParSensiConversion', 'sensitivityInputFile', 'intermediate_file', null, 'sensitivity.csv', 3, 3, 'A file one analytic writes and another reads, named by sensitivityInputFile in the zeroToParSensiConversion analytic.', 104),
        ('additionalResults.outputFileName', 'additionalResults', 'outputFileName', 'output_file', null, 'additional_results.csv', 5, 7, 'A file the run writes, named by outputFileName in the additionalResults analytic.', 105),
        ('calibration.meanReversionOutputFileName', 'calibration', 'meanReversionOutputFileName', 'output_file', null, 'meanReversion.csv', 2, 2, 'A file the run writes, named by meanReversionOutputFileName in the calibration analytic.', 106),
        ('calibration.outputFile', 'calibration', 'outputFile', 'output_file', null, 'calibration.csv', 12, 12, 'A file the run writes, named by outputFile in the calibration analytic.', 107),
        ('calibration.pcaOutputFileName', 'calibration', 'pcaOutputFileName', 'output_file', null, 'pca.csv', 1, 1, 'A file the run writes, named by pcaOutputFileName in the calibration analytic.', 108),
        ('cashflow.outputFileName', 'cashflow', 'outputFileName', 'output_file', null, 'flows.csv', 314, 336, 'A file the run writes, named by outputFileName in the cashflow analytic.', 109),
        ('correlation.outputFile', 'correlation', 'outputFile', 'output_file', null, 'correlation.csv', 1, 1, 'A file the run writes, named by outputFile in the correlation analytic.', 110),
        ('creditcurves.outputFileName', 'creditcurves', 'outputFileName', 'output_file', null, 'creditcurves.csv', 15, 15, 'A file the run writes, named by outputFileName in the creditcurves analytic.', 111),
        ('crif.crifOutputFile', 'crif', 'crifOutputFile', 'output_file', null, 'crif.csv', 2, 2, 'A file the run writes, named by crifOutputFile in the crif analytic.', 112),
        ('curves.outputFileName', 'curves', 'outputFileName', 'output_file', null, 'curves.csv', 315, 331, 'A file the run writes, named by outputFileName in the curves analytic.', 113),
        ('historicalSimulationVar.outputFile', 'historicalSimulationVar', 'outputFile', 'output_file', null, 'var.csv', 5, 5, 'A file the run writes, named by outputFile in the historicalSimulationVar analytic.', 114),
        ('npv.outputFileName', 'npv', 'outputFileName', 'output_file', null, 'npv.csv', 333, 368, 'A file the run writes, named by outputFileName in the npv analytic.', 115),
        ('parStressConversion.scenarioOutputFile', 'parStressConversion', 'scenarioOutputFile', 'output_file', null, 'stresstest.csv', 1, 1, 'A file the run writes, named by scenarioOutputFile in the parStressConversion analytic.', 116),
        ('parametricVar.outputFile', 'parametricVar', 'outputFile', 'output_file', null, 'var.csv', 3, 3, 'A file the run writes, named by outputFile in the parametricVar analytic.', 117),
        ('pfe.netCubeOutputFile', 'pfe', 'netCubeOutputFile', 'output_file', null, 'netcube.csv', 1, 1, 'A file the run writes, named by netCubeOutputFile in the pfe analytic.', 118),
        ('pfe.rawCubeOutputFile', 'pfe', 'rawCubeOutputFile', 'output_file', null, 'rawcube.csv', 1, 1, 'A file the run writes, named by rawCubeOutputFile in the pfe analytic.', 119),
        ('pnl.outputFileName', 'pnl', 'outputFileName', 'output_file', null, 'pnl.csv', 3, 3, 'A file the run writes, named by outputFileName in the pnl analytic.', 120),
        ('pnlExplain.outputFileName', 'pnlExplain', 'outputFileName', 'output_file', null, 'pnl_explain.csv', 5, 5, 'A file the run writes, named by outputFileName in the pnlExplain analytic.', 121),
        ('scenario.scenarioOutputFile', 'scenario', 'scenarioOutputFile', 'output_file', null, 'scenario.csv', 3, 3, 'A file the run writes, named by scenarioOutputFile in the scenario analytic.', 122),
        ('scenarioGeneration.scenarioOutputFile', 'scenarioGeneration', 'scenarioOutputFile', 'output_file', null, 'stress_scenarios.csv', 1, 3, 'A file the run writes, named by scenarioOutputFile in the scenarioGeneration analytic.', 123),
        ('sensitivity.crossGammaOutputFile', 'sensitivity', 'crossGammaOutputFile', 'output_file', null, 'crossgamma.csv', 22, 31, 'A file the run writes, named by crossGammaOutputFile in the sensitivity analytic.', 124),
        ('sensitivity.jacobiInverseOutputFile', 'sensitivity', 'jacobiInverseOutputFile', 'output_file', null, 'jacobi_inverse.csv', 26, 26, 'A file the run writes, named by jacobiInverseOutputFile in the sensitivity analytic.', 125),
        ('sensitivity.jacobiOutputFile', 'sensitivity', 'jacobiOutputFile', 'output_file', null, 'jacobi.csv', 26, 26, 'A file the run writes, named by jacobiOutputFile in the sensitivity analytic.', 126),
        ('sensitivity.parRateSensitivityOutputFile', 'sensitivity', 'parRateSensitivityOutputFile', 'output_file', null, 'parsensi.csv', 16, 16, 'A file the run writes, named by parRateSensitivityOutputFile in the sensitivity analytic.', 127),
        ('sensitivity.parSensitivityOutputFile', 'sensitivity', 'parSensitivityOutputFile', 'output_file', null, 'parsensitivity.csv', 17, 26, 'A file the run writes, named by parSensitivityOutputFile in the sensitivity analytic.', 128),
        ('sensitivity.productClassMappingOutputFile', 'sensitivity', 'productClassMappingOutputFile', 'output_file', null, 'productclass_mapping.csv', 2, 2, 'A file the run writes, named by productClassMappingOutputFile in the sensitivity analytic.', 129),
        ('sensitivity.scenarioOutputFile', 'sensitivity', 'scenarioOutputFile', 'output_file', null, 'scenario.csv', 54, 73, 'A file the run writes, named by scenarioOutputFile in the sensitivity analytic.', 130),
        ('sensitivity.sensitivityOutputFile', 'sensitivity', 'sensitivityOutputFile', 'output_file', null, 'sensitivity.csv', 57, 70, 'A file the run writes, named by sensitivityOutputFile in the sensitivity analytic.', 131),
        ('stress.scenarioCashflowOutputFile', 'stress', 'scenarioCashflowOutputFile', 'output_file', null, 'stresstest_flows.csv', 5, 5, 'A file the run writes, named by scenarioCashflowOutputFile in the stress analytic.', 132),
        ('stress.scenarioOutputFile', 'stress', 'scenarioOutputFile', 'output_file', null, 'stresstest.csv', 14, 17, 'A file the run writes, named by scenarioOutputFile in the stress analytic.', 133),
        ('xva.netCubeOutputFile', 'xva', 'netCubeOutputFile', 'output_file', null, 'netcube.csv', 230, 230, 'A file the run writes, named by netCubeOutputFile in the xva analytic.', 134),
        ('xva.rawCubeOutputFile', 'xva', 'rawCubeOutputFile', 'output_file', null, 'rawcube.csv', 229, 229, 'A file the run writes, named by rawCubeOutputFile in the xva analytic.', 135),
        ('xva.timeAveragedNettedExposureOutputFile', 'xva', 'timeAveragedNettedExposureOutputFile', 'output_file', null, 'timeaveragedexposure.csv', 1, 1, 'A file the run writes, named by timeAveragedNettedExposureOutputFile in the xva analytic.', 136),
        ('xvaSensitivity.jacobiInverseOutputFile', 'xvaSensitivity', 'jacobiInverseOutputFile', 'output_file', null, 'xva_sensitivity_jacobi_inverse.csv', 5, 5, 'A file the run writes, named by jacobiInverseOutputFile in the xvaSensitivity analytic.', 137),
        ('xvaSensitivity.jacobiOutputFile', 'xvaSensitivity', 'jacobiOutputFile', 'output_file', null, 'xva_sensitivity_jacobi.csv', 5, 5, 'A file the run writes, named by jacobiOutputFile in the xvaSensitivity analytic.', 138),
        ('zeroToParSensiConversion.jacobiInverseOutputFile', 'zeroToParSensiConversion', 'jacobiInverseOutputFile', 'output_file', null, 'jacobi_inverse.csv', 3, 3, 'A file the run writes, named by jacobiInverseOutputFile in the zeroToParSensiConversion analytic.', 139),
        ('zeroToParSensiConversion.jacobiOutputFile', 'zeroToParSensiConversion', 'jacobiOutputFile', 'output_file', null, 'jacobi.csv', 3, 3, 'A file the run writes, named by jacobiOutputFile in the zeroToParSensiConversion analytic.', 140),
        ('zeroToParSensiConversion.outputFile', 'zeroToParSensiConversion', 'outputFile', 'output_file', null, 'parconversion_sensitivity.csv', 3, 3, 'A file the run writes, named by outputFile in the zeroToParSensiConversion analytic.', 141),
        ('zeroToParShift.scenarioOutputFile', 'zeroToParShift', 'scenarioOutputFile', 'output_file', null, 'stresstest.csv', 2, 2, 'A file the run writes, named by scenarioOutputFile in the zeroToParShift analytic.', 142)
        ) as t (code, analytic_type_code, ore_parameter, kind, configuration_type_code,
                name_pattern, sample_count, sample_total, description, display_order)
    loop
        if not exists (
            select 1 from ores_reporting_file_types_tbl
            where tenant_id = v_sys_tenant and code = r.code
              and valid_to = ores_utility_infinity_timestamp_fn()
        ) then
            insert into ores_reporting_file_types_tbl (
                code, tenant_id, version,
                analytic_type_code, ore_parameter, kind, configuration_type_code,
                name_pattern, sample_count, sample_total, description, display_order,
                modified_by, change_reason_code, change_commentary
            ) values (
                r.code, v_sys_tenant, 0,
                r.analytic_type_code, r.ore_parameter, r.kind, r.configuration_type_code,
                r.name_pattern, r.sample_count, r.sample_total, r.description, r.display_order,
                current_user, 'system.initial_load', 'Seed file type: ' || r.code
            );
            raise debug 'Created file type: %', r.code;
        else
            raise debug 'File type already exists: %', r.code;
        end if;
    end loop;
end $$;
