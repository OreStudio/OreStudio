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
 * Parameter Definitions Population Script
 *
 * Seeds one definition per (scope, subtype, name) the shipped run documents
 * use: scope is the analytic, subtype is the analytic type -- npv, xva and the
 * rest -- and name is the parameter. The value domain is read from the values
 * the corpus writes for that pair. Positions count across the whole seed.
 * This script is idempotent.
 */

\echo '--- Parameter Definitions ---'

do $$
declare
    v_sys_tenant uuid := ores_utility_system_tenant_id_fn();
begin
    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'npv' and name = 'active'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'npv', 'active', 1, 'boolean', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: npv.active'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'npv' and name = 'baseCurrency'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'npv', 'baseCurrency', 2, 'string', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: npv.baseCurrency'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'npv' and name = 'outputFileName'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'npv', 'outputFileName', 3, 'string', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: npv.outputFileName'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'cashflow' and name = 'active'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'cashflow', 'active', 4, 'boolean', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: cashflow.active'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'cashflow' and name = 'outputFileName'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'cashflow', 'outputFileName', 5, 'string', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: cashflow.outputFileName'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'curves' and name = 'active'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'curves', 'active', 6, 'boolean', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: curves.active'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'curves' and name = 'configuration'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'curves', 'configuration', 7, 'string', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: curves.configuration'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'curves' and name = 'grid'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'curves', 'grid', 8, 'string', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: curves.grid'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'curves' and name = 'outputFileName'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'curves', 'outputFileName', 9, 'string', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: curves.outputFileName'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'simulation' and name = 'active'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'simulation', 'active', 10, 'boolean', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: simulation.active'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'simulation' and name = 'amc'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'simulation', 'amc', 11, 'boolean', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: simulation.amc'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'simulation' and name = 'amcTradeTypes'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'simulation', 'amcTradeTypes', 12, 'string', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: simulation.amcTradeTypes'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'simulation' and name = 'simulationConfigFile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'simulation', 'simulationConfigFile', 13, 'string', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: simulation.simulationConfigFile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'simulation' and name = 'pricingEnginesFile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'simulation', 'pricingEnginesFile', 14, 'string', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: simulation.pricingEnginesFile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'simulation' and name = 'amcPricingEnginesFile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'simulation', 'amcPricingEnginesFile', 15, 'string', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: simulation.amcPricingEnginesFile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'simulation' and name = 'baseCurrency'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'simulation', 'baseCurrency', 16, 'string', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: simulation.baseCurrency'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'simulation' and name = 'storeScenarios'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'simulation', 'storeScenarios', 17, 'boolean', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: simulation.storeScenarios'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'simulation' and name = 'cubeFile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'simulation', 'cubeFile', 18, 'string', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: simulation.cubeFile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'simulation' and name = 'aggregationScenarioDataFileName'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'simulation', 'aggregationScenarioDataFileName', 19, 'string', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: simulation.aggregationScenarioDataFileName'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'simulation' and name = 'aggregationScenarioDataDump'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'simulation', 'aggregationScenarioDataDump', 20, 'string', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: simulation.aggregationScenarioDataDump'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'simulation' and name = 'allowPartialScenarios'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'simulation', 'allowPartialScenarios', 21, 'boolean', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: simulation.allowPartialScenarios'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'xva' and name = 'active'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'xva', 'active', 22, 'boolean', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: xva.active'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'xva' and name = 'csaFile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'xva', 'csaFile', 23, 'string', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: xva.csaFile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'xva' and name = 'cubeFile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'xva', 'cubeFile', 24, 'string', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: xva.cubeFile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'xva' and name = 'scenarioFile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'xva', 'scenarioFile', 25, 'string', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: xva.scenarioFile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'xva' and name = 'baseCurrency'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'xva', 'baseCurrency', 26, 'string', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: xva.baseCurrency'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'xva' and name = 'exposureProfiles'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'xva', 'exposureProfiles', 27, 'boolean', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: xva.exposureProfiles'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'xva' and name = 'exposureProfilesByTrade'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'xva', 'exposureProfilesByTrade', 28, 'boolean', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: xva.exposureProfilesByTrade'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'xva' and name = 'quantile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'xva', 'quantile', 29, 'double', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: xva.quantile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'xva' and name = 'calculationType'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'xva', 'calculationType', 30, 'string', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: xva.calculationType'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'xva' and name = 'cva'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'xva', 'cva', 31, 'boolean', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: xva.cva'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'xva' and name = 'rawCubeOutputFile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'xva', 'rawCubeOutputFile', 32, 'string', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: xva.rawCubeOutputFile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'xva' and name = 'netCubeOutputFile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'xva', 'netCubeOutputFile', 33, 'string', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: xva.netCubeOutputFile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'xva' and name = 'dim'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'xva', 'dim', 34, 'boolean', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: xva.dim'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'xva' and name = 'dimModel'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'xva', 'dimModel', 35, 'string', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: xva.dimModel'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'xva' and name = 'dimQuantile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'xva', 'dimQuantile', 36, 'double', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: xva.dimQuantile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'xva' and name = 'dimHorizonCalendarDays'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'xva', 'dimHorizonCalendarDays', 37, 'integer', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: xva.dimHorizonCalendarDays'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'xva' and name = 'dimRegressionOrder'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'xva', 'dimRegressionOrder', 38, 'integer', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: xva.dimRegressionOrder'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'xvaSensitivity' and name = 'active'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'xvaSensitivity', 'active', 39, 'boolean', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: xvaSensitivity.active'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'xvaSensitivity' and name = 'marketConfigFile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'xvaSensitivity', 'marketConfigFile', 40, 'string', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: xvaSensitivity.marketConfigFile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'xvaSensitivity' and name = 'sensitivityConfigFile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'xvaSensitivity', 'sensitivityConfigFile', 41, 'string', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: xvaSensitivity.sensitivityConfigFile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'xvaSensitivity' and name = 'outputSensitivityThreshold'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'xvaSensitivity', 'outputSensitivityThreshold', 42, 'double', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: xvaSensitivity.outputSensitivityThreshold'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'xvaSensitivity' and name = 'parSensitivity'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'xvaSensitivity', 'parSensitivity', 43, 'boolean', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: xvaSensitivity.parSensitivity'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'xvaSensitivity' and name = 'outputJacobi'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'xvaSensitivity', 'outputJacobi', 44, 'boolean', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: xvaSensitivity.outputJacobi'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'xvaSensitivity' and name = 'jacobiOutputFile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'xvaSensitivity', 'jacobiOutputFile', 45, 'string', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: xvaSensitivity.jacobiOutputFile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'xvaSensitivity' and name = 'jacobiInverseOutputFile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'xvaSensitivity', 'jacobiInverseOutputFile', 46, 'string', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: xvaSensitivity.jacobiInverseOutputFile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'xva' and name = 'allocationMethod'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'xva', 'allocationMethod', 47, 'string', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: xva.allocationMethod'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'xva' and name = 'marginalAllocationLimit'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'xva', 'marginalAllocationLimit', 48, 'double', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: xva.marginalAllocationLimit'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'xva' and name = 'exerciseNextBreak'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'xva', 'exerciseNextBreak', 49, 'boolean', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: xva.exerciseNextBreak'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'xva' and name = 'dva'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'xva', 'dva', 50, 'boolean', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: xva.dva'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'xva' and name = 'dvaName'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'xva', 'dvaName', 51, 'string', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: xva.dvaName'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'xva' and name = 'fva'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'xva', 'fva', 52, 'boolean', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: xva.fva'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'xva' and name = 'fvaBorrowingCurve'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'xva', 'fvaBorrowingCurve', 53, 'string', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: xva.fvaBorrowingCurve'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'xva' and name = 'fvaLendingCurve'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'xva', 'fvaLendingCurve', 54, 'string', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: xva.fvaLendingCurve'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'xva' and name = 'colva'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'xva', 'colva', 55, 'boolean', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: xva.colva'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'xva' and name = 'collateralSpread'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'xva', 'collateralSpread', 56, 'double', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: xva.collateralSpread'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'xva' and name = 'collateralFloor'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'xva', 'collateralFloor', 57, 'boolean', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: xva.collateralFloor'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'xva' and name = 'dimScaling'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'xva', 'dimScaling', 58, 'double', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: xva.dimScaling'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'xva' and name = 'dimEvolutionFile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'xva', 'dimEvolutionFile', 59, 'string', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: xva.dimEvolutionFile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'xva' and name = 'dimRegressionFiles'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'xva', 'dimRegressionFiles', 60, 'string', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: xva.dimRegressionFiles'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'xva' and name = 'dimOutputNettingSet'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'xva', 'dimOutputNettingSet', 61, 'string', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: xva.dimOutputNettingSet'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'xva' and name = 'dimOutputGridPoints'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'xva', 'dimOutputGridPoints', 62, 'integer', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: xva.dimOutputGridPoints'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'xva' and name = 'dimLocalRegressionEvaluations'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'xva', 'dimLocalRegressionEvaluations', 63, 'integer', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: xva.dimLocalRegressionEvaluations'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'xva' and name = 'dimLocalRegressionBandwidth'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'xva', 'dimLocalRegressionBandwidth', 64, 'double', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: xva.dimLocalRegressionBandwidth'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'xvaExplain' and name = 'active'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'xvaExplain', 'active', 65, 'boolean', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: xvaExplain.active'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'xvaExplain' and name = 'marketConfigFile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'xvaExplain', 'marketConfigFile', 66, 'string', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: xvaExplain.marketConfigFile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'xvaExplain' and name = 'sensitivityConfigFile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'xvaExplain', 'sensitivityConfigFile', 67, 'string', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: xvaExplain.sensitivityConfigFile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'xvaExplain' and name = 'writeCubes'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'xvaExplain', 'writeCubes', 68, 'boolean', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: xvaExplain.writeCubes'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'xvaExplain' and name = 'shiftThreshold'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'xvaExplain', 'shiftThreshold', 69, 'double', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: xvaExplain.shiftThreshold'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'xvaExplain' and name = 'mporDays'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'xvaExplain', 'mporDays', 70, 'integer', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: xvaExplain.mporDays'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'xvaExplain' and name = 'mporCalendar'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'xvaExplain', 'mporCalendar', 71, 'string', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: xvaExplain.mporCalendar'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'bacva' and name = 'active'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'bacva', 'active', 72, 'boolean', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: bacva.active'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'bacva' and name = 'simmVersion'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'bacva', 'simmVersion', 73, 'double', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: bacva.simmVersion'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'bacva' and name = 'csaFile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'bacva', 'csaFile', 74, 'string', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: bacva.csaFile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'bacva' and name = 'collateralBalancesFile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'bacva', 'collateralBalancesFile', 75, 'string', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: bacva.collateralBalancesFile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'simulation' and name = 'scenariodump'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'simulation', 'scenariodump', 76, 'string', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: simulation.scenariodump'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'xvaStress' and name = 'active'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'xvaStress', 'active', 77, 'boolean', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: xvaStress.active'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'xvaStress' and name = 'marketConfigFile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'xvaStress', 'marketConfigFile', 78, 'string', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: xvaStress.marketConfigFile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'xvaStress' and name = 'stressConfigFile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'xvaStress', 'stressConfigFile', 79, 'string', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: xvaStress.stressConfigFile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'xvaStress' and name = 'writeCubes'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'xvaStress', 'writeCubes', 80, 'boolean', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: xvaStress.writeCubes'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'sacva' and name = 'active'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'sacva', 'active', 81, 'boolean', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: sacva.active'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'zeroToParShift' and name = 'active'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'zeroToParShift', 'active', 82, 'boolean', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: zeroToParShift.active'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'zeroToParShift' and name = 'marketConfigFile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'zeroToParShift', 'marketConfigFile', 83, 'string', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: zeroToParShift.marketConfigFile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'zeroToParShift' and name = 'stressConfigFile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'zeroToParShift', 'stressConfigFile', 84, 'string', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: zeroToParShift.stressConfigFile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'zeroToParShift' and name = 'sensitivityConfigFile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'zeroToParShift', 'sensitivityConfigFile', 85, 'string', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: zeroToParShift.sensitivityConfigFile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'zeroToParShift' and name = 'pricingEnginesFile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'zeroToParShift', 'pricingEnginesFile', 86, 'string', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: zeroToParShift.pricingEnginesFile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'zeroToParShift' and name = 'scenarioOutputFile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'zeroToParShift', 'scenarioOutputFile', 87, 'string', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: zeroToParShift.scenarioOutputFile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'zeroToParShift' and name = 'parShiftsFile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'zeroToParShift', 'parShiftsFile', 88, 'string', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: zeroToParShift.parShiftsFile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'additionalResults' and name = 'active'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'additionalResults', 'active', 89, 'boolean', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: additionalResults.active'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'additionalResults' and name = 'outputFileName'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'additionalResults', 'outputFileName', 90, 'string', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: additionalResults.outputFileName'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'sensitivity' and name = 'active'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'sensitivity', 'active', 91, 'boolean', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: sensitivity.active'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'sensitivity' and name = 'marketConfigFile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'sensitivity', 'marketConfigFile', 92, 'string', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: sensitivity.marketConfigFile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'sensitivity' and name = 'sensitivityConfigFile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'sensitivity', 'sensitivityConfigFile', 93, 'string', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: sensitivity.sensitivityConfigFile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'sensitivity' and name = 'pricingEnginesFile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'sensitivity', 'pricingEnginesFile', 94, 'string', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: sensitivity.pricingEnginesFile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'sensitivity' and name = 'scenarioOutputFile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'sensitivity', 'scenarioOutputFile', 95, 'string', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: sensitivity.scenarioOutputFile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'sensitivity' and name = 'sensitivityOutputFile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'sensitivity', 'sensitivityOutputFile', 96, 'string', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: sensitivity.sensitivityOutputFile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'sensitivity' and name = 'crossGammaOutputFile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'sensitivity', 'crossGammaOutputFile', 97, 'string', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: sensitivity.crossGammaOutputFile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'sensitivity' and name = 'outputSensitivityThreshold'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'sensitivity', 'outputSensitivityThreshold', 98, 'double', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: sensitivity.outputSensitivityThreshold'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'sensitivity' and name = 'recalibrateModels'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'sensitivity', 'recalibrateModels', 99, 'boolean', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: sensitivity.recalibrateModels'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'calibration' and name = 'active'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'calibration', 'active', 100, 'boolean', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: calibration.active'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'calibration' and name = 'configFile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'calibration', 'configFile', 101, 'string', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: calibration.configFile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'calibration' and name = 'outputFile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'calibration', 'outputFile', 102, 'string', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: calibration.outputFile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'initialMargin' and name = 'active'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'initialMargin', 'active', 103, 'boolean', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: initialMargin.active'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'sensitivity' and name = 'applySimmExemptions'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'sensitivity', 'applySimmExemptions', 104, 'boolean', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: sensitivity.applySimmExemptions'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'sensitivity' and name = 'outputProductClassMapping'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'sensitivity', 'outputProductClassMapping', 105, 'boolean', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: sensitivity.outputProductClassMapping'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'sensitivity' and name = 'productClassMappingOutputFile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'sensitivity', 'productClassMappingOutputFile', 106, 'string', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: sensitivity.productClassMappingOutputFile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'zeroToParSensiConversion' and name = 'active'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'zeroToParSensiConversion', 'active', 107, 'boolean', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: zeroToParSensiConversion.active'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'zeroToParSensiConversion' and name = 'marketConfigFile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'zeroToParSensiConversion', 'marketConfigFile', 108, 'string', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: zeroToParSensiConversion.marketConfigFile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'zeroToParSensiConversion' and name = 'sensitivityConfigFile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'zeroToParSensiConversion', 'sensitivityConfigFile', 109, 'string', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: zeroToParSensiConversion.sensitivityConfigFile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'zeroToParSensiConversion' and name = 'pricingEnginesFile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'zeroToParSensiConversion', 'pricingEnginesFile', 110, 'string', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: zeroToParSensiConversion.pricingEnginesFile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'zeroToParSensiConversion' and name = 'sensitivityInputFile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'zeroToParSensiConversion', 'sensitivityInputFile', 111, 'string', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: zeroToParSensiConversion.sensitivityInputFile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'zeroToParSensiConversion' and name = 'outputThreshold'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'zeroToParSensiConversion', 'outputThreshold', 112, 'double', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: zeroToParSensiConversion.outputThreshold'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'zeroToParSensiConversion' and name = 'outputFile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'zeroToParSensiConversion', 'outputFile', 113, 'string', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: zeroToParSensiConversion.outputFile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'zeroToParSensiConversion' and name = 'outputJacobi'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'zeroToParSensiConversion', 'outputJacobi', 114, 'boolean', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: zeroToParSensiConversion.outputJacobi'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'zeroToParSensiConversion' and name = 'jacobiOutputFile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'zeroToParSensiConversion', 'jacobiOutputFile', 115, 'string', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: zeroToParSensiConversion.jacobiOutputFile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'zeroToParSensiConversion' and name = 'jacobiInverseOutputFile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'zeroToParSensiConversion', 'jacobiInverseOutputFile', 116, 'string', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: zeroToParSensiConversion.jacobiInverseOutputFile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'simulation' and name = 'amcCg'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'simulation', 'amcCg', 117, 'string', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: simulation.amcCg'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'simulation' and name = 'xvaCgUseExternalComputeDevice'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'simulation', 'xvaCgUseExternalComputeDevice', 118, 'boolean', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: simulation.xvaCgUseExternalComputeDevice'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'simulation' and name = 'xvaCgExternalDeviceCompatibilityMode'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'simulation', 'xvaCgExternalDeviceCompatibilityMode', 119, 'boolean', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: simulation.xvaCgExternalDeviceCompatibilityMode'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'simulation' and name = 'xvaCgUseDoublePrecisionForExternalCalculation'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'simulation', 'xvaCgUseDoublePrecisionForExternalCalculation', 120, 'boolean', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: simulation.xvaCgUseDoublePrecisionForExternalCalculation'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'simulation' and name = 'xvaCgExternalComputeDevice'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'simulation', 'xvaCgExternalComputeDevice', 121, 'string', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: simulation.xvaCgExternalComputeDevice'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'simulation' and name = 'amcCgPricingEnginesFile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'simulation', 'amcCgPricingEnginesFile', 122, 'string', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: simulation.amcCgPricingEnginesFile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'simulation' and name = 'xvaCgDynamicIM'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'simulation', 'xvaCgDynamicIM', 123, 'boolean', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: simulation.xvaCgDynamicIM'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'simulation' and name = 'xvaCgDynamicIMStepSize'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'simulation', 'xvaCgDynamicIMStepSize', 124, 'integer', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: simulation.xvaCgDynamicIMStepSize'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'simulation' and name = 'xvaCgRegressionOrder'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'simulation', 'xvaCgRegressionOrder', 125, 'integer', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: simulation.xvaCgRegressionOrder'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'simulation' and name = 'xvaCgRegressionOrderDynamicIm'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'simulation', 'xvaCgRegressionOrderDynamicIm', 126, 'integer', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: simulation.xvaCgRegressionOrderDynamicIm'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'simulation' and name = 'xvaCgTradeLevelBreakDown'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'simulation', 'xvaCgTradeLevelBreakDown', 127, 'boolean', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: simulation.xvaCgTradeLevelBreakDown'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'simulation' and name = 'xvaCgUseRedBlocks'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'simulation', 'xvaCgUseRedBlocks', 128, 'boolean', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: simulation.xvaCgUseRedBlocks'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'simulation' and name = 'storeFlows'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'simulation', 'storeFlows', 129, 'boolean', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: simulation.storeFlows'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'xva' and name = 'mva'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'xva', 'mva', 130, 'boolean', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: xva.mva'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'xva' and name = 'dimRegressors'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'xva', 'dimRegressors', 131, 'string', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: xva.dimRegressors'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'simulation' and name = 'storeSensis'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'simulation', 'storeSensis', 132, 'boolean', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: simulation.storeSensis'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'simulation' and name = 'curveSensiGrid'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'simulation', 'curveSensiGrid', 133, 'string', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: simulation.curveSensiGrid'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'simulation' and name = 'vegaSensiGrid'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'simulation', 'vegaSensiGrid', 134, 'string', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: simulation.vegaSensiGrid'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'xva' and name = 'collateralBalancesFile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'xva', 'collateralBalancesFile', 135, 'string', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: xva.collateralBalancesFile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'saccr' and name = 'active'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'saccr', 'active', 136, 'boolean', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: saccr.active'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'saccr' and name = 'csaFile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'saccr', 'csaFile', 137, 'string', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: saccr.csaFile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'saccr' and name = 'collateralBalancesFile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'saccr', 'collateralBalancesFile', 138, 'string', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: saccr.collateralBalancesFile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'smrc' and name = 'active'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'smrc', 'active', 139, 'boolean', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: smrc.active'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'xva' and name = 'useXvaRunner'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'xva', 'useXvaRunner', 140, 'boolean', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: xva.useXvaRunner'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'sensitivity' and name = 'parSensitivity'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'sensitivity', 'parSensitivity', 141, 'boolean', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: sensitivity.parSensitivity'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'sensitivity' and name = 'parSensitivityOutputFile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'sensitivity', 'parSensitivityOutputFile', 142, 'string', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: sensitivity.parSensitivityOutputFile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'sensitivity' and name = 'outputJacobi'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'sensitivity', 'outputJacobi', 143, 'boolean', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: sensitivity.outputJacobi'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'sensitivity' and name = 'jacobiOutputFile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'sensitivity', 'jacobiOutputFile', 144, 'string', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: sensitivity.jacobiOutputFile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'sensitivity' and name = 'jacobiInverseOutputFile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'sensitivity', 'jacobiInverseOutputFile', 145, 'string', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: sensitivity.jacobiInverseOutputFile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'portfolioDetails' and name = 'active'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'portfolioDetails', 'active', 146, 'boolean', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: portfolioDetails.active'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'portfolioDetails' and name = 'riskFactorFileName'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'portfolioDetails', 'riskFactorFileName', 147, 'string', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: portfolioDetails.riskFactorFileName'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'portfolioDetails' and name = 'marketObjectFileName'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'portfolioDetails', 'marketObjectFileName', 148, 'string', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: portfolioDetails.marketObjectFileName'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'simulation' and name = 'storeSurvivalProbabilities'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'simulation', 'storeSurvivalProbabilities', 149, 'boolean', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: simulation.storeSurvivalProbabilities'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'xva' and name = 'dynamicCredit'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'xva', 'dynamicCredit', 150, 'boolean', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: xva.dynamicCredit'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'simulation' and name = 'simulationModelFile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'simulation', 'simulationModelFile', 151, 'string', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: simulation.simulationModelFile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'simulation' and name = 'simulationMarketFile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'simulation', 'simulationMarketFile', 152, 'string', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: simulation.simulationMarketFile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'simulation' and name = 'simulationParamFile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'simulation', 'simulationParamFile', 153, 'string', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: simulation.simulationParamFile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'xva' and name = 'fvaFundingCurve'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'xva', 'fvaFundingCurve', 154, 'string', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: xva.fvaFundingCurve'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'xva' and name = 'fvaInvestmentCurve'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'xva', 'fvaInvestmentCurve', 155, 'string', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: xva.fvaInvestmentCurve'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'xva' and name = 'colvaSpread'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'xva', 'colvaSpread', 156, 'double', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: xva.colvaSpread'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'xva' and name = 'eoniaFloor'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'xva', 'eoniaFloor', 157, 'boolean', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: xva.eoniaFloor'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'npv' and name = 'extendedResults'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'npv', 'extendedResults', 158, 'boolean', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: npv.extendedResults'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'stress' and name = 'active'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'stress', 'active', 159, 'boolean', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: stress.active'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'stress' and name = 'precision'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'stress', 'precision', 160, 'integer', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: stress.precision'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'stress' and name = 'marketConfigFile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'stress', 'marketConfigFile', 161, 'string', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: stress.marketConfigFile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'stress' and name = 'stressConfigFile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'stress', 'stressConfigFile', 162, 'string', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: stress.stressConfigFile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'stress' and name = 'pricingEnginesFile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'stress', 'pricingEnginesFile', 163, 'string', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: stress.pricingEnginesFile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'stress' and name = 'scenarioOutputFile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'stress', 'scenarioOutputFile', 164, 'string', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: stress.scenarioOutputFile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'stress' and name = 'outputThreshold'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'stress', 'outputThreshold', 165, 'double', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: stress.outputThreshold'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'stress' and name = 'generateCashflows'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'stress', 'generateCashflows', 166, 'boolean', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: stress.generateCashflows'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'stress' and name = 'scenarioCashflowOutputFile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'stress', 'scenarioCashflowOutputFile', 167, 'string', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: stress.scenarioCashflowOutputFile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'parametricVar' and name = 'active'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'parametricVar', 'active', 168, 'boolean', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: parametricVar.active'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'parametricVar' and name = 'sensitivityInputFile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'parametricVar', 'sensitivityInputFile', 169, 'string', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: parametricVar.sensitivityInputFile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'parametricVar' and name = 'covarianceInputFile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'parametricVar', 'covarianceInputFile', 170, 'string', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: parametricVar.covarianceInputFile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'parametricVar' and name = 'quantiles'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'parametricVar', 'quantiles', 171, 'string', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: parametricVar.quantiles'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'parametricVar' and name = 'breakdown'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'parametricVar', 'breakdown', 172, 'boolean', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: parametricVar.breakdown'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'parametricVar' and name = 'portfolioFilter'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'parametricVar', 'portfolioFilter', 173, 'string', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: parametricVar.portfolioFilter'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'parametricVar' and name = 'method'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'parametricVar', 'method', 174, 'string', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: parametricVar.method'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'parametricVar' and name = 'outputFile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'parametricVar', 'outputFile', 175, 'string', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: parametricVar.outputFile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'simm' and name = 'active'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'simm', 'active', 176, 'boolean', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: simm.active'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'simm' and name = 'version'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'simm', 'version', 177, 'string', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: simm.version'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'simm' and name = 'crif'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'simm', 'crif', 178, 'string', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: simm.crif'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'simm' and name = 'calculationCurrency'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'simm', 'calculationCurrency', 179, 'string', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: simm.calculationCurrency'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'simm' and name = 'enforceIMRegulations'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'simm', 'enforceIMRegulations', 180, 'boolean', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: simm.enforceIMRegulations'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'simm' and name = 'mporDays'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'simm', 'mporDays', 181, 'integer', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: simm.mporDays'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'imschedule' and name = 'active'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'imschedule', 'active', 182, 'boolean', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: imschedule.active'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'imschedule' and name = 'crif'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'imschedule', 'crif', 183, 'string', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: imschedule.crif'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'imschedule' and name = 'calculationCurrency'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'imschedule', 'calculationCurrency', 184, 'string', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: imschedule.calculationCurrency'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'simm' and name = 'simmCalibration'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'simm', 'simmCalibration', 185, 'string', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: simm.simmCalibration'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'simm' and name = 'writeIntermediateReports'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'simm', 'writeIntermediateReports', 186, 'boolean', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: simm.writeIntermediateReports'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'creditcurves' and name = 'active'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'creditcurves', 'active', 187, 'boolean', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: creditcurves.active'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'creditcurves' and name = 'configuration'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'creditcurves', 'configuration', 188, 'string', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: creditcurves.configuration'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'creditcurves' and name = 'grid'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'creditcurves', 'grid', 189, 'string', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: creditcurves.grid'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'creditcurves' and name = 'outputFileName'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'creditcurves', 'outputFileName', 190, 'string', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: creditcurves.outputFileName'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'simulation' and name = 'computeCube'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'simulation', 'computeCube', 191, 'boolean', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: simulation.computeCube'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'simulation' and name = 'additionalScenarioDataFileName'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'simulation', 'additionalScenarioDataFileName', 192, 'string', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: simulation.additionalScenarioDataFileName'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'simulation' and name = 'storeCreditStateNPVs'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'simulation', 'storeCreditStateNPVs', 193, 'integer', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: simulation.storeCreditStateNPVs'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'xva' and name = 'hyperCube'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'xva', 'hyperCube', 194, 'boolean', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: xva.hyperCube'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'xva' and name = 'creditMigration'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'xva', 'creditMigration', 195, 'boolean', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: xva.creditMigration'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'xva' and name = 'creditMigrationDistributionGrid'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'xva', 'creditMigrationDistributionGrid', 196, 'string', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: xva.creditMigrationDistributionGrid'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'xva' and name = 'creditMigrationTimeSteps'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'xva', 'creditMigrationTimeSteps', 197, 'string', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: xva.creditMigrationTimeSteps'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'xva' and name = 'creditMigrationConfig'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'xva', 'creditMigrationConfig', 198, 'string', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: xva.creditMigrationConfig'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'xva' and name = 'creditMigrationOutputFiles'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'xva', 'creditMigrationOutputFiles', 199, 'string', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: xva.creditMigrationOutputFiles'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'sensitivity' and name = 'parRateSensitivityOutputFile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'sensitivity', 'parRateSensitivityOutputFile', 200, 'string', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: sensitivity.parRateSensitivityOutputFile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'stress' and name = 'sensitivityConfigFile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'stress', 'sensitivityConfigFile', 201, 'string', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: stress.sensitivityConfigFile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'sensitivity' and name = 'optimiseRiskFactors'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'sensitivity', 'optimiseRiskFactors', 202, 'boolean', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: sensitivity.optimiseRiskFactors'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'xvaStress' and name = 'sensitivityConfigFile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'xvaStress', 'sensitivityConfigFile', 203, 'string', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: xvaStress.sensitivityConfigFile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'xvaExplain' and name = 'stressConfigFile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'xvaExplain', 'stressConfigFile', 204, 'string', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: xvaExplain.stressConfigFile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'scenario' and name = 'active'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'scenario', 'active', 205, 'boolean', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: scenario.active'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'scenario' and name = 'simulationConfigFile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'scenario', 'simulationConfigFile', 206, 'string', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: scenario.simulationConfigFile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'scenario' and name = 'scenarioOutputFile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'scenario', 'scenarioOutputFile', 207, 'string', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: scenario.scenarioOutputFile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'npv' and name = 'additionalResults'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'npv', 'additionalResults', 208, 'boolean', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: npv.additionalResults'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'curves' and name = 'outputTodaysMarketCalibration'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'curves', 'outputTodaysMarketCalibration', 209, 'boolean', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: curves.outputTodaysMarketCalibration'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'simulation' and name = 'observationModel'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'simulation', 'observationModel', 210, 'string', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: simulation.observationModel'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'xva' and name = 'flipViewXVA'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'xva', 'flipViewXVA', 211, 'boolean', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: xva.flipViewXVA'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'xva' and name = 'flipViewBorrowingCurvePostfix'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'xva', 'flipViewBorrowingCurvePostfix', 212, 'string', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: xva.flipViewBorrowingCurvePostfix'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'xva' and name = 'flipViewLendingCurvePostfix'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'xva', 'flipViewLendingCurvePostfix', 213, 'string', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: xva.flipViewLendingCurvePostfix'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'initialMargin' and name = 'method'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'initialMargin', 'method', 214, 'string', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: initialMargin.method'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'npv' and name = 'additionalResultsReportPrecision'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'npv', 'additionalResultsReportPrecision', 215, 'integer', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: npv.additionalResultsReportPrecision'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'xva' and name = 'timeAveragedNettedExposureOutputFile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'xva', 'timeAveragedNettedExposureOutputFile', 216, 'string', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: xva.timeAveragedNettedExposureOutputFile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'simulation' and name = 'includeTodaysCashFlows'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'simulation', 'includeTodaysCashFlows', 217, 'boolean', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: simulation.includeTodaysCashFlows'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'pnlExplain' and name = 'active'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'pnlExplain', 'active', 218, 'boolean', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: pnlExplain.active'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'pnlExplain' and name = 'mporDate'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'pnlExplain', 'mporDate', 219, 'date', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: pnlExplain.mporDate'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'pnlExplain' and name = 'simulationConfigFile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'pnlExplain', 'simulationConfigFile', 220, 'string', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: pnlExplain.simulationConfigFile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'pnlExplain' and name = 'sensitivityConfigFile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'pnlExplain', 'sensitivityConfigFile', 221, 'string', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: pnlExplain.sensitivityConfigFile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'pnlExplain' and name = 'parSensitivity'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'pnlExplain', 'parSensitivity', 222, 'boolean', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: pnlExplain.parSensitivity'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'pnlExplain' and name = 'curveConfigMporFile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'pnlExplain', 'curveConfigMporFile', 223, 'string', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: pnlExplain.curveConfigMporFile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'pnlExplain' and name = 'conventionsMporFile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'pnlExplain', 'conventionsMporFile', 224, 'string', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: pnlExplain.conventionsMporFile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'pnlExplain' and name = 'portfolioMporFile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'pnlExplain', 'portfolioMporFile', 225, 'string', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: pnlExplain.portfolioMporFile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'pnlExplain' and name = 'outputFileName'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'pnlExplain', 'outputFileName', 226, 'string', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: pnlExplain.outputFileName'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'pnl' and name = 'active'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'pnl', 'active', 227, 'boolean', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: pnl.active'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'pnl' and name = 'mporDays'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'pnl', 'mporDays', 228, 'integer', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: pnl.mporDays'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'pnl' and name = 'mporCalendar'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'pnl', 'mporCalendar', 229, 'string', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: pnl.mporCalendar'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'pnl' and name = 'curveConfigMporFile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'pnl', 'curveConfigMporFile', 230, 'string', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: pnl.curveConfigMporFile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'pnl' and name = 'conventionsMporFile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'pnl', 'conventionsMporFile', 231, 'string', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: pnl.conventionsMporFile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'pnl' and name = 'portfolioMporFile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'pnl', 'portfolioMporFile', 232, 'string', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: pnl.portfolioMporFile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'pnl' and name = 'simulationConfigFile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'pnl', 'simulationConfigFile', 233, 'string', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: pnl.simulationConfigFile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'pnl' and name = 'outputFileName'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'pnl', 'outputFileName', 234, 'string', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: pnl.outputFileName'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'xva' and name = 'firstMporCollateralAdjustment'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'xva', 'firstMporCollateralAdjustment', 235, 'boolean', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: xva.firstMporCollateralAdjustment'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'historicalSimulationVar' and name = 'active'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'historicalSimulationVar', 'active', 236, 'boolean', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: historicalSimulationVar.active'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'historicalSimulationVar' and name = 'historicalScenarioFile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'historicalSimulationVar', 'historicalScenarioFile', 237, 'string', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: historicalSimulationVar.historicalScenarioFile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'historicalSimulationVar' and name = 'simulationConfigFile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'historicalSimulationVar', 'simulationConfigFile', 238, 'string', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: historicalSimulationVar.simulationConfigFile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'historicalSimulationVar' and name = 'historicalPeriod'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'historicalSimulationVar', 'historicalPeriod', 239, 'string', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: historicalSimulationVar.historicalPeriod'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'historicalSimulationVar' and name = 'mporDays'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'historicalSimulationVar', 'mporDays', 240, 'integer', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: historicalSimulationVar.mporDays'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'historicalSimulationVar' and name = 'mporCalendar'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'historicalSimulationVar', 'mporCalendar', 241, 'string', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: historicalSimulationVar.mporCalendar'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'historicalSimulationVar' and name = 'mporOverlappingPeriods'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'historicalSimulationVar', 'mporOverlappingPeriods', 242, 'boolean', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: historicalSimulationVar.mporOverlappingPeriods'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'historicalSimulationVar' and name = 'quantiles'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'historicalSimulationVar', 'quantiles', 243, 'string', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: historicalSimulationVar.quantiles'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'historicalSimulationVar' and name = 'outputFile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'historicalSimulationVar', 'outputFile', 244, 'string', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: historicalSimulationVar.outputFile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'simulation' and name = 'filterRiskKeys'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'simulation', 'filterRiskKeys', 245, 'string', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: simulation.filterRiskKeys'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'simulation' and name = 'xvaCgSensitivityConfigFile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'simulation', 'xvaCgSensitivityConfigFile', 246, 'string', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: simulation.xvaCgSensitivityConfigFile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'simulation' and name = 'xvaCgBumpSensis'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'simulation', 'xvaCgBumpSensis', 247, 'boolean', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: simulation.xvaCgBumpSensis'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'simulation' and name = 'xvaCgRegressionCacheSize'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'simulation', 'xvaCgRegressionCacheSize', 248, 'integer', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: simulation.xvaCgRegressionCacheSize'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'amc' and name = 'active'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'amc', 'active', 249, 'boolean', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: amc.active'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'amc' and name = 'pricingEnginesFile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'amc', 'pricingEnginesFile', 250, 'string', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: amc.pricingEnginesFile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'amc' and name = 'amcTradeTypes'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'amc', 'amcTradeTypes', 251, 'string', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: amc.amcTradeTypes'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'simulation' and name = 'disableEvaluationDateObservation'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'simulation', 'disableEvaluationDateObservation', 252, 'boolean', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: simulation.disableEvaluationDateObservation'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'scenarioGeneration' and name = 'active'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'scenarioGeneration', 'active', 253, 'boolean', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: scenarioGeneration.active'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'scenarioGeneration' and name = 'simulationConfigFile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'scenarioGeneration', 'simulationConfigFile', 254, 'string', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: scenarioGeneration.simulationConfigFile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'scenarioGeneration' and name = 'scenarioOutputFile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'scenarioGeneration', 'scenarioOutputFile', 255, 'string', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: scenarioGeneration.scenarioOutputFile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'scenarioGeneration' and name = 'amcPathDataOutput'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'scenarioGeneration', 'amcPathDataOutput', 256, 'string', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: scenarioGeneration.amcPathDataOutput'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'scenarioGeneration' and name = 'outputStatistics'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'scenarioGeneration', 'outputStatistics', 257, 'boolean', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: scenarioGeneration.outputStatistics'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'scenarioGeneration' and name = 'outputDistributions'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'scenarioGeneration', 'outputDistributions', 258, 'boolean', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: scenarioGeneration.outputDistributions'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'simulation' and name = 'scenarioFile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'simulation', 'scenarioFile', 259, 'string', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: simulation.scenarioFile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'simulation' and name = 'amcPathDataOutput'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'simulation', 'amcPathDataOutput', 260, 'string', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: simulation.amcPathDataOutput'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'simulation' and name = 'amcIndividualTrainingOutput'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'simulation', 'amcIndividualTrainingOutput', 261, 'boolean', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: simulation.amcIndividualTrainingOutput'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'simulation' and name = 'amcIndividualTrainingInput'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'simulation', 'amcIndividualTrainingInput', 262, 'boolean', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: simulation.amcIndividualTrainingInput'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'simulation' and name = 'includeReferenceDateEvents'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'simulation', 'includeReferenceDateEvents', 263, 'boolean', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: simulation.includeReferenceDateEvents'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'stress' and name = 'stressZeroScenarioDataFile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'stress', 'stressZeroScenarioDataFile', 264, 'string', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: stress.stressZeroScenarioDataFile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'parStressConversion' and name = 'active'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'parStressConversion', 'active', 265, 'boolean', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: parStressConversion.active'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'parStressConversion' and name = 'marketConfigFile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'parStressConversion', 'marketConfigFile', 266, 'string', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: parStressConversion.marketConfigFile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'parStressConversion' and name = 'stressConfigFile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'parStressConversion', 'stressConfigFile', 267, 'string', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: parStressConversion.stressConfigFile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'parStressConversion' and name = 'sensitivityConfigFile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'parStressConversion', 'sensitivityConfigFile', 268, 'string', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: parStressConversion.sensitivityConfigFile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'parStressConversion' and name = 'pricingEnginesFile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'parStressConversion', 'pricingEnginesFile', 269, 'string', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: parStressConversion.pricingEnginesFile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'parStressConversion' and name = 'scenarioOutputFile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'parStressConversion', 'scenarioOutputFile', 270, 'string', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: parStressConversion.scenarioOutputFile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'parStressConversion' and name = 'outputThreshold'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'parStressConversion', 'outputThreshold', 271, 'double', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: parStressConversion.outputThreshold'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'parStressConversion' and name = 'stressZeroScenarioDataFile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'parStressConversion', 'stressZeroScenarioDataFile', 272, 'string', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: parStressConversion.stressZeroScenarioDataFile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'xva' and name = 'outputCrossAssetModelData'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'xva', 'outputCrossAssetModelData', 273, 'boolean', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: xva.outputCrossAssetModelData'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'xva' and name = 'correlationInputFile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'xva', 'correlationInputFile', 274, 'string', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: xva.correlationInputFile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'xva' and name = 'scenarioCorrSimulation'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'xva', 'scenarioCorrSimulation', 275, 'string', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: xva.scenarioCorrSimulation'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'xva' and name = 'simulationConfigFile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'xva', 'simulationConfigFile', 276, 'string', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: xva.simulationConfigFile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'xva' and name = 'historicalScenarioFile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'xva', 'historicalScenarioFile', 277, 'string', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: xva.historicalScenarioFile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'xva' and name = 'sensitivityConfigFile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'xva', 'sensitivityConfigFile', 278, 'string', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: xva.sensitivityConfigFile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'xva' and name = 'historicalPeriod'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'xva', 'historicalPeriod', 279, 'string', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: xva.historicalPeriod'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'xva' and name = 'mporDays'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'xva', 'mporDays', 280, 'integer', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: xva.mporDays'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'xva' and name = 'mporCalendar'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'xva', 'mporCalendar', 281, 'string', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: xva.mporCalendar'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'calibration' and name = 'model'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'calibration', 'model', 282, 'string', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: calibration.model'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'calibration' and name = 'mode'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'calibration', 'mode', 283, 'string', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: calibration.mode'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'calibration' and name = 'foreignCurrencies'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'calibration', 'foreignCurrencies', 284, 'string', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: calibration.foreignCurrencies'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'calibration' and name = 'pcaCalibration'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'calibration', 'pcaCalibration', 285, 'boolean', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: calibration.pcaCalibration'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'calibration' and name = 'curveTenors'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'calibration', 'curveTenors', 286, 'string', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: calibration.curveTenors'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'calibration' and name = 'useForwardOrZeroRate'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'calibration', 'useForwardOrZeroRate', 287, 'string', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: calibration.useForwardOrZeroRate'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'calibration' and name = 'pcaInputFileName'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'calibration', 'pcaInputFileName', 288, 'string', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: calibration.pcaInputFileName'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'calibration' and name = 'meanReversionCalibration'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'calibration', 'meanReversionCalibration', 289, 'boolean', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: calibration.meanReversionCalibration'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'calibration' and name = 'basisFunctionNumber'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'calibration', 'basisFunctionNumber', 290, 'integer', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: calibration.basisFunctionNumber'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'calibration' and name = 'kappaUpperBound'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'calibration', 'kappaUpperBound', 291, 'double', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: calibration.kappaUpperBound'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'calibration' and name = 'haltonMaxGuess'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'calibration', 'haltonMaxGuess', 292, 'integer', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: calibration.haltonMaxGuess'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'calibration' and name = 'meanReversionOutputFileName'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'calibration', 'meanReversionOutputFileName', 293, 'string', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: calibration.meanReversionOutputFileName'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'calibration' and name = 'scenarioInputFile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'calibration', 'scenarioInputFile', 294, 'string', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: calibration.scenarioInputFile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'calibration' and name = 'startDate'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'calibration', 'startDate', 295, 'date', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: calibration.startDate'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'calibration' and name = 'endDate'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'calibration', 'endDate', 296, 'date', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: calibration.endDate'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'calibration' and name = 'lambda'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'calibration', 'lambda', 297, 'integer', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: calibration.lambda'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'calibration' and name = 'varianceRetained'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'calibration', 'varianceRetained', 298, 'double', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: calibration.varianceRetained'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'calibration' and name = 'pcaOutputFileName'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'calibration', 'pcaOutputFileName', 299, 'string', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: calibration.pcaOutputFileName'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'saccr' and name = 'simmVersion'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'saccr', 'simmVersion', 300, 'double', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: saccr.simmVersion'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'pfe' and name = 'active'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'pfe', 'active', 301, 'boolean', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: pfe.active'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'pfe' and name = 'csaFile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'pfe', 'csaFile', 302, 'string', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: pfe.csaFile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'pfe' and name = 'cubeFile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'pfe', 'cubeFile', 303, 'string', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: pfe.cubeFile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'pfe' and name = 'scenarioFile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'pfe', 'scenarioFile', 304, 'string', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: pfe.scenarioFile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'pfe' and name = 'baseCurrency'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'pfe', 'baseCurrency', 305, 'string', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: pfe.baseCurrency'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'pfe' and name = 'exposureProfiles'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'pfe', 'exposureProfiles', 306, 'boolean', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: pfe.exposureProfiles'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'pfe' and name = 'exposureProfilesByTrade'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'pfe', 'exposureProfilesByTrade', 307, 'boolean', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: pfe.exposureProfilesByTrade'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'pfe' and name = 'quantile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'pfe', 'quantile', 308, 'double', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: pfe.quantile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'pfe' and name = 'calculationType'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'pfe', 'calculationType', 309, 'string', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: pfe.calculationType'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'pfe' and name = 'allocationMethod'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'pfe', 'allocationMethod', 310, 'string', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: pfe.allocationMethod'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'pfe' and name = 'marginalAllocationLimit'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'pfe', 'marginalAllocationLimit', 311, 'double', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: pfe.marginalAllocationLimit'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'pfe' and name = 'exerciseNextBreak'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'pfe', 'exerciseNextBreak', 312, 'boolean', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: pfe.exerciseNextBreak'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'pfe' and name = 'rawCubeOutputFile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'pfe', 'rawCubeOutputFile', 313, 'string', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: pfe.rawCubeOutputFile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'pfe' and name = 'netCubeOutputFile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'pfe', 'netCubeOutputFile', 314, 'string', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: pfe.netCubeOutputFile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'xva' and name = 'deterministicInitialMarginFile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'xva', 'deterministicInitialMarginFile', 315, 'string', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: xva.deterministicInitialMarginFile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'crif' and name = 'active'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'crif', 'active', 316, 'boolean', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: crif.active'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'crif' and name = 'marketConfigFile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'crif', 'marketConfigFile', 317, 'string', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: crif.marketConfigFile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'crif' and name = 'sensitivityConfigFile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'crif', 'sensitivityConfigFile', 318, 'string', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: crif.sensitivityConfigFile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'crif' and name = 'baseCurrency'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'crif', 'baseCurrency', 319, 'string', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: crif.baseCurrency'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'crif' and name = 'simmVersion'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'crif', 'simmVersion', 320, 'double', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: crif.simmVersion'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'crif' and name = 'crifOutputFile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'crif', 'crifOutputFile', 321, 'string', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: crif.crifOutputFile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'imschedule' and name = 'version'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'imschedule', 'version', 322, 'double', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: imschedule.version'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'simulation' and name = 'xvaCgRegressionReportTimeStepsDynamicIM'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'simulation', 'xvaCgRegressionReportTimeStepsDynamicIM', 323, 'string', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: simulation.xvaCgRegressionReportTimeStepsDynamicIM'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'simulation' and name = 'xvaCgUsePythonIntegration'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'simulation', 'xvaCgUsePythonIntegration', 324, 'boolean', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: simulation.xvaCgUsePythonIntegration'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'simulation' and name = 'xvaCgUsePythonIntegrationDynamicIm'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'simulation', 'xvaCgUsePythonIntegrationDynamicIm', 325, 'boolean', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: simulation.xvaCgUsePythonIntegrationDynamicIm'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'simulation' and name = 'xvaCgTradeLevelBreakdown'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'simulation', 'xvaCgTradeLevelBreakdown', 326, 'boolean', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: simulation.xvaCgTradeLevelBreakdown'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'xva' and name = 'fullInitialCollateralisation'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'xva', 'fullInitialCollateralisation', 327, 'boolean', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: xva.fullInitialCollateralisation'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'xva' and name = 'dimDistributionCoveredStdDevs'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'xva', 'dimDistributionCoveredStdDevs', 328, 'double', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: xva.dimDistributionCoveredStdDevs'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'xva' and name = 'dimDistributionGridSize'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'xva', 'dimDistributionGridSize', 329, 'integer', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: xva.dimDistributionGridSize'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'simulation' and name = 'storeExerciseValues'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'simulation', 'storeExerciseValues', 330, 'boolean', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: simulation.storeExerciseValues'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'simulation' and name = 'xvaCgEnableCgOptimization'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'simulation', 'xvaCgEnableCgOptimization', 331, 'boolean', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: simulation.xvaCgEnableCgOptimization'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'curves' and name = 'calendar'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'curves', 'calendar', 332, 'string', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: curves.calendar'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'xva' and name = 'borrowingCurve'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'xva', 'borrowingCurve', 333, 'string', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: xva.borrowingCurve'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'xva' and name = 'lendingCurve'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'xva', 'lendingCurve', 334, 'string', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: xva.lendingCurve'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'cashflow' and name = 'includePastCashflows'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'cashflow', 'includePastCashflows', 335, 'boolean', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: cashflow.includePastCashflows'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'sensitivity' and name = 'laxFxConversion'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'sensitivity', 'laxFxConversion', 336, 'boolean', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: sensitivity.laxFxConversion'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'scenarioGeneration' and name = 'scenarioType'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'scenarioGeneration', 'scenarioType', 337, 'string', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: scenarioGeneration.scenarioType'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'scenarioGeneration' and name = 'stressConfigFile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'scenarioGeneration', 'stressConfigFile', 338, 'string', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: scenarioGeneration.stressConfigFile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'scenarioGeneration' and name = 'scenarioPrecision'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'scenarioGeneration', 'scenarioPrecision', 339, 'integer', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: scenarioGeneration.scenarioPrecision'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'sensitivity' and name = 'decomposeIndexSensitivities'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'sensitivity', 'decomposeIndexSensitivities', 340, 'boolean', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: sensitivity.decomposeIndexSensitivities'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'sensitivityStress' and name = 'active'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'sensitivityStress', 'active', 341, 'boolean', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: sensitivityStress.active'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'sensitivityStress' and name = 'marketConfigFile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'sensitivityStress', 'marketConfigFile', 342, 'string', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: sensitivityStress.marketConfigFile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'sensitivityStress' and name = 'stressConfigFile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'sensitivityStress', 'stressConfigFile', 343, 'string', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: sensitivityStress.stressConfigFile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'sensitivityStress' and name = 'sensitivityConfigFile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'sensitivityStress', 'sensitivityConfigFile', 344, 'string', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: sensitivityStress.sensitivityConfigFile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'sensitivityStress' and name = 'calcBaseScenario'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'sensitivityStress', 'calcBaseScenario', 345, 'boolean', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: sensitivityStress.calcBaseScenario'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'correlation' and name = 'active'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'correlation', 'active', 346, 'boolean', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: correlation.active'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'correlation' and name = 'correlation_method'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'correlation', 'correlation_method', 347, 'string', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: correlation.correlation_method'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'correlation' and name = 'scenarioCorrSimulation'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'correlation', 'scenarioCorrSimulation', 348, 'string', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: correlation.scenarioCorrSimulation'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'correlation' and name = 'historicalScenarioFile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'correlation', 'historicalScenarioFile', 349, 'string', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: correlation.historicalScenarioFile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'correlation' and name = 'sensitivityConfigFile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'correlation', 'sensitivityConfigFile', 350, 'string', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: correlation.sensitivityConfigFile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'correlation' and name = 'historicalPeriod'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'correlation', 'historicalPeriod', 351, 'string', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: correlation.historicalPeriod'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'correlation' and name = 'mporDays'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'correlation', 'mporDays', 352, 'integer', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: correlation.mporDays'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'correlation' and name = 'mporCalendar'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'correlation', 'mporCalendar', 353, 'string', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: correlation.mporCalendar'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'correlation' and name = 'filterCamCorrelationScenarioTenor'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'correlation', 'filterCamCorrelationScenarioTenor', 354, 'string', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: correlation.filterCamCorrelationScenarioTenor'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'correlation' and name = 'outputFile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'correlation', 'outputFile', 355, 'string', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: correlation.outputFile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'historicalSimulationVar' and name = 'includeExpectedShortfall'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'historicalSimulationVar', 'includeExpectedShortfall', 356, 'boolean', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: historicalSimulationVar.includeExpectedShortfall'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'historicalSimulationVar' and name = 'breakdown'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'historicalSimulationVar', 'breakdown', 357, 'boolean', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: historicalSimulationVar.breakdown'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'historicalSimulationVar' and name = 'tradePnl'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'historicalSimulationVar', 'tradePnl', 358, 'boolean', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: historicalSimulationVar.tradePnl'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'historicalSimulationVar' and name = 'riskFactorBreakdown'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'historicalSimulationVar', 'riskFactorBreakdown', 359, 'boolean', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: historicalSimulationVar.riskFactorBreakdown'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'historicalSimulationVar' and name = 'riskClassBreakdown'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'historicalSimulationVar', 'riskClassBreakdown', 360, 'boolean', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: historicalSimulationVar.riskClassBreakdown'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'historicalSimulationVar' and name = 'outputHistoricalScenarios'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'historicalSimulationVar', 'outputHistoricalScenarios', 361, 'boolean', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: historicalSimulationVar.outputHistoricalScenarios'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'sensitivity' and name = 'alignPillars'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'sensitivity', 'alignPillars', 362, 'boolean', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: sensitivity.alignPillars'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'historicalSimulationVar' and name = 'includeTheta'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'historicalSimulationVar', 'includeTheta', 363, 'boolean', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: historicalSimulationVar.includeTheta'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'historicalSimulationVar' and name = 'includePeriodCashflow'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'historicalSimulationVar', 'includePeriodCashflow', 364, 'boolean', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: historicalSimulationVar.includePeriodCashflow'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'stress' and name = 'scenarioFile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'analytic', 'stress', 'scenarioFile', 365, 'string', false,
            current_user, 'system.initial_load', 'Seed analytic parameter: stress.scenarioFile'
        );
    end if;

end;
$$;

select 'Parameter Definitions' as entity, count(*) as count
from ores_reporting_parameter_definitions_tbl
where tenant_id = ores_utility_system_tenant_id_fn()
  and valid_to = ores_utility_infinity_timestamp_fn();
