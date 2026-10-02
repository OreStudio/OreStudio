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
 * Configuration Types Population Script
 *
 * Seeds the configuration_types lookup for the system tenant with the
 * seventeen kinds of ORE configuration document a report can bind. This
 * script is idempotent.
 *
 * The kinds are the global elements of ORE's configuration schemas under
 * external/ore/xsd. The run document is not one of them: a report owns its run
 * setup directly. Trades, portfolios, collateral balances, the script library
 * and shared type fragments are not configuration. run_parameter is the
 * parameter a shipped run document uses to name the file, and is null where no
 * shipped run document names one. A version of a kind that already exists keeps
 * its values and only has its empty mapping columns filled.
 */

\echo '--- Configuration Types ---'

do $$
declare
    v_sys_tenant uuid := ores_utility_system_tenant_id_fn();
    v_kind record;
begin
    for v_kind in
        select * from (values
            ( 1, 'pricing_engines',          'Pricing engines',          'PricingEngines',          'ores.analytics', 'pricingEnginesFile'),
            ( 2, 'todays_market',            'Today''s market',          'TodaysMarket',            'ores.analytics', 'marketConfigFile'),
            ( 3, 'simulation',               'Simulation',               'Simulation',              'ores.analytics', 'simulationConfigFile'),
            ( 4, 'sensitivity',              'Sensitivity analysis',     'SensitivityAnalysis',     'ores.analytics', 'sensitivityConfigFile'),
            ( 5, 'stress_test',              'Stress test',              'StressTesting',           'ores.analytics', 'stressConfigFile'),
            ( 6, 'credit_simulation',        'Credit simulation',        'CreditSimulation',        'ores.analytics', 'creditMigrationConfig'),
            ( 7, 'simm_calibration',         'SIMM calibration',         'SIMMCalibrationData',     'ores.analytics', 'simmCalibration'),
            ( 8, 'historical_return',        'Historical return',        'ReturnConfiguration',     'ores.analytics', null),
            ( 9, 'basel_traffic_light',      'Basel traffic light',      'BaselTrafficLightConfig', 'ores.analytics', null),
            (10, 'curve_configuration',      'Curve configuration',      'CurveConfiguration',      'ores.refdata',   'curveConfigFile'),
            (11, 'conventions',              'Conventions',              'Conventions',             'ores.refdata',   'conventionsFile'),
            (12, 'reference_data',           'Reference data',           'ReferenceData',           'ores.refdata',   'referenceDataFile'),
            (13, 'calendar_adjustment',      'Calendar adjustments',     'CalendarAdjustments',     'ores.refdata',   'calendarAdjustment'),
            (14, 'currency_configuration',   'Currency configuration',   'CurrencyConfig',          'ores.refdata',   'currencyConfiguration'),
            (15, 'ibor_fallback',            'IBOR fallback',            'IborFallbackConfig',      'ores.refdata',   'iborFallbackConfig'),
            (16, 'counterparty_information', 'Counterparty information', 'CounterpartyInformation', 'ores.refdata',   'counterpartyFile'),
            (17, 'netting_set_definitions',  'Netting set definitions',  'NettingSetDefinitions',   'ores.trading',   'csaFile')
        ) as k(display_order, code, name, ore_root_element, owning_component, run_parameter)
    loop
        -- Every version of a kind that predates the mapping columns, current or
        -- closed, gets them filled in place. It is a correction of missing data,
        -- not a change to the kind, so it writes no new version.
        update ores_reporting_configuration_types_tbl
        set ore_root_element = coalesce(ore_root_element, v_kind.ore_root_element),
            owning_component = coalesce(owning_component, v_kind.owning_component),
            run_parameter = coalesce(run_parameter, v_kind.run_parameter)
        where tenant_id = v_sys_tenant and code = v_kind.code
          and (ore_root_element is null or owning_component is null);

        if not exists (
            select 1 from ores_reporting_configuration_types_tbl
            where tenant_id = v_sys_tenant and code = v_kind.code
              and valid_to = ores_utility_infinity_timestamp_fn()
        ) then
            insert into ores_reporting_configuration_types_tbl (
                code, tenant_id, version,
                name, display_order, ore_root_element, owning_component, run_parameter,
                modified_by, change_reason_code, change_commentary
            ) values (
                v_kind.code, v_sys_tenant, 0,
                v_kind.name, v_kind.display_order, v_kind.ore_root_element,
                v_kind.owning_component, v_kind.run_parameter,
                current_user, 'system.initial_load', 'Seed configuration type: ' || v_kind.code
            );
            raise debug 'Created configuration type: %', v_kind.code;
        else
            raise debug 'Configuration type already exists: %', v_kind.code;
        end if;
    end loop;
end $$;

select 'Configuration Types' as entity, count(*) as count
from ores_reporting_configuration_types_tbl
where tenant_id = ores_utility_system_tenant_id_fn()
  and valid_to = ores_utility_infinity_timestamp_fn();
