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
 * Analytic Types Population Script
 *
 * Seeds the analytic_types lookup for the system tenant with the thirty-five
 * types the shipped ORE run documents use. This script is idempotent.
 *
 * parameter_entity is left null until the entity that holds a type's
 * parameters lands; the column names it then.
 */

\echo '--- Analytic Types ---'

do $$
declare
    v_sys_tenant uuid := ores_utility_system_tenant_id_fn();
begin
    if not exists (
        select 1 from ores_reporting_analytic_types_tbl
        where tenant_id = v_sys_tenant and code = 'npv'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_analytic_types_tbl (
            code, tenant_id, version,
            name, description, parameter_entity, display_order,
            modified_by, change_reason_code, change_commentary
        ) values (
            'npv', v_sys_tenant, 0,
            'Net Present Value',
            'Present value of the portfolio''s trades at the as-of date.',
            null,
            1,
            current_user, 'system.initial_load', 'Seed analytic type: npv'
        );
        raise debug 'Created analytic type: npv';
    else
        raise debug 'Analytic type already exists: npv';
    end if;

    if not exists (
        select 1 from ores_reporting_analytic_types_tbl
        where tenant_id = v_sys_tenant and code = 'cashflow'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_analytic_types_tbl (
            code, tenant_id, version,
            name, description, parameter_entity, display_order,
            modified_by, change_reason_code, change_commentary
        ) values (
            'cashflow', v_sys_tenant, 0,
            'Cashflows',
            'The portfolio''s projected cashflows, one row per flow.',
            null,
            2,
            current_user, 'system.initial_load', 'Seed analytic type: cashflow'
        );
        raise debug 'Created analytic type: cashflow';
    else
        raise debug 'Analytic type already exists: cashflow';
    end if;

    if not exists (
        select 1 from ores_reporting_analytic_types_tbl
        where tenant_id = v_sys_tenant and code = 'curves'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_analytic_types_tbl (
            code, tenant_id, version,
            name, description, parameter_entity, display_order,
            modified_by, change_reason_code, change_commentary
        ) values (
            'curves', v_sys_tenant, 0,
            'Curves',
            'The yield, credit and other curves the run built, as a report.',
            null,
            3,
            current_user, 'system.initial_load', 'Seed analytic type: curves'
        );
        raise debug 'Created analytic type: curves';
    else
        raise debug 'Analytic type already exists: curves';
    end if;

    if not exists (
        select 1 from ores_reporting_analytic_types_tbl
        where tenant_id = v_sys_tenant and code = 'simulation'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_analytic_types_tbl (
            code, tenant_id, version,
            name, description, parameter_entity, display_order,
            modified_by, change_reason_code, change_commentary
        ) values (
            'simulation', v_sys_tenant, 0,
            'Simulation',
            'Monte Carlo simulation of the portfolio under the market''s model.',
            null,
            4,
            current_user, 'system.initial_load', 'Seed analytic type: simulation'
        );
        raise debug 'Created analytic type: simulation';
    else
        raise debug 'Analytic type already exists: simulation';
    end if;

    if not exists (
        select 1 from ores_reporting_analytic_types_tbl
        where tenant_id = v_sys_tenant and code = 'xva'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_analytic_types_tbl (
            code, tenant_id, version,
            name, description, parameter_entity, display_order,
            modified_by, change_reason_code, change_commentary
        ) values (
            'xva', v_sys_tenant, 0,
            'XVA',
            'Credit valuation, debt valuation and funding adjustments over the portfolio.',
            null,
            5,
            current_user, 'system.initial_load', 'Seed analytic type: xva'
        );
        raise debug 'Created analytic type: xva';
    else
        raise debug 'Analytic type already exists: xva';
    end if;

    if not exists (
        select 1 from ores_reporting_analytic_types_tbl
        where tenant_id = v_sys_tenant and code = 'initialMargin'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_analytic_types_tbl (
            code, tenant_id, version,
            name, description, parameter_entity, display_order,
            modified_by, change_reason_code, change_commentary
        ) values (
            'initialMargin', v_sys_tenant, 0,
            'Initial Margin',
            'The initial margin the portfolio requires, under a stated calculation.',
            null,
            6,
            current_user, 'system.initial_load', 'Seed analytic type: initialMargin'
        );
        raise debug 'Created analytic type: initialMargin';
    else
        raise debug 'Analytic type already exists: initialMargin';
    end if;

    if not exists (
        select 1 from ores_reporting_analytic_types_tbl
        where tenant_id = v_sys_tenant and code = 'sensitivity'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_analytic_types_tbl (
            code, tenant_id, version,
            name, description, parameter_entity, display_order,
            modified_by, change_reason_code, change_commentary
        ) values (
            'sensitivity', v_sys_tenant, 0,
            'Sensitivity',
            'First and second order sensitivities of the portfolio''s value.',
            null,
            7,
            current_user, 'system.initial_load', 'Seed analytic type: sensitivity'
        );
        raise debug 'Created analytic type: sensitivity';
    else
        raise debug 'Analytic type already exists: sensitivity';
    end if;

    if not exists (
        select 1 from ores_reporting_analytic_types_tbl
        where tenant_id = v_sys_tenant and code = 'stress'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_analytic_types_tbl (
            code, tenant_id, version,
            name, description, parameter_entity, display_order,
            modified_by, change_reason_code, change_commentary
        ) values (
            'stress', v_sys_tenant, 0,
            'Stress Test',
            'The portfolio''s value under each named stress scenario.',
            null,
            8,
            current_user, 'system.initial_load', 'Seed analytic type: stress'
        );
        raise debug 'Created analytic type: stress';
    else
        raise debug 'Analytic type already exists: stress';
    end if;

    if not exists (
        select 1 from ores_reporting_analytic_types_tbl
        where tenant_id = v_sys_tenant and code = 'simm'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_analytic_types_tbl (
            code, tenant_id, version,
            name, description, parameter_entity, display_order,
            modified_by, change_reason_code, change_commentary
        ) values (
            'simm', v_sys_tenant, 0,
            'SIMM',
            'The standard initial margin model''s calculation for the portfolio.',
            null,
            9,
            current_user, 'system.initial_load', 'Seed analytic type: simm'
        );
        raise debug 'Created analytic type: simm';
    else
        raise debug 'Analytic type already exists: simm';
    end if;

    if not exists (
        select 1 from ores_reporting_analytic_types_tbl
        where tenant_id = v_sys_tenant and code = 'creditcurves'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_analytic_types_tbl (
            code, tenant_id, version,
            name, description, parameter_entity, display_order,
            modified_by, change_reason_code, change_commentary
        ) values (
            'creditcurves', v_sys_tenant, 0,
            'Credit Curves',
            'The credit curves the run built from the market''s default probabilities.',
            null,
            10,
            current_user, 'system.initial_load', 'Seed analytic type: creditcurves'
        );
        raise debug 'Created analytic type: creditcurves';
    else
        raise debug 'Analytic type already exists: creditcurves';
    end if;

    if not exists (
        select 1 from ores_reporting_analytic_types_tbl
        where tenant_id = v_sys_tenant and code = 'calibration'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_analytic_types_tbl (
            code, tenant_id, version,
            name, description, parameter_entity, display_order,
            modified_by, change_reason_code, change_commentary
        ) values (
            'calibration', v_sys_tenant, 0,
            'Calibration',
            'Calibration of the model the run''s simulation drew on.',
            null,
            11,
            current_user, 'system.initial_load', 'Seed analytic type: calibration'
        );
        raise debug 'Created analytic type: calibration';
    else
        raise debug 'Analytic type already exists: calibration';
    end if;

    if not exists (
        select 1 from ores_reporting_analytic_types_tbl
        where tenant_id = v_sys_tenant and code = 'xvaStress'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_analytic_types_tbl (
            code, tenant_id, version,
            name, description, parameter_entity, display_order,
            modified_by, change_reason_code, change_commentary
        ) values (
            'xvaStress', v_sys_tenant, 0,
            'XVA Stress',
            'XVA under each named stress scenario.',
            null,
            12,
            current_user, 'system.initial_load', 'Seed analytic type: xvaStress'
        );
        raise debug 'Created analytic type: xvaStress';
    else
        raise debug 'Analytic type already exists: xvaStress';
    end if;

    if not exists (
        select 1 from ores_reporting_analytic_types_tbl
        where tenant_id = v_sys_tenant and code = 'xvaSensitivity'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_analytic_types_tbl (
            code, tenant_id, version,
            name, description, parameter_entity, display_order,
            modified_by, change_reason_code, change_commentary
        ) values (
            'xvaSensitivity', v_sys_tenant, 0,
            'XVA Sensitivity',
            'Sensitivities of the XVA adjustments.',
            null,
            13,
            current_user, 'system.initial_load', 'Seed analytic type: xvaSensitivity'
        );
        raise debug 'Created analytic type: xvaSensitivity';
    else
        raise debug 'Analytic type already exists: xvaSensitivity';
    end if;

    if not exists (
        select 1 from ores_reporting_analytic_types_tbl
        where tenant_id = v_sys_tenant and code = 'pnlExplain'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_analytic_types_tbl (
            code, tenant_id, version,
            name, description, parameter_entity, display_order,
            modified_by, change_reason_code, change_commentary
        ) values (
            'pnlExplain', v_sys_tenant, 0,
            'P&L Explain',
            'The day''s profit and loss attributed to its risk factors.',
            null,
            14,
            current_user, 'system.initial_load', 'Seed analytic type: pnlExplain'
        );
        raise debug 'Created analytic type: pnlExplain';
    else
        raise debug 'Analytic type already exists: pnlExplain';
    end if;

    if not exists (
        select 1 from ores_reporting_analytic_types_tbl
        where tenant_id = v_sys_tenant and code = 'historicalSimulationVar'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_analytic_types_tbl (
            code, tenant_id, version,
            name, description, parameter_entity, display_order,
            modified_by, change_reason_code, change_commentary
        ) values (
            'historicalSimulationVar', v_sys_tenant, 0,
            'Historical Simulation VaR',
            'Value at risk from a historical simulation over the portfolio.',
            null,
            15,
            current_user, 'system.initial_load', 'Seed analytic type: historicalSimulationVar'
        );
        raise debug 'Created analytic type: historicalSimulationVar';
    else
        raise debug 'Analytic type already exists: historicalSimulationVar';
    end if;

    if not exists (
        select 1 from ores_reporting_analytic_types_tbl
        where tenant_id = v_sys_tenant and code = 'pnl'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_analytic_types_tbl (
            code, tenant_id, version,
            name, description, parameter_entity, display_order,
            modified_by, change_reason_code, change_commentary
        ) values (
            'pnl', v_sys_tenant, 0,
            'P&L',
            'The portfolio''s profit and loss between two dates.',
            null,
            16,
            current_user, 'system.initial_load', 'Seed analytic type: pnl'
        );
        raise debug 'Created analytic type: pnl';
    else
        raise debug 'Analytic type already exists: pnl';
    end if;

    if not exists (
        select 1 from ores_reporting_analytic_types_tbl
        where tenant_id = v_sys_tenant and code = 'parametricVar'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_analytic_types_tbl (
            code, tenant_id, version,
            name, description, parameter_entity, display_order,
            modified_by, change_reason_code, change_commentary
        ) values (
            'parametricVar', v_sys_tenant, 0,
            'Parametric VaR',
            'Value at risk from a parametric model over the portfolio.',
            null,
            17,
            current_user, 'system.initial_load', 'Seed analytic type: parametricVar'
        );
        raise debug 'Created analytic type: parametricVar';
    else
        raise debug 'Analytic type already exists: parametricVar';
    end if;

    if not exists (
        select 1 from ores_reporting_analytic_types_tbl
        where tenant_id = v_sys_tenant and code = 'zeroToParSensiConversion'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_analytic_types_tbl (
            code, tenant_id, version,
            name, description, parameter_entity, display_order,
            modified_by, change_reason_code, change_commentary
        ) values (
            'zeroToParSensiConversion', v_sys_tenant, 0,
            'Zero to Par Sensitivity Conversion',
            'Convert zero-rate sensitivities to par-rate sensitivities.',
            null,
            18,
            current_user, 'system.initial_load', 'Seed analytic type: zeroToParSensiConversion'
        );
        raise debug 'Created analytic type: zeroToParSensiConversion';
    else
        raise debug 'Analytic type already exists: zeroToParSensiConversion';
    end if;

    if not exists (
        select 1 from ores_reporting_analytic_types_tbl
        where tenant_id = v_sys_tenant and code = 'scenarioGeneration'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_analytic_types_tbl (
            code, tenant_id, version,
            name, description, parameter_entity, display_order,
            modified_by, change_reason_code, change_commentary
        ) values (
            'scenarioGeneration', v_sys_tenant, 0,
            'Scenario Generation',
            'Generate the scenarios a later analytic reads, rather than using them.',
            null,
            19,
            current_user, 'system.initial_load', 'Seed analytic type: scenarioGeneration'
        );
        raise debug 'Created analytic type: scenarioGeneration';
    else
        raise debug 'Analytic type already exists: scenarioGeneration';
    end if;

    if not exists (
        select 1 from ores_reporting_analytic_types_tbl
        where tenant_id = v_sys_tenant and code = 'xvaExplain'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_analytic_types_tbl (
            code, tenant_id, version,
            name, description, parameter_entity, display_order,
            modified_by, change_reason_code, change_commentary
        ) values (
            'xvaExplain', v_sys_tenant, 0,
            'XVA Explain',
            'The XVA adjustments attributed to their drivers.',
            null,
            20,
            current_user, 'system.initial_load', 'Seed analytic type: xvaExplain'
        );
        raise debug 'Created analytic type: xvaExplain';
    else
        raise debug 'Analytic type already exists: xvaExplain';
    end if;

    if not exists (
        select 1 from ores_reporting_analytic_types_tbl
        where tenant_id = v_sys_tenant and code = 'zeroToParShift'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_analytic_types_tbl (
            code, tenant_id, version,
            name, description, parameter_entity, display_order,
            modified_by, change_reason_code, change_commentary
        ) values (
            'zeroToParShift', v_sys_tenant, 0,
            'Zero to Par Shift',
            'Convert a zero-rate shift into the equivalent par-rate shift.',
            null,
            21,
            current_user, 'system.initial_load', 'Seed analytic type: zeroToParShift'
        );
        raise debug 'Created analytic type: zeroToParShift';
    else
        raise debug 'Analytic type already exists: zeroToParShift';
    end if;

    if not exists (
        select 1 from ores_reporting_analytic_types_tbl
        where tenant_id = v_sys_tenant and code = 'additionalResults'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_analytic_types_tbl (
            code, tenant_id, version,
            name, description, parameter_entity, display_order,
            modified_by, change_reason_code, change_commentary
        ) values (
            'additionalResults', v_sys_tenant, 0,
            'Additional Results',
            'The additional results the run''s configuration asked for.',
            null,
            22,
            current_user, 'system.initial_load', 'Seed analytic type: additionalResults'
        );
        raise debug 'Created analytic type: additionalResults';
    else
        raise debug 'Analytic type already exists: additionalResults';
    end if;

    if not exists (
        select 1 from ores_reporting_analytic_types_tbl
        where tenant_id = v_sys_tenant and code = 'pfe'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_analytic_types_tbl (
            code, tenant_id, version,
            name, description, parameter_entity, display_order,
            modified_by, change_reason_code, change_commentary
        ) values (
            'pfe', v_sys_tenant, 0,
            'PFE',
            'Potential future exposure over the portfolio, at the stated confidence.',
            null,
            23,
            current_user, 'system.initial_load', 'Seed analytic type: pfe'
        );
        raise debug 'Created analytic type: pfe';
    else
        raise debug 'Analytic type already exists: pfe';
    end if;

    if not exists (
        select 1 from ores_reporting_analytic_types_tbl
        where tenant_id = v_sys_tenant and code = 'bacva'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_analytic_types_tbl (
            code, tenant_id, version,
            name, description, parameter_entity, display_order,
            modified_by, change_reason_code, change_commentary
        ) values (
            'bacva', v_sys_tenant, 0,
            'BACVA',
            'The Basel CVA capital charge for the portfolio.',
            null,
            24,
            current_user, 'system.initial_load', 'Seed analytic type: bacva'
        );
        raise debug 'Created analytic type: bacva';
    else
        raise debug 'Analytic type already exists: bacva';
    end if;

    if not exists (
        select 1 from ores_reporting_analytic_types_tbl
        where tenant_id = v_sys_tenant and code = 'portfolioDetails'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_analytic_types_tbl (
            code, tenant_id, version,
            name, description, parameter_entity, display_order,
            modified_by, change_reason_code, change_commentary
        ) values (
            'portfolioDetails', v_sys_tenant, 0,
            'Portfolio Details',
            'The portfolio''s trades and their attributes, as a report.',
            null,
            25,
            current_user, 'system.initial_load', 'Seed analytic type: portfolioDetails'
        );
        raise debug 'Created analytic type: portfolioDetails';
    else
        raise debug 'Analytic type already exists: portfolioDetails';
    end if;

    if not exists (
        select 1 from ores_reporting_analytic_types_tbl
        where tenant_id = v_sys_tenant and code = 'crif'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_analytic_types_tbl (
            code, tenant_id, version,
            name, description, parameter_entity, display_order,
            modified_by, change_reason_code, change_commentary
        ) values (
            'crif', v_sys_tenant, 0,
            'CRIF',
            'The common risk interchange format file the regulator''s SIMM reads.',
            null,
            26,
            current_user, 'system.initial_load', 'Seed analytic type: crif'
        );
        raise debug 'Created analytic type: crif';
    else
        raise debug 'Analytic type already exists: crif';
    end if;

    if not exists (
        select 1 from ores_reporting_analytic_types_tbl
        where tenant_id = v_sys_tenant and code = 'saccr'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_analytic_types_tbl (
            code, tenant_id, version,
            name, description, parameter_entity, display_order,
            modified_by, change_reason_code, change_commentary
        ) values (
            'saccr', v_sys_tenant, 0,
            'SA-CCR',
            'The standardised approach counterparty credit risk exposure.',
            null,
            27,
            current_user, 'system.initial_load', 'Seed analytic type: saccr'
        );
        raise debug 'Created analytic type: saccr';
    else
        raise debug 'Analytic type already exists: saccr';
    end if;

    if not exists (
        select 1 from ores_reporting_analytic_types_tbl
        where tenant_id = v_sys_tenant and code = 'correlation'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_analytic_types_tbl (
            code, tenant_id, version,
            name, description, parameter_entity, display_order,
            modified_by, change_reason_code, change_commentary
        ) values (
            'correlation', v_sys_tenant, 0,
            'Correlation',
            'The correlation matrix the run''s model calibrated to.',
            null,
            28,
            current_user, 'system.initial_load', 'Seed analytic type: correlation'
        );
        raise debug 'Created analytic type: correlation';
    else
        raise debug 'Analytic type already exists: correlation';
    end if;

    if not exists (
        select 1 from ores_reporting_analytic_types_tbl
        where tenant_id = v_sys_tenant and code = 'sacva'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_analytic_types_tbl (
            code, tenant_id, version,
            name, description, parameter_entity, display_order,
            modified_by, change_reason_code, change_commentary
        ) values (
            'sacva', v_sys_tenant, 0,
            'SA-CVA',
            'The standardised approach CVA capital charge.',
            null,
            29,
            current_user, 'system.initial_load', 'Seed analytic type: sacva'
        );
        raise debug 'Created analytic type: sacva';
    else
        raise debug 'Analytic type already exists: sacva';
    end if;

    if not exists (
        select 1 from ores_reporting_analytic_types_tbl
        where tenant_id = v_sys_tenant and code = 'scenario'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_analytic_types_tbl (
            code, tenant_id, version,
            name, description, parameter_entity, display_order,
            modified_by, change_reason_code, change_commentary
        ) values (
            'scenario', v_sys_tenant, 0,
            'Scenario',
            'The portfolio''s value under one named scenario.',
            null,
            30,
            current_user, 'system.initial_load', 'Seed analytic type: scenario'
        );
        raise debug 'Created analytic type: scenario';
    else
        raise debug 'Analytic type already exists: scenario';
    end if;

    if not exists (
        select 1 from ores_reporting_analytic_types_tbl
        where tenant_id = v_sys_tenant and code = 'parStressConversion'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_analytic_types_tbl (
            code, tenant_id, version,
            name, description, parameter_entity, display_order,
            modified_by, change_reason_code, change_commentary
        ) values (
            'parStressConversion', v_sys_tenant, 0,
            'Par Stress Conversion',
            'Convert a par-rate stress into the equivalent zero-rate stress.',
            null,
            31,
            current_user, 'system.initial_load', 'Seed analytic type: parStressConversion'
        );
        raise debug 'Created analytic type: parStressConversion';
    else
        raise debug 'Analytic type already exists: parStressConversion';
    end if;

    if not exists (
        select 1 from ores_reporting_analytic_types_tbl
        where tenant_id = v_sys_tenant and code = 'imschedule'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_analytic_types_tbl (
            code, tenant_id, version,
            name, description, parameter_entity, display_order,
            modified_by, change_reason_code, change_commentary
        ) values (
            'imschedule', v_sys_tenant, 0,
            'IM Schedule',
            'The initial margin schedule over the margin period of risk.',
            null,
            32,
            current_user, 'system.initial_load', 'Seed analytic type: imschedule'
        );
        raise debug 'Created analytic type: imschedule';
    else
        raise debug 'Analytic type already exists: imschedule';
    end if;

    if not exists (
        select 1 from ores_reporting_analytic_types_tbl
        where tenant_id = v_sys_tenant and code = 'sensitivityStress'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_analytic_types_tbl (
            code, tenant_id, version,
            name, description, parameter_entity, display_order,
            modified_by, change_reason_code, change_commentary
        ) values (
            'sensitivityStress', v_sys_tenant, 0,
            'Sensitivity Stress',
            'Sensitivities under each named stress scenario.',
            null,
            33,
            current_user, 'system.initial_load', 'Seed analytic type: sensitivityStress'
        );
        raise debug 'Created analytic type: sensitivityStress';
    else
        raise debug 'Analytic type already exists: sensitivityStress';
    end if;

    if not exists (
        select 1 from ores_reporting_analytic_types_tbl
        where tenant_id = v_sys_tenant and code = 'smrc'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_analytic_types_tbl (
            code, tenant_id, version,
            name, description, parameter_entity, display_order,
            modified_by, change_reason_code, change_commentary
        ) values (
            'smrc', v_sys_tenant, 0,
            'SMRC',
            'The standardised method for measuring counterparty risk charge.',
            null,
            34,
            current_user, 'system.initial_load', 'Seed analytic type: smrc'
        );
        raise debug 'Created analytic type: smrc';
    else
        raise debug 'Analytic type already exists: smrc';
    end if;

    if not exists (
        select 1 from ores_reporting_analytic_types_tbl
        where tenant_id = v_sys_tenant and code = 'amc'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_analytic_types_tbl (
            code, tenant_id, version,
            name, description, parameter_entity, display_order,
            modified_by, change_reason_code, change_commentary
        ) values (
            'amc', v_sys_tenant, 0,
            'AMC',
            'The aggregate margin calculation for the portfolio.',
            null,
            35,
            current_user, 'system.initial_load', 'Seed analytic type: amc'
        );
        raise debug 'Created analytic type: amc';
    else
        raise debug 'Analytic type already exists: amc';
    end if;

end;
$$;

select 'Analytic Types' as entity, count(*) as count
from ores_reporting_analytic_types_tbl
where tenant_id = ores_utility_system_tenant_id_fn()
  and valid_to = ores_utility_infinity_timestamp_fn();
