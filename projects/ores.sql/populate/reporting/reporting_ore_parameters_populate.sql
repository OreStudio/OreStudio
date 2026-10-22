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
 * AUTO-GENERATED FILE - DO NOT EDIT MANUALLY
 * Generator: projects/ores.sql/utility/generate_analytic_ore_parameters_populate.py
 *
 * Analytic Ore Parameters Population Script
 *
 * Seeds the parameters of the analytics of ore.xml. The scope is analytic, the
 * subtype is the analytic type and the name is the parameter. The text of each
 * one is what a screen shows for it. This script is idempotent.
 */

\echo '--- Analytic Ore Parameters ---'

do $$
declare
    v_sys_tenant uuid := ores_utility_system_tenant_id_fn();
begin
    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'npv' and name = 'active'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''npv'']/Parameter[@name=''active'']',
            'analytic', 'npv', 'active', 1, 'boolean', true,
            'Run the NPV analytic', 'A flag. Y runs the analytic.',
            'Switches the NPV analytic on or off. When it is on, the run prices every trade of the portfolio at the valuation date and writes the NPV of each trade to the NPV report. Use N to keep the entry in the document without running it. Every analytic has this parameter.',
            'No. The analytic does not run.', 'A flag, written Y or N, or true or false.',
            '1.8.0.7 or earlier', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: npv.active'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'npv' and name = 'baseCurrency'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''npv'']/Parameter[@name=''baseCurrency'']',
            'analytic', 'npv', 'baseCurrency', 2, 'string', false,
            'Base currency', 'A currency code, for example EUR.',
            'Names the currency in which the NPV report states every NPV. ORE converts the NPV of a trade in another currency at the spot rate of the valuation date. The Setup block has a parameter of the same name, and ORE reads that one first. This parameter is the older place for the same choice and ORE reads it only when the Setup block has none. For example, EUR states all NPVs in euro.',
            'The base currency of the Setup block. When that is also missing, the currency of the discounting index.', 'A currency code of three letters, for example EUR.',
            '1.8.0.7 or earlier', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: npv.baseCurrency'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'npv' and name = 'outputFileName'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''npv'']/Parameter[@name=''outputFileName'']',
            'analytic', 'npv', 'outputFileName', 3, 'string', false,
            'NPV report file', 'A file name.',
            'Names the file that holds the NPV report: one row for each trade with its NPV, its currency and its NPV in the base currency. Change it when one run writes several reports and you need to tell the files apart. For example, npv.csv writes the report to npv.csv in the output path.',
            'npv.csv', 'A file name, written without a directory. ORE writes the file in the output path of the Setup block.',
            '1.8.0.7 or earlier', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: npv.outputFileName'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'npv' and name = 'extendedResults'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''npv'']/Parameter[@name=''extendedResults'']',
            'analytic', 'npv', 'extendedResults', 4, 'boolean', false,
            'Extended results', 'A flag. ORE ignores it.',
            'ORE does not read this parameter in any release. Example files carry it with the value N, so it appears in many run documents. Leave it as the document has it, because a value has no effect on the run. To get more output from the NPV analytic, use Write additional results.',
            'No. ORE ignores the parameter, so nothing changes with either value.', 'A flag, written Y or N, or true or false.',
            '1.8.10.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: npv.extendedResults'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'npv' and name = 'additionalResults'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''npv'']/Parameter[@name=''additionalResults'']',
            'analytic', 'npv', 'additionalResults', 5, 'boolean', false,
            'Write additional results', 'A flag. Y writes the report.',
            'Decides whether the run writes the additional results of the pricing engines. These are values that an engine computes on the way to the NPV, for example the annuity, the forward rate and the standard deviation of a swaption. Switch it on to check how an engine reached an NPV. This is the older place for the choice. The newer way is the additional results analytic with its own Active parameter, and ORE reads this parameter only when that analytic does not set it.',
            'No. The run does not write the additional results.', 'A flag, written Y or N, or true or false.',
            '1.8.6.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: npv.additionalResults'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'npv' and name = 'additionalResultsReportPrecision'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''npv'']/Parameter[@name=''additionalResultsReportPrecision'']',
            'analytic', 'npv', 'additionalResultsReportPrecision', 6, 'integer', false,
            'Additional results precision', 'A whole number of decimal places.',
            'Sets the number of decimal places of the numbers in the additional results report. A higher value shows more of each result and makes the file larger. For example, 12 writes a result such as 0.016641350123. This is the older place for the choice. The additional results analytic has a parameter of the same name, and ORE reads this one only when that analytic does not set it.',
            '6', 'A whole number of zero or more.',
            '1.8.12.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: npv.additionalResultsReportPrecision'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'npv' and name = 'outputCurves'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''npv'']/Parameter[@name=''outputCurves'']',
            'analytic', 'npv', 'outputCurves', 7, 'boolean', false,
            'Write curves with the NPV', 'A flag. Y also runs the curves analytic.',
            'Is an older way to switch on the curves report from inside the NPV analytic. When it is Y, the run writes the yield curves as the curves analytic does. ORE reads it only when the curves analytic is not active. For a new document, set Active in the curves analytic instead.',
            'No. The NPV analytic does not write the curves report.', 'A flag, written Y or N, or true or false.',
            '1.8.16.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: npv.outputCurves'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'npv' and name = 'nameMappingInputFile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''npv'']/Parameter[@name=''nameMappingInputFile'']',
            'analytic', 'npv', 'nameMappingInputFile', 8, 'string', false,
            'Name mapping file', 'A file name.',
            'Names the file that maps the names in the portfolio to the names that the CRIF report uses. The CRIF report needs this mapping. ORE reads the file name from the first of these blocks that has one: crif, saccr, simm, npv and Setup. This is the fourth place. The Setup block is the best place, because it serves all of the analytics.',
            'None. When no block names a file, ORE uses the names as they are.', 'A file name, written without a directory. ORE writes the file in the output path of the Setup block.',
            '1.8.13.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: npv.nameMappingInputFile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'npv' and name = 'bucketMappingInputFile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''npv'']/Parameter[@name=''bucketMappingInputFile'']',
            'analytic', 'npv', 'bucketMappingInputFile', 9, 'string', false,
            'Bucket mapping file', 'A file name.',
            'Names the file that maps risk factors to the SIMM buckets of the CRIF report. ORE reads the file name from the first of these blocks that has one: crif, saccr, simm, npv and Setup. This is the fourth place. The Setup block is the best place, because it serves all of the analytics.',
            'None. When no block names a file, ORE uses the standard SIMM buckets.', 'A file name, written without a directory. ORE writes the file in the output path of the Setup block.',
            '1.8.13.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: npv.bucketMappingInputFile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'cashflow' and name = 'active'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''cashflow'']/Parameter[@name=''active'']',
            'analytic', 'cashflow', 'active', 10, 'boolean', true,
            'Run the cash flow analytic', 'A flag. Y runs the analytic.',
            'Switches the cash flow analytic on or off. When it is on, the run writes a report with every future cash flow of every trade: the pay date, the amount, the currency, the coupon, the accrual and the fixing. Use it to check the cash flows that sit behind an NPV.',
            'No. The analytic does not run.', 'A flag, written Y or N, or true or false.',
            '1.8.0.7 or earlier', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: cashflow.active'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'cashflow' and name = 'outputFileName'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''cashflow'']/Parameter[@name=''outputFileName'']',
            'analytic', 'cashflow', 'outputFileName', 11, 'string', false,
            'Cash flow report file', 'A file name.',
            'Names the file that holds the cash flow report. Change it when one run writes several reports and you need to tell the files apart. For example, flows.csv writes the report to flows.csv in the output path.',
            'cashflow.csv', 'A file name, written without a directory. ORE writes the file in the output path of the Setup block.',
            '1.8.0.7 or earlier', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: cashflow.outputFileName'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'cashflow' and name = 'includePastCashflows'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''cashflow'']/Parameter[@name=''includePastCashflows'']',
            'analytic', 'cashflow', 'includePastCashflows', 12, 'boolean', false,
            'Include past cash flows', 'A flag. Y adds the cash flows before the valuation date.',
            'Decides whether the report also lists cash flows with a pay date before the valuation date. Past cash flows are useful to check fixings and accruals of the current period. They make the report longer. This is the older place for the choice. The Setup block has a parameter of the same name, and ORE reads that one first.',
            'No. The report lists only the cash flows from the valuation date on.', 'A flag, written Y or N, or true or false.',
            '1.8.6.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: cashflow.includePastCashflows'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'cashflow' and name = 'cashFlowHorizon'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''cashflow'']/Parameter[@name=''cashFlowHorizon'']',
            'analytic', 'cashflow', 'cashFlowHorizon', 13, 'date', false,
            'Cash flow horizon', 'A date.',
            'Sets the last pay date that the cash flow NPV analytic counts. The analytic adds up the present value of the cash flows of each trade that pay after the valuation date and on or before this date. Use it to see how much of an NPV falls due in the near term. For example, 2027-12-31 counts the cash flows up to the end of 2027. The cash flow NPV analytic has a parameter of the same name.',
            'None. Without a horizon the analytic counts no cash flow, so every present value is 0.', 'A date, written as YYYY-MM-DD or YYYYMMDD.',
            '1.8.9.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: cashflow.cashFlowHorizon'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'cashflow' and name = 'portfolioFilterDate'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''cashflow'']/Parameter[@name=''portfolioFilterDate'']',
            'analytic', 'cashflow', 'portfolioFilterDate', 14, 'date', false,
            'Portfolio filter date', 'A date.',
            'Sets the date that decides which trades the run keeps. ORE removes every trade that matures before this date. This applies to all analytics of the run, not only to the cash flow analytic. For example, 2027-01-01 removes all trades that mature before the start of 2027.',
            'The valuation date. ORE removes the trades that have matured.', 'A date, written as YYYY-MM-DD or YYYYMMDD.',
            '1.8.9.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: cashflow.portfolioFilterDate'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'curves' and name = 'active'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''curves'']/Parameter[@name=''active'']',
            'analytic', 'curves', 'active', 15, 'boolean', true,
            'Run the curves analytic', 'A flag. Y runs the analytic.',
            'Switches the curves analytic on or off. When it is on, the run exports the discount factors of the yield curves that it builds from the market data, as set in the todaysmarket file. Use it to check the curves behind an NPV.',
            'No. The analytic does not run.', 'A flag, written Y or N, or true or false.',
            '1.8.0.7 or earlier', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: curves.active'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'curves' and name = 'configuration'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''curves'']/Parameter[@name=''configuration'']',
            'analytic', 'curves', 'configuration', 16, 'string', false,
            'Market configuration', 'The name of a market configuration.',
            'Selects the set of curves that the analytic exports. The todaysmarket file groups the curves in named configurations, and this parameter picks one. For example, default exports the curves of the default configuration.',
            'default', 'The name of a configuration in the todaysmarket file.',
            '1.8.0.7 or earlier', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: curves.configuration'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'curves' and name = 'grid'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''curves'']/Parameter[@name=''grid'']',
            'analytic', 'curves', 'grid', 17, 'string', false,
            'Time grid', 'A number of steps and a step length, or a list of tenors.',
            'Sets the dates at which the analytic exports the discount factors. Write it as a number of steps followed by the length of a step, or as a list of tenors. ORE builds the grid from the valuation date and rolls it with the calendar of this analytic. For example, 240,1M gives 240 monthly steps from the valuation date, and 1W,1M,2M,3M gives the four dates one week, one month, two months and three months out.',
            '240,1M. That is 240 monthly steps, 20 years.', 'Either a number of steps, a comma and a period such as 1M, or a comma-separated list of periods.',
            '1.8.0.7 or earlier', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: curves.grid'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'curves' and name = 'outputFileName'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''curves'']/Parameter[@name=''outputFileName'']',
            'analytic', 'curves', 'outputFileName', 18, 'string', false,
            'Curves report file', 'A file name.',
            'Names the file that holds the exported discount factors. The file has one row for each date of the grid and one column for each curve. For example, curves.csv writes the report to curves.csv in the output path.',
            'curves.csv', 'A file name, written without a directory. ORE writes the file in the output path of the Setup block.',
            '1.8.0.7 or earlier', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: curves.outputFileName'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'curves' and name = 'outputTodaysMarketCalibration'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''curves'']/Parameter[@name=''outputTodaysMarketCalibration'']',
            'analytic', 'curves', 'outputTodaysMarketCalibration', 19, 'boolean', false,
            'Write the market calibration report', 'A flag. Y writes the calibration report.',
            'Decides whether the run also writes a report on how it built the market objects of the valuation date. The report shows the calibration of each curve and surface. Switch it on to find out why a curve has the shape it has, or to find a build that failed.',
            'No. The run does not write the calibration report.', 'A flag, written Y or N, or true or false.',
            '1.8.9.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: curves.outputTodaysMarketCalibration'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'curves' and name = 'calendar'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''curves'']/Parameter[@name=''calendar'']',
            'analytic', 'curves', 'calendar', 20, 'string', false,
            'Grid calendar', 'A calendar name.',
            'Names the calendar that ORE uses to roll the dates of the grid to business days. Change it when the curves belong to a market with other holidays than the euro area. For example, UK rolls the grid dates to business days of the United Kingdom.',
            'TARGET', 'A calendar name such as TARGET or UK.',
            '1.8.6.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: curves.calendar'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'curves' and name = 'todaysMarketCalibrationPrecision'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''curves'']/Parameter[@name=''todaysMarketCalibrationPrecision'']',
            'analytic', 'curves', 'todaysMarketCalibrationPrecision', 21, 'integer', false,
            'Calibration report precision', 'A whole number of decimal places.',
            'Sets the number of decimal places of the numbers in the market calibration report. A higher value shows more of each number and makes the file larger. It has an effect only when the market calibration report is on. The NPV analytic has a similar parameter for its additional results.',
            '8', 'A whole number of zero or more.',
            '1.8.14.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: curves.todaysMarketCalibrationPrecision'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'simulation' and name = 'active'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''simulation'']/Parameter[@name=''active'']',
            'analytic', 'simulation', 'active', 22, 'boolean', true,
            'Run the exposure simulation', 'A flag. Y runs the simulation.',
            'Switches the exposure simulation on or off. When it is on, the run evolves the market with a Monte Carlo simulation and values every trade of the portfolio at each date of the simulation grid on each path. The result is an NPV cube. The XVA, PFE and other exposure analytics use this cube. Use N to keep the entry in the document without running it.',
            'No. The simulation does not run.', 'A flag, written Y or N, or true or false.',
            '1.8.0.7 or earlier', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: simulation.active'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'simulation' and name = 'amc'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''simulation'']/Parameter[@name=''amc'']',
            'analytic', 'simulation', 'amc', 23, 'boolean', false,
            'Use American Monte Carlo', 'A flag. Y uses American Monte Carlo for the trade types listed.',
            'Decides whether ORE values some trade types with American Monte Carlo (AMC) instead of the classic simulation. AMC regresses the trade value on the simulated state and is much faster for products with early exercise. The parameter AMC trade types lists the trade types that AMC covers. The classic simulation values the other trades, and ORE joins the two cubes.',
            'No. The classic simulation values all trades.', 'A flag, written Y or N, or true or false.',
            '1.8.9.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: simulation.amc'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'simulation' and name = 'amcTradeTypes'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''simulation'']/Parameter[@name=''amcTradeTypes'']',
            'analytic', 'simulation', 'amcTradeTypes', 24, 'string', false,
            'AMC trade types', 'A comma-separated list of trade types.',
            'Lists the trade types that ORE values with American Monte Carlo when AMC is on. The allowed types are Swap, Swaption, FxForward, FxOption, ForwardBond, FxTaRF, ScriptedTrade and CompositeTrade. The classic simulation values every other trade of the portfolio. For example, Swap,Swaption,FxOption.',
            'None. With AMC on and no list, no trade type is covered by AMC.', 'A comma-separated list of trade type names.',
            '1.8.9.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: simulation.amcTradeTypes'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'simulation' and name = 'simulationConfigFile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''simulation'']/Parameter[@name=''simulationConfigFile'']',
            'analytic', 'simulation', 'simulationConfigFile', 25, 'string', false,
            'Simulation configuration file', 'A file name.',
            'Names the XML file that sets up the simulation. The file holds three things: the simulated market (which curves, volatilities and rates evolve), the model (the cross asset model and its calibration) and the scenario generator (the date grid, the number of samples and the random number generator). ORE reads all three from this one file, unless the parameters Cross asset model data or Scenario generator data name another file. For example, simulation.xml.',
            'None. The simulation needs this file, so a run without it fails.', 'A file name. ORE looks for the file in the input path of the Setup block.',
            '1.8.0.7 or earlier', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: simulation.simulationConfigFile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'simulation' and name = 'pricingEnginesFile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''simulation'']/Parameter[@name=''pricingEnginesFile'']',
            'analytic', 'simulation', 'pricingEnginesFile', 26, 'string', false,
            'Pricing engines file for the simulation', 'A file name.',
            'Names the XML file that sets the pricing engines for the valuations on the simulated scenarios. These can differ from the engines of the valuation date, for example faster approximations. For example, pricingengine.xml.',
            'The pricing engines file of the Setup block.', 'A file name. ORE looks for the file in the input path of the Setup block.',
            '1.8.0.7 or earlier', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: simulation.pricingEnginesFile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'simulation' and name = 'amcPricingEnginesFile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''simulation'']/Parameter[@name=''amcPricingEnginesFile'']',
            'analytic', 'simulation', 'amcPricingEnginesFile', 27, 'string', false,
            'Pricing engines file for AMC', 'A file name.',
            'Names the XML file that sets the pricing engines for the trades that AMC values. These engines differ from the classic ones, because they are the AMC pricers. For example, pricingengine_amc.xml.',
            'The pricing engines file of the Setup block.', 'A file name. ORE looks for the file in the input path of the Setup block.',
            '1.8.9.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: simulation.amcPricingEnginesFile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'simulation' and name = 'baseCurrency'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''simulation'']/Parameter[@name=''baseCurrency'']',
            'analytic', 'simulation', 'baseCurrency', 28, 'string', false,
            'Base currency for the cube', 'A currency code, for example EUR.',
            'Names the currency in which ORE converts all NPVs before it writes them to the trade-level cube and the netting set cube. Exposures and XVA are then stated in this currency. For example, EUR stores all cube values in euro.',
            'The base currency of the Setup block.', 'A currency code of three letters, for example EUR.',
            '1.8.0.7 or earlier', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: simulation.baseCurrency'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'simulation' and name = 'storeScenarios'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''simulation'']/Parameter[@name=''storeScenarios'']',
            'analytic', 'simulation', 'storeScenarios', 29, 'boolean', false,
            'Store scenarios', 'A flag. ORE ignores it.',
            'ORE does not read this parameter in any release. Example files carry it with the value N. To write the scenarios, use Scenario dump file or Write scenarios.',
            'No. ORE ignores the parameter, so a value has no effect.', 'A flag, written Y or N, or true or false.',
            '1.8.0.7 or earlier', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: simulation.storeScenarios'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'simulation' and name = 'cubeFile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''simulation'']/Parameter[@name=''cubeFile'']',
            'analytic', 'simulation', 'cubeFile', 30, 'string', false,
            'Cube file', 'A file name.',
            'Names the file that holds the NPV cube of the simulation: the value of every trade on every date and every path. Setting it also switches the cube output on. The XVA and PFE analytics can read the file back with their own Cube file parameter, so a later run does not need to simulate again. Example files use cube.csv.gz and cube.dat.',
            'None. When Write cube is on, the name is cube.csv.gz.', 'A file name, written without a directory. ORE writes the file in the output path of the Setup block.',
            '1.8.0.7 or earlier', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: simulation.cubeFile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'simulation' and name = 'aggregationScenarioDataFileName'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''simulation'']/Parameter[@name=''aggregationScenarioDataFileName'']',
            'analytic', 'simulation', 'aggregationScenarioDataFileName', 31, 'string', false,
            'Aggregation scenario data file', 'A file name.',
            'Names the file that holds the market data that the XVA post processing needs, for example the simulated FX rates and index fixings that the variation margin calculation uses. The simulation configuration file lists the data to store. For example, scenariodata.csv.gz.',
            'None. When Write cube is on, the name is scenariodata.csv.gz.', 'A file name, written without a directory. ORE writes the file in the output path of the Setup block.',
            '1.8.3.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: simulation.aggregationScenarioDataFileName'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'simulation' and name = 'aggregationScenarioDataDump'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''simulation'']/Parameter[@name=''aggregationScenarioDataDump'']',
            'analytic', 'simulation', 'aggregationScenarioDataDump', 32, 'string', false,
            'Aggregation scenario data dump', 'A file name. ORE no longer reads it.',
            'Named a CSV file for a dump of the aggregation scenario data. ORE stopped reading it in 1.8.9.1. Use Aggregation scenario data file to name the file that holds this data.',
            'None. ORE ignores the parameter.', 'A file name, written without a directory. ORE writes the file in the output path of the Setup block.',
            '1.8.3.0', '1.8.9.1',
            current_user, 'system.initial_load', 'Seed analytic parameter: simulation.aggregationScenarioDataDump'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'simulation' and name = 'allowPartialScenarios'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''simulation'']/Parameter[@name=''allowPartialScenarios'']',
            'analytic', 'simulation', 'allowPartialScenarios', 33, 'boolean', false,
            'Allow partial scenarios', 'A flag. Y accepts scenarios that touch only part of the market.',
            'Decides whether the simulated market accepts scenarios that do not set every risk factor of the simulation market. When it is N, ORE checks that each scenario covers all of the market and stops if a risk factor is missing. Switch it on when you read scenarios from a file that covers only part of the market.',
            'No. ORE checks that scenarios cover the whole market.', 'A flag, written Y or N, or true or false.',
            '1.8.13.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: simulation.allowPartialScenarios'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'simulation' and name = 'scenariodump'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''simulation'']/Parameter[@name=''scenariodump'']',
            'analytic', 'simulation', 'scenariodump', 34, 'string', false,
            'Scenario dump file', 'A file name.',
            'Names the file that receives the raw market scenarios of the simulation: the discount factors of the yield curves and the other simulated values, for each date and path. Setting it also switches the output on. Use it to inspect the scenarios or to feed them to another tool. Filter risk keys limits the content. For example, scenariodump.csv.',
            'None. When Write scenarios is on, the name is scenario.csv.', 'A file name, written without a directory. ORE writes the file in the output path of the Setup block.',
            '1.8.0.7 or earlier', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: simulation.scenariodump'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'simulation' and name = 'amcCg'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''simulation'']/Parameter[@name=''amcCg'']',
            'analytic', 'simulation', 'amcCg', 35, 'string', false,
            'Computation graph engine mode', 'One of Disabled, CubeGeneration or Full.',
            'Selects the use of the computation graph (CG) engine for AMC. Disabled keeps the normal AMC. CubeGeneration builds the NPV cube with the CG engine and leaves the XVA post processing to the normal code. Full also computes the XVA with the CG engine. The CG engine allows sensitivities by algorithmic differentiation and dynamic initial margin.',
            'Disabled', 'One of Disabled, CubeGeneration or Full.',
            '1.8.12.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: simulation.amcCg'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'simulation' and name = 'xvaCgUseExternalComputeDevice'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''simulation'']/Parameter[@name=''xvaCgUseExternalComputeDevice'']',
            'analytic', 'simulation', 'xvaCgUseExternalComputeDevice', 36, 'boolean', false,
            'CG use an external compute device', 'A flag. Y runs the graph on an external device.',
            'Decides whether the CG engine runs the computation graph on an external compute device, for example a GPU, instead of the CPU. The parameter CG external compute device names the device.',
            'No. The engine runs on the CPU.', 'A flag, written Y or N, or true or false.',
            '1.8.13.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: simulation.xvaCgUseExternalComputeDevice'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'simulation' and name = 'xvaCgExternalDeviceCompatibilityMode'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''simulation'']/Parameter[@name=''xvaCgExternalDeviceCompatibilityMode'']',
            'analytic', 'simulation', 'xvaCgExternalDeviceCompatibilityMode', 37, 'boolean', false,
            'CG device compatibility mode', 'A flag. Y uses the compatibility mode.',
            'Decides whether the CG engine runs the external device in its compatibility mode. Switch it on when the device does not run the normal mode correctly. Example files set it together with the name of the device.',
            'No.', 'A flag, written Y or N, or true or false.',
            '1.8.13.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: simulation.xvaCgExternalDeviceCompatibilityMode'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'simulation' and name = 'xvaCgUseDoublePrecisionForExternalCalculation'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''simulation'']/Parameter[@name=''xvaCgUseDoublePrecisionForExternalCalculation'']',
            'analytic', 'simulation', 'xvaCgUseDoublePrecisionForExternalCalculation', 38, 'boolean', false,
            'CG double precision on the device', 'A flag. Y uses double precision.',
            'Decides whether the external device calculates with double precision numbers instead of single precision. Double precision is more exact and slower, and some devices do not support it.',
            'No. The device uses single precision.', 'A flag, written Y or N, or true or false.',
            '1.8.13.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: simulation.xvaCgUseDoublePrecisionForExternalCalculation'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'simulation' and name = 'xvaCgExternalComputeDevice'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''simulation'']/Parameter[@name=''xvaCgExternalComputeDevice'']',
            'analytic', 'simulation', 'xvaCgExternalComputeDevice', 39, 'string', false,
            'CG external compute device', 'The name of a compute device.',
            'Names the device that the CG engine uses when it runs on an external compute device. The name has the form framework, vendor and device, separated by slashes. For example, OpenCL/Apple/Apple M2 Max, or BasicCpu/Default/Default for a plain CPU back end.',
            'None. Without a name, ORE uses no external device.', 'Three names separated by slashes, for example OpenCL/Apple/Apple M2 Max.',
            '1.8.13.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: simulation.xvaCgExternalComputeDevice'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'simulation' and name = 'amcCgPricingEnginesFile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''simulation'']/Parameter[@name=''amcCgPricingEnginesFile'']',
            'analytic', 'simulation', 'amcCgPricingEnginesFile', 40, 'string', false,
            'Pricing engines file for the CG engine', 'A file name.',
            'Names the XML file that sets the pricing engines for the computation graph engine. For example, pricingengine_amccg.xml.',
            'The pricing engines file of the Setup block.', 'A file name. ORE looks for the file in the input path of the Setup block.',
            '1.8.13.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: simulation.amcCgPricingEnginesFile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'simulation' and name = 'xvaCgDynamicIM'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''simulation'']/Parameter[@name=''xvaCgDynamicIM'']',
            'analytic', 'simulation', 'xvaCgDynamicIM', 41, 'boolean', false,
            'CG dynamic initial margin', 'A flag. Y computes dynamic initial margin with the CG engine.',
            'Decides whether the CG engine also computes the initial margin along each path. The DIM model DynamicIM of the XVA analytic needs this. It makes the run longer and the memory use higher.',
            'No.', 'A flag, written Y or N, or true or false.',
            '1.8.13.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: simulation.xvaCgDynamicIM'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'simulation' and name = 'xvaCgDynamicIMStepSize'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''simulation'']/Parameter[@name=''xvaCgDynamicIMStepSize'']',
            'analytic', 'simulation', 'xvaCgDynamicIMStepSize', 42, 'integer', false,
            'CG dynamic IM step size', 'A whole number of time steps.',
            'Sets how often the CG engine computes dynamic initial margin: on every n-th time step of the grid. A value of 1 computes it on every step. With a larger value the run is faster, and ORE copies the margin of the last computed step to the steps in between. For example, 4 computes it on every fourth step.',
            '1', 'A whole number of one or more.',
            '1.8.13.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: simulation.xvaCgDynamicIMStepSize'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'simulation' and name = 'xvaCgRegressionOrder'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''simulation'']/Parameter[@name=''xvaCgRegressionOrder'']',
            'analytic', 'simulation', 'xvaCgRegressionOrder', 43, 'integer', false,
            'CG regression order', 'A whole number.',
            'Sets the order of the polynomial that the CG engine uses in the regression for the exposures. A higher order fits the value better and needs more paths to stay stable. For example, 4.',
            '4', 'A whole number of one or more.',
            '1.8.13.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: simulation.xvaCgRegressionOrder'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'simulation' and name = 'xvaCgRegressionOrderDynamicIm'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''simulation'']/Parameter[@name=''xvaCgRegressionOrderDynamicIm'']',
            'analytic', 'simulation', 'xvaCgRegressionOrderDynamicIm', 44, 'integer', false,
            'CG regression order for dynamic IM', 'A whole number.',
            'Sets the order of the regression polynomial that the CG engine uses for dynamic initial margin. It works like the CG regression order and applies to the margin only.',
            '4', 'A whole number of one or more.',
            '1.8.14.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: simulation.xvaCgRegressionOrderDynamicIm'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'simulation' and name = 'xvaCgTradeLevelBreakDown'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''simulation'']/Parameter[@name=''xvaCgTradeLevelBreakDown'']',
            'analytic', 'simulation', 'xvaCgTradeLevelBreakDown', 45, 'boolean', false,
            'CG trade level breakdown', 'A flag. Y keeps the results for each trade.',
            'Decides whether the CG engine produces the exposure results for each trade as well as for each netting set. Switch it off when you need only the netting set results.',
            'Yes. The engine keeps the results for each trade.', 'A flag, written Y or N, or true or false.',
            '1.8.13.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: simulation.xvaCgTradeLevelBreakDown'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'simulation' and name = 'xvaCgUseRedBlocks'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''simulation'']/Parameter[@name=''xvaCgUseRedBlocks'']',
            'analytic', 'simulation', 'xvaCgUseRedBlocks', 46, 'boolean', false,
            'CG use red blocks', 'A flag. Y uses red blocks.',
            'Decides whether the CG engine puts the part of the graph that belongs to one trade into a red block while it builds the graph. It is on by default. Switch it off to build the graph without red blocks, for example to check a result.',
            'Yes.', 'A flag, written Y or N, or true or false.',
            '1.8.13.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: simulation.xvaCgUseRedBlocks'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'simulation' and name = 'storeFlows'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''simulation'']/Parameter[@name=''storeFlows'']',
            'analytic', 'simulation', 'storeFlows', 47, 'boolean', false,
            'Store cash flows in the cube', 'A flag. Y stores the cash flows.',
            'Decides whether the cube also stores the cumulative cash flows between the simulation dates. Some dynamic initial margin and variation margin models need them in the XVA post processing.',
            'No.', 'A flag, written Y or N, or true or false.',
            '1.8.0.7 or earlier', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: simulation.storeFlows'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'simulation' and name = 'storeSensis'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''simulation'']/Parameter[@name=''storeSensis'']',
            'analytic', 'simulation', 'storeSensis', 48, 'boolean', false,
            'Store sensitivities along the paths', 'A flag. Y stores the sensitivities.',
            'Decides whether the simulation stores the analytical sensitivities of the trades along each path, in the netting set cube. Some dynamic initial margin models need them. The parameters Curve sensitivity grid and Vega sensitivity grid set the points. The run stops if it stores sensitivities and neither grid is set.',
            'No.', 'A flag, written Y or N, or true or false.',
            '1.8.13.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: simulation.storeSensis'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'simulation' and name = 'curveSensiGrid'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''simulation'']/Parameter[@name=''curveSensiGrid'']',
            'analytic', 'simulation', 'curveSensiGrid', 49, 'string', false,
            'Curve sensitivity grid', 'A comma-separated list of numbers of years.',
            'Lists the points on the yield curves for which the simulation stores sensitivities when Store sensitivities along the paths is on. For example, 1,2,3,5,7,10,15,20.',
            'None.', 'A comma-separated list of numbers of years, for example 1,2,3,5,7,10.',
            '1.8.13.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: simulation.curveSensiGrid'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'simulation' and name = 'vegaSensiGrid'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''simulation'']/Parameter[@name=''vegaSensiGrid'']',
            'analytic', 'simulation', 'vegaSensiGrid', 50, 'string', false,
            'Vega sensitivity grid', 'A comma-separated list of numbers of years.',
            'Lists the option expiries for which the simulation stores vega sensitivities when Store sensitivities along the paths is on. For example, 1,2,3,5,7,10,15,20.',
            'None.', 'A comma-separated list of numbers of years, for example 1,2,3,5,7,10.',
            '1.8.13.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: simulation.vegaSensiGrid'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'simulation' and name = 'storeSurvivalProbabilities'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''simulation'']/Parameter[@name=''storeSurvivalProbabilities'']',
            'analytic', 'simulation', 'storeSurvivalProbabilities', 51, 'boolean', false,
            'Store survival probabilities', 'A flag. Y stores the survival probabilities.',
            'Decides whether the cube stores the survival probabilities of the counterparties on each simulation date. The dynamic credit XVA calculation needs them.',
            'No.', 'A flag, written Y or N, or true or false.',
            '1.8.6.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: simulation.storeSurvivalProbabilities'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'simulation' and name = 'simulationModelFile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''simulation'']/Parameter[@name=''simulationModelFile'']',
            'analytic', 'simulation', 'simulationModelFile', 52, 'string', false,
            'Simulation model file', 'A file name. ORE ignores it.',
            'ORE does not read this parameter in any release. Example files of an older layout carry it. ORE reads the model from the simulation configuration file.',
            'None. ORE ignores the parameter, so a value has no effect.', 'A file name. ORE looks for the file in the input path of the Setup block.',
            '1.8.0.7 or earlier', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: simulation.simulationModelFile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'simulation' and name = 'simulationMarketFile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''simulation'']/Parameter[@name=''simulationMarketFile'']',
            'analytic', 'simulation', 'simulationMarketFile', 53, 'string', false,
            'Simulation market file', 'A file name. ORE ignores it.',
            'ORE does not read this parameter in any release. Example files of an older layout carry it. ORE reads the simulated market from the simulation configuration file.',
            'None. ORE ignores the parameter, so a value has no effect.', 'A file name. ORE looks for the file in the input path of the Setup block.',
            '1.8.0.7 or earlier', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: simulation.simulationMarketFile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'simulation' and name = 'simulationParamFile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''simulation'']/Parameter[@name=''simulationParamFile'']',
            'analytic', 'simulation', 'simulationParamFile', 54, 'string', false,
            'Simulation parameter file', 'A file name. ORE ignores it.',
            'ORE does not read this parameter in any release. Example files of an older layout carry it. ORE reads the scenario generator from the simulation configuration file.',
            'None. ORE ignores the parameter, so a value has no effect.', 'A file name. ORE looks for the file in the input path of the Setup block.',
            '1.8.0.7 or earlier', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: simulation.simulationParamFile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'simulation' and name = 'computeCube'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''simulation'']/Parameter[@name=''computeCube'']',
            'analytic', 'simulation', 'computeCube', 55, 'boolean', false,
            'Compute the cube', 'A flag. ORE ignores it.',
            'ORE does not read this parameter in any release. Example files carry it with the value Y. The simulation always computes the cube when it runs.',
            'Yes. ORE ignores the parameter, so a value has no effect.', 'A flag, written Y or N, or true or false.',
            '1.8.10.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: simulation.computeCube'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'simulation' and name = 'additionalScenarioDataFileName'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''simulation'']/Parameter[@name=''additionalScenarioDataFileName'']',
            'analytic', 'simulation', 'additionalScenarioDataFileName', 56, 'string', false,
            'Additional scenario data file', 'A file name. ORE no longer reads it.',
            'Named the file for the additional market data of the simulation. ORE stopped reading it in 1.8.3.0. Use Aggregation scenario data file to name the file that holds this data.',
            'None. ORE ignores the parameter.', 'A file name, written without a directory. ORE writes the file in the output path of the Setup block.',
            '1.8.0.7 or earlier', '1.8.3.0',
            current_user, 'system.initial_load', 'Seed analytic parameter: simulation.additionalScenarioDataFileName'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'simulation' and name = 'storeCreditStateNPVs'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''simulation'']/Parameter[@name=''storeCreditStateNPVs'']',
            'analytic', 'simulation', 'storeCreditStateNPVs', 57, 'integer', false,
            'Store NPVs by credit state', 'A whole number of credit states.',
            'Sets the number of credit states for which the cube also stores the NPV of each trade. A value of zero stores none. The credit migration and the dynamic credit XVA calculations need these values. For example, 8 stores the NPV for 8 credit states.',
            '0. The cube stores no NPVs by credit state.', 'A whole number of zero or more.',
            '1.8.10.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: simulation.storeCreditStateNPVs'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'simulation' and name = 'observationModel'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''simulation'']/Parameter[@name=''observationModel'']',
            'analytic', 'simulation', 'observationModel', 58, 'string', false,
            'Observation model for the simulation', 'The name of an observation model, for example Disable.',
            'Controls how many QuantLib observer notifications the simulation uses when market data and fixings are updated and when the evaluation date moves along a path. Notifications cost time in a simulation. None is the normal QuantLib behaviour. Disable switches the notifications off and lets the engine trigger the recalculations. Defer holds the updates back until notifications are on again. Unregister keeps notifications on but unregisters floating rate coupons from their indices. Disable and Unregister give a similar speed up. The Setup block has a parameter of the same name, and this one overrides it for the simulation.',
            'The observation model of the Setup block, which is None by default.', 'One of None, Disable, Defer or Unregister.',
            '1.8.0.7 or earlier', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: simulation.observationModel'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'simulation' and name = 'includeTodaysCashFlows'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''simulation'']/Parameter[@name=''includeTodaysCashFlows'']',
            'analytic', 'simulation', 'includeTodaysCashFlows', 59, 'boolean', false,
            'Include today''s cash flows in the simulation', 'A flag. Y counts the cash flows of the valuation date.',
            'Decides whether the simulation counts a cash flow that pays on the valuation date itself, or treats it as matured. Set it to Y when a payment of the day must be part of the exposure. The Setup block has a parameter of the same name, and this one overrides it for the simulation.',
            'The choice of the Setup block. When that is missing too, No: a cash flow of the valuation date is treated as matured.', 'A flag, written Y or N, or true or false.',
            '1.8.13.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: simulation.includeTodaysCashFlows'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'simulation' and name = 'filterRiskKeys'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''simulation'']/Parameter[@name=''filterRiskKeys'']',
            'analytic', 'simulation', 'filterRiskKeys', 60, 'string', false,
            'Risk factor types in the dump', 'A comma-separated list of risk factor types.',
            'Limits the scenario dump to the listed types of risk factor. Without it the dump holds every risk factor of the simulation market and can be very large. For example, DiscountCurve writes the discount factors only.',
            'None. The dump holds all risk factors.', 'A comma-separated list of risk factor key types such as DiscountCurve.',
            '1.8.17.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: simulation.filterRiskKeys'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'simulation' and name = 'xvaCgSensitivityConfigFile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''simulation'']/Parameter[@name=''xvaCgSensitivityConfigFile'']',
            'analytic', 'simulation', 'xvaCgSensitivityConfigFile', 61, 'string', false,
            'CG sensitivity configuration file', 'A file name.',
            'Names the XML file that sets which sensitivities the CG engine computes: the risk factors and the shifts. Without it the CG engine computes no sensitivities. For example, xvasensiconfig.xml.',
            'None. The CG engine computes no sensitivities.', 'A file name. ORE looks for the file in the input path of the Setup block.',
            '1.8.12.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: simulation.xvaCgSensitivityConfigFile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'simulation' and name = 'xvaCgBumpSensis'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''simulation'']/Parameter[@name=''xvaCgBumpSensis'']',
            'analytic', 'simulation', 'xvaCgBumpSensis', 62, 'boolean', false,
            'CG bump and revalue sensitivities', 'A flag. Y bumps and revalues instead of using differentiation.',
            'Decides how the CG engine computes the CVA sensitivities. When it is Y, the engine bumps each risk factor and revalues. When it is N, the engine uses algorithmic differentiation. Use Y to check the differentiated sensitivities.',
            'No. The engine uses algorithmic differentiation.', 'A flag, written Y or N, or true or false.',
            '1.8.12.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: simulation.xvaCgBumpSensis'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'simulation' and name = 'xvaCgRegressionCacheSize'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''simulation'']/Parameter[@name=''xvaCgRegressionCacheSize'']',
            'analytic', 'simulation', 'xvaCgRegressionCacheSize', 63, 'integer', false,
            'CG regression cache size', 'A whole number.',
            'Sets the size of the cache of regression results that the CG engine uses. A larger cache can save repeated regressions and uses more memory.',
            '512', 'A whole number of one or more.',
            '1.8.17.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: simulation.xvaCgRegressionCacheSize'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'simulation' and name = 'disableEvaluationDateObservation'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''simulation'']/Parameter[@name=''disableEvaluationDateObservation'']',
            'analytic', 'simulation', 'disableEvaluationDateObservation', 64, 'boolean', false,
            'Disable evaluation date observation', 'A flag. ORE ignores it.',
            'ORE does not read this parameter in any release. Example files carry it with the value Y. To reduce the cost of notifications, use the observation model.',
            'None. ORE ignores the parameter, so a value has no effect.', 'A flag, written Y or N, or true or false.',
            '1.8.0.7 or earlier', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: simulation.disableEvaluationDateObservation'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'simulation' and name = 'scenarioFile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''simulation'']/Parameter[@name=''scenarioFile'']',
            'analytic', 'simulation', 'scenarioFile', 65, 'string', false,
            'Scenario input file', 'A file name.',
            'Names a file with ready-made market scenarios. When it is set, the simulation reads its scenarios from the file and does not generate them with the model. Use it to value a portfolio on scenarios that an earlier run wrote. For example, ../Output/scenarios.csv.',
            'None. The simulation generates its own scenarios.', 'A file name. ORE looks for the file in the input path of the Setup block.',
            '1.8.0.7 or earlier', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: simulation.scenarioFile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'simulation' and name = 'amcPathDataOutput'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''simulation'']/Parameter[@name=''amcPathDataOutput'']',
            'analytic', 'simulation', 'amcPathDataOutput', 66, 'string', false,
            'AMC path data output file', 'A file name.',
            'Names the file that receives the path data of the AMC run, that is the simulated states of the model. A later run can read the file with the AMC path data input parameter. For example, amcBuffer.dat.',
            'None. The run does not write the path data.', 'A file name, written without a directory. ORE writes the file in the output path of the Setup block.',
            '1.8.13.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: simulation.amcPathDataOutput'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'simulation' and name = 'amcIndividualTrainingOutput'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''simulation'']/Parameter[@name=''amcIndividualTrainingOutput'']',
            'analytic', 'simulation', 'amcIndividualTrainingOutput', 67, 'boolean', false,
            'AMC individual training output', 'A flag. Y writes the trained regression of each trade.',
            'Decides whether the AMC run writes the trained regression of each trade to a file, so that a later run can reuse it. Switch it on to avoid the training step in repeat runs.',
            'No. The run does not write the regressions.', 'A flag, written Y or N, or true or false.',
            '1.8.13.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: simulation.amcIndividualTrainingOutput'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'simulation' and name = 'amcIndividualTrainingInput'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''simulation'']/Parameter[@name=''amcIndividualTrainingInput'']',
            'analytic', 'simulation', 'amcIndividualTrainingInput', 68, 'boolean', false,
            'AMC individual training input', 'A flag. ORE does not read this setting from the simulation block.',
            'Is meant to make the AMC run read the trained regression of each trade from a file. ORE reads the parameter from a block with the wrong name, xsimulationva, so a value in the simulation block has no effect in release 1.8.17.0.',
            'No. The value in the simulation block has no effect.', 'A flag, written Y or N, or true or false.',
            '1.8.13.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: simulation.amcIndividualTrainingInput'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'simulation' and name = 'includeReferenceDateEvents'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''simulation'']/Parameter[@name=''includeReferenceDateEvents'']',
            'analytic', 'simulation', 'includeReferenceDateEvents', 69, 'boolean', false,
            'Include reference date events in the simulation', 'A flag. Y counts the events of the valuation date.',
            'Decides whether the simulation counts an event that falls on the valuation date, for example an exercise or a fixing. The Setup block has a parameter of the same name, and this one overrides it for the simulation.',
            'The choice of the Setup block. When that is missing too, No: an event of the valuation date is treated as matured.', 'A flag, written Y or N, or true or false.',
            '1.8.13.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: simulation.includeReferenceDateEvents'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'simulation' and name = 'xvaCgRegressionReportTimeStepsDynamicIM'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''simulation'']/Parameter[@name=''xvaCgRegressionReportTimeStepsDynamicIM'']',
            'analytic', 'simulation', 'xvaCgRegressionReportTimeStepsDynamicIM', 70, 'string', false,
            'CG regression report time steps', 'A comma-separated list of time step numbers.',
            'Lists the time steps for which the CG engine writes a report of the dynamic initial margin regression. Use it to check the quality of the fit on a few dates. For example, 1,2,3 writes the report for the first three steps.',
            'None. The engine writes no regression report.', 'A comma-separated list of whole numbers.',
            '1.8.14.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: simulation.xvaCgRegressionReportTimeStepsDynamicIM'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'simulation' and name = 'xvaCgUsePythonIntegration'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''simulation'']/Parameter[@name=''xvaCgUsePythonIntegration'']',
            'analytic', 'simulation', 'xvaCgUsePythonIntegration', 71, 'boolean', false,
            'CG Python integration', 'A flag. Y uses the Python integration.',
            'Decides whether the CG regression calls Python code for the conditional expectations, through the Python integration of ORE. Switch it on only when that integration is installed.',
            'No.', 'A flag, written Y or N, or true or false.',
            '1.8.13.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: simulation.xvaCgUsePythonIntegration'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'simulation' and name = 'xvaCgUsePythonIntegrationDynamicIm'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''simulation'']/Parameter[@name=''xvaCgUsePythonIntegrationDynamicIm'']',
            'analytic', 'simulation', 'xvaCgUsePythonIntegrationDynamicIm', 72, 'boolean', false,
            'CG Python integration for dynamic IM', 'A flag. Y uses the Python integration for the margin.',
            'Decides whether the CG engine calls Python code for the dynamic initial margin. It works like the CG Python integration parameter and applies to the margin only.',
            'No.', 'A flag, written Y or N, or true or false.',
            '1.8.14.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: simulation.xvaCgUsePythonIntegrationDynamicIm'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'simulation' and name = 'xvaCgTradeLevelBreakdown'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''simulation'']/Parameter[@name=''xvaCgTradeLevelBreakdown'']',
            'analytic', 'simulation', 'xvaCgTradeLevelBreakdown', 73, 'boolean', false,
            'CG trade level breakdown (misspelt)', 'A flag. ORE ignores this spelling.',
            'Is a spelling of the CG trade level breakdown parameter that ORE does not read. ORE reads only the spelling with a capital D in Break Down: xvaCgTradeLevelBreakDown. Example files use both spellings, so a document with this spelling does not change the run.',
            'Not applicable. ORE ignores the parameter, so a value has no effect.', 'A flag, written Y or N, or true or false.',
            '1.8.13.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: simulation.xvaCgTradeLevelBreakdown'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'simulation' and name = 'storeExerciseValues'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''simulation'']/Parameter[@name=''storeExerciseValues'']',
            'analytic', 'simulation', 'storeExerciseValues', 74, 'boolean', false,
            'Store exercise values in the cube', 'A flag. Y stores the exercise values.',
            'Decides whether the cube also stores the exercise value of each trade on each date. Switch it on when a later analytic needs the value of an option if it is exercised.',
            'No.', 'A flag, written Y or N, or true or false.',
            '1.8.14.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: simulation.storeExerciseValues'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'simulation' and name = 'xvaCgEnableCgOptimization'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''simulation'']/Parameter[@name=''xvaCgEnableCgOptimization'']',
            'analytic', 'simulation', 'xvaCgEnableCgOptimization', 75, 'boolean', false,
            'CG graph optimisation', 'A flag. Y optimises the graph.',
            'Decides whether the CG engine optimises the computation graph while it builds the graph. Switch it on to try the optimisation on a large portfolio.',
            'No.', 'A flag, written Y or N, or true or false.',
            '1.8.17.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: simulation.xvaCgEnableCgOptimization'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'simulation' and name = 'amcPathDataInput'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''simulation'']/Parameter[@name=''amcPathDataInput'']',
            'analytic', 'simulation', 'amcPathDataInput', 76, 'string', false,
            'AMC path data input file', 'A file name.',
            'Names a file with the path data of an earlier AMC run, that is the simulated states of the model. The run reads the path data from the file. Use it together with the output parameter to repeat an AMC valuation on the same paths.',
            'None. The run computes the path data.', 'A file name. ORE looks for the file in the input path of the Setup block.',
            '1.8.13.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: simulation.amcPathDataInput'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'simulation' and name = 'amcUseGpuRegression'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''simulation'']/Parameter[@name=''amcUseGpuRegression'']',
            'analytic', 'simulation', 'amcUseGpuRegression', 77, 'boolean', false,
            'Use the GPU for the AMC regression', 'A flag. Y runs the regression on the GPU.',
            'Decides whether the AMC regression runs its QR step on a GPU through cuSOLVER. It is an opt-in for runs with a large portfolio on a machine with a supported GPU. The results do not change.',
            'No. The regression runs on the CPU.', 'A flag, written Y or N, or true or false.',
            '1.8.17.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: simulation.amcUseGpuRegression'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'simulation' and name = 'crossAssetModelData'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''simulation'']/Parameter[@name=''crossAssetModelData'']',
            'analytic', 'simulation', 'crossAssetModelData', 78, 'string', false,
            'Cross asset model file', 'A file name.',
            'Names an XML file for the cross asset model, if the model sits in another file than the rest of the simulation setup. Use it to share one calibrated model between several runs. When the parameter is missing, ORE reads the model from the simulation configuration file.',
            'The simulation configuration file.', 'A file name. ORE looks for the file in the input path of the Setup block.',
            '1.8.16.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: simulation.crossAssetModelData'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'simulation' and name = 'scenarioGeneratorData'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''simulation'']/Parameter[@name=''scenarioGeneratorData'']',
            'analytic', 'simulation', 'scenarioGeneratorData', 79, 'string', false,
            'Scenario generator file', 'A file name.',
            'Names an XML file for the scenario generator, if it sits in another file than the rest of the simulation setup. The scenario generator sets the date grid, the number of samples and the random number generator. Use it to run the same model on a different grid or sample size. When the parameter is missing, ORE reads the scenario generator from the simulation configuration file.',
            'The simulation configuration file.', 'A file name. ORE looks for the file in the input path of the Setup block.',
            '1.8.16.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: simulation.scenarioGeneratorData'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'simulation' and name = 'maxScenario'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''simulation'']/Parameter[@name=''maxScenario'']',
            'analytic', 'simulation', 'maxScenario', 80, 'integer', false,
            'Maximum number of scenarios', 'A whole number of paths.',
            'Caps the number of Monte Carlo paths. When the scenario generator asks for more samples than this number, ORE uses this number. It never raises the number of samples. Use a small value for a quick test run. For example, 100 runs at most 100 paths.',
            'None. The scenario generator decides.', 'A whole number of zero or more.',
            '1.8.16.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: simulation.maxScenario'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'simulation' and name = 'writeCube'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''simulation'']/Parameter[@name=''writeCube'']',
            'analytic', 'simulation', 'writeCube', 81, 'boolean', false,
            'Write the cube', 'A flag. Y writes the cube files.',
            'Decides whether the run writes the NPV cube, the scenario data, the netting set cube and the counterparty cube to files. A value in Cube file switches the output on by itself. Use this flag to write the files with the standard names. The files are cube.csv.gz, scenariodata.csv.gz, nettingsetcube.csv.gz and cptycube.csv.gz.',
            'No. The run writes no cube unless Cube file is set.', 'A flag, written Y or N, or true or false.',
            '1.8.16.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: simulation.writeCube'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'simulation' and name = 'writeScenarios'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''simulation'']/Parameter[@name=''writeScenarios'']',
            'analytic', 'simulation', 'writeScenarios', 82, 'boolean', false,
            'Write the scenarios', 'A flag. Y writes the scenario dump.',
            'Decides whether the run writes the raw scenarios to a file with the standard name scenario.csv. A value in Scenario dump file switches the output on by itself and sets the name.',
            'No. The run writes no scenarios unless Scenario dump file is set.', 'A flag, written Y or N, or true or false.',
            '1.8.16.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: simulation.writeScenarios'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'simulation' and name = 'nettingSetId'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''simulation'']/Parameter[@name=''nettingSetId'']',
            'analytic', 'simulation', 'nettingSetId', 83, 'string', false,
            'Netting set', 'A netting set id. ORE reads it and does not use it.',
            'Is read by ORE in release 1.8.17.0 and not used by any later step. A value has no effect.',
            'None. ORE ignores the parameter, so a value has no effect.', 'A netting set id.',
            '1.8.9.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: simulation.nettingSetId'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'simulation' and name = 'xvaCgRegressionVarianceCutoff'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''simulation'']/Parameter[@name=''xvaCgRegressionVarianceCutoff'']',
            'analytic', 'simulation', 'xvaCgRegressionVarianceCutoff', 84, 'double', false,
            'CG regression variance cutoff', 'A number between 0 and 1.',
            'Switches on a principal component analysis of the regressors in the CG regression and sets its variance cutoff. The analysis rotates the regressors, and the regression drops the directions with a variance below the cutoff. Use it when the regressors are strongly correlated. For example, 0.01.',
            'None. The regression uses the regressors as they are.', 'A number, written with a decimal point.',
            '1.8.13.1', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: simulation.xvaCgRegressionVarianceCutoff'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'simulation' and name = 'xvaCgRegressionVarianceCutoffDynamicIm'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''simulation'']/Parameter[@name=''xvaCgRegressionVarianceCutoffDynamicIm'']',
            'analytic', 'simulation', 'xvaCgRegressionVarianceCutoffDynamicIm', 85, 'double', false,
            'CG regression variance cutoff for dynamic IM', 'A number between 0 and 1.',
            'Sets the variance cutoff of the regression that the CG engine uses for dynamic initial margin. It works like the CG regression variance cutoff and applies to the margin only.',
            'None. The regression uses the regressors as they are.', 'A number, written with a decimal point.',
            '1.8.14.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: simulation.xvaCgRegressionVarianceCutoffDynamicIm'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'simulation' and name = 'cubeNpvOverlay'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''simulation'']/Parameter[@name=''cubeNpvOverlay'']',
            'analytic', 'simulation', 'cubeNpvOverlay', 86, 'boolean', false,
            'Correct the cube with the first NPV', 'A flag. Y corrects the cube entries.',
            'Decides whether ORE corrects every raw NPV of the cube by the difference between the NPV of the valuation date from the pricing analytic and the NPV of the valuation date from the simulation. Use it when the simulation engines give a slightly different NPV today than the pricing engines.',
            'No.', 'A flag, written Y or N, or true or false.',
            '1.8.15.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: simulation.cubeNpvOverlay'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'simulation' and name = 'pricingEnginesOverride'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''simulation'']/Parameter[@name=''pricingEnginesOverride'']',
            'analytic', 'simulation', 'pricingEnginesOverride', 87, 'string', false,
            'Pricing engines override', 'A file name.',
            'Names an XML file with pricing engine settings that replace the matching settings of the pricing engines file for the simulation. Use it to change a few engines and keep the rest. ORE applies the file on top of the pricing engines that the simulation uses.',
            'None. The engines stay as the pricing engines file sets them.', 'A file name. ORE looks for the file in the input path of the Setup block.',
            '1.8.17.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: simulation.pricingEnginesOverride'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'simulation' and name = 'amcPricingEnginesOverride'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''simulation'']/Parameter[@name=''amcPricingEnginesOverride'']',
            'analytic', 'simulation', 'amcPricingEnginesOverride', 88, 'string', false,
            'Pricing engines override for AMC', 'A file name.',
            'Names an XML file with pricing engine settings that replace the matching settings of the AMC pricing engines file. Use it to change a few engines and keep the rest.',
            'None. The engines stay as the AMC pricing engines file sets them.', 'A file name. ORE looks for the file in the input path of the Setup block.',
            '1.8.17.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: simulation.amcPricingEnginesOverride'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'simulation' and name = 'amcCgPricingEnginesOverride'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''simulation'']/Parameter[@name=''amcCgPricingEnginesOverride'']',
            'analytic', 'simulation', 'amcCgPricingEnginesOverride', 89, 'string', false,
            'Pricing engines override for the CG engine', 'A file name.',
            'Names an XML file with pricing engine settings that replace the matching settings of the CG pricing engines file. Use it to change a few engines and keep the rest.',
            'None. The engines stay as the CG pricing engines file sets them.', 'A file name. ORE looks for the file in the input path of the Setup block.',
            '1.8.17.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: simulation.amcCgPricingEnginesOverride'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'xva' and name = 'active'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''xva'']/Parameter[@name=''active'']',
            'analytic', 'xva', 'active', 90, 'boolean', true,
            'Run the XVA analytic', 'A flag. Y runs the analytic.',
            'Switches the XVA analytic on or off. When it is on, the run post-processes an NPV cube: it nets the trades, applies the collateral agreement and computes the exposure profiles, and then the value adjustments that the flags below select. The cube comes from the simulation analytic of the same run or from a file.',
            'No. The analytic does not run.', 'A flag, written Y or N, or true or false.',
            '1.8.0.7 or earlier', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: xva.active'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'xva' and name = 'csaFile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''xva'']/Parameter[@name=''csaFile'']',
            'analytic', 'xva', 'csaFile', 91, 'string', false,
            'Netting set definitions file', 'A file name.',
            'Names the XML file with the netting set definitions: the CSA details such as the margining frequency, the thresholds, the minimum transfer amounts and the margin period of risk. The post-processing reads the file, so you can change the CSA terms and see their effect on the results without a new simulation. ORE looks for the file name in the xva block, then in the pfe block and last in the Setup block. For example, netting.xml.',
            'The netting set definitions file of the Setup block.', 'A file name. ORE looks for the file in the input path of the Setup block.',
            '1.8.0.7 or earlier', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: xva.csaFile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'xva' and name = 'cubeFile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''xva'']/Parameter[@name=''cubeFile'']',
            'analytic', 'xva', 'cubeFile', 92, 'string', false,
            'Cube file', 'A file name.',
            'Names the NPV cube file that the XVA analytic reads, if the run has no simulation. The file is usually the cube that an earlier simulation wrote, so you can run the post-processing again without a new simulation. ORE looks for the file in the output path of the Setup block. For example, cube.csv.gz.',
            'None. When the run has a simulation, the analytic uses its cube.', 'A file name. ORE looks for the file in the output path of the Setup block.',
            '1.8.0.7 or earlier', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: xva.cubeFile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'xva' and name = 'scenarioFile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''xva'']/Parameter[@name=''scenarioFile'']',
            'analytic', 'xva', 'scenarioFile', 93, 'string', false,
            'Scenario data file', 'A file name.',
            'Names the file with the scenario data that the simulation wrote for the post-processing, such as simulated index fixings and FX rates. Variation margin and dynamic initial margin need it. For example, scenariodata.csv.gz.',
            'None. When the run has a simulation, the analytic uses its scenario data.', 'A file name. ORE looks for the file in the output path of the Setup block.',
            '1.8.0.7 or earlier', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: xva.scenarioFile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'xva' and name = 'baseCurrency'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''xva'']/Parameter[@name=''baseCurrency'']',
            'analytic', 'xva', 'baseCurrency', 94, 'string', false,
            'Base currency', 'A currency code, for example EUR.',
            'Names the currency in which the analytic states all NPVs, exposures and value adjustments. ORE converts the amounts that are in another currency. For example, EUR states all values in euro.',
            'The base currency of the Setup block.', 'A currency code of three letters, for example EUR.',
            '1.8.0.7 or earlier', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: xva.baseCurrency'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'xva' and name = 'exposureProfiles'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''xva'']/Parameter[@name=''exposureProfiles'']',
            'analytic', 'xva', 'exposureProfiles', 95, 'boolean', false,
            'Write exposure profiles', 'A flag. Y writes the profiles for each netting set.',
            'Decides whether the analytic writes an exposure report for each netting set: the expected positive and negative exposure and the potential future exposure, for each date. The XVA analytic has the same parameter.',
            'Yes.', 'A flag, written Y or N, or true or false.',
            '1.8.0.7 or earlier', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: xva.exposureProfiles'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'xva' and name = 'exposureProfilesByTrade'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''xva'']/Parameter[@name=''exposureProfilesByTrade'']',
            'analytic', 'xva', 'exposureProfilesByTrade', 96, 'boolean', false,
            'Write exposure profiles by trade', 'A flag. Y writes the stand-alone profiles for each trade.',
            'Decides whether the analytic writes an exposure report for each trade on its own, without the netting. With many trades this makes many files, unless Write individual exposure reports is off. The XVA analytic has the same parameter.',
            'Yes.', 'A flag, written Y or N, or true or false.',
            '1.8.4.1', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: xva.exposureProfilesByTrade'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'xva' and name = 'quantile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''xva'']/Parameter[@name=''quantile'']',
            'analytic', 'xva', 'quantile', 97, 'double', false,
            'PFE confidence level', 'A number between 0 and 1.',
            'Sets the confidence level of the potential future exposure (PFE) in the exposure reports. For example, 0.95 reports the 95th percentile of the exposure distribution. The XVA analytic has the same parameter.',
            '0.95', 'A number, written with a decimal point.',
            '1.8.0.7 or earlier', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: xva.quantile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'xva' and name = 'calculationType'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''xva'']/Parameter[@name=''calculationType'']',
            'analytic', 'xva', 'calculationType', 98, 'string', false,
            'Margin settlement type', 'NoLag, Symmetric, AsymmetricCVA or AsymmetricDVA.',
            'Decides how ORE settles margin calls. Symmetric settles the margin of both parties after the margin period of risk. AsymmetricCVA settles the margin that we request from the counterparty with the delay and the margin that the counterparty requests from us at once. AsymmetricDVA is the reverse. NoLag turns the delay off and works together with a close-out grid in the simulation. Without a close-out grid, use one of the three others. With one, use NoLag. When there is no close-out grid, the results are correct only if the simulation grid is equally spaced with steps equal to the margin period of risk. The XVA analytic has the same parameter.',
            'NoLag', 'One of NoLag, Symmetric, AsymmetricCVA or AsymmetricDVA.',
            '1.8.0.7 or earlier', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: xva.calculationType'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'xva' and name = 'cva'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''xva'']/Parameter[@name=''cva'']',
            'analytic', 'xva', 'cva', 99, 'boolean', false,
            'Compute CVA', 'A flag. Y computes the credit value adjustment.',
            'Switches the credit value adjustment (CVA) on or off. CVA prices the loss from the default of the counterparty on the positive exposure.',
            'Yes.', 'A flag, written Y or N, or true or false.',
            '1.8.0.7 or earlier', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: xva.cva'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'xva' and name = 'rawCubeOutputFile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''xva'']/Parameter[@name=''rawCubeOutputFile'']',
            'analytic', 'xva', 'rawCubeOutputFile', 100, 'string', false,
            'Raw cube report file', 'A file name.',
            'Names the file for the trade NPV cube as a text file with one row for each trade, date and sample. Setting it switches the report on. Leave it out to skip the file. The file is for further analysis and can be very large. The XVA analytic has the same parameter.',
            'None. The analytic does not write the file.', 'A file name, written without a directory. ORE writes the file in the output path of the Setup block.',
            '1.8.0.7 or earlier', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: xva.rawCubeOutputFile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'xva' and name = 'netCubeOutputFile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''xva'']/Parameter[@name=''netCubeOutputFile'']',
            'analytic', 'xva', 'netCubeOutputFile', 101, 'string', false,
            'Net cube report file', 'A file name.',
            'Names the file for the aggregated NPV cube as a text file with one row for each netting set, date and sample, after the collateral. Setting it switches the report on. Leave it out to skip the file. The XVA analytic has the same parameter.',
            'None. The analytic does not write the file.', 'A file name, written without a directory. ORE writes the file in the output path of the Setup block.',
            '1.8.0.7 or earlier', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: xva.netCubeOutputFile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'xva' and name = 'dim'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''xva'']/Parameter[@name=''dim'']',
            'analytic', 'xva', 'dim', 102, 'boolean', false,
            'Compute DIM', 'A flag. Y computes the dynamic initial margin.',
            'Switches the dynamic initial margin (DIM) calculation on or off. DIM projects the initial margin of each netting set along each path. The parameter DIM model selects the method. MVA needs it.',
            'No.', 'A flag, written Y or N, or true or false.',
            '1.8.0.7 or earlier', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: xva.dim'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'xva' and name = 'dimModel'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''xva'']/Parameter[@name=''dimModel'']',
            'analytic', 'xva', 'dimModel', 103, 'string', false,
            'DIM model', 'Regression, Flat, DeltaVaR, DeltaGammaNormalVaR, DeltaGammaVaR, DynamicIM or SimmAnalytic.',
            'Selects the method for the dynamic initial margin. Regression regresses the change of the netting set value over the margin period on the value at the start. Flat projects the initial margin of the valuation date unchanged along each path, and needs the collateral balances file. DeltaVaR, DeltaGammaNormalVaR and DeltaGammaVaR use stored sensitivities and need the simulation to store them. SimmAnalytic uses stored sensitivities and the SIMM formula. DynamicIM needs the CG engine with its dynamic IM switch on. The run stops if the value is none of these. ORE uses it only when DIM is on.',
            'Regression', 'One of Regression, Flat, DeltaVaR, DeltaGammaNormalVaR, DeltaGammaVaR, DynamicIM or SimmAnalytic.',
            '1.8.12.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: xva.dimModel'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'xva' and name = 'dimQuantile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''xva'']/Parameter[@name=''dimQuantile'']',
            'analytic', 'xva', 'dimQuantile', 104, 'double', false,
            'DIM quantile', 'A number between 0 and 1.',
            'Sets the quantile of the margin movement that the DIM covers. For example, 0.99 covers the 99th percentile. ORE uses it only when DIM is on.',
            '0.99', 'A number, written with a decimal point.',
            '1.8.0.7 or earlier', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: xva.dimQuantile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'xva' and name = 'dimHorizonCalendarDays'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''xva'']/Parameter[@name=''dimHorizonCalendarDays'']',
            'analytic', 'xva', 'dimHorizonCalendarDays', 105, 'integer', false,
            'DIM horizon in calendar days', 'A whole number of days.',
            'Sets the length of the margin period of risk that the DIM covers, in calendar days. For example, 14 is two weeks. ORE uses it only when DIM is on.',
            '14', 'A whole number of zero or more.',
            '1.8.0.7 or earlier', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: xva.dimHorizonCalendarDays'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'xva' and name = 'dimRegressionOrder'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''xva'']/Parameter[@name=''dimRegressionOrder'']',
            'analytic', 'xva', 'dimRegressionOrder', 106, 'integer', false,
            'DIM regression order', 'A whole number.',
            'Sets the order of the regression polynomial of the DIM model Regression. The regression relates the clean change of the netting set value over the period to the value at the start of the period. ORE uses it only when DIM is on.',
            '0', 'A whole number of zero or more.',
            '1.8.0.7 or earlier', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: xva.dimRegressionOrder'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'xva' and name = 'allocationMethod'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''xva'']/Parameter[@name=''allocationMethod'']',
            'analytic', 'xva', 'allocationMethod', 107, 'string', false,
            'Exposure allocation method', 'None, Marginal, RelativeXVA, RelativeFairValueGross or RelativeFairValueNet.',
            'Selects how ORE allocates the netting set exposure and the value adjustments to the trades in the netting set. None gives no allocation. Marginal uses the marginal method of Pykhtin and Rosen. The other three split the amount in proportion to the XVA of the trade, to its gross fair value or to its net fair value. The XVA analytic has the same parameter.',
            'None', 'One of None, Marginal, RelativeXVA, RelativeFairValueGross or RelativeFairValueNet.',
            '1.8.0.7 or earlier', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: xva.allocationMethod'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'xva' and name = 'marginalAllocationLimit'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''xva'']/Parameter[@name=''marginalAllocationLimit'']',
            'analytic', 'xva', 'marginalAllocationLimit', 108, 'double', false,
            'Marginal allocation limit', 'A number.',
            'Sets a cutoff for the marginal allocation. The marginal method breaks down when the value of the netting set is close to zero while its exposure is not. Below this absolute value, ORE splits the exposure equally among the trades. The parameter has an effect only when the allocation method is Marginal. The XVA analytic has the same parameter.',
            '1.0', 'A number, written with a decimal point.',
            '1.8.0.7 or earlier', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: xva.marginalAllocationLimit'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'xva' and name = 'exerciseNextBreak'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''xva'']/Parameter[@name=''exerciseNextBreak'']',
            'analytic', 'xva', 'exerciseNextBreak', 109, 'boolean', false,
            'Exercise at the next break', 'A flag. Y ends each trade at its next break date.',
            'Decides whether ORE ends all trades at their next break date before it aggregates them. Use it to model a counterparty relationship in which each party can end a trade at its break dates. The XVA analytic has the same parameter.',
            'No.', 'A flag, written Y or N, or true or false.',
            '1.8.0.7 or earlier', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: xva.exerciseNextBreak'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'xva' and name = 'dva'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''xva'']/Parameter[@name=''dva'']',
            'analytic', 'xva', 'dva', 110, 'boolean', false,
            'Compute DVA', 'A flag. Y computes the debit value adjustment.',
            'Switches the debit value adjustment (DVA) on or off. DVA is the gain from our own default on the negative exposure. Set the DVA name too, so that ORE can find our own default curve.',
            'No.', 'A flag, written Y or N, or true or false.',
            '1.8.0.7 or earlier', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: xva.dva'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'xva' and name = 'dvaName'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''xva'']/Parameter[@name=''dvaName'']',
            'analytic', 'xva', 'dvaName', 111, 'string', false,
            'DVA name', 'A credit name.',
            'Names the credit entity that ORE uses to look up our own default probability curve and recovery rate for the DVA. For example, BANK.',
            'None. A DVA calculation needs a name.', 'A credit name as it appears in the market data, for example BANK.',
            '1.8.0.7 or earlier', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: xva.dvaName'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'xva' and name = 'fva'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''xva'']/Parameter[@name=''fva'']',
            'analytic', 'xva', 'fva', 112, 'boolean', false,
            'Compute FVA', 'A flag. Y computes the funding value adjustment.',
            'Switches the funding value adjustment (FVA) on or off. FVA prices the cost of funding the uncollateralised exposure. It needs a borrowing curve and a lending curve.',
            'No.', 'A flag, written Y or N, or true or false.',
            '1.8.0.7 or earlier', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: xva.fva'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'xva' and name = 'fvaBorrowingCurve'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''xva'']/Parameter[@name=''fvaBorrowingCurve'']',
            'analytic', 'xva', 'fvaBorrowingCurve', 113, 'string', false,
            'FVA borrowing curve', 'The name of a yield curve.',
            'Names the yield curve at which we borrow, for the FVA. This spelling is the older one, and ORE reads it only when the parameter Borrowing curve is missing. For example, BANK_EUR_BORROW.',
            'None. An FVA calculation needs a curve.', 'The name of a yield curve in the market data.',
            '1.8.0.7 or earlier', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: xva.fvaBorrowingCurve'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'xva' and name = 'fvaLendingCurve'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''xva'']/Parameter[@name=''fvaLendingCurve'']',
            'analytic', 'xva', 'fvaLendingCurve', 114, 'string', false,
            'FVA lending curve', 'The name of a yield curve.',
            'Names the yield curve at which we lend, for the FVA. This spelling is the older one, and ORE reads it only when the parameter Lending curve is missing. For example, BANK_EUR_LEND.',
            'None. An FVA calculation needs a curve.', 'The name of a yield curve in the market data.',
            '1.8.0.7 or earlier', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: xva.fvaLendingCurve'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'xva' and name = 'colva'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''xva'']/Parameter[@name=''colva'']',
            'analytic', 'xva', 'colva', 115, 'boolean', false,
            'Compute COLVA', 'A flag. Y computes the collateral value adjustment.',
            'Switches the collateral value adjustment (COLVA) on or off. COLVA prices the difference between the rate that the collateral earns and the overnight rate.',
            'No.', 'A flag, written Y or N, or true or false.',
            '1.8.0.7 or earlier', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: xva.colva'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'xva' and name = 'collateralSpread'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''xva'']/Parameter[@name=''collateralSpread'']',
            'analytic', 'xva', 'collateralSpread', 116, 'double', false,
            'Collateral spread', 'A number. ORE no longer reads it.',
            'Set the difference between the collateral rate and the overnight rate for the COLVA, as an absolute number: one basis point is 0.0001. ORE stopped reading it in 1.8.3.0. Today the collateral model of the netting set definitions sets the collateral rate.',
            'None. ORE ignores the parameter, so a value has no effect.', 'A number, written with a decimal point.',
            '1.8.0.7 or earlier', '1.8.3.0',
            current_user, 'system.initial_load', 'Seed analytic parameter: xva.collateralSpread'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'xva' and name = 'collateralFloor'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''xva'']/Parameter[@name=''collateralFloor'']',
            'analytic', 'xva', 'collateralFloor', 117, 'boolean', false,
            'Compute the collateral floor', 'A flag. Y computes the collateral floor value.',
            'Switches the calculation of the collateral floor on or off. The calculation values a floor on the collateral rate.',
            'No.', 'A flag, written Y or N, or true or false.',
            '1.8.0.7 or earlier', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: xva.collateralFloor'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'xva' and name = 'dimScaling'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''xva'']/Parameter[@name=''dimScaling'']',
            'analytic', 'xva', 'dimScaling', 118, 'double', false,
            'DIM scaling', 'A number.',
            'Sets a factor that ORE applies to the DIM values of the Regression and DeltaVaR family models. The factor applies to all netting sets, and ORE then ignores the initial margin of the collateral balances. When the parameter is missing, ORE derives a factor for each netting set from the initial margin of the collateral balances file divided by the model DIM at the valuation date. That initial margin must be above zero, or the run stops. A value of 1.0 applies no scaling. It has no effect for SimmAnalytic and DynamicIM. ORE uses it only when DIM is on.',
            'None. ORE derives the factor from the collateral balances.', 'A number, written with a decimal point.',
            '1.8.0.7 or earlier', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: xva.dimScaling'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'xva' and name = 'dimEvolutionFile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''xva'']/Parameter[@name=''dimEvolutionFile'']',
            'analytic', 'xva', 'dimEvolutionFile', 119, 'string', false,
            'DIM evolution file', 'A file name.',
            'Names the file for the evolution of the zero-order DIM and the average of the n-th order DIM over time. ORE uses it only when DIM is on.',
            'None. The analytic does not write the file.', 'A file name, written without a directory. ORE writes the file in the output path of the Setup block.',
            '1.8.0.7 or earlier', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: xva.dimEvolutionFile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'xva' and name = 'dimRegressionFiles'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''xva'']/Parameter[@name=''dimRegressionFiles'']',
            'analytic', 'xva', 'dimRegressionFiles', 120, 'string', false,
            'DIM regression snapshot files', 'A comma-separated list of file names.',
            'Names the files for snapshots of the DIM regression. The files go with the grid points that DIM output grid points lists, one file for each point. ORE uses it only when DIM is on.',
            'None. The analytic does not write the files.', 'A comma-separated list of file names.',
            '1.8.2.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: xva.dimRegressionFiles'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'xva' and name = 'dimOutputNettingSet'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''xva'']/Parameter[@name=''dimOutputNettingSet'']',
            'analytic', 'xva', 'dimOutputNettingSet', 121, 'string', false,
            'DIM snapshot netting set', 'A netting set id.',
            'Names the netting set for which the analytic writes the DIM regression snapshot. For example, CPTY_A. ORE uses it only when DIM is on.',
            'None.', 'A netting set id.',
            '1.8.0.7 or earlier', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: xva.dimOutputNettingSet'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'xva' and name = 'dimOutputGridPoints'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''xva'']/Parameter[@name=''dimOutputGridPoints'']',
            'analytic', 'xva', 'dimOutputGridPoints', 122, 'integer', false,
            'DIM snapshot grid points', 'A comma-separated list of whole numbers.',
            'Lists the points of the time grid for which the analytic writes a DIM regression snapshot. ORE uses it only when DIM is on.',
            'None.', 'A comma-separated list of whole numbers.',
            '1.8.2.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: xva.dimOutputGridPoints'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'xva' and name = 'dimLocalRegressionEvaluations'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''xva'']/Parameter[@name=''dimLocalRegressionEvaluations'']',
            'analytic', 'xva', 'dimLocalRegressionEvaluations', 123, 'integer', false,
            'DIM local regression evaluations', 'A whole number.',
            'Sets the number of points at which ORE evaluates a local (Nadaraya-Watson) regression to validate the polynomial regression. The local regression needs many samples to give a meaningful result, and each evaluation costs time, so keep the number small. A value of zero turns the check off. ORE uses it only when DIM is on.',
            '0', 'A whole number of zero or more.',
            '1.8.0.7 or earlier', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: xva.dimLocalRegressionEvaluations'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'xva' and name = 'dimLocalRegressionBandwidth'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''xva'']/Parameter[@name=''dimLocalRegressionBandwidth'']',
            'analytic', 'xva', 'dimLocalRegressionBandwidth', 124, 'double', false,
            'DIM local regression bandwidth', 'A number of standard deviations.',
            'Sets the bandwidth of the local regression that validates the polynomial regression, in standard deviations of the NPV. ORE uses it only when DIM is on.',
            '0.25', 'A number, written with a decimal point.',
            '1.8.0.7 or earlier', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: xva.dimLocalRegressionBandwidth'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'xva' and name = 'mva'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''xva'']/Parameter[@name=''mva'']',
            'analytic', 'xva', 'mva', 125, 'boolean', false,
            'Compute MVA', 'A flag. Y computes the margin value adjustment.',
            'Switches the margin value adjustment (MVA) on or off. MVA prices the funding cost of the initial margin. It takes the dynamic initial margin from the DIM model that the DIM model parameter selects.',
            'No.', 'A flag, written Y or N, or true or false.',
            '1.8.0.7 or earlier', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: xva.mva'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'xva' and name = 'dimRegressors'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''xva'']/Parameter[@name=''dimRegressors'']',
            'analytic', 'xva', 'dimRegressors', 126, 'string', false,
            'DIM regressors', 'A comma-separated list of names.',
            'Lists the variables that the DIM regression uses as regressors, for a regression in one or more dimensions. The names must match the entries of the aggregation scenario data in the simulation configuration file, that is its currencies and indices. When the list is empty, the regression uses the NPV as its only regressor. For example, EUR-EURIBOR-3M,USD-LIBOR-3M,USD. ORE uses it only when DIM is on.',
            'None. The regression uses the NPV.', 'A comma-separated list of currency codes and index names.',
            '1.8.0.7 or earlier', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: xva.dimRegressors'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'xva' and name = 'collateralBalancesFile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''xva'']/Parameter[@name=''collateralBalancesFile'']',
            'analytic', 'xva', 'collateralBalancesFile', 127, 'string', false,
            'Collateral balances file', 'A file name.',
            'Names the XML file with the current variation margin and initial margin balances for each netting set. The analytic uses them as the margin at the valuation date, and the flat DIM model and the scaling of the DIM model use the initial margin. ORE looks for the file name in the xva block, then in the pfe block and last in the Setup block. For example, collateralbalances.xml.',
            'The collateral balances file of the Setup block.', 'A file name. ORE looks for the file in the input path of the Setup block.',
            '1.8.12.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: xva.collateralBalancesFile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'xva' and name = 'useXvaRunner'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''xva'']/Parameter[@name=''useXvaRunner'']',
            'analytic', 'xva', 'useXvaRunner', 128, 'boolean', false,
            'Use the XVA runner', 'A flag. ORE no longer reads it.',
            'Selected an older way to run the XVA calculation. ORE stopped reading it in 1.8.9.1.',
            'None. ORE ignores the parameter, so a value has no effect.', 'A flag, written Y or N, or true or false.',
            '1.8.6.0', '1.8.9.1',
            current_user, 'system.initial_load', 'Seed analytic parameter: xva.useXvaRunner'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'xva' and name = 'dynamicCredit'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''xva'']/Parameter[@name=''dynamicCredit'']',
            'analytic', 'xva', 'dynamicCredit', 129, 'boolean', false,
            'Dynamic credit', 'A flag. Y uses the survival probabilities along each path.',
            'Decides whether ORE uses the survival probabilities of each simulation path when it computes the increments of CVA, DVA, FVA and MVA from the exposures. When it is N, ORE takes the survival probabilities from the curves of the valuation date. The simulation must store the survival probabilities.',
            'No. ORE uses the survival probabilities of the valuation date.', 'A flag, written Y or N, or true or false.',
            '1.8.6.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: xva.dynamicCredit'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'xva' and name = 'fvaFundingCurve'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''xva'']/Parameter[@name=''fvaFundingCurve'']',
            'analytic', 'xva', 'fvaFundingCurve', 130, 'string', false,
            'FVA funding curve', 'The name of a yield curve. ORE ignores it.',
            'ORE does not read this parameter in any release. Example files carry it. Use Borrowing curve and Lending curve to name the curves of the FVA.',
            'None. ORE ignores the parameter, so a value has no effect.', 'The name of a yield curve in the market data.',
            '1.8.0.7 or earlier', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: xva.fvaFundingCurve'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'xva' and name = 'fvaInvestmentCurve'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''xva'']/Parameter[@name=''fvaInvestmentCurve'']',
            'analytic', 'xva', 'fvaInvestmentCurve', 131, 'string', false,
            'FVA investment curve', 'The name of a yield curve. ORE ignores it.',
            'ORE does not read this parameter in any release. Example files carry it. Use Borrowing curve and Lending curve to name the curves of the FVA.',
            'None. ORE ignores the parameter, so a value has no effect.', 'The name of a yield curve in the market data.',
            '1.8.0.7 or earlier', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: xva.fvaInvestmentCurve'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'xva' and name = 'colvaSpread'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''xva'']/Parameter[@name=''colvaSpread'']',
            'analytic', 'xva', 'colvaSpread', 132, 'double', false,
            'COLVA spread', 'A number. ORE ignores it.',
            'ORE does not read this parameter in any release. Example files carry it. The collateral model of the netting set definitions sets the collateral rate.',
            'None. ORE ignores the parameter, so a value has no effect.', 'A number, written with a decimal point.',
            '1.8.0.7 or earlier', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: xva.colvaSpread'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'xva' and name = 'eoniaFloor'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''xva'']/Parameter[@name=''eoniaFloor'']',
            'analytic', 'xva', 'eoniaFloor', 133, 'boolean', false,
            'EONIA floor', 'A flag. ORE ignores it.',
            'ORE does not read this parameter in any release. Example files carry it. To floor the collateral rate, use Compute the collateral floor.',
            'None. ORE ignores the parameter, so a value has no effect.', 'A flag, written Y or N, or true or false.',
            '1.8.0.7 or earlier', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: xva.eoniaFloor'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'xva' and name = 'hyperCube'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''xva'']/Parameter[@name=''hyperCube'']',
            'analytic', 'xva', 'hyperCube', 134, 'boolean', false,
            'Hyper cube', 'A flag. ORE no longer reads it.',
            'Selected the use of the hyper cube in an older version of the XVA calculation. ORE stopped reading it in 1.8.9.1.',
            'None. ORE ignores the parameter, so a value has no effect.', 'A flag, written Y or N, or true or false.',
            '1.8.0.7 or earlier', '1.8.9.1',
            current_user, 'system.initial_load', 'Seed analytic parameter: xva.hyperCube'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'xva' and name = 'creditMigration'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''xva'']/Parameter[@name=''creditMigration'']',
            'analytic', 'xva', 'creditMigration', 135, 'boolean', false,
            'Credit migration', 'A flag. Y runs the credit migration analytic.',
            'Switches the credit migration calculation on or off. It computes the distribution of the portfolio value under rating migrations of the counterparties. It needs a simulation that stores the NPVs for each credit state.',
            'No.', 'A flag, written Y or N, or true or false.',
            '1.8.10.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: xva.creditMigration'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'xva' and name = 'creditMigrationDistributionGrid'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''xva'']/Parameter[@name=''creditMigrationDistributionGrid'']',
            'analytic', 'xva', 'creditMigrationDistributionGrid', 136, 'string', false,
            'Credit migration distribution grid', 'A comma-separated list of numbers.',
            'Lists the points of the grid on which ORE reports the distribution of the portfolio value in the credit migration calculation. ORE uses it only when credit migration is on.',
            'None.', 'A comma-separated list of numbers.',
            '1.8.10.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: xva.creditMigrationDistributionGrid'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'xva' and name = 'creditMigrationTimeSteps'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''xva'']/Parameter[@name=''creditMigrationTimeSteps'']',
            'analytic', 'xva', 'creditMigrationTimeSteps', 137, 'string', false,
            'Credit migration time steps', 'A comma-separated list of whole numbers.',
            'Lists the time steps of the simulation grid at which ORE reports the credit migration distribution. ORE uses it only when credit migration is on.',
            'None.', 'A comma-separated list of whole numbers.',
            '1.8.10.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: xva.creditMigrationTimeSteps'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'xva' and name = 'creditMigrationConfig'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''xva'']/Parameter[@name=''creditMigrationConfig'']',
            'analytic', 'xva', 'creditMigrationConfig', 138, 'string', false,
            'Credit migration configuration file', 'A file name.',
            'Names the XML file with the credit simulation parameters: the rating states and the migration matrices. ORE uses it only when credit migration is on.',
            'None.', 'A file name. ORE looks for the file in the input path of the Setup block.',
            '1.8.10.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: xva.creditMigrationConfig'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'xva' and name = 'creditMigrationOutputFiles'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''xva'']/Parameter[@name=''creditMigrationOutputFiles'']',
            'analytic', 'xva', 'creditMigrationOutputFiles', 139, 'string', false,
            'Credit migration output files', 'A file name stem.',
            'Sets the stem of the output files of the credit migration. ORE appends an underscore and the time step to the stem to name each file. ORE uses it only when credit migration is on.',
            'None.', 'A file name stem without extension.',
            '1.8.10.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: xva.creditMigrationOutputFiles'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'xva' and name = 'flipViewXVA'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''xva'']/Parameter[@name=''flipViewXVA'']',
            'analytic', 'xva', 'flipViewXVA', 140, 'boolean', false,
            'Flip the view of the XVA', 'A flag. Y computes the XVA from the counterparty''s view.',
            'Decides whether ORE switches the perspective of the XVA calculation to that of the counterparty. ORE then reverses the NPVs and the netting sets. It looks for the borrowing and lending curves under the name of the counterparty plus a postfix, which the next two parameters set.',
            'No.', 'A flag, written Y or N, or true or false.',
            '1.8.6.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: xva.flipViewXVA'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'xva' and name = 'flipViewBorrowingCurvePostfix'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''xva'']/Parameter[@name=''flipViewBorrowingCurvePostfix'']',
            'analytic', 'xva', 'flipViewBorrowingCurvePostfix', 141, 'string', false,
            'Flip view borrowing curve postfix', 'A text.',
            'Sets the text that ORE appends to the name of the counterparty to find the borrowing curve when the view is flipped. For example, _BORROW looks for the curve CPTY_A_BORROW. ORE uses it only when Flip the view is on.',
            '_BORROW', 'A text.',
            '1.8.6.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: xva.flipViewBorrowingCurvePostfix'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'xva' and name = 'flipViewLendingCurvePostfix'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''xva'']/Parameter[@name=''flipViewLendingCurvePostfix'']',
            'analytic', 'xva', 'flipViewLendingCurvePostfix', 142, 'string', false,
            'Flip view lending curve postfix', 'A text.',
            'Sets the text that ORE appends to the name of the counterparty to find the lending curve when the view is flipped. For example, _LEND looks for the curve CPTY_A_LEND. ORE uses it only when Flip the view is on.',
            '_LEND', 'A text.',
            '1.8.6.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: xva.flipViewLendingCurvePostfix'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'xva' and name = 'timeAveragedNettedExposureOutputFile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''xva'']/Parameter[@name=''timeAveragedNettedExposureOutputFile'']',
            'analytic', 'xva', 'timeAveragedNettedExposureOutputFile', 143, 'string', false,
            'Time averaged exposure report file', 'A file name.',
            'Names the file for the time averaged netted exposure of each netting set. Setting it switches the report on.',
            'None. The analytic does not write the file.', 'A file name, written without a directory. ORE writes the file in the output path of the Setup block.',
            '1.8.13.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: xva.timeAveragedNettedExposureOutputFile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'xva' and name = 'firstMporCollateralAdjustment'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''xva'']/Parameter[@name=''firstMporCollateralAdjustment'']',
            'analytic', 'xva', 'firstMporCollateralAdjustment', 144, 'boolean', false,
            'First MPoR collateral adjustment', 'A flag. Y adjusts the collateral in the first margin period.',
            'Decides whether ORE corrects the collateral in the first margin period of risk for a mismatch between the variation margin balance and the NPV at the valuation date. Switch it on when you start from real balances that do not equal the NPV.',
            'No.', 'A flag, written Y or N, or true or false.',
            '1.8.13.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: xva.firstMporCollateralAdjustment'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'xva' and name = 'outputCrossAssetModelData'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''xva'']/Parameter[@name=''outputCrossAssetModelData'']',
            'analytic', 'xva', 'outputCrossAssetModelData', 145, 'boolean', false,
            'Write the cross asset model', 'A flag. Y writes the model data.',
            'Decides whether the analytic writes the cross asset model data of the run to the file crossassetmodel_xva.xml in the output path. Use it to check the model that the run used.',
            'No.', 'A flag, written Y or N, or true or false.',
            '1.8.17.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: xva.outputCrossAssetModelData'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'xva' and name = 'correlationInputFile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''xva'']/Parameter[@name=''correlationInputFile'']',
            'analytic', 'xva', 'correlationInputFile', 146, 'string', false,
            'Correlation input file', 'A file name.',
            'Names a file with correlation data for the risk factor pairs. The analytic reads the correlations from the file. The name correlationUri is an older name for the same parameter. ORE looks for the file in the input path of the Setup block.',
            'None.', 'A file name. ORE looks for the file in the input path of the Setup block.',
            '1.8.14.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: xva.correlationInputFile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'xva' and name = 'scenarioCorrSimulation'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''xva'']/Parameter[@name=''scenarioCorrSimulation'']',
            'analytic', 'xva', 'scenarioCorrSimulation', 147, 'string', false,
            'Correlation scenario simulation file', 'A file name.',
            'Names the XML file with the simulation market for the correlation calculation. The name scenarioSimulationUri is an older name for the same parameter. The correlation calculation reads this parameter from the xva block when generate correlations is on.',
            'None.', 'A file name. ORE looks for the file in the input path of the Setup block.',
            '1.8.17.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: xva.scenarioCorrSimulation'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'xva' and name = 'simulationConfigFile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''xva'']/Parameter[@name=''simulationConfigFile'']',
            'analytic', 'xva', 'simulationConfigFile', 148, 'string', false,
            'Simulation configuration file', 'A file name.',
            'Names the XML file with the simulation configuration for the calculation that ORE runs from the xva block. For example, simulation.xml. The correlation calculation reads this parameter from the xva block when generate correlations is on.',
            'None.', 'A file name. ORE looks for the file in the input path of the Setup block.',
            '1.8.0.7 or earlier', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: xva.simulationConfigFile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'xva' and name = 'historicalScenarioFile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''xva'']/Parameter[@name=''historicalScenarioFile'']',
            'analytic', 'xva', 'historicalScenarioFile', 149, 'string', false,
            'Historical scenario file', 'A file name.',
            'Names the file with the historical market scenarios for the correlation calculation. The name scenarioFile is an older name for the same parameter. The correlation calculation reads this parameter from the xva block when generate correlations is on.',
            'None.', 'A file name. ORE looks for the file in the input path of the Setup block.',
            '1.8.12.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: xva.historicalScenarioFile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'xva' and name = 'sensitivityConfigFile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''xva'']/Parameter[@name=''sensitivityConfigFile'']',
            'analytic', 'xva', 'sensitivityConfigFile', 150, 'string', false,
            'Sensitivity configuration file', 'A file name.',
            'Names the XML file with the sensitivity configuration, that is the risk factors and the shifts, for the correlation calculation. The correlation calculation reads this parameter from the xva block when generate correlations is on.',
            'None.', 'A file name. ORE looks for the file in the input path of the Setup block.',
            '1.8.2.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: xva.sensitivityConfigFile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'xva' and name = 'historicalPeriod'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''xva'']/Parameter[@name=''historicalPeriod'']',
            'analytic', 'xva', 'historicalPeriod', 151, 'string', false,
            'Historical period', 'Two dates or a list of dates.',
            'Sets the period of the history that the correlation calculation uses. The names lookbackPeriod, benchmarkVarPeriod and benchmarkPeriod are other names for the same parameter. The correlation calculation reads this parameter from the xva block when generate correlations is on.',
            'None. The correlation calculation needs it.', 'Dates as the correlation analytic reads them.',
            '1.8.12.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: xva.historicalPeriod'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'xva' and name = 'mporDays'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''xva'']/Parameter[@name=''mporDays'']',
            'analytic', 'xva', 'mporDays', 152, 'integer', false,
            'MPoR days', 'A whole number of days.',
            'Sets the number of days of the horizon over which the correlation calculation measures the moves of the risk factors. The name horizonDays is another name for the same parameter. The correlation calculation reads this parameter from the xva block when generate correlations is on.',
            '10', 'A whole number of zero or more.',
            '1.8.11.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: xva.mporDays'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'xva' and name = 'mporCalendar'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''xva'']/Parameter[@name=''mporCalendar'']',
            'analytic', 'xva', 'mporCalendar', 153, 'string', false,
            'MPoR calendar', 'A calendar name.',
            'Names the calendar that the correlation calculation uses to count the days of the horizon. The name horizonCalendar is another name for the same parameter. The correlation calculation reads this parameter from the xva block when generate correlations is on.',
            'The calendar of the base currency.', 'A calendar name such as TARGET or UK.',
            '1.8.12.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: xva.mporCalendar'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'xva' and name = 'deterministicInitialMarginFile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''xva'']/Parameter[@name=''deterministicInitialMarginFile'']',
            'analytic', 'xva', 'deterministicInitialMarginFile', 154, 'string', false,
            'Deterministic initial margin file', 'A file name.',
            'Names a file with the initial margin of each netting set as a time series. The analytic uses these amounts for the initial margin instead of a model. ORE looks for the file in the input path of the Setup block.',
            'None.', 'A file name. ORE looks for the file in the input path of the Setup block.',
            '1.8.9.1', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: xva.deterministicInitialMarginFile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'xva' and name = 'fullInitialCollateralisation'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''xva'']/Parameter[@name=''fullInitialCollateralisation'']',
            'analytic', 'xva', 'fullInitialCollateralisation', 155, 'boolean', false,
            'Full initial collateralisation', 'A flag. Y sets the collateral at the start to the NPV.',
            'Decides what the collateral balance is at the valuation date. When it is Y, ORE sets the balance of each netting set to its NPV, so that the expected exposure, the expected negative exposure and the PFE are all zero at the start. When it is N, ORE sets the balance to zero.',
            'No. The collateral balance at the start is zero.', 'A flag, written Y or N, or true or false.',
            '1.8.3.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: xva.fullInitialCollateralisation'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'xva' and name = 'dimDistributionCoveredStdDevs'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''xva'']/Parameter[@name=''dimDistributionCoveredStdDevs'']',
            'analytic', 'xva', 'dimDistributionCoveredStdDevs', 156, 'double', false,
            'DIM distribution range', 'A number of standard deviations, or inf.',
            'Sets the range of the DIM distribution report: ORE covers this many standard deviations around the mean of the data. The value inf covers all of the data. ORE uses it only when DIM is on.',
            '5.0', 'A number, or the word inf.',
            '1.8.13.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: xva.dimDistributionCoveredStdDevs'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'xva' and name = 'dimDistributionGridSize'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''xva'']/Parameter[@name=''dimDistributionGridSize'']',
            'analytic', 'xva', 'dimDistributionGridSize', 157, 'integer', false,
            'DIM distribution grid size', 'A whole number.',
            'Sets the number of points of the grid of the DIM distribution report. ORE uses it only when DIM is on.',
            '50', 'A whole number of zero or more.',
            '1.8.13.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: xva.dimDistributionGridSize'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'xva' and name = 'borrowingCurve'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''xva'']/Parameter[@name=''borrowingCurve'']',
            'analytic', 'xva', 'borrowingCurve', 158, 'string', false,
            'Borrowing curve', 'The name of a yield curve.',
            'Names the yield curve at which we borrow, for the FVA. For example, BANK_EUR_BORROW.',
            'None. An FVA calculation needs a curve.', 'The name of a yield curve in the market data.',
            '1.8.17.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: xva.borrowingCurve'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'xva' and name = 'lendingCurve'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''xva'']/Parameter[@name=''lendingCurve'']',
            'analytic', 'xva', 'lendingCurve', 159, 'string', false,
            'Lending curve', 'The name of a yield curve.',
            'Names the yield curve at which we lend, for the FVA. For example, BANK_EUR_LEND.',
            'None. An FVA calculation needs a curve.', 'The name of a yield curve in the market data.',
            '1.8.17.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: xva.lendingCurve'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'xva' and name = 'generateCorrelations'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''xva'']/Parameter[@name=''generateCorrelations'']',
            'analytic', 'xva', 'generateCorrelations', 160, 'boolean', false,
            'Generate correlations', 'A flag. Y runs the correlation analytic.',
            'Decides whether the XVA analytic first runs the correlation analytic and uses its result as the correlations of the simulation model. The correlation analytic reads its parameters from the xva block when it runs this way.',
            'No.', 'A flag, written Y or N, or true or false.',
            '1.8.14.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: xva.generateCorrelations'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'xva' and name = 'useDoublePrecisionCubes'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''xva'']/Parameter[@name=''useDoublePrecisionCubes'']',
            'analytic', 'xva', 'useDoublePrecisionCubes', 161, 'boolean', false,
            'Double precision cubes', 'A flag. Y uses double precision.',
            'Decides whether the XVA analytic builds its cubes with double precision numbers. Single precision uses half the memory. Switch to double precision when the single precision rounding is too large for your netting sets. The XVA analytic has the same parameter.',
            'No. The cubes use single precision.', 'A flag, written Y or N, or true or false.',
            '1.8.13.1', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: xva.useDoublePrecisionCubes'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'xva' and name = 'mporCashFlowMode'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''xva'']/Parameter[@name=''mporCashFlowMode'']',
            'analytic', 'xva', 'mporCashFlowMode', 162, 'string', false,
            'MPoR cash flow mode', 'NonePay, BothPay, WePay, TheyPay or Unspecified.',
            'Sets the assumption about who pays the cash flows inside the margin period of risk. NonePay assumes no payment, BothPay assumes that both parties pay, WePay that only we pay, and TheyPay that only the counterparty pays. With Unspecified, ORE assumes NonePay if the simulation uses a sticky close-out date and BothPay otherwise.',
            'Unspecified', 'One of NonePay, BothPay, WePay, TheyPay or Unspecified.',
            '1.8.12.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: xva.mporCashFlowMode'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'xva' and name = 'exposureProfilesUseCloseOutValues'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''xva'']/Parameter[@name=''exposureProfilesUseCloseOutValues'']',
            'analytic', 'xva', 'exposureProfilesUseCloseOutValues', 163, 'boolean', false,
            'Exposure profiles use close-out values', 'A flag. Y uses the close-out values.',
            'Decides whether the exposure profiles use the close-out value of a trade, which is its value at the end of the margin period of risk, instead of its value at the default date. It needs a simulation with a close-out grid. The XVA analytic has the same parameter.',
            'No. The profiles use the values at the default date.', 'A flag, written Y or N, or true or false.',
            '1.8.13.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: xva.exposureProfilesUseCloseOutValues'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'xva' and name = 'writeIndividualExposureReports'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''xva'']/Parameter[@name=''writeIndividualExposureReports'']',
            'analytic', 'xva', 'writeIndividualExposureReports', 164, 'boolean', false,
            'Write individual exposure reports', 'A flag. Y writes one report for each trade and netting set.',
            'Decides whether the analytic writes one exposure report for each trade and for each netting set. When it is N, the analytic writes one report for all trades and one for all netting sets. Switch it off for a large portfolio to avoid a very large number of files. The XVA analytic has the same parameter.',
            'Yes.', 'A flag, written Y or N, or true or false.',
            '1.8.15.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: xva.writeIndividualExposureReports'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'xva' and name = 'kva'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''xva'']/Parameter[@name=''kva'']',
            'analytic', 'xva', 'kva', 165, 'boolean', false,
            'Compute KVA', 'A flag. Y computes the capital value adjustment.',
            'Switches the capital value adjustment (KVA) on or off. KVA prices the cost of the regulatory capital that the counterparty credit risk needs. The KVA parameters set the capital model.',
            'No.', 'A flag, written Y or N, or true or false.',
            '1.8.4.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: xva.kva'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'xva' and name = 'kvaCapitalDiscountRate'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''xva'']/Parameter[@name=''kvaCapitalDiscountRate'']',
            'analytic', 'xva', 'kvaCapitalDiscountRate', 166, 'double', false,
            'KVA capital discount rate', 'A rate, for example 0.10.',
            'Sets the rate at which ORE discounts the cost of capital in the KVA. ORE uses it only when KVA is on.',
            '0.10', 'A number, written with a decimal point.',
            '1.8.4.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: xva.kvaCapitalDiscountRate'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'xva' and name = 'kvaAlpha'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''xva'']/Parameter[@name=''kvaAlpha'']',
            'analytic', 'xva', 'kvaAlpha', 167, 'double', false,
            'KVA alpha', 'A number.',
            'Sets the multiplier alpha of the regulatory exposure at default in the KVA calculation. ORE uses it only when KVA is on.',
            '1.4', 'A number, written with a decimal point.',
            '1.8.4.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: xva.kvaAlpha'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'xva' and name = 'kvaRegAdjustment'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''xva'']/Parameter[@name=''kvaRegAdjustment'']',
            'analytic', 'xva', 'kvaRegAdjustment', 168, 'double', false,
            'KVA regulatory adjustment', 'A number.',
            'Sets the regulatory adjustment factor that turns the capital requirement into risk weighted assets in the KVA calculation. ORE uses it only when KVA is on.',
            '12.5', 'A number, written with a decimal point.',
            '1.8.4.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: xva.kvaRegAdjustment'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'xva' and name = 'kvaCapitalHurdle'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''xva'']/Parameter[@name=''kvaCapitalHurdle'']',
            'analytic', 'xva', 'kvaCapitalHurdle', 169, 'double', false,
            'KVA capital hurdle', 'A rate, for example 0.012.',
            'Sets the return on capital that the KVA charges, the capital hurdle rate. ORE uses it only when KVA is on.',
            '0.012', 'A number, written with a decimal point.',
            '1.8.4.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: xva.kvaCapitalHurdle'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'xva' and name = 'kvaOurPdFloor'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''xva'']/Parameter[@name=''kvaOurPdFloor'']',
            'analytic', 'xva', 'kvaOurPdFloor', 170, 'double', false,
            'KVA floor on our default probability', 'A probability.',
            'Sets the lowest default probability that the KVA uses for us. ORE uses it only when KVA is on.',
            '0.03', 'A number, written with a decimal point.',
            '1.8.4.1', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: xva.kvaOurPdFloor'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'xva' and name = 'kvaTheirPdFloor'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''xva'']/Parameter[@name=''kvaTheirPdFloor'']',
            'analytic', 'xva', 'kvaTheirPdFloor', 171, 'double', false,
            'KVA floor on their default probability', 'A probability.',
            'Sets the lowest default probability that the KVA uses for the counterparty. ORE uses it only when KVA is on.',
            '0.03', 'A number, written with a decimal point.',
            '1.8.4.1', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: xva.kvaTheirPdFloor'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'xva' and name = 'kvaOurCvaRiskWeight'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''xva'']/Parameter[@name=''kvaOurCvaRiskWeight'']',
            'analytic', 'xva', 'kvaOurCvaRiskWeight', 172, 'double', false,
            'KVA CVA risk weight for us', 'A number.',
            'Sets the CVA risk weight that the KVA uses for us. ORE uses it only when KVA is on.',
            '0.05', 'A number, written with a decimal point.',
            '1.8.4.1', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: xva.kvaOurCvaRiskWeight'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'xva' and name = 'kvaTheirCvaRiskWeight'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''xva'']/Parameter[@name=''kvaTheirCvaRiskWeight'']',
            'analytic', 'xva', 'kvaTheirCvaRiskWeight', 173, 'double', false,
            'KVA CVA risk weight for them', 'A number.',
            'Sets the CVA risk weight that the KVA uses for the counterparty. ORE uses it only when KVA is on.',
            '0.05', 'A number, written with a decimal point.',
            '1.8.4.1', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: xva.kvaTheirCvaRiskWeight'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'xva' and name = 'cvaSensi'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''xva'']/Parameter[@name=''cvaSensi'']',
            'analytic', 'xva', 'cvaSensi', 174, 'boolean', false,
            'CVA sensitivities', 'A flag. Y computes the CVA sensitivities.',
            'Decides whether the analytic computes the sensitivity of the CVA to the credit spreads of the counterparties. The CVA sensitivity grid and the shift size set the points and the bump.',
            'No.', 'A flag, written Y or N, or true or false.',
            '1.8.6.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: xva.cvaSensi'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'xva' and name = 'cvaSensiGrid'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''xva'']/Parameter[@name=''cvaSensiGrid'']',
            'analytic', 'xva', 'cvaSensiGrid', 175, 'string', false,
            'CVA sensitivity grid', 'A comma-separated list of periods.',
            'Lists the tenors of the credit spread curve for which the analytic computes the CVA sensitivity. For example, 6M,1Y,2Y,3Y,5Y,10Y. ORE uses it only when CVA sensitivities are on.',
            'None.', 'A comma-separated list of periods such as 1Y,5Y.',
            '1.8.6.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: xva.cvaSensiGrid'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'xva' and name = 'cvaSensiShiftSize'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''xva'']/Parameter[@name=''cvaSensiShiftSize'']',
            'analytic', 'xva', 'cvaSensiShiftSize', 176, 'double', false,
            'CVA sensitivity shift size', 'A number.',
            'Sets the size of the bump of the credit spread in the CVA sensitivity calculation. The default is one basis point. ORE uses it only when CVA sensitivities are on.',
            '0.0001', 'A number, written with a decimal point.',
            '1.8.6.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: xva.cvaSensiShiftSize'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'xva' and name = 'rawCubeOutput'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''xva'']/Parameter[@name=''rawCubeOutput'']',
            'analytic', 'xva', 'rawCubeOutput', 177, 'boolean', false,
            'Write the raw cube report', 'A flag. Y writes the report.',
            'Decides whether the analytic writes the trade NPV cube as a text report. A value in Raw cube report file also switches the report on and sets its name. Without a name the file is rawcube.csv.',
            'No.', 'A flag, written Y or N, or true or false.',
            '1.8.16.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: xva.rawCubeOutput'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'xva' and name = 'netCubeOutput'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''xva'']/Parameter[@name=''netCubeOutput'']',
            'analytic', 'xva', 'netCubeOutput', 178, 'boolean', false,
            'Write the net cube report', 'A flag. Y writes the report.',
            'Decides whether the analytic writes the aggregated NPV cube as a text report. A value in Net cube report file also switches the report on and sets its name. Without a name the file is netcube.csv.',
            'No.', 'A flag, written Y or N, or true or false.',
            '1.8.16.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: xva.netCubeOutput'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'xva' and name = 'nettingSetCubeFile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''xva'']/Parameter[@name=''nettingSetCubeFile'']',
            'analytic', 'xva', 'nettingSetCubeFile', 179, 'string', false,
            'Netting set cube file', 'A file name.',
            'Names the netting set cube file that the XVA analytic reads when the run has no simulation. It is the file that an earlier simulation wrote. For example, nettingSetCube.csv.gz.',
            'None.', 'A file name. ORE looks for the file in the output path of the Setup block.',
            '1.8.6.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: xva.nettingSetCubeFile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'xva' and name = 'cptyCubeFile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''xva'']/Parameter[@name=''cptyCubeFile'']',
            'analytic', 'xva', 'cptyCubeFile', 180, 'string', false,
            'Counterparty cube file', 'A file name.',
            'Names the counterparty cube file that the XVA analytic reads when the run has no simulation. The cube holds the survival probabilities for the dynamic credit calculation. It is the file that an earlier simulation wrote. For example, cptyCube.csv.gz.',
            'None.', 'A file name. ORE looks for the file in the output path of the Setup block.',
            '1.8.6.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: xva.cptyCubeFile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'xvaSensitivity' and name = 'active'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''xvaSensitivity'']/Parameter[@name=''active'']',
            'analytic', 'xvaSensitivity', 'active', 181, 'boolean', true,
            'Run the XVA sensitivity analytic', 'A flag. Y runs the analytic.',
            'Switches the XVA sensitivity analytic on or off. When it is on, the run computes the sensitivities of the value adjustments and the exposures by bump and revalue: it shifts each risk factor and runs the XVA calculation again. The analytic has no settings of its own for the exposure and the XVA. It reuses those of the simulation and XVA analytics, which you can switch off.',
            'No. The analytic does not run.', 'A flag, written Y or N, or true or false.',
            '1.8.12.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: xvaSensitivity.active'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'xvaSensitivity' and name = 'marketConfigFile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''xvaSensitivity'']/Parameter[@name=''marketConfigFile'']',
            'analytic', 'xvaSensitivity', 'marketConfigFile', 182, 'string', false,
            'Simulation market file', 'A file name.',
            'Names the XML file with the simulation market under which the analytic computes its results. The analytic needs only the part under Market. The analytic replaces the market of the valuation date in the exposure simulation with this simulation market. For example, simulation.xml. The simulation market behaves differently from the market of the valuation date for some risk factors, for example it can use other curve tenors or only the at-the-money swaption volatilities. To reduce the difference, switch on UseSpreadedTermStructures in the scenario setup and simulate the swaption volatility.',
            'None. The analytic needs this file.', 'A file name. ORE looks for the file in the input path of the Setup block.',
            '1.8.12.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: xvaSensitivity.marketConfigFile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'xvaSensitivity' and name = 'sensitivityConfigFile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''xvaSensitivity'']/Parameter[@name=''sensitivityConfigFile'']',
            'analytic', 'xvaSensitivity', 'sensitivityConfigFile', 183, 'string', false,
            'Sensitivity configuration file', 'A file name.',
            'Names the XML file that sets the sensitivity calculation: the risk factors, the shift types and sizes, and the par conversion. The analytic uses the same file format as the sensitivity analytic. For example, sensitivity.xml.',
            'None. The analytic needs this file.', 'A file name. ORE looks for the file in the input path of the Setup block.',
            '1.8.12.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: xvaSensitivity.sensitivityConfigFile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'xvaSensitivity' and name = 'outputSensitivityThreshold'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''xvaSensitivity'']/Parameter[@name=''outputSensitivityThreshold'']',
            'analytic', 'xvaSensitivity', 'outputSensitivityThreshold', 184, 'double', false,
            'Sensitivity output threshold', 'A number.',
            'Sets the smallest absolute value that the analytic writes. It drops every finite difference below the number from the output files. A larger value gives smaller files. For example, 0.000001.',
            '0.000001', 'A number, written with a decimal point.',
            '1.8.12.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: xvaSensitivity.outputSensitivityThreshold'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'xvaSensitivity' and name = 'parSensitivity'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''xvaSensitivity'']/Parameter[@name=''parSensitivity'']',
            'analytic', 'xvaSensitivity', 'parSensitivity', 185, 'boolean', false,
            'Par sensitivities', 'A flag. Y also writes par sensitivities.',
            'Decides whether the analytic converts the raw sensitivities into sensitivities to par rates and writes them in an extra report. The sensitivity configuration file must have par conversion sections.',
            'Yes.', 'A flag, written Y or N, or true or false.',
            '1.8.12.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: xvaSensitivity.parSensitivity'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'xvaSensitivity' and name = 'outputJacobi'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''xvaSensitivity'']/Parameter[@name=''outputJacobi'']',
            'analytic', 'xvaSensitivity', 'outputJacobi', 186, 'boolean', false,
            'Write the Jacobi matrices', 'A flag. Y writes the matrices.',
            'Decides whether the analytic writes the Jacobi matrix and its inverse, which convert between raw and par sensitivities, to files. The two file name parameters set the names.',
            'No.', 'A flag, written Y or N, or true or false.',
            '1.8.12.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: xvaSensitivity.outputJacobi'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'xvaSensitivity' and name = 'jacobiOutputFile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''xvaSensitivity'']/Parameter[@name=''jacobiOutputFile'']',
            'analytic', 'xvaSensitivity', 'jacobiOutputFile', 187, 'string', false,
            'Jacobi matrix file', 'A file name.',
            'Names the file for the Jacobi matrix. It has an effect only when Write the Jacobi matrices is on.',
            'xva_sensi_jacobi.csv', 'A file name, written without a directory. ORE writes the file in the output path of the Setup block.',
            '1.8.12.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: xvaSensitivity.jacobiOutputFile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'xvaSensitivity' and name = 'jacobiInverseOutputFile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''xvaSensitivity'']/Parameter[@name=''jacobiInverseOutputFile'']',
            'analytic', 'xvaSensitivity', 'jacobiInverseOutputFile', 188, 'string', false,
            'Inverse Jacobi matrix file', 'A file name.',
            'Names the file for the inverse Jacobi matrix. It has an effect only when Write the Jacobi matrices is on.',
            'xva_sensi_jacobi_inverse.csv', 'A file name, written without a directory. ORE writes the file in the output path of the Setup block.',
            '1.8.12.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: xvaSensitivity.jacobiInverseOutputFile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'xvaSensitivity' and name = 'outputPrecision'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''xvaSensitivity'']/Parameter[@name=''outputPrecision'']',
            'analytic', 'xvaSensitivity', 'outputPrecision', 189, 'integer', false,
            'Sensitivity output precision', 'A whole number of decimal places.',
            'Sets the number of decimal places of the numbers in the XVA sensitivity reports. A higher number shows more of each value and makes the files larger.',
            '4', 'A whole number of zero or more.',
            '1.8.12.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: xvaSensitivity.outputPrecision'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'xvaExplain' and name = 'active'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''xvaExplain'']/Parameter[@name=''active'']',
            'analytic', 'xvaExplain', 'active', 190, 'boolean', true,
            'Run the XVA explain analytic', 'A flag. Y runs the analytic.',
            'Switches the XVA explain analytic on or off. When it is on, the run computes the change of the value adjustments that the market moves between two dates imply: the XVA with the market data of the second date minus the XVA with the market data of the first, both at the exposure of the first date. The output has the value adjustments under each shifted risk factor and a CVA explain report. The analytic has no settings of its own for the exposure and the XVA. It reuses those of the simulation and XVA analytics, which you can switch off.',
            'No. The analytic does not run.', 'A flag, written Y or N, or true or false.',
            '1.8.13.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: xvaExplain.active'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'xvaExplain' and name = 'marketConfigFile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''xvaExplain'']/Parameter[@name=''marketConfigFile'']',
            'analytic', 'xvaExplain', 'marketConfigFile', 191, 'string', false,
            'Simulation market file', 'A file name.',
            'Names the XML file with the simulation market under which the analytic computes its results. The analytic needs only the part under Market. The analytic replaces the market of the valuation date in the exposure simulation with this simulation market. For example, simulation.xml. The simulation market behaves differently from the market of the valuation date for some risk factors, for example it can use other curve tenors or only the at-the-money swaption volatilities. To reduce the difference, switch on UseSpreadedTermStructures in the scenario setup and simulate the swaption volatility.',
            'None. The analytic needs this file.', 'A file name. ORE looks for the file in the input path of the Setup block.',
            '1.8.13.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: xvaExplain.marketConfigFile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'xvaExplain' and name = 'sensitivityConfigFile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''xvaExplain'']/Parameter[@name=''sensitivityConfigFile'']',
            'analytic', 'xvaExplain', 'sensitivityConfigFile', 192, 'string', false,
            'Sensitivity configuration file', 'A file name.',
            'Names the XML file that lists the risk factors to explain. For each risk factor of the file, ORE computes the change of its par rate between the first date and the second date. It builds a shift scenario from the change and computes the CVA change that the shift implies. For example, sensitivity_stress.xml.',
            'None. The analytic needs this file.', 'A file name. ORE looks for the file in the input path of the Setup block.',
            '1.8.13.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: xvaExplain.sensitivityConfigFile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'xvaExplain' and name = 'writeCubes'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''xvaExplain'']/Parameter[@name=''writeCubes'']',
            'analytic', 'xvaExplain', 'writeCubes', 193, 'boolean', false,
            'Write the cubes', 'A flag. ORE ignores it in the xvaExplain block.',
            'The user guide lists this parameter for the XVA explain analytic, but ORE reads it only for the XVA stress analytic. A value here has no effect in release 1.8.17.0.',
            'No. ORE ignores the parameter, so a value has no effect.', 'A flag, written Y or N, or true or false.',
            '1.8.13.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: xvaExplain.writeCubes'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'xvaExplain' and name = 'shiftThreshold'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''xvaExplain'']/Parameter[@name=''shiftThreshold'']',
            'analytic', 'xvaExplain', 'shiftThreshold', 194, 'double', false,
            'Par rate shift threshold', 'A number.',
            'Sets the smallest par rate shift that the analytic uses. It ignores every shift below this number. For example, 1e-4 ignores shifts smaller than one basis point.',
            '0. The analytic ignores no shift.', 'A number, written with a decimal point.',
            '1.8.13.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: xvaExplain.shiftThreshold'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'xvaExplain' and name = 'mporDays'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''xvaExplain'']/Parameter[@name=''mporDays'']',
            'analytic', 'xvaExplain', 'mporDays', 195, 'integer', false,
            'Days to the second date', 'A whole number of days.',
            'Sets the number of days from the valuation date to the second evaluation date. The analytic derives the second date from the valuation date plus this number of days, and rolls it to a business day with the calendar. The parameter MPoR date sets the date directly. For example, 1 explains the move of one day.',
            '10', 'A whole number of zero or more.',
            '1.8.13.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: xvaExplain.mporDays'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'xvaExplain' and name = 'mporCalendar'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''xvaExplain'']/Parameter[@name=''mporCalendar'']',
            'analytic', 'xvaExplain', 'mporCalendar', 196, 'string', false,
            'Calendar of the second date', 'A calendar name.',
            'Names the calendar that ORE uses to make sure that the second evaluation date is a business day. For example, EUR.',
            'The calendar of the base currency.', 'A calendar name such as TARGET, EUR or UK.',
            '1.8.13.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: xvaExplain.mporCalendar'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'xvaExplain' and name = 'stressConfigFile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''xvaExplain'']/Parameter[@name=''stressConfigFile'']',
            'analytic', 'xvaExplain', 'stressConfigFile', 197, 'string', false,
            'Stress configuration file', 'A file name. ORE ignores it in the xvaExplain block.',
            'The user guide lists this parameter for the XVA explain analytic, but the analytic reads the risk factors from the sensitivity configuration file. A value here has no effect in release 1.8.17.0.',
            'None. ORE ignores the parameter, so a value has no effect.', 'A file name. ORE looks for the file in the input path of the Setup block.',
            '1.8.13.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: xvaExplain.stressConfigFile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'xvaExplain' and name = 'mporDate'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''xvaExplain'']/Parameter[@name=''mporDate'']',
            'analytic', 'xvaExplain', 'mporDate', 198, 'date', false,
            'Second evaluation date', 'A date.',
            'Sets the second evaluation date directly, instead of deriving it from the number of days. For example, 2026-03-02.',
            'None. ORE derives the date from the number of days.', 'A date, written as YYYY-MM-DD or YYYYMMDD.',
            '1.8.13.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: xvaExplain.mporDate'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'bacva' and name = 'active'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''bacva'']/Parameter[@name=''active'']',
            'analytic', 'bacva', 'active', 199, 'boolean', true,
            'Run the BA-CVA analytic', 'A flag. Y runs the analytic.',
            'Switches the basic approach for the CVA capital (BA-CVA) on or off. The calculation uses the SA-CCR analytic under it to compute the exposures at default.',
            'No. The analytic does not run.', 'A flag, written Y or N, or true or false.',
            '1.8.13.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: bacva.active'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'bacva' and name = 'simmVersion'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''bacva'']/Parameter[@name=''simmVersion'']',
            'analytic', 'bacva', 'simmVersion', 200, 'double', false,
            'SIMM version', 'A SIMM version, for example 2.6.',
            'Names the version of the SIMM that the calculation uses for the name and bucket mapping of commodity hedging sets. The run stops if the version is empty.',
            'None. The parameter is mandatory.', 'A SIMM version such as 2.4, 2.5A, 2.6 or 2.7.',
            '1.8.13.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: bacva.simmVersion'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'bacva' and name = 'csaFile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''bacva'']/Parameter[@name=''csaFile'']',
            'analytic', 'bacva', 'csaFile', 201, 'string', false,
            'Netting set definitions file', 'A file name.',
            'Names the XML file with the netting set definitions, as in the XVA analytics. For example, netting.xml.',
            'None. The analytic needs this file, here or in the Setup block.', 'A file name. ORE looks for the file in the input path of the Setup block.',
            '1.8.13.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: bacva.csaFile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'bacva' and name = 'collateralBalancesFile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''bacva'']/Parameter[@name=''collateralBalancesFile'']',
            'analytic', 'bacva', 'collateralBalancesFile', 202, 'string', false,
            'Collateral balances file', 'A file name.',
            'Names the XML file with the variation margin and initial margin balances of the valuation date. The SA-CCR analytic under the analytic reads the file. For example, collateralbalances.xml.',
            'None.', 'A file name. ORE looks for the file in the input path of the Setup block.',
            '1.8.13.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: bacva.collateralBalancesFile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'bacva' and name = 'nameMappingInputFile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''bacva'']/Parameter[@name=''nameMappingInputFile'']',
            'analytic', 'bacva', 'nameMappingInputFile', 203, 'string', false,
            'Name mapping file', 'A file name.',
            'Names the file that maps the names in the portfolio to the SIMM names for the hedging sets. Without it ORE uses the names as they are.',
            'None. ORE uses the names as they are.', 'A file name. ORE looks for the file in the input path of the Setup block.',
            '1.8.13.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: bacva.nameMappingInputFile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'bacva' and name = 'bucketMappingInputFile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''bacva'']/Parameter[@name=''bucketMappingInputFile'']',
            'analytic', 'bacva', 'bucketMappingInputFile', 204, 'string', false,
            'Bucket mapping file', 'A file name.',
            'Names the file that maps risk factors to the SIMM buckets for the hedging sets. Without it ORE uses the standard buckets.',
            'None. ORE uses the standard SIMM buckets.', 'A file name. ORE looks for the file in the input path of the Setup block.',
            '1.8.13.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: bacva.bucketMappingInputFile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'xvaStress' and name = 'active'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''xvaStress'']/Parameter[@name=''active'']',
            'analytic', 'xvaStress', 'active', 205, 'boolean', true,
            'Run the XVA stress analytic', 'A flag. Y runs the analytic.',
            'Switches the XVA stress analytic on or off. When it is on, the run computes the value adjustments and exposures under each stress scenario and writes them next to the base results. Use it to see how the XVA reacts to a market shock. The analytic has no settings of its own for the exposure and the XVA. It reuses those of the simulation and XVA analytics, which you can switch off. For speed, use AMC in the simulation.',
            'No. The analytic does not run.', 'A flag, written Y or N, or true or false.',
            '1.8.12.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: xvaStress.active'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'xvaStress' and name = 'marketConfigFile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''xvaStress'']/Parameter[@name=''marketConfigFile'']',
            'analytic', 'xvaStress', 'marketConfigFile', 206, 'string', false,
            'Simulation market file', 'A file name.',
            'Names the XML file with the simulation market under which the analytic computes its results. The analytic needs only the part under Market. The analytic replaces the market of the valuation date in the exposure simulation with this simulation market. For example, simulation.xml. The simulation market behaves differently from the market of the valuation date for some risk factors, for example it can use other curve tenors or only the at-the-money swaption volatilities. To reduce the difference, switch on UseSpreadedTermStructures in the scenario setup and simulate the swaption volatility.',
            'None. The analytic needs this file.', 'A file name. ORE looks for the file in the input path of the Setup block.',
            '1.8.12.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: xvaStress.marketConfigFile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'xvaStress' and name = 'stressConfigFile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''xvaStress'']/Parameter[@name=''stressConfigFile'']',
            'analytic', 'xvaStress', 'stressConfigFile', 207, 'string', false,
            'Stress configuration file', 'A file name.',
            'Names the XML file with the stress scenarios in the format of the NPV stress test. For example, stresstest.xml.',
            'None. The analytic needs this file.', 'A file name. ORE looks for the file in the input path of the Setup block.',
            '1.8.12.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: xvaStress.stressConfigFile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'xvaStress' and name = 'writeCubes'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''xvaStress'']/Parameter[@name=''writeCubes'']',
            'analytic', 'xvaStress', 'writeCubes', 208, 'boolean', false,
            'Write the cubes under each scenario', 'A flag. Y writes the cubes.',
            'Decides whether the analytic writes the raw cube and the net cube for each stress scenario. The files can be very large.',
            'No.', 'A flag, written Y or N, or true or false.',
            '1.8.12.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: xvaStress.writeCubes'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'xvaStress' and name = 'sensitivityConfigFile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''xvaStress'']/Parameter[@name=''sensitivityConfigFile'']',
            'analytic', 'xvaStress', 'sensitivityConfigFile', 209, 'string', false,
            'Sensitivity configuration file', 'A file name.',
            'Names the XML file that sets the sensitivity calculation, in particular the par conversion that a par stress test needs. Without it, ORE supports no par stress test. For example, sensitivity_stress.xml.',
            'None. Par stress tests are not supported.', 'A file name. ORE looks for the file in the input path of the Setup block.',
            '1.8.12.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: xvaStress.sensitivityConfigFile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'xvaStress' and name = 'scenarioOutputFile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''xvaStress'']/Parameter[@name=''scenarioOutputFile'']',
            'analytic', 'xvaStress', 'scenarioOutputFile', 210, 'string', false,
            'Stress scenario output file', 'A file name.',
            'Names the file for the report of the XVA stress scenarios.',
            'xva_stress.csv', 'A file name, written without a directory. ORE writes the file in the output path of the Setup block.',
            '1.8.12.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: xvaStress.scenarioOutputFile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'sacva' and name = 'active'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''sacva'']/Parameter[@name=''active'']',
            'analytic', 'sacva', 'active', 211, 'boolean', true,
            'Run the SA-CVA analytic', 'A flag. Y runs the analytic.',
            'Switches the standardised approach for the CVA capital (SA-CVA) on or off. The calculation is based on CVA sensitivities. ORE can compute them on the fly, with the XVA sensitivity analytic and the simulation and XVA analytics under it, or read them from a file. The run stops if no sensitivity file is given and the XVA sensitivity analytic is missing.',
            'No. The analytic does not run.', 'A flag, written Y or N, or true or false.',
            '1.8.13.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: sacva.active'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'sacva' and name = 'csaFile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''sacva'']/Parameter[@name=''csaFile'']',
            'analytic', 'sacva', 'csaFile', 212, 'string', false,
            'Netting set definitions file', 'A file name.',
            'Names the XML file with the netting set definitions. ORE looks for the file name in the sacva block, then in the bacva block and last in the Setup block. The SA-CVA analytic passes the value on to the simulation analytic that runs under it.',
            'The netting set definitions file of the Setup block.', 'A file name. ORE looks for the file in the input path of the Setup block.',
            '1.8.13.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: sacva.csaFile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'sacva' and name = 'counterpartyFile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''sacva'']/Parameter[@name=''counterpartyFile'']',
            'analytic', 'sacva', 'counterpartyFile', 213, 'string', false,
            'Counterparty file', 'A file name.',
            'Names the XML file with the counterparty information that the SA-CVA calculator needs. ORE looks for the file name in the sacva block, then in the bacva block and last in the Setup block.',
            'The counterparty file of the Setup block.', 'A file name. ORE looks for the file in the input path of the Setup block.',
            '1.8.13.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: sacva.counterpartyFile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'sacva' and name = 'simulationConfigFile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''sacva'']/Parameter[@name=''simulationConfigFile'']',
            'analytic', 'sacva', 'simulationConfigFile', 214, 'string', false,
            'Simulation configuration file', 'A file name.',
            'Names the XML file with the simulation configuration for the exposure simulation under the analytic. The SA-CVA analytic passes the value on to the simulation analytic that runs under it.',
            'None.', 'A file name. ORE looks for the file in the input path of the Setup block.',
            '1.8.13.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: sacva.simulationConfigFile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'sacva' and name = 'scenarioGeneratorData'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''sacva'']/Parameter[@name=''scenarioGeneratorData'']',
            'analytic', 'sacva', 'scenarioGeneratorData', 215, 'string', false,
            'Scenario generator file', 'A file name.',
            'Names the XML file with the scenario generator for the simulation under the analytic. Without it the simulation uses its own defaults, for example 1000 samples. The SA-CVA analytic passes the value on to the simulation analytic that runs under it.',
            'None.', 'A file name. ORE looks for the file in the input path of the Setup block.',
            '1.8.16.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: sacva.scenarioGeneratorData'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'sacva' and name = 'crossAssetModelData'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''sacva'']/Parameter[@name=''crossAssetModelData'']',
            'analytic', 'sacva', 'crossAssetModelData', 216, 'string', false,
            'Cross asset model file', 'A file name.',
            'Names the XML file with the cross asset model for the simulation under the analytic. The SA-CVA analytic passes the value on to the simulation analytic that runs under it.',
            'None.', 'A file name. ORE looks for the file in the input path of the Setup block.',
            '1.8.16.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: sacva.crossAssetModelData'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'sacva' and name = 'dimModel'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''sacva'']/Parameter[@name=''dimModel'']',
            'analytic', 'sacva', 'dimModel', 217, 'string', false,
            'DIM model', 'A DIM model name, for example DeltaVaR.',
            'Selects the dynamic initial margin model for the XVA analytic under the SA-CVA analytic, and switches DIM on. The names are those of the DIM model parameter of the XVA analytic.',
            'None. The XVA analytic under SA-CVA uses no DIM.', 'One of the DIM models of the XVA analytic.',
            '1.8.13.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: sacva.dimModel'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'sacva' and name = 'dimScaling'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''sacva'']/Parameter[@name=''dimScaling'']',
            'analytic', 'sacva', 'dimScaling', 218, 'double', false,
            'DIM scaling', 'A number.',
            'Sets the scaling factor for the DIM values of the XVA analytic under the SA-CVA analytic. When it is missing, the XVA analytic derives the factor from the initial margin of the collateral balances file.',
            'None.', 'A number, written with a decimal point.',
            '1.8.13.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: sacva.dimScaling'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'sacva' and name = 'collateralBalancesFile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''sacva'']/Parameter[@name=''collateralBalancesFile'']',
            'analytic', 'sacva', 'collateralBalancesFile', 219, 'string', false,
            'Collateral balances file', 'A file name.',
            'Names the XML file with the current variation margin and initial margin balances. ORE passes it on to the XVA analytic under the SA-CVA analytic, which derives the DIM scaling from it when DIM scaling is missing.',
            'None.', 'A file name. ORE looks for the file in the input path of the Setup block.',
            '1.8.13.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: sacva.collateralBalancesFile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'sacva' and name = 'storeSensis'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''sacva'']/Parameter[@name=''storeSensis'']',
            'analytic', 'sacva', 'storeSensis', 220, 'boolean', false,
            'Store sensitivities along the paths', 'A flag. Y stores the sensitivities.',
            'Decides whether the simulation under the SA-CVA analytic stores sensitivities along the paths, which the DIM model needs. The SA-CVA analytic passes the value on to the simulation analytic that runs under it.',
            'No.', 'A flag, written Y or N, or true or false.',
            '1.8.13.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: sacva.storeSensis'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'sacva' and name = 'xvaSensiSimMarketParams'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''sacva'']/Parameter[@name=''xvaSensiSimMarketParams'']',
            'analytic', 'sacva', 'xvaSensiSimMarketParams', 221, 'string', false,
            'XVA sensitivity simulation market', 'A file name.',
            'Names the XML file with the simulation market for the XVA sensitivity analytic that runs under the SA-CVA analytic.',
            'None.', 'A file name. ORE looks for the file in the input path of the Setup block.',
            '1.8.17.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: sacva.xvaSensiSimMarketParams'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'sacva' and name = 'xvaSensiScenarioData'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''sacva'']/Parameter[@name=''xvaSensiScenarioData'']',
            'analytic', 'sacva', 'xvaSensiScenarioData', 222, 'string', false,
            'XVA sensitivity scenario file', 'A file name.',
            'Names the XML file with the sensitivity scenarios for the XVA sensitivity analytic that runs under the SA-CVA analytic.',
            'None.', 'A file name. ORE looks for the file in the input path of the Setup block.',
            '1.8.17.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: sacva.xvaSensiScenarioData'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'sacva' and name = 'saCvaNetSensitivitiesFile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''sacva'']/Parameter[@name=''saCvaNetSensitivitiesFile'']',
            'analytic', 'sacva', 'saCvaNetSensitivitiesFile', 223, 'string', false,
            'Net sensitivities file', 'A file name.',
            'Names a file with net sensitivities that ORE passes directly to the SA-CVA calculator. When it is set, ORE computes no sensitivities. For example, sacva_sensitivity.csv.',
            'None. ORE computes the sensitivities or reads the CVA sensitivities file.', 'A file name. ORE looks for the file in the input path of the Setup block.',
            '1.8.13.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: sacva.saCvaNetSensitivitiesFile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'sacva' and name = 'cvaSensitivitiesFile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''sacva'']/Parameter[@name=''cvaSensitivitiesFile'']',
            'analytic', 'sacva', 'cvaSensitivitiesFile', 224, 'string', false,
            'CVA sensitivities file', 'A file name.',
            'Names a file with CVA sensitivities, the output of the XVA sensitivity analytic. ORE maps and aggregates them before the calculation. ORE reads it only when no net sensitivities file is given. For example, xva_par_sensitivity_cva.csv.',
            'None. ORE computes the sensitivities.', 'A file name. ORE looks for the file in the input path of the Setup block.',
            '1.8.13.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: sacva.cvaSensitivitiesFile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'sacva' and name = 'nameMappingInputFile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''sacva'']/Parameter[@name=''nameMappingInputFile'']',
            'analytic', 'sacva', 'nameMappingInputFile', 225, 'string', false,
            'Name mapping file', 'A file name.',
            'Names the file that maps the names in the portfolio to the names that the SA-CVA calculator uses. Without it ORE uses the names as they are.',
            'None. ORE uses the names as they are.', 'A file name. ORE looks for the file in the input path of the Setup block.',
            '1.8.13.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: sacva.nameMappingInputFile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'sacva' and name = 'curveSensiGrid'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''sacva'']/Parameter[@name=''curveSensiGrid'']',
            'analytic', 'sacva', 'curveSensiGrid', 226, 'string', false,
            'Curve sensitivity grid', 'A comma-separated list of numbers of years.',
            'Lists the points on the yield curves for which the simulation stores sensitivities. The SA-CVA analytic passes the value on to the simulation analytic that runs under it. For example, 1,2,3,5,7,10,15,20.',
            'None.', 'A comma-separated list of numbers of years.',
            '1.8.13.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: sacva.curveSensiGrid'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'sacva' and name = 'vegaSensiGrid'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''sacva'']/Parameter[@name=''vegaSensiGrid'']',
            'analytic', 'sacva', 'vegaSensiGrid', 227, 'string', false,
            'Vega sensitivity grid', 'A comma-separated list of numbers of years.',
            'Lists the option expiries for which the simulation stores vega sensitivities. The SA-CVA analytic passes the value on to the simulation analytic that runs under it. For example, 1,2,3,5,7,10,15,20.',
            'None.', 'A comma-separated list of numbers of years.',
            '1.8.13.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: sacva.vegaSensiGrid'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'zeroToParShift' and name = 'active'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''zeroToParShift'']/Parameter[@name=''active'']',
            'analytic', 'zeroToParShift', 'active', 228, 'boolean', true,
            'Run the zero to par shift conversion', 'A flag. Y runs the analytic.',
            'Switches the zero to par shift conversion on or off. The analytic converts the zero rate shifts of a stress test into the par rate shifts that give the same market, and writes them to a file.',
            'No. The analytic does not run.', 'A flag, written Y or N, or true or false.',
            '1.8.12.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: zeroToParShift.active'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'zeroToParShift' and name = 'marketConfigFile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''zeroToParShift'']/Parameter[@name=''marketConfigFile'']',
            'analytic', 'zeroToParShift', 'marketConfigFile', 229, 'string', false,
            'Simulation market file', 'A file name.',
            'Names the XML file with the simulation market under which the analytic computes its results. The analytic needs only the part under Market. For example, simulation.xml.',
            'None. The analytic needs this file.', 'A file name. ORE looks for the file in the input path of the Setup block.',
            '1.8.12.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: zeroToParShift.marketConfigFile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'zeroToParShift' and name = 'stressConfigFile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''zeroToParShift'']/Parameter[@name=''stressConfigFile'']',
            'analytic', 'zeroToParShift', 'stressConfigFile', 230, 'string', false,
            'Stress configuration file', 'A file name.',
            'Names the XML file with the stress scenarios. The same file also decides whether the stress is in the raw domain or in the par domain. For example, stresstest.xml.',
            'None. The analytic needs this file.', 'A file name. ORE looks for the file in the input path of the Setup block.',
            '1.8.12.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: zeroToParShift.stressConfigFile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'zeroToParShift' and name = 'sensitivityConfigFile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''zeroToParShift'']/Parameter[@name=''sensitivityConfigFile'']',
            'analytic', 'zeroToParShift', 'sensitivityConfigFile', 231, 'string', false,
            'Sensitivity configuration file', 'A file name.',
            'Names the XML file with the sensitivity setup, in particular the par conversion sections. For example, sensitivity.xml.',
            'None. The analytic needs this file.', 'A file name. ORE looks for the file in the input path of the Setup block.',
            '1.8.12.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: zeroToParShift.sensitivityConfigFile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'zeroToParShift' and name = 'pricingEnginesFile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''zeroToParShift'']/Parameter[@name=''pricingEnginesFile'']',
            'analytic', 'zeroToParShift', 'pricingEnginesFile', 232, 'string', false,
            'Pricing engines file', 'A file name.',
            'Names the XML file that sets the pricing engines for the analytic. When it is missing, ORE uses the pricing engines file of the Setup block. For example, pricingengine.xml.',
            'The pricing engines file of the Setup block.', 'A file name. ORE looks for the file in the input path of the Setup block.',
            '1.8.12.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: zeroToParShift.pricingEnginesFile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'zeroToParShift' and name = 'scenarioOutputFile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''zeroToParShift'']/Parameter[@name=''scenarioOutputFile'']',
            'analytic', 'zeroToParShift', 'scenarioOutputFile', 233, 'string', false,
            'Scenario output file', 'A file name. ORE ignores it.',
            'ORE does not read this parameter for the zero to par shift conversion. Example files carry it. The par shifts file names the output.',
            'None. ORE ignores the parameter, so a value has no effect.', 'A file name, written without a directory. ORE writes the file in the output path of the Setup block.',
            '1.8.12.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: zeroToParShift.scenarioOutputFile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'zeroToParShift' and name = 'parShiftsFile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''zeroToParShift'']/Parameter[@name=''parShiftsFile'']',
            'analytic', 'zeroToParShift', 'parShiftsFile', 234, 'string', false,
            'Par shifts file', 'A file name.',
            'Names the file that receives the par rate shifts that the conversion finds. For example, parshifts.csv.',
            'parshifts.csv', 'A file name, written without a directory. ORE writes the file in the output path of the Setup block.',
            '1.8.12.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: zeroToParShift.parShiftsFile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'additionalResults' and name = 'active'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''additionalResults'']/Parameter[@name=''active'']',
            'analytic', 'additionalResults', 'active', 235, 'boolean', true,
            'Run the additional results analytic', 'A flag. Y runs the analytic.',
            'Switches the additional results analytic on or off. When it is on, the run writes a report with the results that the pricing engines compute besides the NPV, for example the annuity, the forward rate and the standard deviation of a swaption. What a trade returns depends on its pricing engine. Use it to check how an engine reached an NPV.',
            'No. The analytic does not run.', 'A flag, written Y or N, or true or false.',
            '1.8.6.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: additionalResults.active'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'additionalResults' and name = 'outputFileName'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''additionalResults'']/Parameter[@name=''outputFileName'']',
            'analytic', 'additionalResults', 'outputFileName', 236, 'string', false,
            'Additional results report file', 'A file name.',
            'Is meant to name the file that holds the additional results. ORE does not read it. The report always has the name additional_results.csv, whatever this parameter says, and has one row for each result of each trade with the trade, the name of the result, its type and its value. Example files carry the parameter.',
            'additional_results.csv. ORE ignores the parameter, so the name is always this one.', 'A file name, written without a directory. ORE writes the file in the output path of the Setup block.',
            '1.8.6.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: additionalResults.outputFileName'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'additionalResults' and name = 'additionalResultsReportPrecision'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''additionalResults'']/Parameter[@name=''additionalResultsReportPrecision'']',
            'analytic', 'additionalResults', 'additionalResultsReportPrecision', 237, 'integer', false,
            'Additional results precision', 'A whole number of decimal places.',
            'Sets the number of decimal places of the numbers in the additional results report. A higher value shows more of each result and makes the file larger. For example, 12 writes a result such as 0.016641350123. ORE reads this parameter first. The NPV analytic has a parameter of the same name that ORE reads only when this one is missing.',
            '6', 'A whole number of zero or more.',
            '1.8.12.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: additionalResults.additionalResultsReportPrecision'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'sensitivity' and name = 'active'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''sensitivity'']/Parameter[@name=''active'']',
            'analytic', 'sensitivity', 'active', 238, 'boolean', true,
            'Run the sensitivity analytic', 'A flag. Y runs the analytic.',
            'Switches the sensitivity analytic on or off. Use N to keep the entry in the document without running it.',
            'No. The analytic does not run.', 'A flag, written Y or N, or true or false.',
            '1.8.2.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: sensitivity.active'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'sensitivity' and name = 'marketConfigFile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''sensitivity'']/Parameter[@name=''marketConfigFile'']',
            'analytic', 'sensitivity', 'marketConfigFile', 239, 'string', false,
            'Simulation market file', 'A file name.',
            'Names the XML file with the simulation market under which the analytic computes its results. The analytic needs only the part under Market. For example, simulation.xml.',
            'None. The analytic needs this file.', 'A file name. ORE looks for the file in the input path of the Setup block.',
            '1.8.2.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: sensitivity.marketConfigFile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'sensitivity' and name = 'sensitivityConfigFile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''sensitivity'']/Parameter[@name=''sensitivityConfigFile'']',
            'analytic', 'sensitivity', 'sensitivityConfigFile', 240, 'string', false,
            'Sensitivity configuration file', 'A file name.',
            'Names the XML file that sets the sensitivity calculation: which risk factors ORE bumps, the shift types and sizes, and the par conversion. For example, sensitivity.xml.',
            'None. The analytic needs this file.', 'A file name. ORE looks for the file in the input path of the Setup block.',
            '1.8.2.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: sensitivity.sensitivityConfigFile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'sensitivity' and name = 'pricingEnginesFile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''sensitivity'']/Parameter[@name=''pricingEnginesFile'']',
            'analytic', 'sensitivity', 'pricingEnginesFile', 241, 'string', false,
            'Pricing engines file', 'A file name.',
            'Names the XML file that sets the pricing engines for the analytic. When it is missing, ORE uses the pricing engines file of the Setup block. For example, pricingengine.xml.',
            'The pricing engines file of the Setup block.', 'A file name. ORE looks for the file in the input path of the Setup block.',
            '1.8.2.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: sensitivity.pricingEnginesFile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'sensitivity' and name = 'scenarioOutputFile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''sensitivity'']/Parameter[@name=''scenarioOutputFile'']',
            'analytic', 'sensitivity', 'scenarioOutputFile', 242, 'string', false,
            'Scenario output file', 'A file name.',
            'Names the file with the result of each scenario: the NPV of the base scenario, the NPV of the shifted scenario and their difference.',
            'sensitivity_scenario.csv', 'A file name, written without a directory. ORE writes the file in the output path of the Setup block.',
            '1.8.2.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: sensitivity.scenarioOutputFile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'sensitivity' and name = 'sensitivityOutputFile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''sensitivity'']/Parameter[@name=''sensitivityOutputFile'']',
            'analytic', 'sensitivity', 'sensitivityOutputFile', 243, 'string', false,
            'Sensitivity output file', 'A file name.',
            'Names the file with the sensitivities: the base NPV, the shift size with its risk factor, and the first and the pure second order finite differences. It also holds a second set with the mixed second order finite differences of the cross gamma.',
            'sensitivity.csv', 'A file name, written without a directory. ORE writes the file in the output path of the Setup block.',
            '1.8.2.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: sensitivity.sensitivityOutputFile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'sensitivity' and name = 'crossGammaOutputFile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''sensitivity'']/Parameter[@name=''crossGammaOutputFile'']',
            'analytic', 'sensitivity', 'crossGammaOutputFile', 244, 'string', false,
            'Cross gamma output file', 'A file name. ORE no longer reads it.',
            'Named the file for the cross gamma. ORE stopped reading it in 1.8.4.0. The cross gamma is now part of the sensitivity output file.',
            'None. ORE ignores the parameter, so a value has no effect.', 'A file name, written without a directory. ORE writes the file in the output path of the Setup block.',
            '1.8.2.0', '1.8.4.0',
            current_user, 'system.initial_load', 'Seed analytic parameter: sensitivity.crossGammaOutputFile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'sensitivity' and name = 'outputSensitivityThreshold'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''sensitivity'']/Parameter[@name=''outputSensitivityThreshold'']',
            'analytic', 'sensitivity', 'outputSensitivityThreshold', 245, 'double', false,
            'Sensitivity output threshold', 'A number.',
            'Sets the smallest absolute value that the analytic writes. It drops every finite difference below the number from the output files. For example, 0.000001.',
            '0.000001', 'A number, written with a decimal point.',
            '1.8.2.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: sensitivity.outputSensitivityThreshold'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'sensitivity' and name = 'recalibrateModels'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''sensitivity'']/Parameter[@name=''recalibrateModels'']',
            'analytic', 'sensitivity', 'recalibrateModels', 246, 'boolean', false,
            'Recalibrate the models', 'A flag. Y recalibrates after each shift.',
            'Decides whether ORE recalibrates the pricing models after it shifts a relevant term structure. Recalibration gives sensitivities that include the model reaction, and it takes more time. When it is N, the models keep their calibration of the base scenario.',
            'Yes.', 'A flag, written Y or N, or true or false.',
            '1.8.2.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: sensitivity.recalibrateModels'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'sensitivity' and name = 'applySimmExemptions'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''sensitivity'']/Parameter[@name=''applySimmExemptions'']',
            'analytic', 'sensitivity', 'applySimmExemptions', 247, 'boolean', false,
            'Apply SIMM exemptions', 'A flag. ORE ignores it in the sensitivity block.',
            'ORE reads this parameter in the crif analytic only. A value in the sensitivity block has no effect.',
            'No. ORE ignores the parameter, so a value has no effect.', 'A flag, written Y or N, or true or false.',
            '1.8.4.1', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: sensitivity.applySimmExemptions'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'sensitivity' and name = 'outputProductClassMapping'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''sensitivity'']/Parameter[@name=''outputProductClassMapping'']',
            'analytic', 'sensitivity', 'outputProductClassMapping', 248, 'boolean', false,
            'Output the product class mapping', 'A flag. ORE ignores it.',
            'ORE does not read this parameter in any release. Example files carry it.',
            'No. ORE ignores the parameter, so a value has no effect.', 'A flag, written Y or N, or true or false.',
            '1.8.4.1', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: sensitivity.outputProductClassMapping'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'sensitivity' and name = 'productClassMappingOutputFile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''sensitivity'']/Parameter[@name=''productClassMappingOutputFile'']',
            'analytic', 'sensitivity', 'productClassMappingOutputFile', 249, 'string', false,
            'Product class mapping output file', 'A file name. ORE ignores it.',
            'ORE does not read this parameter in any release. Example files carry it.',
            'None. ORE ignores the parameter, so a value has no effect.', 'A file name, written without a directory. ORE writes the file in the output path of the Setup block.',
            '1.8.4.1', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: sensitivity.productClassMappingOutputFile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'sensitivity' and name = 'parSensitivity'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''sensitivity'']/Parameter[@name=''parSensitivity'']',
            'analytic', 'sensitivity', 'parSensitivity', 250, 'boolean', false,
            'Par sensitivities', 'A flag. Y also computes par sensitivities.',
            'Decides whether ORE converts the raw sensitivities into sensitivities to par rates after the raw sensitivity analysis. The sensitivity configuration file must have par conversion sections.',
            'No.', 'A flag, written Y or N, or true or false.',
            '1.8.9.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: sensitivity.parSensitivity'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'sensitivity' and name = 'parSensitivityOutputFile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''sensitivity'']/Parameter[@name=''parSensitivityOutputFile'']',
            'analytic', 'sensitivity', 'parSensitivityOutputFile', 251, 'string', false,
            'Par sensitivity output file', 'A file name.',
            'Names the file with the par sensitivities. It has an effect only when Par sensitivities is on.',
            'par_sensitivity.csv', 'A file name, written without a directory. ORE writes the file in the output path of the Setup block.',
            '1.8.9.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: sensitivity.parSensitivityOutputFile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'sensitivity' and name = 'outputJacobi'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''sensitivity'']/Parameter[@name=''outputJacobi'']',
            'analytic', 'sensitivity', 'outputJacobi', 252, 'boolean', false,
            'Write the Jacobi matrices', 'A flag. Y writes the matrices.',
            'Decides whether ORE writes the Jacobi matrix and its inverse, which convert between raw and par sensitivities, to files.',
            'No.', 'A flag, written Y or N, or true or false.',
            '1.8.9.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: sensitivity.outputJacobi'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'sensitivity' and name = 'jacobiOutputFile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''sensitivity'']/Parameter[@name=''jacobiOutputFile'']',
            'analytic', 'sensitivity', 'jacobiOutputFile', 253, 'string', false,
            'Jacobi matrix file', 'A file name.',
            'Names the file for the Jacobi matrix. It has an effect only when Write the Jacobi matrices is on.',
            'jacobi.csv', 'A file name, written without a directory. ORE writes the file in the output path of the Setup block.',
            '1.8.9.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: sensitivity.jacobiOutputFile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'sensitivity' and name = 'jacobiInverseOutputFile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''sensitivity'']/Parameter[@name=''jacobiInverseOutputFile'']',
            'analytic', 'sensitivity', 'jacobiInverseOutputFile', 254, 'string', false,
            'Inverse Jacobi matrix file', 'A file name.',
            'Names the file for the inverse Jacobi matrix. It has an effect only when Write the Jacobi matrices is on.',
            'jacobi_inverse.csv', 'A file name, written without a directory. ORE writes the file in the output path of the Setup block.',
            '1.8.9.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: sensitivity.jacobiInverseOutputFile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'sensitivity' and name = 'parRateSensitivityOutputFile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''sensitivity'']/Parameter[@name=''parRateSensitivityOutputFile'']',
            'analytic', 'sensitivity', 'parRateSensitivityOutputFile', 255, 'string', false,
            'Par rate sensitivity output file', 'A file name. ORE ignores it.',
            'ORE does not read this parameter in any release. Example files carry it. Use the par sensitivity output file.',
            'None. ORE ignores the parameter, so a value has no effect.', 'A file name, written without a directory. ORE writes the file in the output path of the Setup block.',
            '1.8.2.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: sensitivity.parRateSensitivityOutputFile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'sensitivity' and name = 'optimiseRiskFactors'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''sensitivity'']/Parameter[@name=''optimiseRiskFactors'']',
            'analytic', 'sensitivity', 'optimiseRiskFactors', 256, 'boolean', false,
            'Optimise the risk factors', 'A flag. Y limits the par sensitivities to the risk factors that matter.',
            'Decides whether the par sensitivity calculation uses only the risk factors that the raw sensitivity analysis found relevant for the portfolio. It makes the par conversion faster and smaller. It has an effect only when Par sensitivities is on.',
            'No.', 'A flag, written Y or N, or true or false.',
            '1.8.12.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: sensitivity.optimiseRiskFactors'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'sensitivity' and name = 'laxFxConversion'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''sensitivity'']/Parameter[@name=''laxFxConversion'']',
            'analytic', 'sensitivity', 'laxFxConversion', 257, 'boolean', false,
            'Lax FX conversion', 'A flag. Y uses the spot quote of the currency pair directly.',
            'Decides how ORE finds the FX rate that converts a sensitivity into the base currency. With Y, ORE uses the spot quote of the pair directly. With N, ORE uses the stricter rate lookup. Switch it on if the strict lookup fails for a currency pair that the market does not quote.',
            'No.', 'A flag, written Y or N, or true or false.',
            '1.8.13.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: sensitivity.laxFxConversion'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'sensitivity' and name = 'decomposeIndexSensitivities'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''sensitivity'']/Parameter[@name=''decomposeIndexSensitivities'']',
            'analytic', 'sensitivity', 'decomposeIndexSensitivities', 258, 'boolean', false,
            'Decompose index sensitivities', 'A flag. Y splits index sensitivities into their constituents.',
            'Decides whether ORE decomposes the sensitivities of credit, equity and commodity indices into the sensitivities of the constituents. Credit index decomposition also needs SensitivityDecomposition in the pricing engine settings. Equity and commodity decomposition need index_decomposition in the additional fields of the trade, and reference data for the indices.',
            'No.', 'A flag, written Y or N, or true or false.',
            '1.8.14.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: sensitivity.decomposeIndexSensitivities'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'sensitivity' and name = 'alignPillars'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''sensitivity'']/Parameter[@name=''alignPillars'']',
            'analytic', 'sensitivity', 'alignPillars', 259, 'boolean', false,
            'Align the pillars', 'A flag. Y aligns the pillars with the par instruments.',
            'Decides whether ORE aligns the pillars of the simulation market with those of the par instruments, so that the raw and the par sensitivities refer to the same points.',
            'The value of Par sensitivities. Pillars are aligned when par sensitivities are on.', 'A flag, written Y or N, or true or false.',
            '1.8.9.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: sensitivity.alignPillars'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'sensitivity' and name = 'computeTheta'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''sensitivity'']/Parameter[@name=''computeTheta'']',
            'analytic', 'sensitivity', 'computeTheta', 260, 'boolean', false,
            'Compute theta', 'A flag. Y computes the theta.',
            'Decides whether the sensitivity calculation also computes the theta, the change of the NPV when the valuation date moves. ORE reads the parameter from the sensitivity block, the simm block or the crif block, in this order, and one value serves all three analytics.',
            'No.', 'A flag, written Y or N, or true or false.',
            '1.8.16.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: sensitivity.computeTheta'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'sensitivity' and name = 'thetaPeriod'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''sensitivity'']/Parameter[@name=''thetaPeriod'']',
            'analytic', 'sensitivity', 'thetaPeriod', 261, 'string', false,
            'Theta period', 'A period, for example 1D.',
            'Sets the length of the time step for the theta. For example, 1D moves the valuation date by one day. ORE reads it only when Compute theta is on.',
            '1D', 'A period such as 1D or 1W.',
            '1.8.16.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: sensitivity.thetaPeriod'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'sensitivity' and name = 'outputPrecision'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''sensitivity'']/Parameter[@name=''outputPrecision'']',
            'analytic', 'sensitivity', 'outputPrecision', 262, 'integer', false,
            'Sensitivity output precision', 'A whole number of decimal places.',
            'Sets the number of decimal places of the sensitivities in the output files. A higher number shows more of each value and makes the files larger.',
            '2', 'A whole number of zero or more.',
            '1.8.6.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: sensitivity.outputPrecision'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'calibration' and name = 'active'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''calibration'']/Parameter[@name=''active'']',
            'analytic', 'calibration', 'active', 263, 'boolean', true,
            'Run the calibration', 'A flag. Y runs the calibration.',
            'Switches the model calibration on or off. When it is on, the run calibrates the simulation model and writes the calibrated model data, so that a later simulation can start from it without a new calibration. The calibration runs a part of what the simulation does.',
            'No. The analytic does not run.', 'A flag, written Y or N, or true or false.',
            '1.8.13.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: calibration.active'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'calibration' and name = 'configFile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''calibration'']/Parameter[@name=''configFile'']',
            'analytic', 'calibration', 'configFile', 264, 'string', false,
            'Model configuration file', 'A file name.',
            'Names the XML file with the cross asset model data to calibrate: the currencies, the model parametrisations and the calibration instruments. This is usually the simulation configuration file. The output of the run replaces the initial values in this data with the calibrated values. For example, simulation.xml. ORE uses the file only for the CAM model.',
            'None. A CAM calibration needs this file, or the parameter Cross asset model file.', 'A file name. ORE looks for the file in the input path of the Setup block.',
            '1.8.13.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: calibration.configFile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'calibration' and name = 'outputFile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''calibration'']/Parameter[@name=''outputFile'']',
            'analytic', 'calibration', 'outputFile', 265, 'string', false,
            'Calibration report file', 'A file name.',
            'Names the file of the calibration report. The report holds the calibrated cross asset model as one XML string. The run also writes the model data to calibration.xml in the output path, and you can paste its model section into the simulation configuration file to run without a calibration. For example, calibration.csv.',
            'calibration.csv', 'A file name, written without a directory. ORE writes the file in the output path of the Setup block.',
            '1.8.13.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: calibration.outputFile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'calibration' and name = 'model'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''calibration'']/Parameter[@name=''model'']',
            'analytic', 'calibration', 'model', 266, 'string', false,
            'Calibration model', 'CAM or HW.',
            'Selects the model to calibrate. CAM is the cross asset model of the simulation, calibrated to market instruments. HW is the Hull-White model, calibrated to historical data. The HW parameters of this analytic have an effect only for HW.',
            'CAM', 'CAM or HW.',
            '1.8.14.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: calibration.model'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'calibration' and name = 'mode'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''calibration'']/Parameter[@name=''mode'']',
            'analytic', 'calibration', 'mode', 267, 'string', false,
            'HW calibration mode', 'historical',
            'Selects how ORE calibrates the Hull-White model. Only historical is supported: ORE fits the model to historical discount factors and rates. The mode riskNeutral is a stub that does nothing in release 1.8.17.0. ORE uses it only when the model is HW and the mode is historical.',
            'None. With the HW model the parameter is needed.', 'historical',
            '1.8.15.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: calibration.mode'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'calibration' and name = 'foreignCurrencies'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''calibration'']/Parameter[@name=''foreignCurrencies'']',
            'analytic', 'calibration', 'foreignCurrencies', 268, 'string', false,
            'Foreign currencies', 'A comma-separated list of currency codes.',
            'Lists the foreign currencies that the calibration covers, besides the base currency. For example, EUR,GBP calibrates the model for euro and sterling. ORE uses it only when the model is HW and the mode is historical.',
            'None. The calibration covers the base currency only.', 'A comma-separated list of currency codes.',
            '1.8.15.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: calibration.foreignCurrencies'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'calibration' and name = 'pcaCalibration'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''calibration'']/Parameter[@name=''pcaCalibration'']',
            'analytic', 'calibration', 'pcaCalibration', 269, 'boolean', false,
            'PCA calibration', 'A flag. Y runs the principal component analysis.',
            'Decides whether ORE reads the historical discount factors and runs a principal component analysis (PCA) to find the factors of the interest rate curves. Switch it off to supply the eigenvectors from files, with PCA input files. ORE uses it only when the model is HW and the mode is historical.',
            'No.', 'A flag, written Y or N, or true or false.',
            '1.8.15.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: calibration.pcaCalibration'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'calibration' and name = 'curveTenors'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''calibration'']/Parameter[@name=''curveTenors'']',
            'analytic', 'calibration', 'curveTenors', 270, 'string', false,
            'Curve tenors', 'A comma-separated list of periods.',
            'Lists the tenors of each interest rate curve. They are the points of the historical discount factors, and the points of each eigenvector in the files that give them. For example, 1Y,2Y,3Y,5Y,10Y,15Y,20Y,30Y. ORE uses it only when the model is HW and the mode is historical. The run stops if the list is missing.',
            'None. The parameter is mandatory.', 'A comma-separated list of periods such as 1Y,5Y.',
            '1.8.15.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: calibration.curveTenors'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'calibration' and name = 'useForwardOrZeroRate'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''calibration'']/Parameter[@name=''useForwardOrZeroRate'']',
            'analytic', 'calibration', 'useForwardOrZeroRate', 271, 'string', false,
            'Zero or forward rates', 'zero or forward.',
            'Selects the rates of the calibration. With PCA calibration on, it says whether the covariance matrix comes from changes in zero rates or in forward rates. With PCA calibration off, it says how ORE reads the eigenvectors of the input files for the mean reversion calibration. The run stops if the value is neither zero nor forward. ORE uses it only when the model is HW and the mode is historical.',
            'None. The parameter is needed.', 'zero or forward.',
            '1.8.15.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: calibration.useForwardOrZeroRate'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'calibration' and name = 'pcaInputFileName'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''calibration'']/Parameter[@name=''pcaInputFileName'']',
            'analytic', 'calibration', 'pcaInputFileName', 272, 'string', false,
            'PCA input files', 'A comma-separated list of file names.',
            'Lists the files with the eigenvalues and eigenvectors for the mean reversion calibration, one curve for each file. Use it when PCA calibration is off. For example, pca_USD.csv,pca_EUR.csv,pca_GBP.csv. ORE uses it only for the HW model with historical mode and mean reversion calibration on.',
            'None.', 'A comma-separated list of file names.',
            '1.8.15.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: calibration.pcaInputFileName'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'calibration' and name = 'meanReversionCalibration'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''calibration'']/Parameter[@name=''meanReversionCalibration'']',
            'analytic', 'calibration', 'meanReversionCalibration', 273, 'boolean', false,
            'Mean reversion calibration', 'A flag. Y calibrates the mean reversion.',
            'Decides whether ORE calibrates the mean reversion of the principal components that it keeps. The components come from the PCA step or from the PCA input files. ORE uses it only when the model is HW and the mode is historical.',
            'No.', 'A flag, written Y or N, or true or false.',
            '1.8.15.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: calibration.meanReversionCalibration'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'calibration' and name = 'basisFunctionNumber'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''calibration'']/Parameter[@name=''basisFunctionNumber'']',
            'analytic', 'calibration', 'basisFunctionNumber', 274, 'integer', false,
            'Number of basis functions', 'A whole number.',
            'Sets the number of basis functions in the mean reversion calibration. For example, 2. ORE uses it only for the HW model with historical mode and mean reversion calibration on.',
            '0. A run with mean reversion calibration needs a value.', 'A whole number of zero or more.',
            '1.8.15.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: calibration.basisFunctionNumber'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'calibration' and name = 'kappaUpperBound'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''calibration'']/Parameter[@name=''kappaUpperBound'']',
            'analytic', 'calibration', 'kappaUpperBound', 275, 'double', false,
            'Upper bound of kappa', 'A number.',
            'Sets the upper bound of the mean reversion speed (kappa) in the mean reversion calibration. For example, 5.0. ORE uses it only for the HW model with historical mode and mean reversion calibration on.',
            '0. A run with mean reversion calibration needs a value.', 'A number, written with a decimal point.',
            '1.8.15.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: calibration.kappaUpperBound'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'calibration' and name = 'haltonMaxGuess'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''calibration'']/Parameter[@name=''haltonMaxGuess'']',
            'analytic', 'calibration', 'haltonMaxGuess', 276, 'integer', false,
            'Maximum number of guesses', 'A whole number.',
            'Sets the largest number of starting points that the optimisation of the mean reversion calibration tries. The points come from a Halton sequence. A higher number can find a better fit and takes longer. For example, 500. ORE uses it only for the HW model with historical mode and mean reversion calibration on.',
            '0. A run with mean reversion calibration needs a value.', 'A whole number of zero or more.',
            '1.8.15.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: calibration.haltonMaxGuess'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'calibration' and name = 'meanReversionOutputFileName'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''calibration'']/Parameter[@name=''meanReversionOutputFileName'']',
            'analytic', 'calibration', 'meanReversionOutputFileName', 277, 'string', false,
            'Mean reversion output file', 'A file name.',
            'Names the file that receives the calibrated mean reversion speed (kappa) and the volatility (v) of each curve. ORE writes it in the output path. For example, meanReversion.csv. ORE uses it only for the HW model with historical mode and mean reversion calibration on.',
            'None. ORE does not write the file.', 'A file name, written without a directory. ORE writes the file in the output path of the Setup block.',
            '1.8.15.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: calibration.meanReversionOutputFileName'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'calibration' and name = 'scenarioInputFile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''calibration'']/Parameter[@name=''scenarioInputFile'']',
            'analytic', 'calibration', 'scenarioInputFile', 278, 'string', false,
            'Historical scenario file', 'A file name.',
            'Names the file with the historical discount factors and FX spot rates. The file must be in the scenario.csv format of ORE. ORE uses it only for the HW model with historical mode and PCA calibration on. The run stops if the file is missing.',
            'None. The parameter is mandatory.', 'A file name. ORE looks for the file in the input path of the Setup block.',
            '1.8.15.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: calibration.scenarioInputFile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'calibration' and name = 'startDate'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''calibration'']/Parameter[@name=''startDate'']',
            'analytic', 'calibration', 'startDate', 279, 'date', false,
            'History start date', 'A date.',
            'Sets the first date of the historical data that the calibration uses. For example, 2019-09-29. ORE uses it only for the HW model with historical mode and PCA calibration on. The run stops if the date is missing.',
            'None. The parameter is mandatory.', 'A date, written as YYYY-MM-DD or YYYYMMDD.',
            '1.8.13.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: calibration.startDate'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'calibration' and name = 'endDate'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''calibration'']/Parameter[@name=''endDate'']',
            'analytic', 'calibration', 'endDate', 280, 'date', false,
            'History end date', 'A date.',
            'Sets the last date of the historical data that the calibration uses. For example, 2022-09-27. ORE uses it only for the HW model with historical mode and PCA calibration on. The run stops if the date is missing.',
            'None. The parameter is mandatory.', 'A date, written as YYYY-MM-DD or YYYYMMDD.',
            '1.8.15.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: calibration.endDate'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'calibration' and name = 'lambda'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''calibration'']/Parameter[@name=''lambda'']',
            'analytic', 'calibration', 'lambda', 281, 'double', false,
            'Decay factor', 'A number between 0 and 1.',
            'Sets the weight decay of the exponentially weighted differences in the historical rates, when ORE computes the covariance matrix. A value of 1 gives all dates the same weight. A smaller value puts more weight on recent dates. ORE uses it only for the HW model with historical mode and PCA calibration on.',
            '1. All dates have the same weight.', 'A number, written with a decimal point.',
            '1.8.15.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: calibration.lambda'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'calibration' and name = 'varianceRetained'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''calibration'']/Parameter[@name=''varianceRetained'']',
            'analytic', 'calibration', 'varianceRetained', 282, 'double', false,
            'Variance retained', 'A number above 0 and up to 1.',
            'Sets the share of the total variance that the retained principal components must explain. It decides how many principal components ORE keeps. For example, 0.90 keeps as many components as needed to explain 90 percent of the variance. ORE uses it only for the HW model with historical mode and PCA calibration on. The run stops if the value is missing, zero or above 1.',
            'None. The parameter is mandatory.', 'A number, written with a decimal point.',
            '1.8.15.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: calibration.varianceRetained'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'calibration' and name = 'pcaOutputFileName'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''calibration'']/Parameter[@name=''pcaOutputFileName'']',
            'analytic', 'calibration', 'pcaOutputFileName', 283, 'string', false,
            'PCA output file', 'A file name.',
            'Names the file that receives the eigenvalues and eigenvectors that ORE retains for each currency. ORE writes it in the output path. For example, pca.csv. ORE uses it only for the HW model with historical mode and PCA calibration on.',
            'None. ORE does not write the file.', 'A file name, written without a directory. ORE writes the file in the output path of the Setup block.',
            '1.8.15.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: calibration.pcaOutputFileName'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'calibration' and name = 'pricingEnginesFile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''calibration'']/Parameter[@name=''pricingEnginesFile'']',
            'analytic', 'calibration', 'pricingEnginesFile', 284, 'string', false,
            'Pricing engines file for the calibration', 'A file name.',
            'Names the XML file that sets the pricing engines for the calibration instruments, for example the swaptions that the model is calibrated to. For example, pricingengine.xml.',
            'The pricing engines file of the Setup block.', 'A file name. ORE looks for the file in the input path of the Setup block.',
            '1.8.13.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: calibration.pricingEnginesFile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'calibration' and name = 'crossAssetModelData'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''calibration'']/Parameter[@name=''crossAssetModelData'']',
            'analytic', 'calibration', 'crossAssetModelData', 285, 'string', false,
            'Cross asset model file', 'A file name.',
            'Names the XML file with the cross asset model data, if it sits in another file than the model configuration file. When it is set, ORE reads the model data from this file and not from the configuration file.',
            'The model configuration file.', 'A file name. ORE looks for the file in the input path of the Setup block.',
            '1.8.16.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: calibration.crossAssetModelData'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'initialMargin' and name = 'active'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''initialMargin'']/Parameter[@name=''active'']',
            'analytic', 'initialMargin', 'active', 286, 'boolean', false,
            null, null,
            null,
            null, null,
            '1.8.0.7 or earlier', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: initialMargin.active'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'initialMargin' and name = 'method'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''initialMargin'']/Parameter[@name=''method'']',
            'analytic', 'initialMargin', 'method', 287, 'string', false,
            null, null,
            null,
            null, null,
            '1.8.0.7 or earlier', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: initialMargin.method'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'zeroToParSensiConversion' and name = 'active'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''zeroToParSensiConversion'']/Parameter[@name=''active'']',
            'analytic', 'zeroToParSensiConversion', 'active', 288, 'boolean', true,
            'Run the par conversion of sensitivities', 'A flag. Y runs the analytic.',
            'Switches the conversion of raw sensitivities into par sensitivities on or off, as a separate step. The raw sensitivities come from a file. The conversion is the same as the embedded par conversion of the sensitivity analytic.',
            'No. The analytic does not run.', 'A flag, written Y or N, or true or false.',
            '1.8.11.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: zeroToParSensiConversion.active'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'zeroToParSensiConversion' and name = 'marketConfigFile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''zeroToParSensiConversion'']/Parameter[@name=''marketConfigFile'']',
            'analytic', 'zeroToParSensiConversion', 'marketConfigFile', 289, 'string', false,
            'Simulation market file', 'A file name.',
            'Names the XML file with the simulation market under which the analytic computes its results. The analytic needs only the part under Market. For example, simulation.xml.',
            'None. The analytic needs this file.', 'A file name. ORE looks for the file in the input path of the Setup block.',
            '1.8.11.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: zeroToParSensiConversion.marketConfigFile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'zeroToParSensiConversion' and name = 'sensitivityConfigFile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''zeroToParSensiConversion'']/Parameter[@name=''sensitivityConfigFile'']',
            'analytic', 'zeroToParSensiConversion', 'sensitivityConfigFile', 290, 'string', false,
            'Sensitivity configuration file', 'A file name.',
            'Names the XML file with the sensitivity setup, in particular the par conversion sections. For example, sensitivity.xml.',
            'None. The analytic needs this file.', 'A file name. ORE looks for the file in the input path of the Setup block.',
            '1.8.11.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: zeroToParSensiConversion.sensitivityConfigFile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'zeroToParSensiConversion' and name = 'pricingEnginesFile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''zeroToParSensiConversion'']/Parameter[@name=''pricingEnginesFile'']',
            'analytic', 'zeroToParSensiConversion', 'pricingEnginesFile', 291, 'string', false,
            'Pricing engines file', 'A file name.',
            'Names the XML file that sets the pricing engines for the analytic. When it is missing, ORE uses the pricing engines file of the Setup block. For example, pricingengine.xml.',
            'The pricing engines file of the Setup block.', 'A file name. ORE looks for the file in the input path of the Setup block.',
            '1.8.11.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: zeroToParSensiConversion.pricingEnginesFile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'zeroToParSensiConversion' and name = 'sensitivityInputFile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''zeroToParSensiConversion'']/Parameter[@name=''sensitivityInputFile'']',
            'analytic', 'zeroToParSensiConversion', 'sensitivityInputFile', 292, 'string', false,
            'Raw sensitivities file', 'A file name.',
            'Names the CSV file with the raw (zero) sensitivities to convert. The file needs six columns, and the column name parameters set their names. For example, sensitivity.csv.',
            'None. The analytic needs this file.', 'A file name. ORE looks for the file in the input path of the Setup block.',
            '1.8.11.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: zeroToParSensiConversion.sensitivityInputFile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'zeroToParSensiConversion' and name = 'outputThreshold'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''zeroToParSensiConversion'']/Parameter[@name=''outputThreshold'']',
            'analytic', 'zeroToParSensiConversion', 'outputThreshold', 293, 'double', false,
            'Output threshold', 'A number.',
            'Sets the smallest absolute value that the analytic writes. It drops every par sensitivity below the number from the output. For example, 0.000001.',
            '0.000001', 'A number, written with a decimal point.',
            '1.8.11.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: zeroToParSensiConversion.outputThreshold'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'zeroToParSensiConversion' and name = 'outputFile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''zeroToParSensiConversion'']/Parameter[@name=''outputFile'']',
            'analytic', 'zeroToParSensiConversion', 'outputFile', 294, 'string', false,
            'Par sensitivities file', 'A file name.',
            'Names the file that receives the par sensitivities. For example, parconversion_sensitivity.csv.',
            'parConversionSensitivity.csv', 'A file name, written without a directory. ORE writes the file in the output path of the Setup block.',
            '1.8.11.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: zeroToParSensiConversion.outputFile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'zeroToParSensiConversion' and name = 'outputJacobi'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''zeroToParSensiConversion'']/Parameter[@name=''outputJacobi'']',
            'analytic', 'zeroToParSensiConversion', 'outputJacobi', 295, 'boolean', false,
            'Write the Jacobi matrices', 'A flag. Y writes the matrices.',
            'Decides whether the analytic writes the Jacobi matrix and its inverse to files.',
            'No.', 'A flag, written Y or N, or true or false.',
            '1.8.11.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: zeroToParSensiConversion.outputJacobi'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'zeroToParSensiConversion' and name = 'jacobiOutputFile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''zeroToParSensiConversion'']/Parameter[@name=''jacobiOutputFile'']',
            'analytic', 'zeroToParSensiConversion', 'jacobiOutputFile', 296, 'string', false,
            'Jacobi matrix file', 'A file name.',
            'Names the file for the Jacobi matrix. It has an effect only when Write the Jacobi matrices is on.',
            'parConversionJacobi.csv', 'A file name, written without a directory. ORE writes the file in the output path of the Setup block.',
            '1.8.11.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: zeroToParSensiConversion.jacobiOutputFile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'zeroToParSensiConversion' and name = 'jacobiInverseOutputFile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''zeroToParSensiConversion'']/Parameter[@name=''jacobiInverseOutputFile'']',
            'analytic', 'zeroToParSensiConversion', 'jacobiInverseOutputFile', 297, 'string', false,
            'Inverse Jacobi matrix file', 'A file name.',
            'Names the file for the inverse Jacobi matrix. It has an effect only when Write the Jacobi matrices is on.',
            'parConversionJacobi_inverse.csv', 'A file name, written without a directory. ORE writes the file in the output path of the Setup block.',
            '1.8.11.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: zeroToParSensiConversion.jacobiInverseOutputFile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'zeroToParSensiConversion' and name = 'idColumn'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''zeroToParSensiConversion'']/Parameter[@name=''idColumn'']',
            'analytic', 'zeroToParSensiConversion', 'idColumn', 298, 'string', false,
            'Id column', 'A column name.',
            'Names the column of the input file with the unique identifier of the trade, netting set or portfolio.',
            'TradeId', 'A column name.',
            '1.8.11.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: zeroToParSensiConversion.idColumn'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'zeroToParSensiConversion' and name = 'riskFactorColumn'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''zeroToParSensiConversion'']/Parameter[@name=''riskFactorColumn'']',
            'analytic', 'zeroToParSensiConversion', 'riskFactorColumn', 299, 'string', false,
            'Risk factor column', 'A column name.',
            'Names the column of the input file with the identifier of the raw sensitivity. The name must follow the ORE naming convention, for example DiscountCurve/EUR/5/1Y.',
            'Factor_1', 'A column name.',
            '1.8.11.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: zeroToParSensiConversion.riskFactorColumn'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'zeroToParSensiConversion' and name = 'deltaColumn'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''zeroToParSensiConversion'']/Parameter[@name=''deltaColumn'']',
            'analytic', 'zeroToParSensiConversion', 'deltaColumn', 300, 'string', false,
            'Sensitivity column', 'A column name.',
            'Names the column of the input file with the raw sensitivity of the trade, netting set or portfolio to the risk factor.',
            'Delta', 'A column name.',
            '1.8.11.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: zeroToParSensiConversion.deltaColumn'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'zeroToParSensiConversion' and name = 'currencyColumn'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''zeroToParSensiConversion'']/Parameter[@name=''currencyColumn'']',
            'analytic', 'zeroToParSensiConversion', 'currencyColumn', 301, 'string', false,
            'Currency column', 'A column name.',
            'Names the column of the input file with the currency of the raw sensitivity. It must equal the base currency of the simulation settings.',
            'Currency', 'A column name.',
            '1.8.11.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: zeroToParSensiConversion.currencyColumn'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'zeroToParSensiConversion' and name = 'baseNpvColumn'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''zeroToParSensiConversion'']/Parameter[@name=''baseNpvColumn'']',
            'analytic', 'zeroToParSensiConversion', 'baseNpvColumn', 302, 'string', false,
            'Base NPV column', 'A column name.',
            'Names the column of the input file with the base NPV of the trade, netting set or portfolio.',
            'Base NPV', 'A column name.',
            '1.8.11.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: zeroToParSensiConversion.baseNpvColumn'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'zeroToParSensiConversion' and name = 'shiftSizeColumn'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''zeroToParSensiConversion'']/Parameter[@name=''shiftSizeColumn'']',
            'analytic', 'zeroToParSensiConversion', 'shiftSizeColumn', 303, 'string', false,
            'Shift size column', 'A column name.',
            'Names the column of the input file with the shift size that ORE applied to compute the raw sensitivity. It must agree with the sensitivity configuration.',
            'ShiftSize_1', 'A column name.',
            '1.8.11.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: zeroToParSensiConversion.shiftSizeColumn'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'saccr' and name = 'active'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''saccr'']/Parameter[@name=''active'']',
            'analytic', 'saccr', 'active', 304, 'boolean', true,
            'Run the SA-CCR analytic', 'A flag. Y runs the analytic.',
            'Switches the standardised approach for the counterparty credit risk capital (SA-CCR) on or off. The calculation needs the netting set definitions and the collateral balances.',
            'No. The analytic does not run.', 'A flag, written Y or N, or true or false.',
            '1.8.13.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: saccr.active'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'saccr' and name = 'csaFile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''saccr'']/Parameter[@name=''csaFile'']',
            'analytic', 'saccr', 'csaFile', 305, 'string', false,
            'Netting set definitions file', 'A file name.',
            'Names the XML file with the netting set definitions, as in the XVA analytics. ORE looks for the file name in the saccr block, then in the bacva block. For example, netting.xml.',
            'None. The analytic needs this file.', 'A file name. ORE looks for the file in the input path of the Setup block.',
            '1.8.13.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: saccr.csaFile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'saccr' and name = 'collateralBalancesFile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''saccr'']/Parameter[@name=''collateralBalancesFile'']',
            'analytic', 'saccr', 'collateralBalancesFile', 306, 'string', false,
            'Collateral balances file', 'A file name.',
            'Names the XML file with the variation margin and initial margin balances of the valuation date. ORE looks for the file name in the saccr block, then in the bacva block. For example, collateralbalances.xml.',
            'None. The analytic needs this file.', 'A file name. ORE looks for the file in the input path of the Setup block.',
            '1.8.13.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: saccr.collateralBalancesFile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'saccr' and name = 'simmVersion'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''saccr'']/Parameter[@name=''simmVersion'']',
            'analytic', 'saccr', 'simmVersion', 307, 'double', false,
            'SIMM version', 'A SIMM version, for example 2.6.',
            'Names the version of the SIMM that the calculation uses for the name and bucket mapping of commodity hedging sets. The run stops if the version is empty.',
            'None. The parameter is mandatory.', 'A SIMM version such as 2.4, 2.5A, 2.6 or 2.7.',
            '1.8.13.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: saccr.simmVersion'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'saccr' and name = 'counterpartyFile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''saccr'']/Parameter[@name=''counterpartyFile'']',
            'analytic', 'saccr', 'counterpartyFile', 308, 'string', false,
            'Counterparty file', 'A file name.',
            'Names the XML file with the counterparty information for the SA-CCR calculation. A value here replaces the file of the Setup block.',
            'The counterparty file of the Setup block.', 'A file name. ORE looks for the file in the input path of the Setup block.',
            '1.8.13.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: saccr.counterpartyFile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'saccr' and name = 'nameMappingInputFile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''saccr'']/Parameter[@name=''nameMappingInputFile'']',
            'analytic', 'saccr', 'nameMappingInputFile', 309, 'string', false,
            'Name mapping file', 'A file name.',
            'Names the file that maps the names in the portfolio to the SIMM names for the commodity hedging sets. Without it ORE uses the names as they are.',
            'None. ORE uses the names as they are.', 'A file name. ORE looks for the file in the input path of the Setup block.',
            '1.8.13.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: saccr.nameMappingInputFile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'saccr' and name = 'bucketMappingInputFile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''saccr'']/Parameter[@name=''bucketMappingInputFile'']',
            'analytic', 'saccr', 'bucketMappingInputFile', 310, 'string', false,
            'Bucket mapping file', 'A file name.',
            'Names the file that maps risk factors to the SIMM buckets for the commodity hedging sets. Without it ORE uses the standard buckets.',
            'None. ORE uses the standard SIMM buckets.', 'A file name. ORE looks for the file in the input path of the Setup block.',
            '1.8.13.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: saccr.bucketMappingInputFile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'smrc' and name = 'active'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''smrc'']/Parameter[@name=''active'']',
            'analytic', 'smrc', 'active', 311, 'boolean', true,
            'Run the SMRC analytic', 'A flag. Y runs the analytic.',
            'Switches the basic standardised market risk capital calculation (SMRC) on or off. It needs no inputs besides the Setup block.',
            'No. The analytic does not run.', 'A flag, written Y or N, or true or false.',
            '1.8.13.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: smrc.active'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'portfolioDetails' and name = 'active'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''portfolioDetails'']/Parameter[@name=''active'']',
            'analytic', 'portfolioDetails', 'active', 312, 'boolean', false,
            null, null,
            null,
            null, null,
            '1.8.13.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: portfolioDetails.active'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'portfolioDetails' and name = 'riskFactorFileName'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''portfolioDetails'']/Parameter[@name=''riskFactorFileName'']',
            'analytic', 'portfolioDetails', 'riskFactorFileName', 313, 'string', false,
            null, null,
            null,
            null, null,
            '1.8.13.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: portfolioDetails.riskFactorFileName'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'portfolioDetails' and name = 'marketObjectFileName'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''portfolioDetails'']/Parameter[@name=''marketObjectFileName'']',
            'analytic', 'portfolioDetails', 'marketObjectFileName', 314, 'string', false,
            null, null,
            null,
            null, null,
            '1.8.13.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: portfolioDetails.marketObjectFileName'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'stress' and name = 'active'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''stress'']/Parameter[@name=''active'']',
            'analytic', 'stress', 'active', 315, 'boolean', true,
            'Run the stress analytic', 'A flag. Y runs the analytic.',
            'Switches the stress analytic on or off. Use N to keep the entry in the document without running it.',
            'No. The analytic does not run.', 'A flag, written Y or N, or true or false.',
            '1.8.2.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: stress.active'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'stress' and name = 'precision'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''stress'']/Parameter[@name=''precision'']',
            'analytic', 'stress', 'precision', 316, 'integer', false,
            'Output precision', 'A whole number of decimal places.',
            'Sets the number of decimal places of the numbers in the stress result file. For example, 6.',
            '2', 'A whole number of zero or more.',
            '1.8.13.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: stress.precision'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'stress' and name = 'marketConfigFile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''stress'']/Parameter[@name=''marketConfigFile'']',
            'analytic', 'stress', 'marketConfigFile', 317, 'string', false,
            'Simulation market file', 'A file name.',
            'Names the XML file with the simulation market under which the analytic computes its results. The analytic needs only the part under Market. For example, simulation.xml. The analytic replaces the market of the valuation date with this simulation market and applies the stress scenarios to it.',
            'None. The analytic needs this file.', 'A file name. ORE looks for the file in the input path of the Setup block.',
            '1.8.2.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: stress.marketConfigFile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'stress' and name = 'stressConfigFile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''stress'']/Parameter[@name=''stressConfigFile'']',
            'analytic', 'stress', 'stressConfigFile', 318, 'string', false,
            'Stress configuration file', 'A file name.',
            'Names the XML file with the stress scenarios. The same file also decides whether the stress is in the raw domain, as shifts of the zero rates, or in the par domain, as shifts of the par rates. For example, stresstest.xml.',
            'None. The analytic needs this file.', 'A file name. ORE looks for the file in the input path of the Setup block.',
            '1.8.2.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: stress.stressConfigFile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'stress' and name = 'pricingEnginesFile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''stress'']/Parameter[@name=''pricingEnginesFile'']',
            'analytic', 'stress', 'pricingEnginesFile', 319, 'string', false,
            'Pricing engines file', 'A file name.',
            'Names the XML file that sets the pricing engines for the analytic. When it is missing, ORE uses the pricing engines file of the Setup block. For example, pricingengine.xml.',
            'The pricing engines file of the Setup block.', 'A file name. ORE looks for the file in the input path of the Setup block.',
            '1.8.2.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: stress.pricingEnginesFile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'stress' and name = 'scenarioOutputFile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''stress'']/Parameter[@name=''scenarioOutputFile'']',
            'analytic', 'stress', 'scenarioOutputFile', 320, 'string', false,
            'Stress result file', 'A file name.',
            'Names the file with the result of each stress scenario: the NPV of the base scenario, the NPV of the stressed scenario and their difference. For example, stresstest.csv.',
            'stress.csv', 'A file name, written without a directory. ORE writes the file in the output path of the Setup block.',
            '1.8.2.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: stress.scenarioOutputFile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'stress' and name = 'outputThreshold'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''stress'']/Parameter[@name=''outputThreshold'']',
            'analytic', 'stress', 'outputThreshold', 321, 'double', false,
            'Output threshold', 'A number.',
            'Sets the smallest absolute NPV change that the analytic writes. It drops every change below the number from the output. For example, 0.000001.',
            '0. The analytic writes every change.', 'A number, written with a decimal point.',
            '1.8.2.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: stress.outputThreshold'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'stress' and name = 'generateCashflows'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''stress'']/Parameter[@name=''generateCashflows'']',
            'analytic', 'stress', 'generateCashflows', 322, 'boolean', false,
            'Generate cash flows', 'A flag. Y writes the cash flows under each scenario.',
            'Decides whether the analytic also writes the cash flows of the trades under each stress scenario, to the cash flow output file.',
            'No.', 'A flag, written Y or N, or true or false.',
            '1.8.13.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: stress.generateCashflows'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'stress' and name = 'scenarioCashflowOutputFile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''stress'']/Parameter[@name=''scenarioCashflowOutputFile'']',
            'analytic', 'stress', 'scenarioCashflowOutputFile', 323, 'string', false,
            'Stress cash flow file', 'A file name.',
            'Names the file with the cash flows under each stress scenario. It has an effect only when Generate cash flows is on. For example, stresstest_flows.csv.',
            'stress_cashflows.csv', 'A file name, written without a directory. ORE writes the file in the output path of the Setup block.',
            '1.8.13.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: stress.scenarioCashflowOutputFile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'stress' and name = 'sensitivityConfigFile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''stress'']/Parameter[@name=''sensitivityConfigFile'']',
            'analytic', 'stress', 'sensitivityConfigFile', 324, 'string', false,
            'Sensitivity configuration file', 'A file name.',
            'Names the XML file with the sensitivity setup, which a par stress test needs for the conversion of the par shifts. Without it ORE supports no par stress test.',
            'None. Par stress tests are not supported.', 'A file name. ORE looks for the file in the input path of the Setup block.',
            '1.8.2.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: stress.sensitivityConfigFile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'stress' and name = 'stressZeroScenarioDataFile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''stress'']/Parameter[@name=''stressZeroScenarioDataFile'']',
            'analytic', 'stress', 'stressZeroScenarioDataFile', 325, 'string', false,
            'Raw domain stress file', 'A file name.',
            'Names the file that receives the stress test definition in the raw domain, when the stress test file is in the par domain. The converted file can be used again without a new conversion. For example, zeroStressScenarioData.xml.',
            'stress_ZeroStressData.xml. The analytic writes the file only for a stress test in the par domain.', 'A file name, written without a directory. ORE writes the file in the output path of the Setup block.',
            '1.8.12.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: stress.stressZeroScenarioDataFile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'stress' and name = 'scenarioFile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''stress'']/Parameter[@name=''scenarioFile'']',
            'analytic', 'stress', 'scenarioFile', 326, 'string', false,
            'Scenario input file', 'A file name.',
            'Names a file with market scenarios that ORE reads as stress scenarios. For example, StressScenarios/stress_scenarios.csv.',
            'None.', 'A file name. ORE looks for the file in the input path of the Setup block.',
            '1.8.2.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: stress.scenarioFile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'stress' and name = 'optimiseRiskFactors'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''stress'']/Parameter[@name=''optimiseRiskFactors'']',
            'analytic', 'stress', 'optimiseRiskFactors', 327, 'boolean', false,
            'Optimise the risk factors', 'A flag. Y limits the par conversion to the risk factors that matter.',
            'Decides whether the par stress conversion limits itself to the risk factors that the stress scenarios shift. It makes the conversion faster.',
            'No.', 'A flag, written Y or N, or true or false.',
            '1.8.12.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: stress.optimiseRiskFactors'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'stress' and name = 'lowerBoundCapVols'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''stress'']/Parameter[@name=''lowerBoundCapVols'']',
            'analytic', 'stress', 'lowerBoundCapVols', 328, 'double', false,
            'Lower bound of cap volatilities', 'A number.',
            'Is meant to set the lowest cap and floor volatility that the par to raw conversion tries. The conversion uses the bound 1e-8. ORE reads the value for the stress analytic. The conversion of release 1.8.17.0 uses its own fixed bounds and does not take the value.',
            '1e-8', 'A number, written with a decimal point.',
            '1.8.12.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: stress.lowerBoundCapVols'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'stress' and name = 'upperBoundCapVols'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''stress'']/Parameter[@name=''upperBoundCapVols'']',
            'analytic', 'stress', 'upperBoundCapVols', 329, 'double', false,
            'Upper bound of cap volatilities', 'A number.',
            'Is meant to set the highest cap and floor volatility that the par to raw conversion tries, as a multiple of the base volatility. The conversion uses the bound 10. ORE reads the value for the stress analytic. The conversion of release 1.8.17.0 uses its own fixed bounds and does not take the value.',
            '10', 'A number, written with a decimal point.',
            '1.8.12.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: stress.upperBoundCapVols'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'stress' and name = 'lowerBoundDiscountFactors'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''stress'']/Parameter[@name=''lowerBoundDiscountFactors'']',
            'analytic', 'stress', 'lowerBoundDiscountFactors', 330, 'double', false,
            'Lower bound of discount factors', 'A number.',
            'Is meant to set the lowest discount factor that the par to raw conversion tries. The conversion uses the bound 1e-8. ORE reads the value for the stress analytic. The conversion of release 1.8.17.0 uses its own fixed bounds and does not take the value.',
            '1e-8', 'A number, written with a decimal point.',
            '1.8.12.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: stress.lowerBoundDiscountFactors'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'stress' and name = 'upperBoundDiscountFactors'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''stress'']/Parameter[@name=''upperBoundDiscountFactors'']',
            'analytic', 'stress', 'upperBoundDiscountFactors', 331, 'double', false,
            'Upper bound of discount factors', 'A number.',
            'Is meant to set the highest discount factor that the par to raw conversion tries. The conversion uses the bound 10. ORE reads the value for the stress analytic. The conversion of release 1.8.17.0 uses its own fixed bounds and does not take the value.',
            '10', 'A number, written with a decimal point.',
            '1.8.12.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: stress.upperBoundDiscountFactors'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'stress' and name = 'lowerBoundSurvivalProb'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''stress'']/Parameter[@name=''lowerBoundSurvivalProb'']',
            'analytic', 'stress', 'lowerBoundSurvivalProb', 332, 'double', false,
            'Lower bound of survival probabilities', 'A number.',
            'Is meant to set the lowest survival probability that the par to raw conversion tries. The conversion treats survival probabilities like discount factors and uses the bound 1e-8. ORE reads the value for the stress analytic. The conversion of release 1.8.17.0 uses its own fixed bounds and does not take the value.',
            '1e-8', 'A number, written with a decimal point.',
            '1.8.12.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: stress.lowerBoundSurvivalProb'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'stress' and name = 'upperBoundSurvivalProb'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''stress'']/Parameter[@name=''upperBoundSurvivalProb'']',
            'analytic', 'stress', 'upperBoundSurvivalProb', 333, 'double', false,
            'Upper bound of survival probabilities', 'A number.',
            'Is meant to set the highest survival probability that the par to raw conversion tries. The conversion treats survival probabilities like discount factors and uses the bound 10. ORE reads the value for the stress analytic. The conversion of release 1.8.17.0 uses its own fixed bounds and does not take the value.',
            '10', 'A number, written with a decimal point.',
            '1.8.12.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: stress.upperBoundSurvivalProb'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'stress' and name = 'accuracy'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''stress'']/Parameter[@name=''accuracy'']',
            'analytic', 'stress', 'accuracy', 334, 'double', false,
            'Accuracy of the conversion', 'A number.',
            'Is meant to set the accuracy of the root search that turns a par shift into a raw shift. The conversion uses the accuracy 1e-8. ORE reads the value for the stress analytic. The conversion of release 1.8.17.0 uses its own fixed bounds and does not take the value.',
            '1e-8', 'A number, written with a decimal point.',
            '1.8.12.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: stress.accuracy'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'parametricVar' and name = 'active'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''parametricVar'']/Parameter[@name=''active'']',
            'analytic', 'parametricVar', 'active', 335, 'boolean', false,
            null, null,
            null,
            null, null,
            '1.8.3.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: parametricVar.active'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'parametricVar' and name = 'sensitivityInputFile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''parametricVar'']/Parameter[@name=''sensitivityInputFile'']',
            'analytic', 'parametricVar', 'sensitivityInputFile', 336, 'string', false,
            null, null,
            null,
            null, null,
            '1.8.3.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: parametricVar.sensitivityInputFile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'parametricVar' and name = 'covarianceInputFile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''parametricVar'']/Parameter[@name=''covarianceInputFile'']',
            'analytic', 'parametricVar', 'covarianceInputFile', 337, 'string', false,
            null, null,
            null,
            null, null,
            '1.8.3.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: parametricVar.covarianceInputFile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'parametricVar' and name = 'quantiles'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''parametricVar'']/Parameter[@name=''quantiles'']',
            'analytic', 'parametricVar', 'quantiles', 338, 'string', false,
            null, null,
            null,
            null, null,
            '1.8.3.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: parametricVar.quantiles'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'parametricVar' and name = 'breakdown'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''parametricVar'']/Parameter[@name=''breakdown'']',
            'analytic', 'parametricVar', 'breakdown', 339, 'boolean', false,
            null, null,
            null,
            null, null,
            '1.8.3.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: parametricVar.breakdown'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'parametricVar' and name = 'portfolioFilter'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''parametricVar'']/Parameter[@name=''portfolioFilter'']',
            'analytic', 'parametricVar', 'portfolioFilter', 340, 'string', false,
            null, null,
            null,
            null, null,
            '1.8.3.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: parametricVar.portfolioFilter'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'parametricVar' and name = 'method'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''parametricVar'']/Parameter[@name=''method'']',
            'analytic', 'parametricVar', 'method', 341, 'string', false,
            null, null,
            null,
            null, null,
            '1.8.3.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: parametricVar.method'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'parametricVar' and name = 'outputFile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''parametricVar'']/Parameter[@name=''outputFile'']',
            'analytic', 'parametricVar', 'outputFile', 342, 'string', false,
            null, null,
            null,
            null, null,
            '1.8.3.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: parametricVar.outputFile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'simm' and name = 'active'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''simm'']/Parameter[@name=''active'']',
            'analytic', 'simm', 'active', 343, 'boolean', false,
            null, null,
            null,
            null, null,
            '1.8.10.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: simm.active'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'simm' and name = 'version'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''simm'']/Parameter[@name=''version'']',
            'analytic', 'simm', 'version', 344, 'string', false,
            null, null,
            null,
            null, null,
            '1.8.10.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: simm.version'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'simm' and name = 'crif'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''simm'']/Parameter[@name=''crif'']',
            'analytic', 'simm', 'crif', 345, 'string', false,
            null, null,
            null,
            null, null,
            '1.8.10.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: simm.crif'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'simm' and name = 'calculationCurrency'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''simm'']/Parameter[@name=''calculationCurrency'']',
            'analytic', 'simm', 'calculationCurrency', 346, 'string', false,
            null, null,
            null,
            null, null,
            '1.8.10.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: simm.calculationCurrency'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'simm' and name = 'enforceIMRegulations'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''simm'']/Parameter[@name=''enforceIMRegulations'']',
            'analytic', 'simm', 'enforceIMRegulations', 347, 'boolean', false,
            null, null,
            null,
            null, null,
            '1.8.11.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: simm.enforceIMRegulations'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'simm' and name = 'mporDays'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''simm'']/Parameter[@name=''mporDays'']',
            'analytic', 'simm', 'mporDays', 348, 'integer', false,
            null, null,
            null,
            null, null,
            '1.8.11.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: simm.mporDays'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'simm' and name = 'simmCalibration'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''simm'']/Parameter[@name=''simmCalibration'']',
            'analytic', 'simm', 'simmCalibration', 349, 'string', false,
            null, null,
            null,
            null, null,
            '1.8.12.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: simm.simmCalibration'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'simm' and name = 'writeIntermediateReports'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''simm'']/Parameter[@name=''writeIntermediateReports'']',
            'analytic', 'simm', 'writeIntermediateReports', 350, 'boolean', false,
            null, null,
            null,
            null, null,
            '1.8.12.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: simm.writeIntermediateReports'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'imschedule' and name = 'active'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''imschedule'']/Parameter[@name=''active'']',
            'analytic', 'imschedule', 'active', 351, 'boolean', false,
            null, null,
            null,
            null, null,
            '1.8.12.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: imschedule.active'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'imschedule' and name = 'crif'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''imschedule'']/Parameter[@name=''crif'']',
            'analytic', 'imschedule', 'crif', 352, 'string', false,
            null, null,
            null,
            null, null,
            '1.8.12.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: imschedule.crif'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'imschedule' and name = 'calculationCurrency'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''imschedule'']/Parameter[@name=''calculationCurrency'']',
            'analytic', 'imschedule', 'calculationCurrency', 353, 'string', false,
            null, null,
            null,
            null, null,
            '1.8.12.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: imschedule.calculationCurrency'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'imschedule' and name = 'version'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''imschedule'']/Parameter[@name=''version'']',
            'analytic', 'imschedule', 'version', 354, 'double', false,
            null, null,
            null,
            null, null,
            '1.8.12.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: imschedule.version'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'creditcurves' and name = 'active'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''creditcurves'']/Parameter[@name=''active'']',
            'analytic', 'creditcurves', 'active', 355, 'boolean', false,
            null, null,
            null,
            null, null,
            '1.8.10.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: creditcurves.active'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'creditcurves' and name = 'configuration'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''creditcurves'']/Parameter[@name=''configuration'']',
            'analytic', 'creditcurves', 'configuration', 356, 'string', false,
            null, null,
            null,
            null, null,
            '1.8.10.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: creditcurves.configuration'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'creditcurves' and name = 'grid'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''creditcurves'']/Parameter[@name=''grid'']',
            'analytic', 'creditcurves', 'grid', 357, 'string', false,
            null, null,
            null,
            null, null,
            '1.8.10.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: creditcurves.grid'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'creditcurves' and name = 'outputFileName'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''creditcurves'']/Parameter[@name=''outputFileName'']',
            'analytic', 'creditcurves', 'outputFileName', 358, 'string', false,
            null, null,
            null,
            null, null,
            '1.8.10.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: creditcurves.outputFileName'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'scenario' and name = 'active'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''scenario'']/Parameter[@name=''active'']',
            'analytic', 'scenario', 'active', 359, 'boolean', false,
            null, null,
            null,
            null, null,
            '1.8.9.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: scenario.active'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'scenario' and name = 'simulationConfigFile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''scenario'']/Parameter[@name=''simulationConfigFile'']',
            'analytic', 'scenario', 'simulationConfigFile', 360, 'string', false,
            null, null,
            null,
            null, null,
            '1.8.9.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: scenario.simulationConfigFile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'scenario' and name = 'scenarioOutputFile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''scenario'']/Parameter[@name=''scenarioOutputFile'']',
            'analytic', 'scenario', 'scenarioOutputFile', 361, 'string', false,
            null, null,
            null,
            null, null,
            '1.8.9.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: scenario.scenarioOutputFile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'pnlExplain' and name = 'active'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''pnlExplain'']/Parameter[@name=''active'']',
            'analytic', 'pnlExplain', 'active', 362, 'boolean', false,
            null, null,
            null,
            null, null,
            '1.8.12.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: pnlExplain.active'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'pnlExplain' and name = 'mporDate'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''pnlExplain'']/Parameter[@name=''mporDate'']',
            'analytic', 'pnlExplain', 'mporDate', 363, 'date', false,
            null, null,
            null,
            null, null,
            '1.8.12.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: pnlExplain.mporDate'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'pnlExplain' and name = 'simulationConfigFile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''pnlExplain'']/Parameter[@name=''simulationConfigFile'']',
            'analytic', 'pnlExplain', 'simulationConfigFile', 364, 'string', false,
            null, null,
            null,
            null, null,
            '1.8.12.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: pnlExplain.simulationConfigFile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'pnlExplain' and name = 'sensitivityConfigFile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''pnlExplain'']/Parameter[@name=''sensitivityConfigFile'']',
            'analytic', 'pnlExplain', 'sensitivityConfigFile', 365, 'string', false,
            null, null,
            null,
            null, null,
            '1.8.12.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: pnlExplain.sensitivityConfigFile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'pnlExplain' and name = 'parSensitivity'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''pnlExplain'']/Parameter[@name=''parSensitivity'']',
            'analytic', 'pnlExplain', 'parSensitivity', 366, 'boolean', false,
            null, null,
            null,
            null, null,
            '1.8.12.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: pnlExplain.parSensitivity'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'pnlExplain' and name = 'curveConfigMporFile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''pnlExplain'']/Parameter[@name=''curveConfigMporFile'']',
            'analytic', 'pnlExplain', 'curveConfigMporFile', 367, 'string', false,
            null, null,
            null,
            null, null,
            '1.8.13.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: pnlExplain.curveConfigMporFile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'pnlExplain' and name = 'conventionsMporFile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''pnlExplain'']/Parameter[@name=''conventionsMporFile'']',
            'analytic', 'pnlExplain', 'conventionsMporFile', 368, 'string', false,
            null, null,
            null,
            null, null,
            '1.8.13.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: pnlExplain.conventionsMporFile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'pnlExplain' and name = 'portfolioMporFile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''pnlExplain'']/Parameter[@name=''portfolioMporFile'']',
            'analytic', 'pnlExplain', 'portfolioMporFile', 369, 'string', false,
            null, null,
            null,
            null, null,
            '1.8.13.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: pnlExplain.portfolioMporFile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'pnlExplain' and name = 'outputFileName'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''pnlExplain'']/Parameter[@name=''outputFileName'']',
            'analytic', 'pnlExplain', 'outputFileName', 370, 'string', false,
            null, null,
            null,
            null, null,
            '1.8.12.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: pnlExplain.outputFileName'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'pnl' and name = 'active'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''pnl'']/Parameter[@name=''active'']',
            'analytic', 'pnl', 'active', 371, 'boolean', false,
            null, null,
            null,
            null, null,
            '1.8.12.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: pnl.active'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'pnl' and name = 'mporDays'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''pnl'']/Parameter[@name=''mporDays'']',
            'analytic', 'pnl', 'mporDays', 372, 'integer', false,
            null, null,
            null,
            null, null,
            '1.8.12.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: pnl.mporDays'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'pnl' and name = 'mporCalendar'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''pnl'']/Parameter[@name=''mporCalendar'']',
            'analytic', 'pnl', 'mporCalendar', 373, 'string', false,
            null, null,
            null,
            null, null,
            '1.8.12.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: pnl.mporCalendar'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'pnl' and name = 'curveConfigMporFile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''pnl'']/Parameter[@name=''curveConfigMporFile'']',
            'analytic', 'pnl', 'curveConfigMporFile', 374, 'string', false,
            null, null,
            null,
            null, null,
            '1.8.13.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: pnl.curveConfigMporFile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'pnl' and name = 'conventionsMporFile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''pnl'']/Parameter[@name=''conventionsMporFile'']',
            'analytic', 'pnl', 'conventionsMporFile', 375, 'string', false,
            null, null,
            null,
            null, null,
            '1.8.13.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: pnl.conventionsMporFile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'pnl' and name = 'portfolioMporFile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''pnl'']/Parameter[@name=''portfolioMporFile'']',
            'analytic', 'pnl', 'portfolioMporFile', 376, 'string', false,
            null, null,
            null,
            null, null,
            '1.8.13.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: pnl.portfolioMporFile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'pnl' and name = 'simulationConfigFile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''pnl'']/Parameter[@name=''simulationConfigFile'']',
            'analytic', 'pnl', 'simulationConfigFile', 377, 'string', false,
            null, null,
            null,
            null, null,
            '1.8.12.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: pnl.simulationConfigFile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'pnl' and name = 'outputFileName'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''pnl'']/Parameter[@name=''outputFileName'']',
            'analytic', 'pnl', 'outputFileName', 378, 'string', false,
            null, null,
            null,
            null, null,
            '1.8.12.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: pnl.outputFileName'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'historicalSimulationVar' and name = 'active'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''historicalSimulationVar'']/Parameter[@name=''active'']',
            'analytic', 'historicalSimulationVar', 'active', 379, 'boolean', false,
            null, null,
            null,
            null, null,
            '1.8.12.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: historicalSimulationVar.active'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'historicalSimulationVar' and name = 'historicalScenarioFile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''historicalSimulationVar'']/Parameter[@name=''historicalScenarioFile'']',
            'analytic', 'historicalSimulationVar', 'historicalScenarioFile', 380, 'string', false,
            null, null,
            null,
            null, null,
            '1.8.12.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: historicalSimulationVar.historicalScenarioFile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'historicalSimulationVar' and name = 'simulationConfigFile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''historicalSimulationVar'']/Parameter[@name=''simulationConfigFile'']',
            'analytic', 'historicalSimulationVar', 'simulationConfigFile', 381, 'string', false,
            null, null,
            null,
            null, null,
            '1.8.12.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: historicalSimulationVar.simulationConfigFile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'historicalSimulationVar' and name = 'historicalPeriod'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''historicalSimulationVar'']/Parameter[@name=''historicalPeriod'']',
            'analytic', 'historicalSimulationVar', 'historicalPeriod', 382, 'string', false,
            null, null,
            null,
            null, null,
            '1.8.12.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: historicalSimulationVar.historicalPeriod'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'historicalSimulationVar' and name = 'mporDays'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''historicalSimulationVar'']/Parameter[@name=''mporDays'']',
            'analytic', 'historicalSimulationVar', 'mporDays', 383, 'integer', false,
            null, null,
            null,
            null, null,
            '1.8.12.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: historicalSimulationVar.mporDays'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'historicalSimulationVar' and name = 'mporCalendar'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''historicalSimulationVar'']/Parameter[@name=''mporCalendar'']',
            'analytic', 'historicalSimulationVar', 'mporCalendar', 384, 'string', false,
            null, null,
            null,
            null, null,
            '1.8.12.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: historicalSimulationVar.mporCalendar'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'historicalSimulationVar' and name = 'mporOverlappingPeriods'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''historicalSimulationVar'']/Parameter[@name=''mporOverlappingPeriods'']',
            'analytic', 'historicalSimulationVar', 'mporOverlappingPeriods', 385, 'boolean', false,
            null, null,
            null,
            null, null,
            '1.8.12.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: historicalSimulationVar.mporOverlappingPeriods'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'historicalSimulationVar' and name = 'quantiles'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''historicalSimulationVar'']/Parameter[@name=''quantiles'']',
            'analytic', 'historicalSimulationVar', 'quantiles', 386, 'string', false,
            null, null,
            null,
            null, null,
            '1.8.12.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: historicalSimulationVar.quantiles'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'historicalSimulationVar' and name = 'outputFile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''historicalSimulationVar'']/Parameter[@name=''outputFile'']',
            'analytic', 'historicalSimulationVar', 'outputFile', 387, 'string', false,
            null, null,
            null,
            null, null,
            '1.8.12.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: historicalSimulationVar.outputFile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'historicalSimulationVar' and name = 'includeExpectedShortfall'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''historicalSimulationVar'']/Parameter[@name=''includeExpectedShortfall'']',
            'analytic', 'historicalSimulationVar', 'includeExpectedShortfall', 388, 'boolean', false,
            null, null,
            null,
            null, null,
            '1.8.13.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: historicalSimulationVar.includeExpectedShortfall'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'historicalSimulationVar' and name = 'breakdown'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''historicalSimulationVar'']/Parameter[@name=''breakdown'']',
            'analytic', 'historicalSimulationVar', 'breakdown', 389, 'boolean', false,
            null, null,
            null,
            null, null,
            '1.8.12.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: historicalSimulationVar.breakdown'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'historicalSimulationVar' and name = 'tradePnl'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''historicalSimulationVar'']/Parameter[@name=''tradePnl'']',
            'analytic', 'historicalSimulationVar', 'tradePnl', 390, 'boolean', false,
            null, null,
            null,
            null, null,
            '1.8.13.1', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: historicalSimulationVar.tradePnl'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'historicalSimulationVar' and name = 'riskFactorBreakdown'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''historicalSimulationVar'']/Parameter[@name=''riskFactorBreakdown'']',
            'analytic', 'historicalSimulationVar', 'riskFactorBreakdown', 391, 'boolean', false,
            null, null,
            null,
            null, null,
            '1.8.15.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: historicalSimulationVar.riskFactorBreakdown'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'historicalSimulationVar' and name = 'riskClassBreakdown'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''historicalSimulationVar'']/Parameter[@name=''riskClassBreakdown'']',
            'analytic', 'historicalSimulationVar', 'riskClassBreakdown', 392, 'boolean', false,
            null, null,
            null,
            null, null,
            '1.8.16.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: historicalSimulationVar.riskClassBreakdown'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'historicalSimulationVar' and name = 'outputHistoricalScenarios'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''historicalSimulationVar'']/Parameter[@name=''outputHistoricalScenarios'']',
            'analytic', 'historicalSimulationVar', 'outputHistoricalScenarios', 393, 'boolean', false,
            null, null,
            null,
            null, null,
            '1.8.12.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: historicalSimulationVar.outputHistoricalScenarios'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'historicalSimulationVar' and name = 'includeTheta'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''historicalSimulationVar'']/Parameter[@name=''includeTheta'']',
            'analytic', 'historicalSimulationVar', 'includeTheta', 394, 'boolean', false,
            null, null,
            null,
            null, null,
            '1.8.17.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: historicalSimulationVar.includeTheta'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'historicalSimulationVar' and name = 'includePeriodCashflow'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''historicalSimulationVar'']/Parameter[@name=''includePeriodCashflow'']',
            'analytic', 'historicalSimulationVar', 'includePeriodCashflow', 395, 'boolean', false,
            null, null,
            null,
            null, null,
            '1.8.17.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: historicalSimulationVar.includePeriodCashflow'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'amc' and name = 'active'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''amc'']/Parameter[@name=''active'']',
            'analytic', 'amc', 'active', 396, 'boolean', false,
            'AMC block active', 'A flag. ORE ignores the amc block.',
            'ORE has no analytic of the type amc and does not read this block. Example files of an older layout carry it. The switch for American Monte Carlo is the parameter AMC in the simulation analytic.',
            'None. ORE ignores the parameter, so a value has no effect.', 'A flag, written Y or N, or true or false.',
            '1.8.9.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: amc.active'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'amc' and name = 'pricingEnginesFile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''amc'']/Parameter[@name=''pricingEnginesFile'']',
            'analytic', 'amc', 'pricingEnginesFile', 397, 'string', false,
            'Pricing engines file in the amc block', 'A file name. ORE ignores the amc block.',
            'ORE has no analytic of the type amc and does not read this block. The pricing engines for American Monte Carlo are set in the simulation analytic, with the parameter Pricing engines file for AMC.',
            'None. ORE ignores the parameter, so a value has no effect.', 'A file name. ORE looks for the file in the input path of the Setup block.',
            '1.8.9.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: amc.pricingEnginesFile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'amc' and name = 'amcTradeTypes'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''amc'']/Parameter[@name=''amcTradeTypes'']',
            'analytic', 'amc', 'amcTradeTypes', 398, 'string', false,
            'Trade types in the amc block', 'A comma-separated list of trade types. ORE ignores the amc block.',
            'ORE has no analytic of the type amc and does not read this block. The trade types that American Monte Carlo covers are set in the simulation analytic, with the parameter AMC trade types.',
            'None. ORE ignores the parameter, so a value has no effect.', 'A comma-separated list of trade type names.',
            '1.8.9.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: amc.amcTradeTypes'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'scenarioGeneration' and name = 'active'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''scenarioGeneration'']/Parameter[@name=''active'']',
            'analytic', 'scenarioGeneration', 'active', 399, 'boolean', true,
            'Run scenario generation', 'A flag. Y runs the analytic.',
            'Switches the scenario generation on or off. When it is on, the run generates market scenarios and writes statistics and distributions of them. Use it to check the scenarios of a simulation market, a stress test or a sensitivity run.',
            'No. The analytic does not run.', 'A flag, written Y or N, or true or false.',
            '1.8.14.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: scenarioGeneration.active'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'scenarioGeneration' and name = 'simulationConfigFile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''scenarioGeneration'']/Parameter[@name=''simulationConfigFile'']',
            'analytic', 'scenarioGeneration', 'simulationConfigFile', 400, 'string', false,
            'Simulation configuration file', 'A file name.',
            'Names the XML file with the simulation market that the scenarios are for. For an exposure run the file also supplies the model and the scenario generator, unless the parameters Cross asset model file and Scenario generator file name another file. For example, simulation.xml.',
            'None. The analytic needs this file.', 'A file name. ORE looks for the file in the input path of the Setup block.',
            '1.8.14.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: scenarioGeneration.simulationConfigFile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'scenarioGeneration' and name = 'scenarioOutputFile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''scenarioGeneration'']/Parameter[@name=''scenarioOutputFile'']',
            'analytic', 'scenarioGeneration', 'scenarioOutputFile', 401, 'string', false,
            'Scenario output file', 'A file name.',
            'Names the file that holds all scenarios that the analytic generates through the simulation market. For example, scenariodump.csv.',
            'scenario.csv', 'A file name, written without a directory. ORE writes the file in the output path of the Setup block.',
            '1.8.14.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: scenarioGeneration.scenarioOutputFile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'scenarioGeneration' and name = 'amcPathDataOutput'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''scenarioGeneration'']/Parameter[@name=''amcPathDataOutput'']',
            'analytic', 'scenarioGeneration', 'amcPathDataOutput', 402, 'string', false,
            'AMC path data output file', 'A file name.',
            'Names a file that receives the path data of the generated exposure scenarios, so that an AMC run can read it later with its path data input. ORE uses it only for the scenario type exposure.',
            'None. The analytic does not write path data.', 'A file name, written without a directory. ORE writes the file in the output path of the Setup block.',
            '1.8.14.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: scenarioGeneration.amcPathDataOutput'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'scenarioGeneration' and name = 'outputStatistics'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''scenarioGeneration'']/Parameter[@name=''outputStatistics'']',
            'analytic', 'scenarioGeneration', 'outputStatistics', 403, 'boolean', false,
            'Write the statistics report', 'A flag. Y writes the report.',
            'Decides whether the analytic writes a report with statistics of the generated scenarios for each risk factor and date: the minimum, the mean, the maximum, the standard deviation, the skewness and the kurtosis.',
            'Yes.', 'A flag, written Y or N, or true or false.',
            '1.8.14.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: scenarioGeneration.outputStatistics'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'scenarioGeneration' and name = 'outputDistributions'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''scenarioGeneration'']/Parameter[@name=''outputDistributions'']',
            'analytic', 'scenarioGeneration', 'outputDistributions', 404, 'boolean', false,
            'Write the distribution report', 'A flag. Y writes the report.',
            'Decides whether the analytic writes a report with the distribution histogram of the generated scenarios for each risk factor and date. The parameter Distribution buckets sets its resolution.',
            'Yes.', 'A flag, written Y or N, or true or false.',
            '1.8.14.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: scenarioGeneration.outputDistributions'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'scenarioGeneration' and name = 'scenarioType'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''scenarioGeneration'']/Parameter[@name=''scenarioType'']',
            'analytic', 'scenarioGeneration', 'scenarioType', 405, 'string', false,
            'Scenario type', 'stress, sensitivity or exposure.',
            'Selects which scenarios the analytic generates. exposure takes them from the simulation model. stress takes them from a stress test configuration. sensitivity is accepted, but release 1.8.17.0 has no scenario generation for it. The run stops if the value is none of the three.',
            'exposure', 'stress, sensitivity or exposure.',
            '1.8.14.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: scenarioGeneration.scenarioType'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'scenarioGeneration' and name = 'stressConfigFile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''scenarioGeneration'']/Parameter[@name=''stressConfigFile'']',
            'analytic', 'scenarioGeneration', 'stressConfigFile', 406, 'string', false,
            'Stress configuration file', 'A file name.',
            'Names the XML file with the stress scenarios to generate. ORE uses it only when the scenario type is stress.',
            'None. A stress run needs this file.', 'A file name. ORE looks for the file in the input path of the Setup block.',
            '1.8.14.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: scenarioGeneration.stressConfigFile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'scenarioGeneration' and name = 'scenarioPrecision'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''scenarioGeneration'']/Parameter[@name=''scenarioPrecision'']',
            'analytic', 'scenarioGeneration', 'scenarioPrecision', 407, 'integer', false,
            'Scenario precision', 'A whole number of decimal places.',
            'Sets the number of decimal places of the numbers in the scenario output. A higher number shows more of each value and makes the file larger.',
            '8', 'A whole number of zero or more.',
            '1.8.14.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: scenarioGeneration.scenarioPrecision'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'scenarioGeneration' and name = 'distributionBuckets'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''scenarioGeneration'']/Parameter[@name=''distributionBuckets'']',
            'analytic', 'scenarioGeneration', 'distributionBuckets', 408, 'integer', false,
            'Distribution buckets', 'A whole number.',
            'Sets the number of buckets of the distribution histogram of the generated scenarios. A higher number gives a finer histogram and needs more scenarios to be stable. For example, 20.',
            '20', 'A whole number of zero or more.',
            '1.8.14.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: scenarioGeneration.distributionBuckets'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'scenarioGeneration' and name = 'outputZeroRate'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''scenarioGeneration'']/Parameter[@name=''outputZeroRate'']',
            'analytic', 'scenarioGeneration', 'outputZeroRate', 409, 'boolean', false,
            'Report zero rates', 'A flag. Y reports zero rates, N reports discount factors.',
            'Decides whether the statistics report and the distribution report show zero rates or discount factors for the yield curves. Zero rates are easier to compare between dates.',
            'No. The reports show discount factors.', 'A flag, written Y or N, or true or false.',
            '1.8.14.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: scenarioGeneration.outputZeroRate'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'scenarioGeneration' and name = 'filterRiskKeys'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''scenarioGeneration'']/Parameter[@name=''filterRiskKeys'']',
            'analytic', 'scenarioGeneration', 'filterRiskKeys', 410, 'string', false,
            'Risk factor types in the output', 'A comma-separated list of risk factor types.',
            'Limits the scenario output to the listed types of risk factor. Without it the output holds every risk factor of the simulation market and can be very large. For example, DiscountCurve.',
            'None. The output holds all risk factors.', 'A comma-separated list of risk factor key types such as DiscountCurve.',
            '1.8.17.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: scenarioGeneration.filterRiskKeys'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'scenarioGeneration' and name = 'crossAssetModelData'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''scenarioGeneration'']/Parameter[@name=''crossAssetModelData'']',
            'analytic', 'scenarioGeneration', 'crossAssetModelData', 411, 'string', false,
            'Cross asset model file', 'A file name.',
            'Names the XML file with the cross asset model, if it sits in another file than the simulation configuration file. ORE uses it only when the scenario type is exposure.',
            'The simulation configuration file.', 'A file name. ORE looks for the file in the input path of the Setup block.',
            '1.8.16.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: scenarioGeneration.crossAssetModelData'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'scenarioGeneration' and name = 'scenarioGeneratorData'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''scenarioGeneration'']/Parameter[@name=''scenarioGeneratorData'']',
            'analytic', 'scenarioGeneration', 'scenarioGeneratorData', 412, 'string', false,
            'Scenario generator file', 'A file name.',
            'Names the XML file with the scenario generator, if it sits in another file than the simulation configuration file. ORE uses it only when the scenario type is exposure.',
            'The simulation configuration file.', 'A file name. ORE looks for the file in the input path of the Setup block.',
            '1.8.16.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: scenarioGeneration.scenarioGeneratorData'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'parStressConversion' and name = 'active'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''parStressConversion'']/Parameter[@name=''active'']',
            'analytic', 'parStressConversion', 'active', 413, 'boolean', true,
            'Run the par stress conversion', 'A flag. Y runs the analytic.',
            'Switches the par stress conversion on or off. The analytic does not run a stress test. It turns a stress test definition in the par domain into one in the raw domain, so that you can apply the result again and again.',
            'No. The analytic does not run.', 'A flag, written Y or N, or true or false.',
            '1.8.12.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: parStressConversion.active'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'parStressConversion' and name = 'marketConfigFile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''parStressConversion'']/Parameter[@name=''marketConfigFile'']',
            'analytic', 'parStressConversion', 'marketConfigFile', 414, 'string', false,
            'Simulation market file', 'A file name.',
            'Names the XML file with the simulation market under which the analytic computes its results. The analytic needs only the part under Market. For example, simulation.xml.',
            'None. The analytic needs this file.', 'A file name. ORE looks for the file in the input path of the Setup block.',
            '1.8.12.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: parStressConversion.marketConfigFile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'parStressConversion' and name = 'stressConfigFile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''parStressConversion'']/Parameter[@name=''stressConfigFile'']',
            'analytic', 'parStressConversion', 'stressConfigFile', 415, 'string', false,
            'Stress configuration file', 'A file name.',
            'Names the XML file with the stress scenarios. The same file also decides whether the stress is in the raw domain or in the par domain. For example, stresstest.xml.',
            'None. The analytic needs this file.', 'A file name. ORE looks for the file in the input path of the Setup block.',
            '1.8.12.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: parStressConversion.stressConfigFile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'parStressConversion' and name = 'sensitivityConfigFile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''parStressConversion'']/Parameter[@name=''sensitivityConfigFile'']',
            'analytic', 'parStressConversion', 'sensitivityConfigFile', 416, 'string', false,
            'Sensitivity configuration file', 'A file name.',
            'Names the XML file with the sensitivity setup, in particular the par conversion sections that the conversion needs. For example, sensitivity.xml.',
            'None. The analytic needs this file.', 'A file name. ORE looks for the file in the input path of the Setup block.',
            '1.8.12.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: parStressConversion.sensitivityConfigFile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'parStressConversion' and name = 'pricingEnginesFile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''parStressConversion'']/Parameter[@name=''pricingEnginesFile'']',
            'analytic', 'parStressConversion', 'pricingEnginesFile', 417, 'string', false,
            'Pricing engines file', 'A file name.',
            'Names the XML file that sets the pricing engines for the analytic. When it is missing, ORE uses the pricing engines file of the Setup block. For example, pricingengine.xml.',
            'The pricing engines file of the Setup block.', 'A file name. ORE looks for the file in the input path of the Setup block.',
            '1.8.12.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: parStressConversion.pricingEnginesFile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'parStressConversion' and name = 'scenarioOutputFile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''parStressConversion'']/Parameter[@name=''scenarioOutputFile'']',
            'analytic', 'parStressConversion', 'scenarioOutputFile', 418, 'string', false,
            'Scenario output file', 'A file name. ORE ignores it.',
            'ORE does not read this parameter for the par stress conversion. Example files carry it.',
            'None. ORE ignores the parameter, so a value has no effect.', 'A file name, written without a directory. ORE writes the file in the output path of the Setup block.',
            '1.8.12.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: parStressConversion.scenarioOutputFile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'parStressConversion' and name = 'outputThreshold'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''parStressConversion'']/Parameter[@name=''outputThreshold'']',
            'analytic', 'parStressConversion', 'outputThreshold', 419, 'double', false,
            'Output threshold', 'A number. ORE ignores it.',
            'ORE does not read this parameter for the par stress conversion. Example files carry it.',
            'None. ORE ignores the parameter, so a value has no effect.', 'A number, written with a decimal point.',
            '1.8.12.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: parStressConversion.outputThreshold'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'parStressConversion' and name = 'stressZeroScenarioDataFile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''parStressConversion'']/Parameter[@name=''stressZeroScenarioDataFile'']',
            'analytic', 'parStressConversion', 'stressZeroScenarioDataFile', 420, 'string', false,
            'Raw domain stress file', 'A file name.',
            'Names the file that receives the stress test definition in the raw domain. For example, results.xml.',
            'parStress_ZeroStressData.xml', 'A file name, written without a directory. ORE writes the file in the output path of the Setup block.',
            '1.8.12.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: parStressConversion.stressZeroScenarioDataFile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'parStressConversion' and name = 'lowerBoundCapVols'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''parStressConversion'']/Parameter[@name=''lowerBoundCapVols'']',
            'analytic', 'parStressConversion', 'lowerBoundCapVols', 421, 'double', false,
            'Lower bound of cap volatilities', 'A number.',
            'Is meant to set the lowest cap and floor volatility that the par to raw conversion tries. The conversion uses the bound 1e-8. ORE reads the value for the par stress conversion analytic. The conversion of release 1.8.17.0 uses its own fixed bounds and does not take the value.',
            '1e-8', 'A number, written with a decimal point.',
            '1.8.12.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: parStressConversion.lowerBoundCapVols'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'parStressConversion' and name = 'upperBoundCapVols'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''parStressConversion'']/Parameter[@name=''upperBoundCapVols'']',
            'analytic', 'parStressConversion', 'upperBoundCapVols', 422, 'double', false,
            'Upper bound of cap volatilities', 'A number.',
            'Is meant to set the highest cap and floor volatility that the par to raw conversion tries, as a multiple of the base volatility. The conversion uses the bound 10. ORE reads the value for the par stress conversion analytic. The conversion of release 1.8.17.0 uses its own fixed bounds and does not take the value.',
            '10', 'A number, written with a decimal point.',
            '1.8.12.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: parStressConversion.upperBoundCapVols'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'parStressConversion' and name = 'lowerBoundDiscountFactors'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''parStressConversion'']/Parameter[@name=''lowerBoundDiscountFactors'']',
            'analytic', 'parStressConversion', 'lowerBoundDiscountFactors', 423, 'double', false,
            'Lower bound of discount factors', 'A number.',
            'Is meant to set the lowest discount factor that the par to raw conversion tries. The conversion uses the bound 1e-8. ORE reads the value for the par stress conversion analytic. The conversion of release 1.8.17.0 uses its own fixed bounds and does not take the value.',
            '1e-8', 'A number, written with a decimal point.',
            '1.8.12.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: parStressConversion.lowerBoundDiscountFactors'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'parStressConversion' and name = 'upperBoundDiscountFactors'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''parStressConversion'']/Parameter[@name=''upperBoundDiscountFactors'']',
            'analytic', 'parStressConversion', 'upperBoundDiscountFactors', 424, 'double', false,
            'Upper bound of discount factors', 'A number.',
            'Is meant to set the highest discount factor that the par to raw conversion tries. The conversion uses the bound 10. ORE reads the value for the par stress conversion analytic. The conversion of release 1.8.17.0 uses its own fixed bounds and does not take the value.',
            '10', 'A number, written with a decimal point.',
            '1.8.12.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: parStressConversion.upperBoundDiscountFactors'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'parStressConversion' and name = 'lowerBoundSurvivalProb'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''parStressConversion'']/Parameter[@name=''lowerBoundSurvivalProb'']',
            'analytic', 'parStressConversion', 'lowerBoundSurvivalProb', 425, 'double', false,
            'Lower bound of survival probabilities', 'A number.',
            'Is meant to set the lowest survival probability that the par to raw conversion tries. The conversion treats survival probabilities like discount factors and uses the bound 1e-8. ORE reads the value for the par stress conversion analytic. The conversion of release 1.8.17.0 uses its own fixed bounds and does not take the value.',
            '1e-8', 'A number, written with a decimal point.',
            '1.8.12.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: parStressConversion.lowerBoundSurvivalProb'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'parStressConversion' and name = 'upperBoundSurvivalProb'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''parStressConversion'']/Parameter[@name=''upperBoundSurvivalProb'']',
            'analytic', 'parStressConversion', 'upperBoundSurvivalProb', 426, 'double', false,
            'Upper bound of survival probabilities', 'A number.',
            'Is meant to set the highest survival probability that the par to raw conversion tries. The conversion treats survival probabilities like discount factors and uses the bound 10. ORE reads the value for the par stress conversion analytic. The conversion of release 1.8.17.0 uses its own fixed bounds and does not take the value.',
            '10', 'A number, written with a decimal point.',
            '1.8.12.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: parStressConversion.upperBoundSurvivalProb'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'parStressConversion' and name = 'accuracy'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''parStressConversion'']/Parameter[@name=''accuracy'']',
            'analytic', 'parStressConversion', 'accuracy', 427, 'double', false,
            'Accuracy of the conversion', 'A number.',
            'Is meant to set the accuracy of the root search that turns a par shift into a raw shift. The conversion uses the accuracy 1e-8. ORE reads the value for the par stress conversion analytic. The conversion of release 1.8.17.0 uses its own fixed bounds and does not take the value.',
            '1e-8', 'A number, written with a decimal point.',
            '1.8.12.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: parStressConversion.accuracy'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'pfe' and name = 'active'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''pfe'']/Parameter[@name=''active'']',
            'analytic', 'pfe', 'active', 428, 'boolean', true,
            'Run the PFE analytic', 'A flag. Y runs the analytic.',
            'Switches the PFE analytic on or off. When it is on, the run post-processes an NPV cube: it nets the trades, applies the collateral agreement and computes the exposure profiles and the potential future exposure. The cube comes from the simulation analytic of the same run or from a file.',
            'No. The analytic does not run.', 'A flag, written Y or N, or true or false.',
            '1.8.13.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: pfe.active'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'pfe' and name = 'csaFile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''pfe'']/Parameter[@name=''csaFile'']',
            'analytic', 'pfe', 'csaFile', 429, 'string', false,
            'Netting set definitions file', 'A file name.',
            'Names the XML file with the netting set definitions: the CSA details such as the margining frequency, the thresholds, the minimum transfer amounts and the margin period of risk. The post-processing reads the file, so you can change the CSA terms and see their effect on the results without a new simulation. ORE looks for the file name in the xva block, then in the pfe block and last in the Setup block. For example, netting.xml.',
            'The netting set definitions file of the Setup block.', 'A file name. ORE looks for the file in the input path of the Setup block.',
            '1.8.13.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: pfe.csaFile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'pfe' and name = 'cubeFile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''pfe'']/Parameter[@name=''cubeFile'']',
            'analytic', 'pfe', 'cubeFile', 430, 'string', false,
            'Cube file', 'A file name.',
            'Names the NPV cube file that the PFE analytic reads, if the run has no simulation. The file is usually the cube that an earlier simulation wrote, so you can run the post-processing again without a new simulation. ORE looks for the file in the output path of the Setup block. For example, cube.csv.gz.',
            'None. When the run has a simulation, the analytic uses its cube.', 'A file name. ORE looks for the file in the output path of the Setup block.',
            '1.8.13.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: pfe.cubeFile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'pfe' and name = 'scenarioFile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''pfe'']/Parameter[@name=''scenarioFile'']',
            'analytic', 'pfe', 'scenarioFile', 431, 'string', false,
            'Scenario data file', 'A file name.',
            'Names the file with the scenario data that the simulation wrote for the post-processing, such as simulated index fixings and FX rates. Variation margin and dynamic initial margin need it. For example, scenariodata.csv.gz.',
            'None. When the run has a simulation, the analytic uses its scenario data.', 'A file name. ORE looks for the file in the output path of the Setup block.',
            '1.8.13.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: pfe.scenarioFile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'pfe' and name = 'baseCurrency'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''pfe'']/Parameter[@name=''baseCurrency'']',
            'analytic', 'pfe', 'baseCurrency', 432, 'string', false,
            'Base currency', 'A currency code, for example EUR.',
            'Names the currency in which the analytic states all NPVs, exposures and value adjustments. ORE converts the amounts that are in another currency. For example, EUR states all values in euro.',
            'The base currency of the Setup block.', 'A currency code of three letters, for example EUR.',
            '1.8.13.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: pfe.baseCurrency'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'pfe' and name = 'exposureProfiles'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''pfe'']/Parameter[@name=''exposureProfiles'']',
            'analytic', 'pfe', 'exposureProfiles', 433, 'boolean', false,
            'Write exposure profiles', 'A flag. Y writes the profiles for each netting set.',
            'Decides whether the analytic writes an exposure report for each netting set: the expected positive and negative exposure and the potential future exposure, for each date. The XVA analytic has the same parameter.',
            'Yes.', 'A flag, written Y or N, or true or false.',
            '1.8.13.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: pfe.exposureProfiles'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'pfe' and name = 'exposureProfilesByTrade'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''pfe'']/Parameter[@name=''exposureProfilesByTrade'']',
            'analytic', 'pfe', 'exposureProfilesByTrade', 434, 'boolean', false,
            'Write exposure profiles by trade', 'A flag. Y writes the stand-alone profiles for each trade.',
            'Decides whether the analytic writes an exposure report for each trade on its own, without the netting. With many trades this makes many files, unless Write individual exposure reports is off. The XVA analytic has the same parameter.',
            'Yes.', 'A flag, written Y or N, or true or false.',
            '1.8.13.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: pfe.exposureProfilesByTrade'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'pfe' and name = 'quantile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''pfe'']/Parameter[@name=''quantile'']',
            'analytic', 'pfe', 'quantile', 435, 'double', false,
            'PFE confidence level', 'A number between 0 and 1.',
            'Sets the confidence level of the potential future exposure (PFE) in the exposure reports. For example, 0.95 reports the 95th percentile of the exposure distribution. The XVA analytic has the same parameter.',
            '0.95', 'A number, written with a decimal point.',
            '1.8.13.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: pfe.quantile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'pfe' and name = 'calculationType'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''pfe'']/Parameter[@name=''calculationType'']',
            'analytic', 'pfe', 'calculationType', 436, 'string', false,
            'Margin settlement type', 'NoLag, Symmetric, AsymmetricCVA or AsymmetricDVA.',
            'Decides how ORE settles margin calls. Symmetric settles the margin of both parties after the margin period of risk. AsymmetricCVA settles the margin that we request from the counterparty with the delay and the margin that the counterparty requests from us at once. AsymmetricDVA is the reverse. NoLag turns the delay off and works together with a close-out grid in the simulation. Without a close-out grid, use one of the three others. With one, use NoLag. When there is no close-out grid, the results are correct only if the simulation grid is equally spaced with steps equal to the margin period of risk. The XVA analytic has the same parameter.',
            'NoLag', 'One of NoLag, Symmetric, AsymmetricCVA or AsymmetricDVA.',
            '1.8.13.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: pfe.calculationType'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'pfe' and name = 'allocationMethod'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''pfe'']/Parameter[@name=''allocationMethod'']',
            'analytic', 'pfe', 'allocationMethod', 437, 'string', false,
            'Exposure allocation method', 'None, Marginal, RelativeXVA, RelativeFairValueGross or RelativeFairValueNet.',
            'Selects how ORE allocates the netting set exposure and the value adjustments to the trades in the netting set. None gives no allocation. Marginal uses the marginal method of Pykhtin and Rosen. The other three split the amount in proportion to the XVA of the trade, to its gross fair value or to its net fair value. The XVA analytic has the same parameter.',
            'None', 'One of None, Marginal, RelativeXVA, RelativeFairValueGross or RelativeFairValueNet.',
            '1.8.13.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: pfe.allocationMethod'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'pfe' and name = 'marginalAllocationLimit'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''pfe'']/Parameter[@name=''marginalAllocationLimit'']',
            'analytic', 'pfe', 'marginalAllocationLimit', 438, 'double', false,
            'Marginal allocation limit', 'A number.',
            'Sets a cutoff for the marginal allocation. The marginal method breaks down when the value of the netting set is close to zero while its exposure is not. Below this absolute value, ORE splits the exposure equally among the trades. The parameter has an effect only when the allocation method is Marginal. The XVA analytic has the same parameter.',
            '1.0', 'A number, written with a decimal point.',
            '1.8.13.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: pfe.marginalAllocationLimit'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'pfe' and name = 'exerciseNextBreak'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''pfe'']/Parameter[@name=''exerciseNextBreak'']',
            'analytic', 'pfe', 'exerciseNextBreak', 439, 'boolean', false,
            'Exercise at the next break', 'A flag. Y ends each trade at its next break date.',
            'Decides whether ORE ends all trades at their next break date before it aggregates them. Use it to model a counterparty relationship in which each party can end a trade at its break dates. The XVA analytic has the same parameter.',
            'No.', 'A flag, written Y or N, or true or false.',
            '1.8.13.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: pfe.exerciseNextBreak'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'pfe' and name = 'rawCubeOutputFile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''pfe'']/Parameter[@name=''rawCubeOutputFile'']',
            'analytic', 'pfe', 'rawCubeOutputFile', 440, 'string', false,
            'Raw cube report file', 'A file name.',
            'Names the file for the trade NPV cube as a text file with one row for each trade, date and sample. Setting it switches the report on. Leave it out to skip the file. The file is for further analysis and can be very large. The XVA analytic has the same parameter.',
            'None. The analytic does not write the file.', 'A file name, written without a directory. ORE writes the file in the output path of the Setup block.',
            '1.8.13.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: pfe.rawCubeOutputFile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'pfe' and name = 'netCubeOutputFile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''pfe'']/Parameter[@name=''netCubeOutputFile'']',
            'analytic', 'pfe', 'netCubeOutputFile', 441, 'string', false,
            'Net cube report file', 'A file name.',
            'Names the file for the aggregated NPV cube as a text file with one row for each netting set, date and sample, after the collateral. Setting it switches the report on. Leave it out to skip the file. The XVA analytic has the same parameter.',
            'None. The analytic does not write the file.', 'A file name, written without a directory. ORE writes the file in the output path of the Setup block.',
            '1.8.13.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: pfe.netCubeOutputFile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'pfe' and name = 'useDoublePrecisionCubes'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''pfe'']/Parameter[@name=''useDoublePrecisionCubes'']',
            'analytic', 'pfe', 'useDoublePrecisionCubes', 442, 'boolean', false,
            'Double precision cubes', 'A flag. ORE ignores it in the pfe block.',
            'The user guide shows this parameter in the pfe block, but ORE reads it only from the xva block. A value in the pfe block has no effect in release 1.8.17.0.',
            'No. ORE ignores the parameter, so a value has no effect.', 'A flag, written Y or N, or true or false.',
            '1.8.13.1', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: pfe.useDoublePrecisionCubes'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'pfe' and name = 'exposureProfilesUseCloseOutValues'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''pfe'']/Parameter[@name=''exposureProfilesUseCloseOutValues'']',
            'analytic', 'pfe', 'exposureProfilesUseCloseOutValues', 443, 'boolean', false,
            'Exposure profiles use close-out values', 'A flag. Y uses the close-out values.',
            'Decides whether the exposure profiles use the close-out value of a trade, which is its value at the end of the margin period of risk, instead of its value at the default date. It needs a simulation with a close-out grid. The XVA analytic has the same parameter.',
            'No. The profiles use the values at the default date.', 'A flag, written Y or N, or true or false.',
            '1.8.13.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: pfe.exposureProfilesUseCloseOutValues'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'pfe' and name = 'writeIndividualExposureReports'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''pfe'']/Parameter[@name=''writeIndividualExposureReports'']',
            'analytic', 'pfe', 'writeIndividualExposureReports', 444, 'boolean', false,
            'Write individual exposure reports', 'A flag. Y writes one report for each trade and netting set.',
            'Decides whether the analytic writes one exposure report for each trade and for each netting set. When it is N, the analytic writes one report for all trades and one for all netting sets. Switch it off for a large portfolio to avoid a very large number of files. The XVA analytic has the same parameter.',
            'Yes.', 'A flag, written Y or N, or true or false.',
            '1.8.15.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: pfe.writeIndividualExposureReports'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'pfe' and name = 'collateralBalancesFile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''pfe'']/Parameter[@name=''collateralBalancesFile'']',
            'analytic', 'pfe', 'collateralBalancesFile', 445, 'string', false,
            'Collateral balances file', 'A file name.',
            'Names the XML file with the current variation margin and initial margin balances for each netting set. The analytic uses them as the margin at the valuation date. ORE looks for the file name in the xva block, then in the pfe block and last in the Setup block. For example, collateralbalances.xml.',
            'The collateral balances file of the Setup block.', 'A file name. ORE looks for the file in the input path of the Setup block.',
            '1.8.13.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: pfe.collateralBalancesFile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'crif' and name = 'active'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''crif'']/Parameter[@name=''active'']',
            'analytic', 'crif', 'active', 446, 'boolean', false,
            null, null,
            null,
            null, null,
            '1.8.10.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: crif.active'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'crif' and name = 'marketConfigFile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''crif'']/Parameter[@name=''marketConfigFile'']',
            'analytic', 'crif', 'marketConfigFile', 447, 'string', false,
            null, null,
            null,
            null, null,
            '1.8.10.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: crif.marketConfigFile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'crif' and name = 'sensitivityConfigFile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''crif'']/Parameter[@name=''sensitivityConfigFile'']',
            'analytic', 'crif', 'sensitivityConfigFile', 448, 'string', false,
            null, null,
            null,
            null, null,
            '1.8.10.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: crif.sensitivityConfigFile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'crif' and name = 'baseCurrency'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''crif'']/Parameter[@name=''baseCurrency'']',
            'analytic', 'crif', 'baseCurrency', 449, 'string', false,
            null, null,
            null,
            null, null,
            '1.8.10.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: crif.baseCurrency'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'crif' and name = 'simmVersion'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''crif'']/Parameter[@name=''simmVersion'']',
            'analytic', 'crif', 'simmVersion', 450, 'double', false,
            null, null,
            null,
            null, null,
            '1.8.13.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: crif.simmVersion'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'crif' and name = 'crifOutputFile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''crif'']/Parameter[@name=''crifOutputFile'']',
            'analytic', 'crif', 'crifOutputFile', 451, 'string', false,
            null, null,
            null,
            null, null,
            '1.8.13.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: crif.crifOutputFile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'cashflownpv' and name = 'active'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''cashflownpv'']/Parameter[@name=''active'']',
            'analytic', 'cashflownpv', 'active', 452, 'boolean', true,
            'Run the cash flow NPV analytic', 'A flag. Y runs the analytic.',
            'Switches the cash flow NPV analytic on or off. When it is on, the run builds the cash flow report and adds up, for each trade, the present value of the cash flows up to the horizon. The result is one row for each trade with its NPV in the base currency.',
            'No. The analytic does not run.', 'A flag, written Y or N, or true or false.',
            '1.8.9.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: cashflownpv.active'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'cashflownpv' and name = 'cashFlowHorizon'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''cashflownpv'']/Parameter[@name=''cashFlowHorizon'']',
            'analytic', 'cashflownpv', 'cashFlowHorizon', 453, 'date', false,
            'Cash flow horizon', 'A date.',
            'Sets the last pay date that the report counts. ORE adds up the present value of the cash flows of each trade that pay after the valuation date and on or before this date. For example, 2027-12-31 counts the cash flows up to the end of 2027. The cash flow analytic has a parameter of the same name.',
            'None. Without a horizon the report counts no cash flow, so every present value is 0.', 'A date, written as YYYY-MM-DD or YYYYMMDD.',
            '1.8.9.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: cashflownpv.cashFlowHorizon'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'sensitivityStress' and name = 'active'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''sensitivityStress'']/Parameter[@name=''active'']',
            'analytic', 'sensitivityStress', 'active', 454, 'boolean', true,
            'Run the stressed sensitivity analytic', 'A flag. Y runs the analytic.',
            'Switches the stressed sensitivity analytic on or off. It combines the stress test and the sensitivity analysis. For each stress scenario, it replaces the market of the valuation date with the simulation market shifted by the scenario, and runs the sensitivity calculation on it.',
            'No. The analytic does not run.', 'A flag, written Y or N, or true or false.',
            '1.8.13.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: sensitivityStress.active'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'sensitivityStress' and name = 'marketConfigFile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''sensitivityStress'']/Parameter[@name=''marketConfigFile'']',
            'analytic', 'sensitivityStress', 'marketConfigFile', 455, 'string', false,
            'Simulation market file', 'A file name.',
            'Names the XML file with the simulation market under which the analytic computes its results. The analytic needs only the part under Market. For example, simulation.xml. For each stress scenario the analytic replaces the market of the valuation date with this simulation market, shifted by the scenario, and runs the sensitivity calculation on it.',
            'None. The analytic needs this file.', 'A file name. ORE looks for the file in the input path of the Setup block.',
            '1.8.13.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: sensitivityStress.marketConfigFile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'sensitivityStress' and name = 'stressConfigFile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''sensitivityStress'']/Parameter[@name=''stressConfigFile'']',
            'analytic', 'sensitivityStress', 'stressConfigFile', 456, 'string', false,
            'Stress configuration file', 'A file name.',
            'Names the XML file with the stress scenarios. The same file also decides whether the stress is in the raw domain or in the par domain. For example, stresstest.xml.',
            'None. The analytic needs this file.', 'A file name. ORE looks for the file in the input path of the Setup block.',
            '1.8.13.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: sensitivityStress.stressConfigFile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'sensitivityStress' and name = 'sensitivityConfigFile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''sensitivityStress'']/Parameter[@name=''sensitivityConfigFile'']',
            'analytic', 'sensitivityStress', 'sensitivityConfigFile', 457, 'string', false,
            'Sensitivity configuration file', 'A file name.',
            'Names the XML file that sets the sensitivity calculation, as for the sensitivity analytic. The analytic runs the calculation once for each stress scenario.',
            'None. The analytic needs this file.', 'A file name. ORE looks for the file in the input path of the Setup block.',
            '1.8.13.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: sensitivityStress.sensitivityConfigFile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'sensitivityStress' and name = 'calcBaseScenario'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''sensitivityStress'']/Parameter[@name=''calcBaseScenario'']',
            'analytic', 'sensitivityStress', 'calcBaseScenario', 458, 'boolean', false,
            'Compute the base scenario', 'A flag. Y also computes the unshifted scenario.',
            'Decides whether the analytic also computes the sensitivities of the unshifted base scenario, next to those of the stress scenarios.',
            'No.', 'A flag, written Y or N, or true or false.',
            '1.8.13.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: sensitivityStress.calcBaseScenario'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'sensitivityStress' and name = 'scenarioOutputFile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''sensitivityStress'']/Parameter[@name=''scenarioOutputFile'']',
            'analytic', 'sensitivityStress', 'scenarioOutputFile', 459, 'string', false,
            'Stressed sensitivity result file', 'A file name.',
            'Names the file with the result of the stressed sensitivity calculation.',
            'sensitivity_stress.csv', 'A file name, written without a directory. ORE writes the file in the output path of the Setup block.',
            '1.8.13.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: sensitivityStress.scenarioOutputFile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'correlation' and name = 'active'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''correlation'']/Parameter[@name=''active'']',
            'analytic', 'correlation', 'active', 460, 'boolean', false,
            null, null,
            null,
            null, null,
            '1.8.9.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: correlation.active'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'correlation' and name = 'correlation_method'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''correlation'']/Parameter[@name=''correlation_method'']',
            'analytic', 'correlation', 'correlation_method', 461, 'string', false,
            null, null,
            null,
            null, null,
            '1.8.14.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: correlation.correlation_method'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'correlation' and name = 'scenarioCorrSimulation'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''correlation'']/Parameter[@name=''scenarioCorrSimulation'']',
            'analytic', 'correlation', 'scenarioCorrSimulation', 462, 'string', false,
            null, null,
            null,
            null, null,
            '1.8.17.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: correlation.scenarioCorrSimulation'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'correlation' and name = 'historicalScenarioFile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''correlation'']/Parameter[@name=''historicalScenarioFile'']',
            'analytic', 'correlation', 'historicalScenarioFile', 463, 'string', false,
            null, null,
            null,
            null, null,
            '1.8.12.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: correlation.historicalScenarioFile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'correlation' and name = 'sensitivityConfigFile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''correlation'']/Parameter[@name=''sensitivityConfigFile'']',
            'analytic', 'correlation', 'sensitivityConfigFile', 464, 'string', false,
            null, null,
            null,
            null, null,
            '1.8.9.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: correlation.sensitivityConfigFile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'correlation' and name = 'historicalPeriod'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''correlation'']/Parameter[@name=''historicalPeriod'']',
            'analytic', 'correlation', 'historicalPeriod', 465, 'string', false,
            null, null,
            null,
            null, null,
            '1.8.12.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: correlation.historicalPeriod'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'correlation' and name = 'mporDays'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''correlation'']/Parameter[@name=''mporDays'']',
            'analytic', 'correlation', 'mporDays', 466, 'integer', false,
            null, null,
            null,
            null, null,
            '1.8.11.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: correlation.mporDays'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'correlation' and name = 'mporCalendar'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''correlation'']/Parameter[@name=''mporCalendar'']',
            'analytic', 'correlation', 'mporCalendar', 467, 'string', false,
            null, null,
            null,
            null, null,
            '1.8.12.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: correlation.mporCalendar'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'correlation' and name = 'filterCamCorrelationScenarioTenor'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''correlation'']/Parameter[@name=''filterCamCorrelationScenarioTenor'']',
            'analytic', 'correlation', 'filterCamCorrelationScenarioTenor', 468, 'string', false,
            null, null,
            null,
            null, null,
            '1.8.17.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: correlation.filterCamCorrelationScenarioTenor'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'analytic'
          and subtype = 'correlation' and name = 'outputFile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Analytics/Analytic[@type=''correlation'']/Parameter[@name=''outputFile'']',
            'analytic', 'correlation', 'outputFile', 469, 'string', false,
            null, null,
            null,
            null, null,
            '1.8.9.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: correlation.outputFile'
        );
    end if;

end $$;
