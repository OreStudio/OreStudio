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
            'analytic', 'simulation', 'active', 22, 'boolean', false,
            null, null,
            null,
            null, null,
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
            null, null,
            null,
            null, null,
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
            null, null,
            null,
            null, null,
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
            null, null,
            null,
            null, null,
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
            null, null,
            null,
            null, null,
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
            null, null,
            null,
            null, null,
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
            null, null,
            null,
            null, null,
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
            null, null,
            null,
            null, null,
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
            null, null,
            null,
            null, null,
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
            null, null,
            null,
            null, null,
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
            null, null,
            null,
            null, null,
            '1.8.3.0', null,
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
            null, null,
            null,
            null, null,
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
            null, null,
            null,
            null, null,
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
            null, null,
            null,
            null, null,
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
            null, null,
            null,
            null, null,
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
            null, null,
            null,
            null, null,
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
            null, null,
            null,
            null, null,
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
            null, null,
            null,
            null, null,
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
            null, null,
            null,
            null, null,
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
            null, null,
            null,
            null, null,
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
            null, null,
            null,
            null, null,
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
            null, null,
            null,
            null, null,
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
            null, null,
            null,
            null, null,
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
            null, null,
            null,
            null, null,
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
            null, null,
            null,
            null, null,
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
            null, null,
            null,
            null, null,
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
            null, null,
            null,
            null, null,
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
            null, null,
            null,
            null, null,
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
            null, null,
            null,
            null, null,
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
            null, null,
            null,
            null, null,
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
            null, null,
            null,
            null, null,
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
            null, null,
            null,
            null, null,
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
            null, null,
            null,
            null, null,
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
            null, null,
            null,
            null, null,
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
            null, null,
            null,
            null, null,
            '1.8.0.7 or earlier', null,
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
            null, null,
            null,
            null, null,
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
            null, null,
            null,
            null, null,
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
            null, null,
            null,
            null, null,
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
            null, null,
            null,
            null, null,
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
            null, null,
            null,
            null, null,
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
            null, null,
            null,
            null, null,
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
            null, null,
            null,
            null, null,
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
            null, null,
            null,
            null, null,
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
            null, null,
            null,
            null, null,
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
            null, null,
            null,
            null, null,
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
            null, null,
            null,
            null, null,
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
            null, null,
            null,
            null, null,
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
            null, null,
            null,
            null, null,
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
            null, null,
            null,
            null, null,
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
            null, null,
            null,
            null, null,
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
            null, null,
            null,
            null, null,
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
            null, null,
            null,
            null, null,
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
            null, null,
            null,
            null, null,
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
            null, null,
            null,
            null, null,
            '1.8.17.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: simulation.xvaCgEnableCgOptimization'
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
            'analytic', 'xva', 'active', 76, 'boolean', false,
            null, null,
            null,
            null, null,
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
            'analytic', 'xva', 'csaFile', 77, 'string', false,
            null, null,
            null,
            null, null,
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
            'analytic', 'xva', 'cubeFile', 78, 'string', false,
            null, null,
            null,
            null, null,
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
            'analytic', 'xva', 'scenarioFile', 79, 'string', false,
            null, null,
            null,
            null, null,
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
            'analytic', 'xva', 'baseCurrency', 80, 'string', false,
            null, null,
            null,
            null, null,
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
            'analytic', 'xva', 'exposureProfiles', 81, 'boolean', false,
            null, null,
            null,
            null, null,
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
            'analytic', 'xva', 'exposureProfilesByTrade', 82, 'boolean', false,
            null, null,
            null,
            null, null,
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
            'analytic', 'xva', 'quantile', 83, 'double', false,
            null, null,
            null,
            null, null,
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
            'analytic', 'xva', 'calculationType', 84, 'string', false,
            null, null,
            null,
            null, null,
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
            'analytic', 'xva', 'cva', 85, 'boolean', false,
            null, null,
            null,
            null, null,
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
            'analytic', 'xva', 'rawCubeOutputFile', 86, 'string', false,
            null, null,
            null,
            null, null,
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
            'analytic', 'xva', 'netCubeOutputFile', 87, 'string', false,
            null, null,
            null,
            null, null,
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
            'analytic', 'xva', 'dim', 88, 'boolean', false,
            null, null,
            null,
            null, null,
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
            'analytic', 'xva', 'dimModel', 89, 'string', false,
            null, null,
            null,
            null, null,
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
            'analytic', 'xva', 'dimQuantile', 90, 'double', false,
            null, null,
            null,
            null, null,
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
            'analytic', 'xva', 'dimHorizonCalendarDays', 91, 'integer', false,
            null, null,
            null,
            null, null,
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
            'analytic', 'xva', 'dimRegressionOrder', 92, 'integer', false,
            null, null,
            null,
            null, null,
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
            'analytic', 'xva', 'allocationMethod', 93, 'string', false,
            null, null,
            null,
            null, null,
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
            'analytic', 'xva', 'marginalAllocationLimit', 94, 'double', false,
            null, null,
            null,
            null, null,
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
            'analytic', 'xva', 'exerciseNextBreak', 95, 'boolean', false,
            null, null,
            null,
            null, null,
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
            'analytic', 'xva', 'dva', 96, 'boolean', false,
            null, null,
            null,
            null, null,
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
            'analytic', 'xva', 'dvaName', 97, 'string', false,
            null, null,
            null,
            null, null,
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
            'analytic', 'xva', 'fva', 98, 'boolean', false,
            null, null,
            null,
            null, null,
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
            'analytic', 'xva', 'fvaBorrowingCurve', 99, 'string', false,
            null, null,
            null,
            null, null,
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
            'analytic', 'xva', 'fvaLendingCurve', 100, 'string', false,
            null, null,
            null,
            null, null,
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
            'analytic', 'xva', 'colva', 101, 'boolean', false,
            null, null,
            null,
            null, null,
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
            'analytic', 'xva', 'collateralSpread', 102, 'double', false,
            null, null,
            null,
            null, null,
            '1.8.0.7 or earlier', null,
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
            'analytic', 'xva', 'collateralFloor', 103, 'boolean', false,
            null, null,
            null,
            null, null,
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
            'analytic', 'xva', 'dimScaling', 104, 'double', false,
            null, null,
            null,
            null, null,
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
            'analytic', 'xva', 'dimEvolutionFile', 105, 'string', false,
            null, null,
            null,
            null, null,
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
            'analytic', 'xva', 'dimRegressionFiles', 106, 'string', false,
            null, null,
            null,
            null, null,
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
            'analytic', 'xva', 'dimOutputNettingSet', 107, 'string', false,
            null, null,
            null,
            null, null,
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
            'analytic', 'xva', 'dimOutputGridPoints', 108, 'integer', false,
            null, null,
            null,
            null, null,
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
            'analytic', 'xva', 'dimLocalRegressionEvaluations', 109, 'integer', false,
            null, null,
            null,
            null, null,
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
            'analytic', 'xva', 'dimLocalRegressionBandwidth', 110, 'double', false,
            null, null,
            null,
            null, null,
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
            'analytic', 'xva', 'mva', 111, 'boolean', false,
            null, null,
            null,
            null, null,
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
            'analytic', 'xva', 'dimRegressors', 112, 'string', false,
            null, null,
            null,
            null, null,
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
            'analytic', 'xva', 'collateralBalancesFile', 113, 'string', false,
            null, null,
            null,
            null, null,
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
            'analytic', 'xva', 'useXvaRunner', 114, 'boolean', false,
            null, null,
            null,
            null, null,
            '1.8.6.0', null,
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
            'analytic', 'xva', 'dynamicCredit', 115, 'boolean', false,
            null, null,
            null,
            null, null,
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
            'analytic', 'xva', 'fvaFundingCurve', 116, 'string', false,
            null, null,
            null,
            null, null,
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
            'analytic', 'xva', 'fvaInvestmentCurve', 117, 'string', false,
            null, null,
            null,
            null, null,
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
            'analytic', 'xva', 'colvaSpread', 118, 'double', false,
            null, null,
            null,
            null, null,
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
            'analytic', 'xva', 'eoniaFloor', 119, 'boolean', false,
            null, null,
            null,
            null, null,
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
            'analytic', 'xva', 'hyperCube', 120, 'boolean', false,
            null, null,
            null,
            null, null,
            '1.8.0.7 or earlier', null,
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
            'analytic', 'xva', 'creditMigration', 121, 'boolean', false,
            null, null,
            null,
            null, null,
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
            'analytic', 'xva', 'creditMigrationDistributionGrid', 122, 'string', false,
            null, null,
            null,
            null, null,
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
            'analytic', 'xva', 'creditMigrationTimeSteps', 123, 'string', false,
            null, null,
            null,
            null, null,
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
            'analytic', 'xva', 'creditMigrationConfig', 124, 'string', false,
            null, null,
            null,
            null, null,
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
            'analytic', 'xva', 'creditMigrationOutputFiles', 125, 'string', false,
            null, null,
            null,
            null, null,
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
            'analytic', 'xva', 'flipViewXVA', 126, 'boolean', false,
            null, null,
            null,
            null, null,
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
            'analytic', 'xva', 'flipViewBorrowingCurvePostfix', 127, 'string', false,
            null, null,
            null,
            null, null,
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
            'analytic', 'xva', 'flipViewLendingCurvePostfix', 128, 'string', false,
            null, null,
            null,
            null, null,
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
            'analytic', 'xva', 'timeAveragedNettedExposureOutputFile', 129, 'string', false,
            null, null,
            null,
            null, null,
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
            'analytic', 'xva', 'firstMporCollateralAdjustment', 130, 'boolean', false,
            null, null,
            null,
            null, null,
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
            'analytic', 'xva', 'outputCrossAssetModelData', 131, 'boolean', false,
            null, null,
            null,
            null, null,
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
            'analytic', 'xva', 'correlationInputFile', 132, 'string', false,
            null, null,
            null,
            null, null,
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
            'analytic', 'xva', 'scenarioCorrSimulation', 133, 'string', false,
            null, null,
            null,
            null, null,
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
            'analytic', 'xva', 'simulationConfigFile', 134, 'string', false,
            null, null,
            null,
            null, null,
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
            'analytic', 'xva', 'historicalScenarioFile', 135, 'string', false,
            null, null,
            null,
            null, null,
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
            'analytic', 'xva', 'sensitivityConfigFile', 136, 'string', false,
            null, null,
            null,
            null, null,
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
            'analytic', 'xva', 'historicalPeriod', 137, 'string', false,
            null, null,
            null,
            null, null,
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
            'analytic', 'xva', 'mporDays', 138, 'integer', false,
            null, null,
            null,
            null, null,
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
            'analytic', 'xva', 'mporCalendar', 139, 'string', false,
            null, null,
            null,
            null, null,
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
            'analytic', 'xva', 'deterministicInitialMarginFile', 140, 'string', false,
            null, null,
            null,
            null, null,
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
            'analytic', 'xva', 'fullInitialCollateralisation', 141, 'boolean', false,
            null, null,
            null,
            null, null,
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
            'analytic', 'xva', 'dimDistributionCoveredStdDevs', 142, 'double', false,
            null, null,
            null,
            null, null,
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
            'analytic', 'xva', 'dimDistributionGridSize', 143, 'integer', false,
            null, null,
            null,
            null, null,
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
            'analytic', 'xva', 'borrowingCurve', 144, 'string', false,
            null, null,
            null,
            null, null,
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
            'analytic', 'xva', 'lendingCurve', 145, 'string', false,
            null, null,
            null,
            null, null,
            '1.8.17.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: xva.lendingCurve'
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
            'analytic', 'xvaSensitivity', 'active', 146, 'boolean', false,
            null, null,
            null,
            null, null,
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
            'analytic', 'xvaSensitivity', 'marketConfigFile', 147, 'string', false,
            null, null,
            null,
            null, null,
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
            'analytic', 'xvaSensitivity', 'sensitivityConfigFile', 148, 'string', false,
            null, null,
            null,
            null, null,
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
            'analytic', 'xvaSensitivity', 'outputSensitivityThreshold', 149, 'double', false,
            null, null,
            null,
            null, null,
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
            'analytic', 'xvaSensitivity', 'parSensitivity', 150, 'boolean', false,
            null, null,
            null,
            null, null,
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
            'analytic', 'xvaSensitivity', 'outputJacobi', 151, 'boolean', false,
            null, null,
            null,
            null, null,
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
            'analytic', 'xvaSensitivity', 'jacobiOutputFile', 152, 'string', false,
            null, null,
            null,
            null, null,
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
            'analytic', 'xvaSensitivity', 'jacobiInverseOutputFile', 153, 'string', false,
            null, null,
            null,
            null, null,
            '1.8.12.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: xvaSensitivity.jacobiInverseOutputFile'
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
            'analytic', 'xvaExplain', 'active', 154, 'boolean', false,
            null, null,
            null,
            null, null,
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
            'analytic', 'xvaExplain', 'marketConfigFile', 155, 'string', false,
            null, null,
            null,
            null, null,
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
            'analytic', 'xvaExplain', 'sensitivityConfigFile', 156, 'string', false,
            null, null,
            null,
            null, null,
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
            'analytic', 'xvaExplain', 'writeCubes', 157, 'boolean', false,
            null, null,
            null,
            null, null,
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
            'analytic', 'xvaExplain', 'shiftThreshold', 158, 'double', false,
            null, null,
            null,
            null, null,
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
            'analytic', 'xvaExplain', 'mporDays', 159, 'integer', false,
            null, null,
            null,
            null, null,
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
            'analytic', 'xvaExplain', 'mporCalendar', 160, 'string', false,
            null, null,
            null,
            null, null,
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
            'analytic', 'xvaExplain', 'stressConfigFile', 161, 'string', false,
            null, null,
            null,
            null, null,
            '1.8.13.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: xvaExplain.stressConfigFile'
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
            'analytic', 'bacva', 'active', 162, 'boolean', false,
            null, null,
            null,
            null, null,
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
            'analytic', 'bacva', 'simmVersion', 163, 'double', false,
            null, null,
            null,
            null, null,
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
            'analytic', 'bacva', 'csaFile', 164, 'string', false,
            null, null,
            null,
            null, null,
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
            'analytic', 'bacva', 'collateralBalancesFile', 165, 'string', false,
            null, null,
            null,
            null, null,
            '1.8.13.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: bacva.collateralBalancesFile'
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
            'analytic', 'xvaStress', 'active', 166, 'boolean', false,
            null, null,
            null,
            null, null,
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
            'analytic', 'xvaStress', 'marketConfigFile', 167, 'string', false,
            null, null,
            null,
            null, null,
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
            'analytic', 'xvaStress', 'stressConfigFile', 168, 'string', false,
            null, null,
            null,
            null, null,
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
            'analytic', 'xvaStress', 'writeCubes', 169, 'boolean', false,
            null, null,
            null,
            null, null,
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
            'analytic', 'xvaStress', 'sensitivityConfigFile', 170, 'string', false,
            null, null,
            null,
            null, null,
            '1.8.12.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: xvaStress.sensitivityConfigFile'
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
            'analytic', 'sacva', 'active', 171, 'boolean', false,
            null, null,
            null,
            null, null,
            '1.8.13.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: sacva.active'
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
            'analytic', 'zeroToParShift', 'active', 172, 'boolean', false,
            null, null,
            null,
            null, null,
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
            'analytic', 'zeroToParShift', 'marketConfigFile', 173, 'string', false,
            null, null,
            null,
            null, null,
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
            'analytic', 'zeroToParShift', 'stressConfigFile', 174, 'string', false,
            null, null,
            null,
            null, null,
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
            'analytic', 'zeroToParShift', 'sensitivityConfigFile', 175, 'string', false,
            null, null,
            null,
            null, null,
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
            'analytic', 'zeroToParShift', 'pricingEnginesFile', 176, 'string', false,
            null, null,
            null,
            null, null,
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
            'analytic', 'zeroToParShift', 'scenarioOutputFile', 177, 'string', false,
            null, null,
            null,
            null, null,
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
            'analytic', 'zeroToParShift', 'parShiftsFile', 178, 'string', false,
            null, null,
            null,
            null, null,
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
            'analytic', 'additionalResults', 'active', 179, 'boolean', true,
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
            'analytic', 'additionalResults', 'outputFileName', 180, 'string', false,
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
            'analytic', 'additionalResults', 'additionalResultsReportPrecision', 181, 'integer', false,
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
            'analytic', 'sensitivity', 'active', 182, 'boolean', false,
            null, null,
            null,
            null, null,
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
            'analytic', 'sensitivity', 'marketConfigFile', 183, 'string', false,
            null, null,
            null,
            null, null,
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
            'analytic', 'sensitivity', 'sensitivityConfigFile', 184, 'string', false,
            null, null,
            null,
            null, null,
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
            'analytic', 'sensitivity', 'pricingEnginesFile', 185, 'string', false,
            null, null,
            null,
            null, null,
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
            'analytic', 'sensitivity', 'scenarioOutputFile', 186, 'string', false,
            null, null,
            null,
            null, null,
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
            'analytic', 'sensitivity', 'sensitivityOutputFile', 187, 'string', false,
            null, null,
            null,
            null, null,
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
            'analytic', 'sensitivity', 'crossGammaOutputFile', 188, 'string', false,
            null, null,
            null,
            null, null,
            '1.8.2.0', null,
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
            'analytic', 'sensitivity', 'outputSensitivityThreshold', 189, 'double', false,
            null, null,
            null,
            null, null,
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
            'analytic', 'sensitivity', 'recalibrateModels', 190, 'boolean', false,
            null, null,
            null,
            null, null,
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
            'analytic', 'sensitivity', 'applySimmExemptions', 191, 'boolean', false,
            null, null,
            null,
            null, null,
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
            'analytic', 'sensitivity', 'outputProductClassMapping', 192, 'boolean', false,
            null, null,
            null,
            null, null,
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
            'analytic', 'sensitivity', 'productClassMappingOutputFile', 193, 'string', false,
            null, null,
            null,
            null, null,
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
            'analytic', 'sensitivity', 'parSensitivity', 194, 'boolean', false,
            null, null,
            null,
            null, null,
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
            'analytic', 'sensitivity', 'parSensitivityOutputFile', 195, 'string', false,
            null, null,
            null,
            null, null,
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
            'analytic', 'sensitivity', 'outputJacobi', 196, 'boolean', false,
            null, null,
            null,
            null, null,
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
            'analytic', 'sensitivity', 'jacobiOutputFile', 197, 'string', false,
            null, null,
            null,
            null, null,
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
            'analytic', 'sensitivity', 'jacobiInverseOutputFile', 198, 'string', false,
            null, null,
            null,
            null, null,
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
            'analytic', 'sensitivity', 'parRateSensitivityOutputFile', 199, 'string', false,
            null, null,
            null,
            null, null,
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
            'analytic', 'sensitivity', 'optimiseRiskFactors', 200, 'boolean', false,
            null, null,
            null,
            null, null,
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
            'analytic', 'sensitivity', 'laxFxConversion', 201, 'boolean', false,
            null, null,
            null,
            null, null,
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
            'analytic', 'sensitivity', 'decomposeIndexSensitivities', 202, 'boolean', false,
            null, null,
            null,
            null, null,
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
            'analytic', 'sensitivity', 'alignPillars', 203, 'boolean', false,
            null, null,
            null,
            null, null,
            '1.8.9.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: sensitivity.alignPillars'
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
            'analytic', 'calibration', 'active', 204, 'boolean', false,
            null, null,
            null,
            null, null,
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
            'analytic', 'calibration', 'configFile', 205, 'string', false,
            null, null,
            null,
            null, null,
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
            'analytic', 'calibration', 'outputFile', 206, 'string', false,
            null, null,
            null,
            null, null,
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
            'analytic', 'calibration', 'model', 207, 'string', false,
            null, null,
            null,
            null, null,
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
            'analytic', 'calibration', 'mode', 208, 'string', false,
            null, null,
            null,
            null, null,
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
            'analytic', 'calibration', 'foreignCurrencies', 209, 'string', false,
            null, null,
            null,
            null, null,
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
            'analytic', 'calibration', 'pcaCalibration', 210, 'boolean', false,
            null, null,
            null,
            null, null,
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
            'analytic', 'calibration', 'curveTenors', 211, 'string', false,
            null, null,
            null,
            null, null,
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
            'analytic', 'calibration', 'useForwardOrZeroRate', 212, 'string', false,
            null, null,
            null,
            null, null,
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
            'analytic', 'calibration', 'pcaInputFileName', 213, 'string', false,
            null, null,
            null,
            null, null,
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
            'analytic', 'calibration', 'meanReversionCalibration', 214, 'boolean', false,
            null, null,
            null,
            null, null,
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
            'analytic', 'calibration', 'basisFunctionNumber', 215, 'integer', false,
            null, null,
            null,
            null, null,
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
            'analytic', 'calibration', 'kappaUpperBound', 216, 'double', false,
            null, null,
            null,
            null, null,
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
            'analytic', 'calibration', 'haltonMaxGuess', 217, 'integer', false,
            null, null,
            null,
            null, null,
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
            'analytic', 'calibration', 'meanReversionOutputFileName', 218, 'string', false,
            null, null,
            null,
            null, null,
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
            'analytic', 'calibration', 'scenarioInputFile', 219, 'string', false,
            null, null,
            null,
            null, null,
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
            'analytic', 'calibration', 'startDate', 220, 'date', false,
            null, null,
            null,
            null, null,
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
            'analytic', 'calibration', 'endDate', 221, 'date', false,
            null, null,
            null,
            null, null,
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
            'analytic', 'calibration', 'lambda', 222, 'integer', false,
            null, null,
            null,
            null, null,
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
            'analytic', 'calibration', 'varianceRetained', 223, 'double', false,
            null, null,
            null,
            null, null,
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
            'analytic', 'calibration', 'pcaOutputFileName', 224, 'string', false,
            null, null,
            null,
            null, null,
            '1.8.15.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: calibration.pcaOutputFileName'
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
            'analytic', 'initialMargin', 'active', 225, 'boolean', false,
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
            'analytic', 'initialMargin', 'method', 226, 'string', false,
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
            'analytic', 'zeroToParSensiConversion', 'active', 227, 'boolean', false,
            null, null,
            null,
            null, null,
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
            'analytic', 'zeroToParSensiConversion', 'marketConfigFile', 228, 'string', false,
            null, null,
            null,
            null, null,
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
            'analytic', 'zeroToParSensiConversion', 'sensitivityConfigFile', 229, 'string', false,
            null, null,
            null,
            null, null,
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
            'analytic', 'zeroToParSensiConversion', 'pricingEnginesFile', 230, 'string', false,
            null, null,
            null,
            null, null,
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
            'analytic', 'zeroToParSensiConversion', 'sensitivityInputFile', 231, 'string', false,
            null, null,
            null,
            null, null,
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
            'analytic', 'zeroToParSensiConversion', 'outputThreshold', 232, 'double', false,
            null, null,
            null,
            null, null,
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
            'analytic', 'zeroToParSensiConversion', 'outputFile', 233, 'string', false,
            null, null,
            null,
            null, null,
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
            'analytic', 'zeroToParSensiConversion', 'outputJacobi', 234, 'boolean', false,
            null, null,
            null,
            null, null,
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
            'analytic', 'zeroToParSensiConversion', 'jacobiOutputFile', 235, 'string', false,
            null, null,
            null,
            null, null,
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
            'analytic', 'zeroToParSensiConversion', 'jacobiInverseOutputFile', 236, 'string', false,
            null, null,
            null,
            null, null,
            '1.8.11.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: zeroToParSensiConversion.jacobiInverseOutputFile'
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
            'analytic', 'saccr', 'active', 237, 'boolean', false,
            null, null,
            null,
            null, null,
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
            'analytic', 'saccr', 'csaFile', 238, 'string', false,
            null, null,
            null,
            null, null,
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
            'analytic', 'saccr', 'collateralBalancesFile', 239, 'string', false,
            null, null,
            null,
            null, null,
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
            'analytic', 'saccr', 'simmVersion', 240, 'double', false,
            null, null,
            null,
            null, null,
            '1.8.13.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: saccr.simmVersion'
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
            'analytic', 'smrc', 'active', 241, 'boolean', false,
            null, null,
            null,
            null, null,
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
            'analytic', 'portfolioDetails', 'active', 242, 'boolean', false,
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
            'analytic', 'portfolioDetails', 'riskFactorFileName', 243, 'string', false,
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
            'analytic', 'portfolioDetails', 'marketObjectFileName', 244, 'string', false,
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
            'analytic', 'stress', 'active', 245, 'boolean', false,
            null, null,
            null,
            null, null,
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
            'analytic', 'stress', 'precision', 246, 'integer', false,
            null, null,
            null,
            null, null,
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
            'analytic', 'stress', 'marketConfigFile', 247, 'string', false,
            null, null,
            null,
            null, null,
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
            'analytic', 'stress', 'stressConfigFile', 248, 'string', false,
            null, null,
            null,
            null, null,
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
            'analytic', 'stress', 'pricingEnginesFile', 249, 'string', false,
            null, null,
            null,
            null, null,
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
            'analytic', 'stress', 'scenarioOutputFile', 250, 'string', false,
            null, null,
            null,
            null, null,
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
            'analytic', 'stress', 'outputThreshold', 251, 'double', false,
            null, null,
            null,
            null, null,
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
            'analytic', 'stress', 'generateCashflows', 252, 'boolean', false,
            null, null,
            null,
            null, null,
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
            'analytic', 'stress', 'scenarioCashflowOutputFile', 253, 'string', false,
            null, null,
            null,
            null, null,
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
            'analytic', 'stress', 'sensitivityConfigFile', 254, 'string', false,
            null, null,
            null,
            null, null,
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
            'analytic', 'stress', 'stressZeroScenarioDataFile', 255, 'string', false,
            null, null,
            null,
            null, null,
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
            'analytic', 'stress', 'scenarioFile', 256, 'string', false,
            null, null,
            null,
            null, null,
            '1.8.2.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: stress.scenarioFile'
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
            'analytic', 'parametricVar', 'active', 257, 'boolean', false,
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
            'analytic', 'parametricVar', 'sensitivityInputFile', 258, 'string', false,
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
            'analytic', 'parametricVar', 'covarianceInputFile', 259, 'string', false,
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
            'analytic', 'parametricVar', 'quantiles', 260, 'string', false,
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
            'analytic', 'parametricVar', 'breakdown', 261, 'boolean', false,
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
            'analytic', 'parametricVar', 'portfolioFilter', 262, 'string', false,
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
            'analytic', 'parametricVar', 'method', 263, 'string', false,
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
            'analytic', 'parametricVar', 'outputFile', 264, 'string', false,
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
            'analytic', 'simm', 'active', 265, 'boolean', false,
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
            'analytic', 'simm', 'version', 266, 'string', false,
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
            'analytic', 'simm', 'crif', 267, 'string', false,
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
            'analytic', 'simm', 'calculationCurrency', 268, 'string', false,
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
            'analytic', 'simm', 'enforceIMRegulations', 269, 'boolean', false,
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
            'analytic', 'simm', 'mporDays', 270, 'integer', false,
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
            'analytic', 'simm', 'simmCalibration', 271, 'string', false,
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
            'analytic', 'simm', 'writeIntermediateReports', 272, 'boolean', false,
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
            'analytic', 'imschedule', 'active', 273, 'boolean', false,
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
            'analytic', 'imschedule', 'crif', 274, 'string', false,
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
            'analytic', 'imschedule', 'calculationCurrency', 275, 'string', false,
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
            'analytic', 'imschedule', 'version', 276, 'double', false,
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
            'analytic', 'creditcurves', 'active', 277, 'boolean', false,
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
            'analytic', 'creditcurves', 'configuration', 278, 'string', false,
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
            'analytic', 'creditcurves', 'grid', 279, 'string', false,
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
            'analytic', 'creditcurves', 'outputFileName', 280, 'string', false,
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
            'analytic', 'scenario', 'active', 281, 'boolean', false,
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
            'analytic', 'scenario', 'simulationConfigFile', 282, 'string', false,
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
            'analytic', 'scenario', 'scenarioOutputFile', 283, 'string', false,
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
            'analytic', 'pnlExplain', 'active', 284, 'boolean', false,
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
            'analytic', 'pnlExplain', 'mporDate', 285, 'date', false,
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
            'analytic', 'pnlExplain', 'simulationConfigFile', 286, 'string', false,
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
            'analytic', 'pnlExplain', 'sensitivityConfigFile', 287, 'string', false,
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
            'analytic', 'pnlExplain', 'parSensitivity', 288, 'boolean', false,
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
            'analytic', 'pnlExplain', 'curveConfigMporFile', 289, 'string', false,
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
            'analytic', 'pnlExplain', 'conventionsMporFile', 290, 'string', false,
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
            'analytic', 'pnlExplain', 'portfolioMporFile', 291, 'string', false,
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
            'analytic', 'pnlExplain', 'outputFileName', 292, 'string', false,
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
            'analytic', 'pnl', 'active', 293, 'boolean', false,
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
            'analytic', 'pnl', 'mporDays', 294, 'integer', false,
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
            'analytic', 'pnl', 'mporCalendar', 295, 'string', false,
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
            'analytic', 'pnl', 'curveConfigMporFile', 296, 'string', false,
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
            'analytic', 'pnl', 'conventionsMporFile', 297, 'string', false,
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
            'analytic', 'pnl', 'portfolioMporFile', 298, 'string', false,
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
            'analytic', 'pnl', 'simulationConfigFile', 299, 'string', false,
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
            'analytic', 'pnl', 'outputFileName', 300, 'string', false,
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
            'analytic', 'historicalSimulationVar', 'active', 301, 'boolean', false,
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
            'analytic', 'historicalSimulationVar', 'historicalScenarioFile', 302, 'string', false,
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
            'analytic', 'historicalSimulationVar', 'simulationConfigFile', 303, 'string', false,
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
            'analytic', 'historicalSimulationVar', 'historicalPeriod', 304, 'string', false,
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
            'analytic', 'historicalSimulationVar', 'mporDays', 305, 'integer', false,
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
            'analytic', 'historicalSimulationVar', 'mporCalendar', 306, 'string', false,
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
            'analytic', 'historicalSimulationVar', 'mporOverlappingPeriods', 307, 'boolean', false,
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
            'analytic', 'historicalSimulationVar', 'quantiles', 308, 'string', false,
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
            'analytic', 'historicalSimulationVar', 'outputFile', 309, 'string', false,
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
            'analytic', 'historicalSimulationVar', 'includeExpectedShortfall', 310, 'boolean', false,
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
            'analytic', 'historicalSimulationVar', 'breakdown', 311, 'boolean', false,
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
            'analytic', 'historicalSimulationVar', 'tradePnl', 312, 'boolean', false,
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
            'analytic', 'historicalSimulationVar', 'riskFactorBreakdown', 313, 'boolean', false,
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
            'analytic', 'historicalSimulationVar', 'riskClassBreakdown', 314, 'boolean', false,
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
            'analytic', 'historicalSimulationVar', 'outputHistoricalScenarios', 315, 'boolean', false,
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
            'analytic', 'historicalSimulationVar', 'includeTheta', 316, 'boolean', false,
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
            'analytic', 'historicalSimulationVar', 'includePeriodCashflow', 317, 'boolean', false,
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
            'analytic', 'amc', 'active', 318, 'boolean', false,
            null, null,
            null,
            null, null,
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
            'analytic', 'amc', 'pricingEnginesFile', 319, 'string', false,
            null, null,
            null,
            null, null,
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
            'analytic', 'amc', 'amcTradeTypes', 320, 'string', false,
            null, null,
            null,
            null, null,
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
            'analytic', 'scenarioGeneration', 'active', 321, 'boolean', false,
            null, null,
            null,
            null, null,
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
            'analytic', 'scenarioGeneration', 'simulationConfigFile', 322, 'string', false,
            null, null,
            null,
            null, null,
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
            'analytic', 'scenarioGeneration', 'scenarioOutputFile', 323, 'string', false,
            null, null,
            null,
            null, null,
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
            'analytic', 'scenarioGeneration', 'amcPathDataOutput', 324, 'string', false,
            null, null,
            null,
            null, null,
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
            'analytic', 'scenarioGeneration', 'outputStatistics', 325, 'boolean', false,
            null, null,
            null,
            null, null,
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
            'analytic', 'scenarioGeneration', 'outputDistributions', 326, 'boolean', false,
            null, null,
            null,
            null, null,
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
            'analytic', 'scenarioGeneration', 'scenarioType', 327, 'string', false,
            null, null,
            null,
            null, null,
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
            'analytic', 'scenarioGeneration', 'stressConfigFile', 328, 'string', false,
            null, null,
            null,
            null, null,
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
            'analytic', 'scenarioGeneration', 'scenarioPrecision', 329, 'integer', false,
            null, null,
            null,
            null, null,
            '1.8.14.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: scenarioGeneration.scenarioPrecision'
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
            'analytic', 'parStressConversion', 'active', 330, 'boolean', false,
            null, null,
            null,
            null, null,
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
            'analytic', 'parStressConversion', 'marketConfigFile', 331, 'string', false,
            null, null,
            null,
            null, null,
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
            'analytic', 'parStressConversion', 'stressConfigFile', 332, 'string', false,
            null, null,
            null,
            null, null,
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
            'analytic', 'parStressConversion', 'sensitivityConfigFile', 333, 'string', false,
            null, null,
            null,
            null, null,
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
            'analytic', 'parStressConversion', 'pricingEnginesFile', 334, 'string', false,
            null, null,
            null,
            null, null,
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
            'analytic', 'parStressConversion', 'scenarioOutputFile', 335, 'string', false,
            null, null,
            null,
            null, null,
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
            'analytic', 'parStressConversion', 'outputThreshold', 336, 'double', false,
            null, null,
            null,
            null, null,
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
            'analytic', 'parStressConversion', 'stressZeroScenarioDataFile', 337, 'string', false,
            null, null,
            null,
            null, null,
            '1.8.12.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: parStressConversion.stressZeroScenarioDataFile'
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
            'analytic', 'pfe', 'active', 338, 'boolean', false,
            null, null,
            null,
            null, null,
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
            'analytic', 'pfe', 'csaFile', 339, 'string', false,
            null, null,
            null,
            null, null,
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
            'analytic', 'pfe', 'cubeFile', 340, 'string', false,
            null, null,
            null,
            null, null,
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
            'analytic', 'pfe', 'scenarioFile', 341, 'string', false,
            null, null,
            null,
            null, null,
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
            'analytic', 'pfe', 'baseCurrency', 342, 'string', false,
            null, null,
            null,
            null, null,
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
            'analytic', 'pfe', 'exposureProfiles', 343, 'boolean', false,
            null, null,
            null,
            null, null,
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
            'analytic', 'pfe', 'exposureProfilesByTrade', 344, 'boolean', false,
            null, null,
            null,
            null, null,
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
            'analytic', 'pfe', 'quantile', 345, 'double', false,
            null, null,
            null,
            null, null,
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
            'analytic', 'pfe', 'calculationType', 346, 'string', false,
            null, null,
            null,
            null, null,
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
            'analytic', 'pfe', 'allocationMethod', 347, 'string', false,
            null, null,
            null,
            null, null,
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
            'analytic', 'pfe', 'marginalAllocationLimit', 348, 'double', false,
            null, null,
            null,
            null, null,
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
            'analytic', 'pfe', 'exerciseNextBreak', 349, 'boolean', false,
            null, null,
            null,
            null, null,
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
            'analytic', 'pfe', 'rawCubeOutputFile', 350, 'string', false,
            null, null,
            null,
            null, null,
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
            'analytic', 'pfe', 'netCubeOutputFile', 351, 'string', false,
            null, null,
            null,
            null, null,
            '1.8.13.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: pfe.netCubeOutputFile'
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
            'analytic', 'crif', 'active', 352, 'boolean', false,
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
            'analytic', 'crif', 'marketConfigFile', 353, 'string', false,
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
            'analytic', 'crif', 'sensitivityConfigFile', 354, 'string', false,
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
            'analytic', 'crif', 'baseCurrency', 355, 'string', false,
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
            'analytic', 'crif', 'simmVersion', 356, 'double', false,
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
            'analytic', 'crif', 'crifOutputFile', 357, 'string', false,
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
            'analytic', 'cashflownpv', 'active', 358, 'boolean', true,
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
            'analytic', 'cashflownpv', 'cashFlowHorizon', 359, 'date', false,
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
            'analytic', 'sensitivityStress', 'active', 360, 'boolean', false,
            null, null,
            null,
            null, null,
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
            'analytic', 'sensitivityStress', 'marketConfigFile', 361, 'string', false,
            null, null,
            null,
            null, null,
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
            'analytic', 'sensitivityStress', 'stressConfigFile', 362, 'string', false,
            null, null,
            null,
            null, null,
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
            'analytic', 'sensitivityStress', 'sensitivityConfigFile', 363, 'string', false,
            null, null,
            null,
            null, null,
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
            'analytic', 'sensitivityStress', 'calcBaseScenario', 364, 'boolean', false,
            null, null,
            null,
            null, null,
            '1.8.13.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: sensitivityStress.calcBaseScenario'
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
            'analytic', 'correlation', 'active', 365, 'boolean', false,
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
            'analytic', 'correlation', 'correlation_method', 366, 'string', false,
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
            'analytic', 'correlation', 'scenarioCorrSimulation', 367, 'string', false,
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
            'analytic', 'correlation', 'historicalScenarioFile', 368, 'string', false,
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
            'analytic', 'correlation', 'sensitivityConfigFile', 369, 'string', false,
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
            'analytic', 'correlation', 'historicalPeriod', 370, 'string', false,
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
            'analytic', 'correlation', 'mporDays', 371, 'integer', false,
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
            'analytic', 'correlation', 'mporCalendar', 372, 'string', false,
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
            'analytic', 'correlation', 'filterCamCorrelationScenarioTenor', 373, 'string', false,
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
            'analytic', 'correlation', 'outputFile', 374, 'string', false,
            null, null,
            null,
            null, null,
            '1.8.9.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: correlation.outputFile'
        );
    end if;

end $$;
