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
            'analytic', 'xva', 'active', 90, 'boolean', false,
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
            'analytic', 'xva', 'csaFile', 91, 'string', false,
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
            'analytic', 'xva', 'cubeFile', 92, 'string', false,
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
            'analytic', 'xva', 'scenarioFile', 93, 'string', false,
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
            'analytic', 'xva', 'baseCurrency', 94, 'string', false,
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
            'analytic', 'xva', 'exposureProfiles', 95, 'boolean', false,
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
            'analytic', 'xva', 'exposureProfilesByTrade', 96, 'boolean', false,
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
            'analytic', 'xva', 'quantile', 97, 'double', false,
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
            'analytic', 'xva', 'calculationType', 98, 'string', false,
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
            'analytic', 'xva', 'cva', 99, 'boolean', false,
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
            'analytic', 'xva', 'rawCubeOutputFile', 100, 'string', false,
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
            'analytic', 'xva', 'netCubeOutputFile', 101, 'string', false,
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
            'analytic', 'xva', 'dim', 102, 'boolean', false,
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
            'analytic', 'xva', 'dimModel', 103, 'string', false,
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
            'analytic', 'xva', 'dimQuantile', 104, 'double', false,
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
            'analytic', 'xva', 'dimHorizonCalendarDays', 105, 'integer', false,
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
            'analytic', 'xva', 'dimRegressionOrder', 106, 'integer', false,
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
            'analytic', 'xva', 'allocationMethod', 107, 'string', false,
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
            'analytic', 'xva', 'marginalAllocationLimit', 108, 'double', false,
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
            'analytic', 'xva', 'exerciseNextBreak', 109, 'boolean', false,
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
            'analytic', 'xva', 'dva', 110, 'boolean', false,
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
            'analytic', 'xva', 'dvaName', 111, 'string', false,
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
            'analytic', 'xva', 'fva', 112, 'boolean', false,
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
            'analytic', 'xva', 'fvaBorrowingCurve', 113, 'string', false,
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
            'analytic', 'xva', 'fvaLendingCurve', 114, 'string', false,
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
            'analytic', 'xva', 'colva', 115, 'boolean', false,
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
            'analytic', 'xva', 'collateralSpread', 116, 'double', false,
            null, null,
            null,
            null, null,
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
            'analytic', 'xva', 'dimScaling', 118, 'double', false,
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
            'analytic', 'xva', 'dimEvolutionFile', 119, 'string', false,
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
            'analytic', 'xva', 'dimRegressionFiles', 120, 'string', false,
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
            'analytic', 'xva', 'dimOutputNettingSet', 121, 'string', false,
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
            'analytic', 'xva', 'dimOutputGridPoints', 122, 'integer', false,
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
            'analytic', 'xva', 'dimLocalRegressionEvaluations', 123, 'integer', false,
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
            'analytic', 'xva', 'dimLocalRegressionBandwidth', 124, 'double', false,
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
            'analytic', 'xva', 'mva', 125, 'boolean', false,
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
            'analytic', 'xva', 'dimRegressors', 126, 'string', false,
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
            'analytic', 'xva', 'collateralBalancesFile', 127, 'string', false,
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
            'analytic', 'xva', 'useXvaRunner', 128, 'boolean', false,
            null, null,
            null,
            null, null,
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
            'analytic', 'xva', 'fvaFundingCurve', 130, 'string', false,
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
            'analytic', 'xva', 'fvaInvestmentCurve', 131, 'string', false,
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
            'analytic', 'xva', 'colvaSpread', 132, 'double', false,
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
            'analytic', 'xva', 'eoniaFloor', 133, 'boolean', false,
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
            'analytic', 'xva', 'hyperCube', 134, 'boolean', false,
            null, null,
            null,
            null, null,
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
            'analytic', 'xva', 'creditMigrationDistributionGrid', 136, 'string', false,
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
            'analytic', 'xva', 'creditMigrationTimeSteps', 137, 'string', false,
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
            'analytic', 'xva', 'creditMigrationConfig', 138, 'string', false,
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
            'analytic', 'xva', 'creditMigrationOutputFiles', 139, 'string', false,
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
            'analytic', 'xva', 'flipViewXVA', 140, 'boolean', false,
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
            'analytic', 'xva', 'flipViewBorrowingCurvePostfix', 141, 'string', false,
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
            'analytic', 'xva', 'flipViewLendingCurvePostfix', 142, 'string', false,
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
            'analytic', 'xva', 'timeAveragedNettedExposureOutputFile', 143, 'string', false,
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
            'analytic', 'xva', 'firstMporCollateralAdjustment', 144, 'boolean', false,
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
            'analytic', 'xva', 'outputCrossAssetModelData', 145, 'boolean', false,
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
            'analytic', 'xva', 'correlationInputFile', 146, 'string', false,
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
            'analytic', 'xva', 'scenarioCorrSimulation', 147, 'string', false,
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
            'analytic', 'xva', 'simulationConfigFile', 148, 'string', false,
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
            'analytic', 'xva', 'historicalScenarioFile', 149, 'string', false,
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
            'analytic', 'xva', 'sensitivityConfigFile', 150, 'string', false,
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
            'analytic', 'xva', 'historicalPeriod', 151, 'string', false,
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
            'analytic', 'xva', 'mporDays', 152, 'integer', false,
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
            'analytic', 'xva', 'mporCalendar', 153, 'string', false,
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
            'analytic', 'xva', 'deterministicInitialMarginFile', 154, 'string', false,
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
            'analytic', 'xva', 'fullInitialCollateralisation', 155, 'boolean', false,
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
            'analytic', 'xva', 'dimDistributionCoveredStdDevs', 156, 'double', false,
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
            'analytic', 'xva', 'dimDistributionGridSize', 157, 'integer', false,
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
            'analytic', 'xva', 'borrowingCurve', 158, 'string', false,
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
            'analytic', 'xva', 'lendingCurve', 159, 'string', false,
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
            'analytic', 'xvaSensitivity', 'active', 160, 'boolean', false,
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
            'analytic', 'xvaSensitivity', 'marketConfigFile', 161, 'string', false,
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
            'analytic', 'xvaSensitivity', 'sensitivityConfigFile', 162, 'string', false,
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
            'analytic', 'xvaSensitivity', 'outputSensitivityThreshold', 163, 'double', false,
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
            'analytic', 'xvaSensitivity', 'parSensitivity', 164, 'boolean', false,
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
            'analytic', 'xvaSensitivity', 'outputJacobi', 165, 'boolean', false,
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
            'analytic', 'xvaSensitivity', 'jacobiOutputFile', 166, 'string', false,
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
            'analytic', 'xvaSensitivity', 'jacobiInverseOutputFile', 167, 'string', false,
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
            'analytic', 'xvaExplain', 'active', 168, 'boolean', false,
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
            'analytic', 'xvaExplain', 'marketConfigFile', 169, 'string', false,
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
            'analytic', 'xvaExplain', 'sensitivityConfigFile', 170, 'string', false,
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
            'analytic', 'xvaExplain', 'writeCubes', 171, 'boolean', false,
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
            'analytic', 'xvaExplain', 'shiftThreshold', 172, 'double', false,
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
            'analytic', 'xvaExplain', 'mporDays', 173, 'integer', false,
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
            'analytic', 'xvaExplain', 'mporCalendar', 174, 'string', false,
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
            'analytic', 'xvaExplain', 'stressConfigFile', 175, 'string', false,
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
            'analytic', 'bacva', 'active', 176, 'boolean', false,
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
            'analytic', 'bacva', 'simmVersion', 177, 'double', false,
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
            'analytic', 'bacva', 'csaFile', 178, 'string', false,
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
            'analytic', 'bacva', 'collateralBalancesFile', 179, 'string', false,
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
            'analytic', 'xvaStress', 'active', 180, 'boolean', false,
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
            'analytic', 'xvaStress', 'marketConfigFile', 181, 'string', false,
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
            'analytic', 'xvaStress', 'stressConfigFile', 182, 'string', false,
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
            'analytic', 'xvaStress', 'writeCubes', 183, 'boolean', false,
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
            'analytic', 'xvaStress', 'sensitivityConfigFile', 184, 'string', false,
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
            'analytic', 'sacva', 'active', 185, 'boolean', false,
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
            'analytic', 'zeroToParShift', 'active', 186, 'boolean', false,
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
            'analytic', 'zeroToParShift', 'marketConfigFile', 187, 'string', false,
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
            'analytic', 'zeroToParShift', 'stressConfigFile', 188, 'string', false,
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
            'analytic', 'zeroToParShift', 'sensitivityConfigFile', 189, 'string', false,
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
            'analytic', 'zeroToParShift', 'pricingEnginesFile', 190, 'string', false,
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
            'analytic', 'zeroToParShift', 'scenarioOutputFile', 191, 'string', false,
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
            'analytic', 'zeroToParShift', 'parShiftsFile', 192, 'string', false,
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
            'analytic', 'additionalResults', 'active', 193, 'boolean', true,
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
            'analytic', 'additionalResults', 'outputFileName', 194, 'string', false,
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
            'analytic', 'additionalResults', 'additionalResultsReportPrecision', 195, 'integer', false,
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
            'analytic', 'sensitivity', 'active', 196, 'boolean', false,
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
            'analytic', 'sensitivity', 'marketConfigFile', 197, 'string', false,
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
            'analytic', 'sensitivity', 'sensitivityConfigFile', 198, 'string', false,
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
            'analytic', 'sensitivity', 'pricingEnginesFile', 199, 'string', false,
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
            'analytic', 'sensitivity', 'scenarioOutputFile', 200, 'string', false,
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
            'analytic', 'sensitivity', 'sensitivityOutputFile', 201, 'string', false,
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
            'analytic', 'sensitivity', 'crossGammaOutputFile', 202, 'string', false,
            null, null,
            null,
            null, null,
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
            'analytic', 'sensitivity', 'outputSensitivityThreshold', 203, 'double', false,
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
            'analytic', 'sensitivity', 'recalibrateModels', 204, 'boolean', false,
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
            'analytic', 'sensitivity', 'applySimmExemptions', 205, 'boolean', false,
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
            'analytic', 'sensitivity', 'outputProductClassMapping', 206, 'boolean', false,
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
            'analytic', 'sensitivity', 'productClassMappingOutputFile', 207, 'string', false,
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
            'analytic', 'sensitivity', 'parSensitivity', 208, 'boolean', false,
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
            'analytic', 'sensitivity', 'parSensitivityOutputFile', 209, 'string', false,
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
            'analytic', 'sensitivity', 'outputJacobi', 210, 'boolean', false,
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
            'analytic', 'sensitivity', 'jacobiOutputFile', 211, 'string', false,
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
            'analytic', 'sensitivity', 'jacobiInverseOutputFile', 212, 'string', false,
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
            'analytic', 'sensitivity', 'parRateSensitivityOutputFile', 213, 'string', false,
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
            'analytic', 'sensitivity', 'optimiseRiskFactors', 214, 'boolean', false,
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
            'analytic', 'sensitivity', 'laxFxConversion', 215, 'boolean', false,
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
            'analytic', 'sensitivity', 'decomposeIndexSensitivities', 216, 'boolean', false,
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
            'analytic', 'sensitivity', 'alignPillars', 217, 'boolean', false,
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
            'analytic', 'calibration', 'active', 218, 'boolean', true,
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
            'analytic', 'calibration', 'configFile', 219, 'string', false,
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
            'analytic', 'calibration', 'outputFile', 220, 'string', false,
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
            'analytic', 'calibration', 'model', 221, 'string', false,
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
            'analytic', 'calibration', 'mode', 222, 'string', false,
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
            'analytic', 'calibration', 'foreignCurrencies', 223, 'string', false,
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
            'analytic', 'calibration', 'pcaCalibration', 224, 'boolean', false,
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
            'analytic', 'calibration', 'curveTenors', 225, 'string', false,
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
            'analytic', 'calibration', 'useForwardOrZeroRate', 226, 'string', false,
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
            'analytic', 'calibration', 'pcaInputFileName', 227, 'string', false,
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
            'analytic', 'calibration', 'meanReversionCalibration', 228, 'boolean', false,
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
            'analytic', 'calibration', 'basisFunctionNumber', 229, 'integer', false,
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
            'analytic', 'calibration', 'kappaUpperBound', 230, 'double', false,
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
            'analytic', 'calibration', 'haltonMaxGuess', 231, 'integer', false,
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
            'analytic', 'calibration', 'meanReversionOutputFileName', 232, 'string', false,
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
            'analytic', 'calibration', 'scenarioInputFile', 233, 'string', false,
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
            'analytic', 'calibration', 'startDate', 234, 'date', false,
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
            'analytic', 'calibration', 'endDate', 235, 'date', false,
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
            'analytic', 'calibration', 'lambda', 236, 'double', false,
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
            'analytic', 'calibration', 'varianceRetained', 237, 'double', false,
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
            'analytic', 'calibration', 'pcaOutputFileName', 238, 'string', false,
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
            'analytic', 'calibration', 'pricingEnginesFile', 239, 'string', false,
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
            'analytic', 'calibration', 'crossAssetModelData', 240, 'string', false,
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
            'analytic', 'initialMargin', 'active', 241, 'boolean', false,
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
            'analytic', 'initialMargin', 'method', 242, 'string', false,
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
            'analytic', 'zeroToParSensiConversion', 'active', 243, 'boolean', false,
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
            'analytic', 'zeroToParSensiConversion', 'marketConfigFile', 244, 'string', false,
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
            'analytic', 'zeroToParSensiConversion', 'sensitivityConfigFile', 245, 'string', false,
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
            'analytic', 'zeroToParSensiConversion', 'pricingEnginesFile', 246, 'string', false,
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
            'analytic', 'zeroToParSensiConversion', 'sensitivityInputFile', 247, 'string', false,
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
            'analytic', 'zeroToParSensiConversion', 'outputThreshold', 248, 'double', false,
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
            'analytic', 'zeroToParSensiConversion', 'outputFile', 249, 'string', false,
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
            'analytic', 'zeroToParSensiConversion', 'outputJacobi', 250, 'boolean', false,
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
            'analytic', 'zeroToParSensiConversion', 'jacobiOutputFile', 251, 'string', false,
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
            'analytic', 'zeroToParSensiConversion', 'jacobiInverseOutputFile', 252, 'string', false,
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
            'analytic', 'saccr', 'active', 253, 'boolean', false,
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
            'analytic', 'saccr', 'csaFile', 254, 'string', false,
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
            'analytic', 'saccr', 'collateralBalancesFile', 255, 'string', false,
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
            'analytic', 'saccr', 'simmVersion', 256, 'double', false,
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
            'analytic', 'smrc', 'active', 257, 'boolean', false,
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
            'analytic', 'portfolioDetails', 'active', 258, 'boolean', false,
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
            'analytic', 'portfolioDetails', 'riskFactorFileName', 259, 'string', false,
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
            'analytic', 'portfolioDetails', 'marketObjectFileName', 260, 'string', false,
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
            'analytic', 'stress', 'active', 261, 'boolean', false,
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
            'analytic', 'stress', 'precision', 262, 'integer', false,
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
            'analytic', 'stress', 'marketConfigFile', 263, 'string', false,
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
            'analytic', 'stress', 'stressConfigFile', 264, 'string', false,
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
            'analytic', 'stress', 'pricingEnginesFile', 265, 'string', false,
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
            'analytic', 'stress', 'scenarioOutputFile', 266, 'string', false,
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
            'analytic', 'stress', 'outputThreshold', 267, 'double', false,
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
            'analytic', 'stress', 'generateCashflows', 268, 'boolean', false,
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
            'analytic', 'stress', 'scenarioCashflowOutputFile', 269, 'string', false,
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
            'analytic', 'stress', 'sensitivityConfigFile', 270, 'string', false,
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
            'analytic', 'stress', 'stressZeroScenarioDataFile', 271, 'string', false,
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
            'analytic', 'stress', 'scenarioFile', 272, 'string', false,
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
            'analytic', 'parametricVar', 'active', 273, 'boolean', false,
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
            'analytic', 'parametricVar', 'sensitivityInputFile', 274, 'string', false,
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
            'analytic', 'parametricVar', 'covarianceInputFile', 275, 'string', false,
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
            'analytic', 'parametricVar', 'quantiles', 276, 'string', false,
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
            'analytic', 'parametricVar', 'breakdown', 277, 'boolean', false,
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
            'analytic', 'parametricVar', 'portfolioFilter', 278, 'string', false,
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
            'analytic', 'parametricVar', 'method', 279, 'string', false,
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
            'analytic', 'parametricVar', 'outputFile', 280, 'string', false,
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
            'analytic', 'simm', 'active', 281, 'boolean', false,
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
            'analytic', 'simm', 'version', 282, 'string', false,
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
            'analytic', 'simm', 'crif', 283, 'string', false,
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
            'analytic', 'simm', 'calculationCurrency', 284, 'string', false,
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
            'analytic', 'simm', 'enforceIMRegulations', 285, 'boolean', false,
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
            'analytic', 'simm', 'mporDays', 286, 'integer', false,
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
            'analytic', 'simm', 'simmCalibration', 287, 'string', false,
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
            'analytic', 'simm', 'writeIntermediateReports', 288, 'boolean', false,
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
            'analytic', 'imschedule', 'active', 289, 'boolean', false,
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
            'analytic', 'imschedule', 'crif', 290, 'string', false,
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
            'analytic', 'imschedule', 'calculationCurrency', 291, 'string', false,
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
            'analytic', 'imschedule', 'version', 292, 'double', false,
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
            'analytic', 'creditcurves', 'active', 293, 'boolean', false,
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
            'analytic', 'creditcurves', 'configuration', 294, 'string', false,
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
            'analytic', 'creditcurves', 'grid', 295, 'string', false,
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
            'analytic', 'creditcurves', 'outputFileName', 296, 'string', false,
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
            'analytic', 'scenario', 'active', 297, 'boolean', false,
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
            'analytic', 'scenario', 'simulationConfigFile', 298, 'string', false,
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
            'analytic', 'scenario', 'scenarioOutputFile', 299, 'string', false,
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
            'analytic', 'pnlExplain', 'active', 300, 'boolean', false,
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
            'analytic', 'pnlExplain', 'mporDate', 301, 'date', false,
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
            'analytic', 'pnlExplain', 'simulationConfigFile', 302, 'string', false,
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
            'analytic', 'pnlExplain', 'sensitivityConfigFile', 303, 'string', false,
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
            'analytic', 'pnlExplain', 'parSensitivity', 304, 'boolean', false,
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
            'analytic', 'pnlExplain', 'curveConfigMporFile', 305, 'string', false,
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
            'analytic', 'pnlExplain', 'conventionsMporFile', 306, 'string', false,
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
            'analytic', 'pnlExplain', 'portfolioMporFile', 307, 'string', false,
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
            'analytic', 'pnlExplain', 'outputFileName', 308, 'string', false,
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
            'analytic', 'pnl', 'active', 309, 'boolean', false,
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
            'analytic', 'pnl', 'mporDays', 310, 'integer', false,
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
            'analytic', 'pnl', 'mporCalendar', 311, 'string', false,
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
            'analytic', 'pnl', 'curveConfigMporFile', 312, 'string', false,
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
            'analytic', 'pnl', 'conventionsMporFile', 313, 'string', false,
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
            'analytic', 'pnl', 'portfolioMporFile', 314, 'string', false,
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
            'analytic', 'pnl', 'simulationConfigFile', 315, 'string', false,
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
            'analytic', 'pnl', 'outputFileName', 316, 'string', false,
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
            'analytic', 'historicalSimulationVar', 'active', 317, 'boolean', false,
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
            'analytic', 'historicalSimulationVar', 'historicalScenarioFile', 318, 'string', false,
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
            'analytic', 'historicalSimulationVar', 'simulationConfigFile', 319, 'string', false,
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
            'analytic', 'historicalSimulationVar', 'historicalPeriod', 320, 'string', false,
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
            'analytic', 'historicalSimulationVar', 'mporDays', 321, 'integer', false,
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
            'analytic', 'historicalSimulationVar', 'mporCalendar', 322, 'string', false,
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
            'analytic', 'historicalSimulationVar', 'mporOverlappingPeriods', 323, 'boolean', false,
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
            'analytic', 'historicalSimulationVar', 'quantiles', 324, 'string', false,
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
            'analytic', 'historicalSimulationVar', 'outputFile', 325, 'string', false,
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
            'analytic', 'historicalSimulationVar', 'includeExpectedShortfall', 326, 'boolean', false,
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
            'analytic', 'historicalSimulationVar', 'breakdown', 327, 'boolean', false,
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
            'analytic', 'historicalSimulationVar', 'tradePnl', 328, 'boolean', false,
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
            'analytic', 'historicalSimulationVar', 'riskFactorBreakdown', 329, 'boolean', false,
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
            'analytic', 'historicalSimulationVar', 'riskClassBreakdown', 330, 'boolean', false,
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
            'analytic', 'historicalSimulationVar', 'outputHistoricalScenarios', 331, 'boolean', false,
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
            'analytic', 'historicalSimulationVar', 'includeTheta', 332, 'boolean', false,
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
            'analytic', 'historicalSimulationVar', 'includePeriodCashflow', 333, 'boolean', false,
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
            'analytic', 'amc', 'active', 334, 'boolean', false,
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
            'analytic', 'amc', 'pricingEnginesFile', 335, 'string', false,
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
            'analytic', 'amc', 'amcTradeTypes', 336, 'string', false,
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
            'analytic', 'scenarioGeneration', 'active', 337, 'boolean', true,
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
            'analytic', 'scenarioGeneration', 'simulationConfigFile', 338, 'string', false,
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
            'analytic', 'scenarioGeneration', 'scenarioOutputFile', 339, 'string', false,
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
            'analytic', 'scenarioGeneration', 'amcPathDataOutput', 340, 'string', false,
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
            'analytic', 'scenarioGeneration', 'outputStatistics', 341, 'boolean', false,
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
            'analytic', 'scenarioGeneration', 'outputDistributions', 342, 'boolean', false,
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
            'analytic', 'scenarioGeneration', 'scenarioType', 343, 'string', false,
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
            'analytic', 'scenarioGeneration', 'stressConfigFile', 344, 'string', false,
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
            'analytic', 'scenarioGeneration', 'scenarioPrecision', 345, 'integer', false,
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
            'analytic', 'scenarioGeneration', 'distributionBuckets', 346, 'integer', false,
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
            'analytic', 'scenarioGeneration', 'outputZeroRate', 347, 'boolean', false,
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
            'analytic', 'scenarioGeneration', 'filterRiskKeys', 348, 'string', false,
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
            'analytic', 'scenarioGeneration', 'crossAssetModelData', 349, 'string', false,
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
            'analytic', 'scenarioGeneration', 'scenarioGeneratorData', 350, 'string', false,
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
            'analytic', 'parStressConversion', 'active', 351, 'boolean', false,
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
            'analytic', 'parStressConversion', 'marketConfigFile', 352, 'string', false,
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
            'analytic', 'parStressConversion', 'stressConfigFile', 353, 'string', false,
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
            'analytic', 'parStressConversion', 'sensitivityConfigFile', 354, 'string', false,
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
            'analytic', 'parStressConversion', 'pricingEnginesFile', 355, 'string', false,
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
            'analytic', 'parStressConversion', 'scenarioOutputFile', 356, 'string', false,
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
            'analytic', 'parStressConversion', 'outputThreshold', 357, 'double', false,
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
            'analytic', 'parStressConversion', 'stressZeroScenarioDataFile', 358, 'string', false,
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
            'analytic', 'pfe', 'active', 359, 'boolean', false,
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
            'analytic', 'pfe', 'csaFile', 360, 'string', false,
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
            'analytic', 'pfe', 'cubeFile', 361, 'string', false,
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
            'analytic', 'pfe', 'scenarioFile', 362, 'string', false,
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
            'analytic', 'pfe', 'baseCurrency', 363, 'string', false,
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
            'analytic', 'pfe', 'exposureProfiles', 364, 'boolean', false,
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
            'analytic', 'pfe', 'exposureProfilesByTrade', 365, 'boolean', false,
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
            'analytic', 'pfe', 'quantile', 366, 'double', false,
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
            'analytic', 'pfe', 'calculationType', 367, 'string', false,
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
            'analytic', 'pfe', 'allocationMethod', 368, 'string', false,
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
            'analytic', 'pfe', 'marginalAllocationLimit', 369, 'double', false,
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
            'analytic', 'pfe', 'exerciseNextBreak', 370, 'boolean', false,
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
            'analytic', 'pfe', 'rawCubeOutputFile', 371, 'string', false,
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
            'analytic', 'pfe', 'netCubeOutputFile', 372, 'string', false,
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
            'analytic', 'crif', 'active', 373, 'boolean', false,
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
            'analytic', 'crif', 'marketConfigFile', 374, 'string', false,
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
            'analytic', 'crif', 'sensitivityConfigFile', 375, 'string', false,
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
            'analytic', 'crif', 'baseCurrency', 376, 'string', false,
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
            'analytic', 'crif', 'simmVersion', 377, 'double', false,
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
            'analytic', 'crif', 'crifOutputFile', 378, 'string', false,
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
            'analytic', 'cashflownpv', 'active', 379, 'boolean', true,
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
            'analytic', 'cashflownpv', 'cashFlowHorizon', 380, 'date', false,
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
            'analytic', 'sensitivityStress', 'active', 381, 'boolean', false,
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
            'analytic', 'sensitivityStress', 'marketConfigFile', 382, 'string', false,
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
            'analytic', 'sensitivityStress', 'stressConfigFile', 383, 'string', false,
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
            'analytic', 'sensitivityStress', 'sensitivityConfigFile', 384, 'string', false,
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
            'analytic', 'sensitivityStress', 'calcBaseScenario', 385, 'boolean', false,
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
            'analytic', 'correlation', 'active', 386, 'boolean', false,
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
            'analytic', 'correlation', 'correlation_method', 387, 'string', false,
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
            'analytic', 'correlation', 'scenarioCorrSimulation', 388, 'string', false,
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
            'analytic', 'correlation', 'historicalScenarioFile', 389, 'string', false,
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
            'analytic', 'correlation', 'sensitivityConfigFile', 390, 'string', false,
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
            'analytic', 'correlation', 'historicalPeriod', 391, 'string', false,
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
            'analytic', 'correlation', 'mporDays', 392, 'integer', false,
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
            'analytic', 'correlation', 'mporCalendar', 393, 'string', false,
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
            'analytic', 'correlation', 'filterCamCorrelationScenarioTenor', 394, 'string', false,
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
            'analytic', 'correlation', 'outputFile', 395, 'string', false,
            null, null,
            null,
            null, null,
            '1.8.9.0', null,
            current_user, 'system.initial_load', 'Seed analytic parameter: correlation.outputFile'
        );
    end if;

end $$;
