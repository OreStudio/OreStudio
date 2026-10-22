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
 * Generator: projects/ores.sql/utility/generate_block_ore_parameters_populate.py
 *
 * Block Ore Parameters Population Script
 *
 * Seeds the parameters of the Setup, Logging and Markets blocks of ore.xml
 * that are not engine settings. The scope is the block and the name is the
 * parameter. The text of each one is what a screen shows for it. This script
 * is idempotent.
 */

\echo '--- Block Ore Parameters ---'

do $$
declare
    v_sys_tenant uuid := ores_utility_system_tenant_id_fn();
begin
    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'Setup'
          and subtype = '' and name = 'asofDate'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Setup/Parameter[@name=''asofDate'']',
            'Setup', '', 'asofDate', 16, 'date', true,
            'Valuation date', 'A date. The date the run values the portfolio at.',
            'Sets the valuation date of the run, also called the as of date. ORE sets the evaluation date of QuantLib to this date, reads the market data and the fixings for it and values every trade at it. Every analytic uses it. For example, 2016-02-05 values the portfolio with the market of 5 February 2016. Use the date of the market data file.',
            'None. The document must carry a date.', 'A date, written as YYYY-MM-DD or YYYYMMDD.',
            '1.8.0.7 or earlier', null,
            current_user, 'system.initial_load', 'Seed Setup parameter: asofDate'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'Setup'
          and subtype = '' and name = 'baseCurrency'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Setup/Parameter[@name=''baseCurrency'']',
            'Setup', '', 'baseCurrency', 17, 'string', false,
            'Base currency', 'A currency code, for example EUR.',
            'Names the currency in which results are stated when an analytic converts them to one currency, for example the NPV report and the exposure reports. ORE converts a result in another currency at the spot rate of the valuation date. For example, EUR states all results in euro. Analytics that carry their own base currency use that one for their own report.',
            'None. ORE takes the currency of the discounting index when that one is set.', 'A currency code of three letters, for example EUR or USD.',
            '1.8.0.7 or earlier', null,
            current_user, 'system.initial_load', 'Seed Setup parameter: baseCurrency'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'Setup'
          and subtype = '' and name = 'discountingIndex'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Setup/Parameter[@name=''discountingIndex'']',
            'Setup', '', 'discountingIndex', 18, 'string', false,
            'Discounting index', 'An index name, for example EUR-EONIA.',
            'Names the main discounting index of the run. When the base currency is not set, ORE takes the currency of this index as the base currency. For example, EUR-EONIA sets the base currency to EUR. Set it only when the base currency is not given in the document.',
            'None.', 'An index name written as currency, dash and index, for example EUR-EONIA or USD-FedFunds.',
            '1.8.16.0', null,
            current_user, 'system.initial_load', 'Seed Setup parameter: discountingIndex'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'Setup'
          and subtype = '' and name = 'dryRun'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Setup/Parameter[@name=''dryRun'']',
            'Setup', '', 'dryRun', 19, 'boolean', false,
            'Dry run', 'A flag. Y builds the run and does not value.',
            'Decides whether the valuation engine does all of its work. When it is Yes, the engine prices the trades for the first scenario sample only. It fills the other samples with the value at the valuation date plus a small made-up shift. Use Yes to test a document and its input files in a short time before a long run. The results of a dry run are not real and are not for use.',
            'No. The engines value the portfolio.', 'A flag, written Y or N, or true or false.',
            '1.8.9.0', null,
            current_user, 'system.initial_load', 'Seed Setup parameter: dryRun'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'Setup'
          and subtype = '' and name = 'inputPath'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Setup/Parameter[@name=''inputPath'']',
            'Setup', '', 'inputPath', 20, 'string', true,
            'Input path', 'A path to the input directory.',
            'Names the directory that ORE reads input files from. Every file name in the Setup block, for example the portfolio file and the curve configuration file, is relative to it. A relative path counts from the directory the program starts in. For example, Input reads the files from the directory Input next to the program.',
            'None. The document must carry a path.', 'A path. ORE reads it relative to the directory the program starts in, or as an absolute path.',
            '1.8.0.7 or earlier', null,
            current_user, 'system.initial_load', 'Seed Setup parameter: inputPath'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'Setup'
          and subtype = '' and name = 'outputPath'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Setup/Parameter[@name=''outputPath'']',
            'Setup', '', 'outputPath', 21, 'string', true,
            'Output path', 'A path to the output directory.',
            'Names the directory that ORE writes the reports, the log and the cubes to. Every output file name in the document is relative to it. A relative path counts from the directory the program starts in. For example, Output writes the results to the directory Output next to the program.',
            'None. The document must carry a path.', 'A path. ORE reads it relative to the directory the program starts in, or as an absolute path.',
            '1.8.0.7 or earlier', null,
            current_user, 'system.initial_load', 'Seed Setup parameter: outputPath'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'Setup'
          and subtype = '' and name = 'inputPathMarket'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Setup/Parameter[@name=''inputPathMarket'']',
            'Setup', '', 'inputPathMarket', 22, 'string', false,
            'Market input path', 'A path. No release of ORE reads it.',
            'No release of the ORE sources reads this parameter. Some example documents carry it to name a separate directory for the market data files. ORE ignores it, so the market data files follow the input path. Do not rely on it.',
            'None. ORE ignores the parameter.', 'A path. ORE reads it relative to the directory the program starts in, or as an absolute path.',
            '1.8.10.0', null,
            current_user, 'system.initial_load', 'Seed Setup parameter: inputPathMarket'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'Setup'
          and subtype = '' and name = 'inputPathPortfolio'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Setup/Parameter[@name=''inputPathPortfolio'']',
            'Setup', '', 'inputPathPortfolio', 23, 'string', false,
            'Portfolio input path', 'A path. No release of ORE reads it.',
            'No release of the ORE sources reads this parameter. Some example documents carry it to name a separate directory for the portfolio files. ORE ignores it, so the portfolio file follows the input path. Do not rely on it.',
            'None. ORE ignores the parameter.', 'A path. ORE reads it relative to the directory the program starts in, or as an absolute path.',
            '1.8.10.0', null,
            current_user, 'system.initial_load', 'Seed Setup parameter: inputPathPortfolio'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'Setup'
          and subtype = '' and name = 'logFile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Setup/Parameter[@name=''logFile'']',
            'Setup', '', 'logFile', 24, 'string', false,
            'Log file', 'A file name for the log.',
            'Names the file that ORE writes its log to. The path is relative to the output path. The Logging block has a parameter of the same name, and the Logging block wins when both are present. For example, log.txt writes the log to the file log.txt in the output directory.',
            'None. ORE writes the log to the file given in the Logging block, or to no file.', 'A file name or a path. ORE writes the file in the output path of the Setup block.',
            '1.8.0.7 or earlier', null,
            current_user, 'system.initial_load', 'Seed Setup parameter: logFile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'Setup'
          and subtype = '' and name = 'marketDataFile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Setup/Parameter[@name=''marketDataFile'']',
            'Setup', '', 'marketDataFile', 25, 'string', true,
            'Market data file', 'A file name of the market quotes.',
            'Names the file that holds the market quotes for the valuation date: one line for each quote with the date, the quote name and the value. ORE builds the curves and surfaces of today''s market from these quotes. You can name more than one file, separated by a comma or a semicolon. For example, market_20160205.txt holds the quotes of 5 February 2016.',
            'None. The document must name the file, unless the run loads the market data some other way.', 'A file name or a path. ORE looks for the file in the input path of the Setup block.',
            '1.8.0.7 or earlier', null,
            current_user, 'system.initial_load', 'Seed Setup parameter: marketDataFile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'Setup'
          and subtype = '' and name = 'fixingDataFile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Setup/Parameter[@name=''fixingDataFile'']',
            'Setup', '', 'fixingDataFile', 26, 'string', true,
            'Fixing data file', 'A file name of the index fixings.',
            'Names the file that holds the historic fixings of the indices: one line for each fixing with the date, the index name and the value. Coupons that have already fixed use these values. You can name more than one file, separated by a comma or a semicolon. For example, fixings_20160205.txt holds the fixings known on 5 February 2016. A missing fixing makes the valuation of a trade that needs it fail, unless the run implies today''s fixing or fills gaps.',
            'None. The document must name the file, unless the run loads the fixings some other way.', 'A file name or a path. ORE looks for the file in the input path of the Setup block.',
            '1.8.0.7 or earlier', null,
            current_user, 'system.initial_load', 'Seed Setup parameter: fixingDataFile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'Setup'
          and subtype = '' and name = 'dividendDataFile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Setup/Parameter[@name=''dividendDataFile'']',
            'Setup', '', 'dividendDataFile', 27, 'string', false,
            'Dividend data file', 'A file name of the equity dividends.',
            'Names the file that holds the historic dividends of equities. ORE needs them to value equity options and forwards that depend on dividends. The file is optional and the run needs none when the portfolio holds no such trade. For example, dividends_20160205.txt.',
            'None. The run loads no dividends.', 'A file name or a path. ORE looks for the file in the input path of the Setup block.',
            '1.8.5.0', null,
            current_user, 'system.initial_load', 'Seed Setup parameter: dividendDataFile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'Setup'
          and subtype = '' and name = 'fixingCutoff'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Setup/Parameter[@name=''fixingCutoff'']',
            'Setup', '', 'fixingCutoff', 28, 'date', false,
            'Fixing cut-off', 'A date. The ORE application reads it, with the spelling fixingCutoff.',
            'Sets the date after which the ORE application drops fixings from the fixing file. A fixing after the cut-off date is ignored, so the run does not use data from the future. For example, 2016-02-04 ignores a fixing dated 5 February 2016. The input parameters of the library read the same choice under the name fixingCutOffDate, with a capital O. When the date is not set, the application cuts at the valuation date, or the day before it when today''s fixings are implied.',
            'None. The application cuts at the valuation date.', 'A date, written as YYYY-MM-DD or YYYYMMDD.',
            '1.8.12.0', null,
            current_user, 'system.initial_load', 'Seed Setup parameter: fixingCutoff'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'Setup'
          and subtype = '' and name = 'fixingCutOffDate'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Setup/Parameter[@name=''fixingCutOffDate'']',
            'Setup', '', 'fixingCutOffDate', 29, 'date', false,
            'Fixing cut-off date', 'A date. The library reads it, with the spelling fixingCutOffDate.',
            'Sets the date after which the library drops fixings. A fixing after the cut-off date is ignored. For example, 2016-02-04 ignores a fixing dated 5 February 2016. The ORE application reads the same choice under the name fixingCutoff. Use this spelling when the document goes to a program built on the library.',
            'None. No fixing is dropped.', 'A date, written as YYYY-MM-DD or YYYYMMDD.',
            '1.8.15.0', null,
            current_user, 'system.initial_load', 'Seed Setup parameter: fixingCutOffDate'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'Setup'
          and subtype = '' and name = 'marketDataLoaderInput'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Setup/Parameter[@name=''marketDataLoaderInput'']',
            'Setup', '', 'marketDataLoaderInput', 30, 'string', false,
            'Market data loader input', 'A name of the loader for the market data.',
            'Names the input of the market data loader. A program built on the library sets the loader to read market data and fixings. The value tells the loader where to read them from. Leave it empty when the market data comes from the market data file.',
            'None. The run reads the market data and fixings from the files.', 'A text, written as in the example.',
            '1.8.13.0', null,
            current_user, 'system.initial_load', 'Seed Setup parameter: marketDataLoaderInput'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'Setup'
          and subtype = '' and name = 'marketDataLoaderOutput'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Setup/Parameter[@name=''marketDataLoaderOutput'']',
            'Setup', '', 'marketDataLoaderOutput', 31, 'string', false,
            'Market data loader output', 'A name for the output of the market data loader.',
            'Names the output of the market data loader. A program built on the library sets the loader to store the market data and the fixings of a run, so that another run can load the same data again. The value tells the loader where to store them. Leave it empty to store nothing.',
            'None. The run stores nothing.', 'A text, written as in the example.',
            '1.8.13.0', null,
            current_user, 'system.initial_load', 'Seed Setup parameter: marketDataLoaderOutput'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'Setup'
          and subtype = '' and name = 'entireMarket'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Setup/Parameter[@name=''entireMarket'']',
            'Setup', '', 'entireMarket', 32, 'boolean', false,
            'Load the entire market', 'A flag. Y loads every quote of the file.',
            'Decides how much of the market data file the run loads. When it is Yes, ORE loads every quote of the file. When it is No, ORE loads only the quotes that the curve configuration asks for, which is faster. The library defaults to No. The ORE application sets Yes when it reads ore.xml, so a document run by the application loads everything.',
            'No in the library. The ORE application sets Yes.', 'A flag, written Y or N, or true or false.',
            '1.8.9.0', null,
            current_user, 'system.initial_load', 'Seed Setup parameter: entireMarket'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'Setup'
          and subtype = '' and name = 'allFixings'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Setup/Parameter[@name=''allFixings'']',
            'Setup', '', 'allFixings', 33, 'boolean', false,
            'Load all fixings', 'A flag. Y loads every fixing of the file.',
            'Decides how much of the fixing file the run loads. When it is Yes, ORE loads every fixing of the file. When it is No, ORE loads only the fixings of the indices that the portfolio and the market need. The library defaults to No. The ORE application sets Yes when it reads ore.xml. Use No to load faster with a large file.',
            'No in the library. The ORE application sets Yes.', 'A flag, written Y or N, or true or false.',
            '1.8.16.0', null,
            current_user, 'system.initial_load', 'Seed Setup parameter: allFixings'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'Setup'
          and subtype = '' and name = 'eomInflationFixings'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Setup/Parameter[@name=''eomInflationFixings'']',
            'Setup', '', 'eomInflationFixings', 34, 'boolean', false,
            'End-of-month inflation fixings', 'A flag. Y loads inflation fixings at the end of the month.',
            'Decides how ORE stores the fixing of an inflation index. When it is Yes, ORE stores the fixing at the end of the month it belongs to, which is how most inflation indices publish. When it is No, ORE stores the fixing at the start of the month. Change it only when the fixing file dates its inflation fixings at the start of the month.',
            'Yes. ORE stores an inflation fixing at the end of its month.', 'A flag, written Y or N, or true or false.',
            '1.8.9.0', null,
            current_user, 'system.initial_load', 'Seed Setup parameter: eomInflationFixings'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'Setup'
          and subtype = '' and name = 'useMarketDataFixings'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Setup/Parameter[@name=''useMarketDataFixings'']',
            'Setup', '', 'useMarketDataFixings', 35, 'boolean', false,
            'Use fixings of the market data', 'A flag. ORE reads it and does not use it.',
            'The input parameters read this parameter and keep it. No code of the ORE sources uses the value, so the choice has no effect on a run. The intent is to take the fixings from the market data file, where the file holds them, and not from the fixing file.',
            'Yes. The value has no effect.', 'A flag, written Y or N, or true or false.',
            '1.8.9.0', null,
            current_user, 'system.initial_load', 'Seed Setup parameter: useMarketDataFixings'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'Setup'
          and subtype = '' and name = 'curveConfigFile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Setup/Parameter[@name=''curveConfigFile'']',
            'Setup', '', 'curveConfigFile', 36, 'string', true,
            'Curve configuration file', 'A file name of the curve configuration.',
            'Names the file that says how ORE builds each curve and surface: which quotes it uses, how it interpolates and which conventions it takes. Today''s market refers to the curves in this file by identifier. For example, curveconfig.xml. You can name more than one file, separated by a comma or a semicolon, and a later file adds to an earlier one.',
            'None. The document must name the file.', 'A file name or a path. ORE looks for the file in the input path of the Setup block.',
            '1.8.0.7 or earlier', null,
            current_user, 'system.initial_load', 'Seed Setup parameter: curveConfigFile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'Setup'
          and subtype = '' and name = 'curveConfigOverride'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Setup/Parameter[@name=''curveConfigOverride'']',
            'Setup', '', 'curveConfigOverride', 37, 'string', false,
            'Curve configuration override', 'Curve configuration XML. It overrides the file.',
            'Holds a curve configuration inside the document. A curve in this block replaces the curve of the same identifier in the curve configuration file. Use it to change one curve for a run without a copy of the whole file.',
            'None. The run uses the file only.', 'An XML element, the curve configuration, written inside the parameter.',
            '1.8.15.0', null,
            current_user, 'system.initial_load', 'Seed Setup parameter: curveConfigOverride'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'Setup'
          and subtype = '' and name = 'conventionsFile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Setup/Parameter[@name=''conventionsFile'']',
            'Setup', '', 'conventionsFile', 38, 'string', true,
            'Conventions file', 'A file name of the market conventions.',
            'Names the file that holds the market conventions: the day count, the calendar, the settlement lag and the business day rule of each instrument and index. The curves and the trades look their conventions up by identifier. For example, conventions.xml.',
            'None. The document must name the file.', 'A file name or a path. ORE looks for the file in the input path of the Setup block.',
            '1.8.0.7 or earlier', null,
            current_user, 'system.initial_load', 'Seed Setup parameter: conventionsFile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'Setup'
          and subtype = '' and name = 'conventionsOverride'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Setup/Parameter[@name=''conventionsOverride'']',
            'Setup', '', 'conventionsOverride', 39, 'string', false,
            'Conventions override', 'Conventions XML. It overrides the file.',
            'Holds conventions inside the document. A convention in this block replaces the convention of the same identifier in the conventions file. Use it to change one convention for a run without a copy of the whole file.',
            'None. The run uses the file only.', 'An XML element, the conventions, written inside the parameter.',
            '1.8.15.0', null,
            current_user, 'system.initial_load', 'Seed Setup parameter: conventionsOverride'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'Setup'
          and subtype = '' and name = 'marketConfigFile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Setup/Parameter[@name=''marketConfigFile'']',
            'Setup', '', 'marketConfigFile', 40, 'string', true,
            'Today''s market file', 'A file name of today''s market configuration.',
            'Names the file that defines today''s market: which curve, surface or quote each market object uses, grouped into named market configurations. The Markets block picks a configuration from this file for each purpose. For example, todaysmarket.xml.',
            'None. The document must name the file.', 'A file name or a path. ORE looks for the file in the input path of the Setup block.',
            '1.8.0.7 or earlier', null,
            current_user, 'system.initial_load', 'Seed Setup parameter: marketConfigFile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'Setup'
          and subtype = '' and name = 'pricingEnginesFile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Setup/Parameter[@name=''pricingEnginesFile'']',
            'Setup', '', 'pricingEnginesFile', 41, 'string', true,
            'Pricing engines file', 'A file name of the pricing engines.',
            'Names the file that says which pricing engine and which model prices each product, and with which engine settings. Every trade looks up its engine here by product type. For example, pricingengine.xml.',
            'None. The document must name the file.', 'A file name or a path. ORE looks for the file in the input path of the Setup block.',
            '1.8.0.7 or earlier', null,
            current_user, 'system.initial_load', 'Seed Setup parameter: pricingEnginesFile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'Setup'
          and subtype = '' and name = 'pricingEnginesOverride'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Setup/Parameter[@name=''pricingEnginesOverride'']',
            'Setup', '', 'pricingEnginesOverride', 42, 'string', false,
            'Pricing engines override', 'Pricing engine XML. It overrides the file.',
            'Holds pricing engine settings inside the document. An entry in this block replaces the entry of the same product in the pricing engines file. Use it to change one engine for a run without a copy of the whole file.',
            'None. The run uses the file only.', 'An XML element, the pricing engines, written inside the parameter.',
            '1.8.17.0', null,
            current_user, 'system.initial_load', 'Seed Setup parameter: pricingEnginesOverride'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'Setup'
          and subtype = '' and name = 'pricingEnginesFileScenario'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Setup/Parameter[@name=''pricingEnginesFileScenario'']',
            'Setup', '', 'pricingEnginesFileScenario', 43, 'string', false,
            'Scenario pricing engines file', 'A file name. No release of ORE reads it.',
            'No release of the ORE sources reads this parameter. Some example documents carry it to name a second pricing engines file for scenario runs. ORE ignores it, so the scenario runs use the file of the pricing engines parameter.',
            'None. ORE ignores the parameter.', 'A file name or a path. ORE looks for the file in the input path of the Setup block.',
            '1.8.10.0', null,
            current_user, 'system.initial_load', 'Seed Setup parameter: pricingEnginesFileScenario'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'Setup'
          and subtype = '' and name = 'portfolioFile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Setup/Parameter[@name=''portfolioFile'']',
            'Setup', '', 'portfolioFile', 44, 'string', true,
            'Portfolio file', 'A file name of the portfolio.',
            'Names the file that holds the trades the run values. You can name more than one file, separated by a comma or a semicolon, and ORE joins their trades. A name that is a directory loads the files inside it. For example, portfolio.xml.',
            'None. The document must name the file.', 'A file name or a path. ORE looks for the file in the input path of the Setup block.',
            '1.8.0.7 or earlier', null,
            current_user, 'system.initial_load', 'Seed Setup parameter: portfolioFile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'Setup'
          and subtype = '' and name = 'portfolioRecurseIntoSubDirectories'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Setup/Parameter[@name=''portfolioRecurseIntoSubDirectories'']',
            'Setup', '', 'portfolioRecurseIntoSubDirectories', 45, 'boolean', false,
            'Portfolio sub-directories', 'A flag. No release of ORE reads it.',
            'No release of the ORE sources reads this parameter. Some example documents carry it to load the portfolio files of the sub-directories of a directory. ORE ignores it, so name each directory or file yourself.',
            'No. ORE ignores the parameter.', 'A flag, written Y or N, or true or false.',
            '1.8.10.0', null,
            current_user, 'system.initial_load', 'Seed Setup parameter: portfolioRecurseIntoSubDirectories'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'Setup'
          and subtype = '' and name = 'counterpartyFile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Setup/Parameter[@name=''counterpartyFile'']',
            'Setup', '', 'counterpartyFile', 46, 'string', false,
            'Counterparty file', 'A file name of the counterparty information.',
            'Names the file that holds the counterparty data some capital analytics need, for example the credit quality and the SA-CCR and CVA parameters of each counterparty. For example, counterparty.xml. Leave it out when no analytic of the run uses counterparty data.',
            'None. The run loads no counterparty data.', 'A file name or a path. ORE looks for the file in the input path of the Setup block.',
            '1.8.13.0', null,
            current_user, 'system.initial_load', 'Seed Setup parameter: counterpartyFile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'Setup'
          and subtype = '' and name = 'calendarAdjustment'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Setup/Parameter[@name=''calendarAdjustment'']',
            'Setup', '', 'calendarAdjustment', 47, 'string', false,
            'Calendar adjustment file', 'A file name of the calendar adjustments.',
            'Names the file that lists holidays and business days to add to named calendars. Use it to correct a calendar of QuantLib, for example to add a closure the library does not know. For example, calendaradjustment.xml.',
            'None. ORE uses the calendars as they are.', 'A file name or a path. ORE looks for the file in the input path of the Setup block.',
            '1.8.5.0', null,
            current_user, 'system.initial_load', 'Seed Setup parameter: calendarAdjustment'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'Setup'
          and subtype = '' and name = 'currencyConfiguration'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Setup/Parameter[@name=''currencyConfiguration'']',
            'Setup', '', 'currencyConfiguration', 48, 'string', false,
            'Currency configuration file', 'A file name of extra currencies.',
            'Names the file that adds currencies to the ones ORE knows. The file adds only currencies that are not yet in QuantLib or QuantExt, and it never changes one that is. The example file lists all ISO currencies and a few unofficial codes. For example, currencies.xml.',
            'None. ORE knows its built-in currencies only.', 'A file name or a path. ORE looks for the file in the input path of the Setup block.',
            '1.8.6.0', null,
            current_user, 'system.initial_load', 'Seed Setup parameter: currencyConfiguration'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'Setup'
          and subtype = '' and name = 'referenceDataFile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Setup/Parameter[@name=''referenceDataFile'']',
            'Setup', '', 'referenceDataFile', 49, 'string', false,
            'Reference data file', 'A file name of the reference data.',
            'Names the file that holds reference data: the details of the bonds, the credit names and the other objects a trade refers to by identifier. A trade that holds only an identifier needs this file. For example, referencedata.xml.',
            'None. The run loads no reference data.', 'A file name or a path. ORE looks for the file in the input path of the Setup block.',
            '1.8.5.0', null,
            current_user, 'system.initial_load', 'Seed Setup parameter: referenceDataFile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'Setup'
          and subtype = '' and name = 'referenceDataOverride'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Setup/Parameter[@name=''referenceDataOverride'']',
            'Setup', '', 'referenceDataOverride', 50, 'string', false,
            'Reference data override', 'Reference data XML. It overrides the file.',
            'Holds reference data inside the document. An entry in this block replaces the entry of the same identifier in the reference data file. Use it to change one object for a run without a copy of the whole file.',
            'None. The run uses the file only.', 'An XML element, the reference data, written inside the parameter.',
            '1.8.15.0', null,
            current_user, 'system.initial_load', 'Seed Setup parameter: referenceDataOverride'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'Setup'
          and subtype = '' and name = 'iborFallbackConfig'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Setup/Parameter[@name=''iborFallbackConfig'']',
            'Setup', '', 'iborFallbackConfig', 51, 'string', false,
            'Ibor fallback file', 'A file name of the Ibor fallback rules.',
            'Names the file that says how a discontinued Ibor index falls back to a risk free rate: the replacement index, the spread and the switch date. Trades on a discontinued index use it. For example, iborFallbackConfig.xml.',
            'None. ORE uses the fallback rules built into the program.', 'A file name or a path. ORE looks for the file in the input path of the Setup block.',
            '1.8.6.0', null,
            current_user, 'system.initial_load', 'Seed Setup parameter: iborFallbackConfig'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'Setup'
          and subtype = '' and name = 'iborFallbackOverride'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Setup/Parameter[@name=''iborFallbackOverride'']',
            'Setup', '', 'iborFallbackOverride', 52, 'boolean', false,
            'Ibor fallback override', 'A flag. Y switches the Ibor fallbacks on.',
            'Decides whether ORE applies the Ibor fallback rules before their switch dates. When it is Yes, ORE moves every switch date to one year before the valuation date, so every index that has a fallback has already switched. When it is No, ORE applies a fallback from its own switch date.',
            'No. ORE applies a fallback from its switch date.', 'A flag, written Y or N, or true or false.',
            '1.8.9.0', null,
            current_user, 'system.initial_load', 'Seed Setup parameter: iborFallbackOverride'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'Setup'
          and subtype = '' and name = 'baselTrafficLightConfig'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Setup/Parameter[@name=''baselTrafficLightConfig'']',
            'Setup', '', 'baselTrafficLightConfig', 53, 'string', false,
            'Basel traffic light file', 'A file name of the traffic light zones.',
            'Names the file that sets the Basel traffic light zones for backtests: how many exceptions place a result in the green, yellow and red zones. The market risk backtest analytic reads it. Leave it out when no run does a backtest.',
            'None. The backtest uses no zones.', 'A file name or a path. ORE looks for the file in the input path of the Setup block.',
            '1.8.13.1', null,
            current_user, 'system.initial_load', 'Seed Setup parameter: baselTrafficLightConfig'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'Setup'
          and subtype = '' and name = 'scriptLibrary'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Setup/Parameter[@name=''scriptLibrary'']',
            'Setup', '', 'scriptLibrary', 54, 'string', false,
            'Script library file', 'A file name of the script library.',
            'Names the file that holds the scripts of scripted trades. A scripted trade picks its payoff script by name from this library. Leave it out when the portfolio has no scripted trade.',
            'None. The run loads no scripts.', 'A file name or a path. ORE looks for the file in the input path of the Setup block.',
            '1.8.11.0', null,
            current_user, 'system.initial_load', 'Seed Setup parameter: scriptLibrary'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'Setup'
          and subtype = '' and name = 'nameMappingInputFile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Setup/Parameter[@name=''nameMappingInputFile'']',
            'Setup', '', 'nameMappingInputFile', 55, 'string', false,
            'Name mapping file', 'A file name of the SIMM name mapping.',
            'Names the file that maps the names of issuers and underlyings to the names the SIMM expects. The CRIF analytics read it. The same parameter in the block of an analytic wins, and this one is the last place ORE looks.',
            'None. The run maps no names.', 'A file name or a path. ORE looks for the file in the input path of the Setup block.',
            '1.8.13.0', null,
            current_user, 'system.initial_load', 'Seed Setup parameter: nameMappingInputFile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'Setup'
          and subtype = '' and name = 'bucketMappingInputFile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Setup/Parameter[@name=''bucketMappingInputFile'']',
            'Setup', '', 'bucketMappingInputFile', 56, 'string', false,
            'Bucket mapping file', 'A file name of the SIMM bucket mapping.',
            'Names the file that maps each issuer and underlying to its SIMM bucket. The CRIF analytics read it. The same parameter in the block of an analytic wins, and this one is the last place ORE looks.',
            'None. The run maps no buckets.', 'A file name or a path. ORE looks for the file in the input path of the Setup block.',
            '1.8.13.0', null,
            current_user, 'system.initial_load', 'Seed Setup parameter: bucketMappingInputFile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'Setup'
          and subtype = '' and name = 'allowModelBuilderFallbacks'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Setup/Parameter[@name=''allowModelBuilderFallbacks'']',
            'Setup', '', 'allowModelBuilderFallbacks', 57, 'boolean', false,
            'Allow model builder fallbacks', 'A flag. ORE reads it and does not use it.',
            'The input parameters read this parameter and keep it. No code of the ORE sources uses the value, so the choice has no effect on a run. The intent is to let a model builder use a fallback when its calibration fails.',
            'Yes. The value has no effect.', 'A flag, written Y or N, or true or false.',
            '1.8.14.0', null,
            current_user, 'system.initial_load', 'Seed Setup parameter: allowModelBuilderFallbacks'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'Setup'
          and subtype = '' and name = 'enableScenarioInformation'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Setup/Parameter[@name=''enableScenarioInformation'']',
            'Setup', '', 'enableScenarioInformation', 58, 'boolean', false,
            'Scenario information', 'A flag. Y keeps the information on scenarios.',
            'Decides whether ORE records extra information on each scenario while it runs the valuation engine, for example which risk factor each scenario shifts. The run takes more memory and time when it is Yes. Switch it on only when a report needs that information.',
            'No. ORE records nothing.', 'A flag, written Y or N, or true or false.',
            '1.8.14.0', null,
            current_user, 'system.initial_load', 'Seed Setup parameter: enableScenarioInformation'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'Setup'
          and subtype = '' and name = 'includePastCashflows'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Setup/Parameter[@name=''includePastCashflows'']',
            'Setup', '', 'includePastCashflows', 59, 'boolean', false,
            'Include past cash flows', 'A flag. Y keeps cash flows before the valuation date.',
            'Decides whether the cash flow report lists the cash flows that paid before the valuation date. When it is Yes, the report lists them. When it is No, the report lists only the cash flows from the valuation date on. The cash flow analytic has a parameter of the same name, and the Setup block wins when both are present.',
            'No. The report holds the future cash flows only.', 'A flag, written Y or N, or true or false.',
            '1.8.6.0', null,
            current_user, 'system.initial_load', 'Seed Setup parameter: includePastCashflows'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'Setup'
          and subtype = '' and name = 'reportNaString'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Setup/Parameter[@name=''reportNaString'']',
            'Setup', '', 'reportNaString', 60, 'string', false,
            'Report not-available text', 'A text for a value that is not available.',
            'Sets the text that a report writes in a field that has no value. For example, #N/A is the text a spreadsheet shows for a missing value. Set another text when a program that reads the report needs a different mark.',
            '#N/A', 'A text.',
            '1.8.9.0', null,
            current_user, 'system.initial_load', 'Seed Setup parameter: reportNaString'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'Setup'
          and subtype = '' and name = 'csvSeparator'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Setup/Parameter[@name=''csvSeparator'']',
            'Setup', '', 'csvSeparator', 61, 'string', false,
            'CSV separator', 'A single character that separates fields.',
            'Sets the character that separates the fields of a CSV report. A semicolon helps when a spreadsheet uses the comma as the decimal sign. The text must be one character. For example, ; writes semicolon separated reports.',
            'A comma.', 'One character.',
            '1.8.12.0', null,
            current_user, 'system.initial_load', 'Seed Setup parameter: csvSeparator'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'Setup'
          and subtype = '' and name = 'csvCommentReportHeader'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Setup/Parameter[@name=''csvCommentReportHeader'']',
            'Setup', '', 'csvCommentReportHeader', 62, 'string', false,
            'CSV comment character', 'A single character. It starts the header line of a report.',
            'Sets the character at the start of the header line of a CSV report. A reader that skips lines that start with this character skips the header, which is the choice for a file with no header. The text must be one character. For example, # writes #Trade,NPV and so on.',
            'A hash mark, #.', 'One character.',
            '1.8.12.0', null,
            current_user, 'system.initial_load', 'Seed Setup parameter: csvCommentReportHeader'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'Setup'
          and subtype = '' and name = 'reportBufferSize'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Setup/Parameter[@name=''reportBufferSize'']',
            'Setup', '', 'reportBufferSize', 63, 'integer', false,
            'Report buffer size', 'A number of rows kept in memory.',
            'Sets how many rows ORE keeps in memory for each block of an in-memory report. A buffer keeps the memory use low for a large report. A value of 0 keeps the whole report in one block. For example, 100000 stores a report in blocks of 100000 rows.',
            '0. The whole report stays in memory.', 'A whole number of zero or more.',
            '1.8.13.0', null,
            current_user, 'system.initial_load', 'Seed Setup parameter: reportBufferSize'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'Setup'
          and subtype = '' and name = 'additionalResults'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Setup/Parameter[@name=''additionalResults'']',
            'Setup', '', 'additionalResults', 64, 'boolean', false,
            'Write additional results', 'A flag. Y writes the additional results report.',
            'Decides whether the NPV analytic writes the additional results of each trade, for example the legs, the curves and the model values that the pricing engine reports. The additionalResults block is the place for the same choice. The Setup spelling is the older one. Use Yes to look inside a price.',
            'No. ORE writes no additional results.', 'A flag, written Y or N, or true or false.',
            '1.8.6.0', null,
            current_user, 'system.initial_load', 'Seed Setup parameter: additionalResults'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'Setup'
          and subtype = '' and name = 'accrualDate'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Setup/Parameter[@name=''accrualDate'']',
            'Setup', '', 'accrualDate', 65, 'string', false,
            'Accrual date', 'A date. No release of ORE reads it.',
            'No release of the ORE sources reads this parameter. Some example documents carry it. ORE ignores it. Do not rely on it.',
            'None. ORE ignores the parameter.', 'A date, written as YYYY-MM-DD or YYYYMMDD.',
            '1.8.10.0', null,
            current_user, 'system.initial_load', 'Seed Setup parameter: accrualDate'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'Setup'
          and subtype = '' and name = 'dateCalendar'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Setup/Parameter[@name=''dateCalendar'']',
            'Setup', '', 'dateCalendar', 66, 'string', false,
            'Date calendar', 'A calendar name. No release of ORE reads it.',
            'No release of the ORE sources reads this parameter. Some example documents carry it. ORE ignores it. Do not rely on it.',
            'None. ORE ignores the parameter.', 'A calendar name, for example TARGET.',
            '1.8.10.0', null,
            current_user, 'system.initial_load', 'Seed Setup parameter: dateCalendar'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'Setup'
          and subtype = '' and name = 'dateConvention'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Setup/Parameter[@name=''dateConvention'']',
            'Setup', '', 'dateConvention', 67, 'string', false,
            'Date convention', 'A convention name. No release of ORE reads it.',
            'No release of the ORE sources reads this parameter. Some example documents carry it. ORE ignores it. Do not rely on it.',
            'None. ORE ignores the parameter.', 'A business day convention, for example Following.',
            '1.8.10.0', null,
            current_user, 'system.initial_load', 'Seed Setup parameter: dateConvention'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'Setup'
          and subtype = '' and name = 'defaultMappingToIdentity'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Setup/Parameter[@name=''defaultMappingToIdentity'']',
            'Setup', '', 'defaultMappingToIdentity', 68, 'boolean', false,
            'Default mapping to identity', 'A flag. No release of ORE reads it.',
            'No release of the ORE sources reads this parameter. Some example documents carry it. ORE ignores it. Do not rely on it.',
            'None. ORE ignores the parameter.', 'A flag, written Y or N, or true or false.',
            '1.8.10.0', null,
            current_user, 'system.initial_load', 'Seed Setup parameter: defaultMappingToIdentity'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'Setup'
          and subtype = '' and name = 'useAnalytics'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Setup/Parameter[@name=''useAnalytics'']',
            'Setup', '', 'useAnalytics', 69, 'boolean', false,
            'Use analytics', 'A flag. The latest release no longer reads it.',
            'Earlier releases read this parameter to switch the analytics on. Later releases switch an analytic on with its own active parameter, and they do not read this one. A document that carries it runs the same.',
            'None. The latest release ignores the parameter.', 'A flag, written Y or N, or true or false.',
            '1.8.9.0', '1.8.9.1',
            current_user, 'system.initial_load', 'Seed Setup parameter: useAnalytics'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'Logging'
          and subtype = '' and name = 'logFile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Logging/Parameter[@name=''logFile'']',
            'Logging', '', 'logFile', 1, 'string', false,
            'Log file', 'A file name for the log.',
            'Names the file that ORE writes its log to. The path is relative to the output path. This parameter wins over the parameter of the same name in the Setup block. For example, log.txt writes the log to the file log.txt in the output directory.',
            'None. ORE uses the log file of the Setup block.', 'A file name or a path. ORE writes the file in the output path of the Setup block.',
            '1.8.0.7 or earlier', null,
            current_user, 'system.initial_load', 'Seed Logging parameter: logFile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'Logging'
          and subtype = '' and name = 'logMask'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Logging/Parameter[@name=''logMask'']',
            'Logging', '', 'logMask', 2, 'integer', false,
            'Log detail', 'A number that selects the kinds of message to log.',
            'Selects which messages ORE writes to the log. Each kind of message has one bit: Alert is 1, Critical is 2, Error is 4, Warning is 8, Notice is 16, Debug is 32 and Data is 64. ORE writes a message when its bit is set in the mask. For example, 31 writes alerts, criticals, errors, warnings and notices. 255 writes everything, the most detail and the largest file. This parameter wins over the parameter of the same name in the Setup block.',
            'None. ORE uses the mask of the Setup block.', 'A whole number from 0 to 255.',
            '1.8.2.0', null,
            current_user, 'system.initial_load', 'Seed Logging parameter: logMask'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'Logging'
          and subtype = '' and name = 'progressLogFile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Logging/Parameter[@name=''progressLogFile'']',
            'Logging', '', 'progressLogFile', 3, 'string', false,
            'Progress log file', 'A file name for the progress log.',
            'Names the file that ORE writes the progress log to. The progress log holds short messages in JSON that say how far a run has come, so a program can show progress. The name can hold a Boost pattern such as %N, which is the number of the file after a rotation. For example, my_log_progress_%N.json.',
            'log_progress.json in the output path.', 'A file name or a path. ORE writes the file in the output path of the Setup block.',
            '1.8.11.0', null,
            current_user, 'system.initial_load', 'Seed Logging parameter: progressLogFile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'Logging'
          and subtype = '' and name = 'progressLogRotationSize'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Logging/Parameter[@name=''progressLogRotationSize'']',
            'Logging', '', 'progressLogRotationSize', 4, 'integer', false,
            'Progress log rotation size', 'The size in bytes at which the progress log starts a new file.',
            'Sets the size a progress log file can grow to. When a file reaches it, ORE starts the next file. For example, 10485760 is 10 MiB. A value of 0 sets no limit.',
            '0. The size has no limit.', 'A whole number of bytes, for example 104857600 for 100 MiB.',
            '1.8.11.0', null,
            current_user, 'system.initial_load', 'Seed Logging parameter: progressLogRotationSize'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'Logging'
          and subtype = '' and name = 'progressLogToConsole'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Logging/Parameter[@name=''progressLogToConsole'']',
            'Logging', '', 'progressLogToConsole', 5, 'boolean', false,
            'Progress log to console', 'A flag. Y also writes the progress log to the console.',
            'Decides whether ORE writes the progress log to the standard output as well as to its file. Use Yes when a program starts ORE and reads its output to show progress.',
            'No. ORE writes the progress log to its file only.', 'A flag, written Y or N, or true or false.',
            '1.8.11.0', null,
            current_user, 'system.initial_load', 'Seed Logging parameter: progressLogToConsole'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'Logging'
          and subtype = '' and name = 'structuredLogFile'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Logging/Parameter[@name=''structuredLogFile'']',
            'Logging', '', 'structuredLogFile', 6, 'string', false,
            'Structured log file', 'A file name for the structured log.',
            'Names the file that ORE writes the structured log to. The structured log holds messages about the data of a run, for example a trade that failed to build, in a form a program can read. The name can hold a Boost pattern such as %N, which is the number of the file after a rotation. For example, my_structured_logs_%N.txt.',
            'log_structured.json in the output path.', 'A file name or a path. ORE writes the file in the output path of the Setup block.',
            '1.8.11.0', null,
            current_user, 'system.initial_load', 'Seed Logging parameter: structuredLogFile'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'Logging'
          and subtype = '' and name = 'structuredLogRotationSize'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Logging/Parameter[@name=''structuredLogRotationSize'']',
            'Logging', '', 'structuredLogRotationSize', 7, 'integer', false,
            'Structured log rotation size', 'The size in bytes at which the structured log starts a new file.',
            'Sets the size a structured log file can grow to. When a file reaches it, ORE starts the next file. For example, 10485760 is 10 MiB. A value of 0 sets no limit.',
            '0. The size has no limit.', 'A whole number of bytes, for example 104857600 for 100 MiB.',
            '1.8.11.0', null,
            current_user, 'system.initial_load', 'Seed Logging parameter: structuredLogRotationSize'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'Markets'
          and subtype = '' and name = 'pricing'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Markets/Parameter[@name=''pricing'']',
            'Markets', '', 'pricing', 1, 'string', false,
            'Pricing market', 'A market configuration name.',
            'Names the market configuration that prices the portfolio: the NPV, the cash flows and the pricing step of the other analytics. The configuration is defined in the today''s market file. For example, collateral_eur discounts with the curves of euro collateral.',
            'default. ORE uses the configuration named default.', 'The name of a market configuration of the today''s market file, for example collateral_eur.',
            '1.8.0.7 or earlier', null,
            current_user, 'system.initial_load', 'Seed Markets parameter: pricing'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'Markets'
          and subtype = '' and name = 'simulation'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Markets/Parameter[@name=''simulation'']',
            'Markets', '', 'simulation', 2, 'string', false,
            'Simulation market', 'A market configuration name.',
            'Names the market configuration that the simulation uses to project the market. A cross currency adjusted discount curve is the usual choice, because it prices FX products correctly. For example, collateral_eur.',
            'default. ORE uses the configuration named default.', 'The name of a market configuration of the today''s market file, for example collateral_eur.',
            '1.8.0.7 or earlier', null,
            current_user, 'system.initial_load', 'Seed Markets parameter: simulation'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'Markets'
          and subtype = '' and name = 'lgmcalibration'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Markets/Parameter[@name=''lgmcalibration'']',
            'Markets', '', 'lgmcalibration', 3, 'string', false,
            'Interest rate calibration market', 'A market configuration name.',
            'Names the market configuration that calibrates the interest rate models of the simulation, the LGM models. Local OIS discounting is the usual choice. For example, collateral_inccy discounts each currency with its own OIS curve.',
            'default. ORE uses the configuration named default.', 'The name of a market configuration of the today''s market file, for example collateral_eur.',
            '1.8.0.7 or earlier', null,
            current_user, 'system.initial_load', 'Seed Markets parameter: lgmcalibration'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'Markets'
          and subtype = '' and name = 'fxcalibration'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Markets/Parameter[@name=''fxcalibration'']',
            'Markets', '', 'fxcalibration', 4, 'string', false,
            'FX calibration market', 'A market configuration name.',
            'Names the market configuration that calibrates the FX models of the simulation. For example, collateral_eur.',
            'default. ORE uses the configuration named default.', 'The name of a market configuration of the today''s market file, for example collateral_eur.',
            '1.8.0.7 or earlier', null,
            current_user, 'system.initial_load', 'Seed Markets parameter: fxcalibration'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'Markets'
          and subtype = '' and name = 'eqcalibration'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Markets/Parameter[@name=''eqcalibration'']',
            'Markets', '', 'eqcalibration', 5, 'string', false,
            'Equity calibration market', 'A market configuration name.',
            'Names the market configuration that calibrates the equity models of the simulation. For example, collateral_inccy.',
            'default. ORE uses the configuration named default.', 'The name of a market configuration of the today''s market file, for example collateral_eur.',
            '1.8.2.0', null,
            current_user, 'system.initial_load', 'Seed Markets parameter: eqcalibration'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'Markets'
          and subtype = '' and name = 'infcalibration'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Markets/Parameter[@name=''infcalibration'']',
            'Markets', '', 'infcalibration', 6, 'string', false,
            'Inflation calibration market', 'A market configuration name.',
            'Names the market configuration that calibrates the inflation models of the simulation. For example, collateral_inccy.',
            'default. ORE uses the configuration named default.', 'The name of a market configuration of the today''s market file, for example collateral_eur.',
            '1.8.3.0', null,
            current_user, 'system.initial_load', 'Seed Markets parameter: infcalibration'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'Markets'
          and subtype = '' and name = 'crcalibration'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Markets/Parameter[@name=''crcalibration'']',
            'Markets', '', 'crcalibration', 7, 'string', false,
            'Credit calibration market', 'A market configuration name.',
            'Names the market configuration that calibrates the credit models of the simulation. For example, collateral_inccy.',
            'default. ORE uses the configuration named default.', 'The name of a market configuration of the today''s market file, for example collateral_eur.',
            '1.8.6.0', null,
            current_user, 'system.initial_load', 'Seed Markets parameter: crcalibration'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'Markets'
          and subtype = '' and name = 'sensitivity'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Markets/Parameter[@name=''sensitivity'']',
            'Markets', '', 'sensitivity', 8, 'string', false,
            'Sensitivity market', 'A market configuration name.',
            'Names the market configuration that the sensitivity analysis uses to build the market it shifts. For example, collateral_eur.',
            'default. ORE uses the configuration named default.', 'The name of a market configuration of the today''s market file, for example collateral_eur.',
            '1.8.2.0', null,
            current_user, 'system.initial_load', 'Seed Markets parameter: sensitivity'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = 'Markets'
          and subtype = '' and name = 'bondpricing'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'ore.xsd', '/ORE/Markets/Parameter[@name=''bondpricing'']',
            'Markets', '', 'bondpricing', 9, 'string', false,
            'Bond pricing market', 'A market configuration name. No release of ORE reads it.',
            'No release of the ORE sources reads this parameter. Some example documents carry it to pick a market configuration for bonds. ORE ignores it, so bonds use the pricing market. Do not rely on it.',
            'None. ORE ignores the parameter.', 'The name of a market configuration of the today''s market file, for example collateral_eur.',
            '1.8.10.0', null,
            current_user, 'system.initial_load', 'Seed Markets parameter: bondpricing'
        );
    end if;

end $$;
