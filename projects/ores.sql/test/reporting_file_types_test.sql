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
 * pgTAP tests for the file types lookup.
 *
 * The table gives each file of the ORE universe its well known name:
 * - The seed holds the well known name of the files every sample names
 * - Each seeded file type refers to an analytic type or to the Setup block, and a
 *   configuration type that exists
 * - A name may be a pattern with a key, and a pattern with a malformed key is refused
 * - A file type has a known kind and a parameter, and cannot be used by more samples than
 *   name it
 * - Each file type names the ORE file it is, which states the format and the XSD, and the
 *   parameters that write and read one file all name the same ORE file
 * - The cube and the scenario data that a simulation writes are read by xva and pfe under the
 *   name the simulation gives them, and the compressed and binary files are named too
 * - A configuration type has one Setup file, so the export finds the file of a bound
 *   document by its type whatever the order of the rows
 * - Two file types cannot name the same file: the analytic and the parameter are unique
 *
 * The seed itself is compared with the ORE samples by
 * projects/ores.sql/utility/generate_file_types_populate.py --check.
 *
 * Run with: pg_prove -d <database> test/reporting_file_types_test.sql
 */

begin;

select plan(25);

select set_config('app.current_tenant_id', ores_utility_system_tenant_id_fn()::text, true);

create temp view t_live as
select * from ores_reporting_file_types_tbl
where tenant_id = ores_utility_system_tenant_id_fn()
  and valid_to = ores_utility_infinity_timestamp_fn();

select results_eq(
    $$select code, name_pattern from t_live
      where code in ('setup.conventionsFile', 'setup.curveConfigFile', 'setup.marketConfigFile',
                     'setup.pricingEnginesFile', 'xva.csaFile', 'xva.collateralBalancesFile',
                     'npv.outputFileName', 'cashflow.outputFileName')
      order by code$$,
    $$values ('cashflow.outputFileName'::text, 'flows.csv'::text),
             ('npv.outputFileName', 'npv.csv'),
             ('setup.conventionsFile', 'conventions.xml'),
             ('setup.curveConfigFile', 'curveconfig.xml'),
             ('setup.marketConfigFile', 'todaysmarket.xml'),
             ('setup.pricingEnginesFile', 'pricingengine.xml'),
             ('xva.collateralBalancesFile', 'collateralbalances.xml'),
             ('xva.csaFile', 'netting.xml')$$,
    'the seed gives the common files the names the samples use');

select cmp_ok((select count(*) from t_live), '>=', 100::bigint,
    'the seed holds a file type for every file the samples name');

select is(
    (select count(*) from t_live where kind not in
        ('input_document', 'data_file', 'output_file', 'intermediate_file')),
    0::bigint,
    'every seeded file type has a known kind');

select is(
    (select count(*) from t_live f
     where f.analytic_type_code is not null
       and not exists (select 1 from ores_reporting_analytic_types_tbl a
                       where a.tenant_id = f.tenant_id and a.code = f.analytic_type_code
                         and a.valid_to = ores_utility_infinity_timestamp_fn())),
    0::bigint,
    'every seeded file type refers to an analytic type that exists');

select results_eq(
    $$select code, configuration_type_code from t_live
      where configuration_type_code is not null order by code$$,
    $$values ('setup.conventionsFile'::text, 'conventions'::text),
             ('setup.curveConfigFile', 'curve_configuration'),
             ('setup.marketConfigFile', 'todays_market'),
             ('setup.pricingEnginesFile', 'pricing_engines')$$,
    'the Setup documents that a configuration type describes name that type');

select is(
    (select count(*) from t_live where name_pattern ~ '\d{8}'),
    0::bigint,
    'no seeded name embeds a date');

select is(
    (select count(*) from t_live where sample_count > sample_total or sample_count < 0),
    0::bigint,
    'each name records how many samples support it, and no more than name the file');

select results_eq(
    $$select code, name_pattern, produced_by_file_type_code from t_live
      where code in ('simulation.cubeFile', 'xva.cubeFile', 'pfe.cubeFile')
      order by code$$,
    $$values ('pfe.cubeFile'::text, 'cube.csv.gz'::text, 'simulation.cubeFile'::text),
             ('simulation.cubeFile', 'cube.csv.gz', null),
             ('xva.cubeFile', 'cube.csv.gz', 'simulation.cubeFile')$$,
    'a reader of the cube is linked to the simulation that writes it');

select is(
    (select count(*) from t_live r
     where r.produced_by_file_type_code is not null
       and not exists (select 1 from t_live w where w.code = r.produced_by_file_type_code
                       and w.kind = 'output_file')),
    0::bigint,
    'every file read from a writer is linked to an output file');

select cmp_ok(
    (select count(*) from t_live where name_pattern ~ '\.(gz|dat)$'), '>=', 4::bigint,
    'the compressed and binary files of the samples are named');

select is(
    (select count(*) from t_live where direction = 'output' and kind <> 'output_file'),
    0::bigint,
    'only an output file is an output');

select is(
    (select count(*) from t_live where direction = 'input' and kind = 'output_file'),
    0::bigint,
    'every output file is an output');

select results_eq(
    $$select t.code, t.ore_file_code, f.format, x.typical_filename
        from t_live t
        join ores_reporting_ore_files_tbl f
          on f.tenant_id = t.tenant_id and f.code = t.ore_file_code
         and f.valid_to = ores_utility_infinity_timestamp_fn()
        left join ores_reporting_ore_files_tbl x
          on x.tenant_id = f.tenant_id and x.code = f.schema_ore_file_code
         and x.valid_to = ores_utility_infinity_timestamp_fn()
       where t.code in ('setup.conventionsFile', 'xva.csaFile', 'xva.collateralBalancesFile',
                        'setup.fixingDataFile', 'xva.cubeFile', 'npv.outputFileName')
       order by t.code$$,
    $$values ('npv.outputFileName'::text, 'npv_report'::text, 'csv'::text, null::text),
             ('setup.conventionsFile', 'conventions', 'xml', 'conventions.xsd'),
             ('setup.fixingDataFile', 'fixing_history', 'txt', null),
             ('xva.collateralBalancesFile', 'collateral_balances', 'xml', 'collateralbalance.xsd'),
             ('xva.csaFile', 'netting_set_definitions', 'xml', 'nettingsetdefinitions.xsd'),
             ('xva.cubeFile', 'npv_cube', 'csv_gz', null)$$,
    'each file type names the ORE file it is, which states the format and names the schema');

select results_eq(
    $$select code, ore_file_code from t_live
       where code in ('simulation.cubeFile', 'xva.cubeFile', 'pfe.cubeFile') order by code$$,
    $$values ('pfe.cubeFile'::text, 'npv_cube'::text),
             ('simulation.cubeFile', 'npv_cube'),
             ('xva.cubeFile', 'npv_cube')$$,
    'the parameters that write and read one file all name the same ORE file');

select cmp_ok(
    (select min(length(description)) from t_live), '>=', 120,
    'every file type says in some detail what the file is for');

create or replace function pg_temp.add_type(p_code text, p_pattern text) returns void as $$
    insert into ores_reporting_file_types_tbl (
        code, tenant_id, version, analytic_type_code, ore_parameter, direction, ore_file_code, kind,
        configuration_type_code, name_pattern, sample_count, sample_total, description,
        display_order, modified_by, performed_by, change_reason_code, change_commentary)
    values (p_code, ores_utility_system_tenant_id_fn(), 0, null, 'testSplitFile', 'input', 'trade_data',
            'data_file',
            null, p_pattern, 1, 1, 'A test file type.', 999,
            current_user, current_user, 'system.test', 'pgTAP fixture');
$$ language sql;

select lives_ok(
    $$select pg_temp.add_type('test.split', 'portfolio_{{BOOK_ID}}.xml')$$,
    'a name may be a pattern with a key');

select throws_ok(
    $$select pg_temp.add_type('test.badkey', 'portfolio_{{book id}}.xml')$$,
    '23514', null,
    'a pattern with a malformed key is refused');

select throws_ok(
    $$select pg_temp.add_type('test.unclosed', 'portfolio_{{BOOK_ID.xml')$$,
    '23514', null,
    'a pattern with an unclosed key is refused');

select throws_ok(
    $$insert into ores_reporting_file_types_tbl (
          code, tenant_id, version, analytic_type_code, ore_parameter, direction, ore_file_code, kind,
          configuration_type_code, name_pattern, sample_count, sample_total, description,
          display_order, modified_by, performed_by, change_reason_code, change_commentary)
      values ('test.nokind', ores_utility_system_tenant_id_fn(), 0, null, 'x', 'input', 'trade_data', 'folder',
              null, 'x.xml', 1, 1, 'A test file type.', 999,
              current_user, current_user, 'system.test', 'pgTAP fixture')$$,
    '23514', null,
    'a file type of an unknown kind is refused');

select throws_ok(
    $$insert into ores_reporting_file_types_tbl (
          code, tenant_id, version, analytic_type_code, ore_parameter, direction, ore_file_code, kind,
          configuration_type_code, name_pattern, sample_count, sample_total, description,
          display_order, modified_by, performed_by, change_reason_code, change_commentary)
      values ('test.noparam', ores_utility_system_tenant_id_fn(), 0, null, '', 'input', 'trade_data', 'data_file',
              null, 'x.xml', 1, 1, 'A test file type.', 999,
              current_user, current_user, 'system.test', 'pgTAP fixture')$$,
    '23514', null,
    'a file type with no ORE parameter is refused');

select throws_ok(
    $$insert into ores_reporting_file_types_tbl (
          code, tenant_id, version, analytic_type_code, ore_parameter, direction, ore_file_code, kind,
          configuration_type_code, name_pattern, sample_count, sample_total, description,
          display_order, modified_by, performed_by, change_reason_code, change_commentary)
      values ('test.duplicate', ores_utility_system_tenant_id_fn(), 0, null, 'conventionsFile',
              'input', 'conventions', 'input_document', null, 'other.xml', 1, 1, 'A test file type.', 999,
              current_user, current_user, 'system.test', 'pgTAP fixture')$$,
    '23505', null,
    'a second file type for the same analytic and parameter is refused');

select throws_ok(
    $$insert into ores_reporting_file_types_tbl (
          code, tenant_id, version, analytic_type_code, ore_parameter, direction, ore_file_code, kind,
          configuration_type_code, name_pattern, sample_count, sample_total, description,
          display_order, modified_by, performed_by, change_reason_code, change_commentary)
      values ('test.baddirection', ores_utility_system_tenant_id_fn(), 0, null, 'x', 'sideways',
              'trade_data', 'data_file', null, 'x.xml', 1, 1, 'A test file type.', 999,
              current_user, current_user, 'system.test', 'pgTAP fixture')$$,
    '23514', null,
    'a file type that is neither an input nor an output is refused');

select throws_like(
    $$insert into ores_reporting_file_types_tbl (
          code, tenant_id, version, analytic_type_code, ore_parameter, direction, ore_file_code, kind,
          configuration_type_code, name_pattern, sample_count, sample_total, description,
          display_order, modified_by, performed_by, change_reason_code, change_commentary)
      values ('test.nofile', ores_utility_system_tenant_id_fn(), 0, null, 'x', 'input',
              'no_such_file', 'data_file', null, 'x.xml', 1, 1, 'A test file type.', 999,
              current_user, current_user, 'system.test', 'pgTAP fixture')$$,
    '%No active ORE file found%',
    'a file type that names no ORE file is refused');

select throws_ok(
    $$insert into ores_reporting_file_types_tbl (
          code, tenant_id, version, analytic_type_code, ore_parameter, direction, ore_file_code, kind,
          configuration_type_code, name_pattern, sample_count, sample_total, description,
          display_order, modified_by, performed_by, change_reason_code, change_commentary)
      values ('test.secondsetup', ores_utility_system_tenant_id_fn(), 0, null, 'otherFile',
              'input', 'conventions', 'input_document', 'conventions', 'other.xml', 1, 1,
              'A test file type.', 999,
              current_user, current_user, 'system.test', 'pgTAP fixture')$$,
    '23505', null,
    'a second Setup file for the same configuration type is refused');

select is(
    (select count(*) from (
        select configuration_type_code from t_live
        where analytic_type_code is null and configuration_type_code is not null
        group by configuration_type_code having count(*) > 1) d),
    0::bigint,
    'every configuration type has one Setup file at most in the seed');

select * from finish();
rollback;
