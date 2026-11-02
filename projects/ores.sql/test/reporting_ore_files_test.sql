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
 * pgTAP tests for the ORE files and their categories.
 *
 * An ORE file is a file of a run as the ORE user guide identifies it:
 * - The seed holds every category and every file, each described at length
 * - Every file belongs to a category and comes from where its category says
 * - A file is recognised by a regular expression anchored at both ends that its
 *   typical name matches, and no typical name matches a second file
 * - Only an xml file has an XSD, and a configuration document names its type
 * - A file of an unknown category, origin or format is refused
 *
 * The seed itself is compared with ore_files.py by
 * projects/ores.sql/utility/generate_file_types_populate.py --check.
 *
 * Run with: pg_prove -d <database> test/reporting_ore_files_test.sql
 */

begin;

select plan(12);

select set_config('app.current_tenant_id', ores_utility_system_tenant_id_fn()::text, true);

create temp view t_files as
select * from ores_reporting_ore_files_tbl
where tenant_id = ores_utility_system_tenant_id_fn()
  and valid_to = ores_utility_infinity_timestamp_fn();

create temp view t_categories as
select * from ores_reporting_ore_file_categories_tbl
where tenant_id = ores_utility_system_tenant_id_fn()
  and valid_to = ores_utility_infinity_timestamp_fn();

select is((select count(*) from t_categories), 13::bigint, 'the seed holds thirteen categories');

select is((select count(*) from t_files), 124::bigint, 'the seed holds every ORE file');

select is(
    (select count(*) from t_files f
      where not exists (select 1 from t_categories c
                         where c.code = f.category_code and c.origin = f.origin)),
    0::bigint,
    'every file belongs to a category and comes from where its category says');

select results_eq(
    $$select code, category_code, origin, format, typical_filename from t_files
       where code in ('master_input', 'market_data', 'npv_report', 'npv_cube', 'input_package')
       order by code$$,
    $$values ('input_package'::text, 'packaging'::text, 'packaged'::text, 'tar_gz'::text,
              'ore_package.tar.gz'::text),
             ('market_data', 'market_data', 'supplied', 'txt', 'market.txt'),
             ('master_input', 'master_input', 'supplied', 'xml', 'ore.xml'),
             ('npv_cube', 'intermediate_data', 'produced', 'csv_gz', 'cube.csv.gz'),
             ('npv_report', 'valuation_report', 'produced', 'csv', 'npv.csv')$$,
    'the files a run always has are named as the guide names them');

select cmp_ok(
    (select min(length(description)) from t_files), '>=', 150,
    'every file says at length what it is for and how it is laid out');

select cmp_ok(
    (select min(length(description)) from t_categories), '>=', 120,
    'every category says what its files have in common');

select is(
    (select count(*) from t_files
      where filename_regex !~ '^\^.*\$$' or typical_filename !~ filename_regex),
    0::bigint,
    'every regular expression is anchored and matches its own typical name');

select is(
    (select count(*) from t_files a join t_files b
       on a.code <> b.code and a.typical_filename ~ b.filename_regex),
    0::bigint,
    'no typical name is recognised as a second file');

select is(
    (select count(*) from t_files where format <> 'xml' and xsd_file is not null),
    0::bigint,
    'only an xml file has an XSD');

select is(
    (select count(*) from t_files
      where category_code = 'configuration' and configuration_type_code is null
        and code not in ('crif_market', 'script_library')),
    0::bigint,
    'a configuration document names its configuration type, unless no type describes it');

create or replace function pg_temp.add_file(p_category text, p_format text) returns void as $$
    insert into ores_reporting_ore_files_tbl (
        code, tenant_id, version, name, category_code, configuration_type_code, guide_section,
        origin, format, xsd_file, typical_filename, filename_regex, description, display_order,
        modified_by, performed_by, change_reason_code, change_commentary)
    values ('test_file', ores_utility_system_tenant_id_fn(), 0, 'Test File', p_category, null,
            null, 'produced', p_format, null, 'test.csv', '^test\.csv$', 'A test file.', 999,
            current_user, current_user, 'system.test', 'pgTAP fixture');
$$ language sql;

select throws_like(
    $$select pg_temp.add_file('no_such_category', 'csv')$$,
    '%No active ORE file category found%',
    'a file of an unknown category is refused');

select throws_ok(
    $$select pg_temp.add_file('valuation_report', 'pdf')$$,
    '23514', null,
    'a file of an unknown format is refused');

select * from finish();
rollback;
