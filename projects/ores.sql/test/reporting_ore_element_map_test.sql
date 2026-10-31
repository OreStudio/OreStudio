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
 * this program; if not, write to the Free Software Foundation, Inc., 51 Franklin
 * Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */
/**
 * pgTAP tests for the shape of the ORE element map.
 *
 * The map is one row per leaf and its child is one row per projection column, so
 * the counts state the coverage the task set out to prove: 480 leaves over 482
 * columns. A leaf no column carries is dead data, and a column that names no leaf
 * is a projection nobody rules, so both directions are asserted as well as the
 * counts.
 * - The map carries 480 leaves
 * - The child carries 482 columns
 * - Every column names a leaf the map holds
 * - Every leaf is carried by at least one column
 * - Every row names the authority that decided its rule
 * - No two rows share a leaf's address
 *
 * Run with: pg_prove -d <database> test/reporting_ore_element_map_test.sql
 */

begin;

select plan(6);

select is(
    (select count(*)::int from ores_reporting_ore_parameter_vocabularies_tbl
      where valid_to = ores_utility_infinity_timestamp_fn()),
    480,
    'the map carries 480 leaves');

select is(
    (select count(*)::int from ores_reporting_ore_element_map_columns_tbl
      where valid_to = ores_utility_infinity_timestamp_fn()),
    482,
    'the child carries 482 columns');

select is_empty(
    $$select k.code, k.element_map_code
        from ores_reporting_ore_element_map_columns_tbl k
       where k.valid_to = ores_utility_infinity_timestamp_fn()
         and not exists (
             select 1 from ores_reporting_ore_parameter_vocabularies_tbl m
              where m.code = k.element_map_code
                and m.valid_to = ores_utility_infinity_timestamp_fn())$$,
    'every column names a leaf the map holds');

select is_empty(
    $$select m.code from ores_reporting_ore_parameter_vocabularies_tbl m
       where m.valid_to = ores_utility_infinity_timestamp_fn()
         and not exists (
             select 1 from ores_reporting_ore_element_map_columns_tbl k
              where k.element_map_code = m.code
                and k.valid_to = ores_utility_infinity_timestamp_fn())$$,
    'every leaf is carried by at least one column');

select is_empty(
    $$select code, xpath from ores_reporting_ore_parameter_vocabularies_tbl
       where valid_to = ores_utility_infinity_timestamp_fn()
         and source not in ('ore_code', 'ore_guide', 'ore_xsd')$$,
    'every row names the authority that decided its rule');

select is_empty(
    $$select schema_name, xpath, count(*) from ores_reporting_ore_parameter_vocabularies_tbl
       where valid_to = ores_utility_infinity_timestamp_fn()
       group by schema_name, xpath having count(*) > 1$$,
    'no two rows share a leaf address');

select * from finish();

rollback;
