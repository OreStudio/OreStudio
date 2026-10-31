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
 * pgTAP tests for a value set that states how many values a document writes.
 *
 * ORE writes a list into one element as a comma separated string, so a set that
 * said only what one value may be could not tell a list from a single value. The
 * kind states one value; the cardinality states how many. A set of cardinality
 * many holds every item to the kind's rule, and an empty item, which is a
 * trailing or doubled comma, is a typo rather than a value.
 * - A list of numbers is in a set of numbers of cardinality many
 * - An item that is not a number leaves the list outside the set
 * - An empty item leaves the list outside the set
 * - A list of periods is in a set of periods of cardinality many
 * - A list that holds a value which is not a period is outside the set
 * - A set of cardinality one does not admit a comma, so a list is not a value
 * - A tenor list states what one item is, and how many
 * - A detachment point list states what one item is, and how many
 *
 * Run with: pg_prove -d <database> test/reporting_ore_parameter_list_value_set_test.sql
 */

begin;

select plan(8);

select is(
    ores_reporting_value_in_set_fn('curve.number_list', '0.01, 0.05, 0.99'),
    true,
    'a list of numbers is in a set of numbers of cardinality many');

select is(
    ores_reporting_value_in_set_fn('curve.number_list', '0.01,not-a-number'),
    false,
    'an item that is not a number leaves the list outside the set');

select is(
    ores_reporting_value_in_set_fn('curve.number_list', '0.01,,0.99'),
    false,
    'an empty item leaves the list outside the set');

select is(
    ores_reporting_value_in_set_fn('curve.period_list', '1Y,2Y,5Y'),
    true,
    'a list of periods is in a set of periods of cardinality many');

select is(
    ores_reporting_value_in_set_fn('curve.period_list', '1Y,2Q'),
    false,
    'a list that holds a value which is not a period is outside the set');

select is(
    ores_reporting_value_in_set_fn('curve.period', '1Y,2Y'),
    false,
    'a set of cardinality one does not admit a comma, so a list is not a value');

-- The map states the item rule as well as the cardinality, so a screen draws a
-- list of periods rather than a box of text.
select is(
    (select value_set_code
       from ores_reporting_ore_parameter_vocabularies_tbl
      where xpath = '/CurveConfiguration/CapFloorVolatilities/CapFloorVolatility/Tenors'
        and valid_to = ores_utility_infinity_timestamp_fn()),
    'curve.period_list',
    'a tenor list states what one item is, and how many');

select is(
    (select value_set_code
       from ores_reporting_ore_parameter_vocabularies_tbl
      where xpath = '/CurveConfiguration/BaseCorrelations/BaseCorrelation/DetachmentPoints'
        and valid_to = ores_utility_infinity_timestamp_fn()),
    'curve.number_list',
    'a detachment point list states what one item is, and how many');

select * from finish();

rollback;
