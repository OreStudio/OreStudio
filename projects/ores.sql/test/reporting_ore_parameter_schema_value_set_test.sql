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
 * pgTAP tests for the value sets the ORE schema states.
 *
 * The schema restricts a simple type with a pattern, a range or a list of
 * values, and each of those is a rule the model states rather than a shape a
 * screen has to know. The pattern is anchored, because a pattern states what the
 * whole value must be and Postgres matches one anywhere in a string otherwise.
 * - A date leaf takes the set the schema's date pattern states
 * - A date the schema's pattern allows is in the set
 * - A date whose shape the pattern refuses is not
 * - A whole number bounded by the schema accepts a value inside the bounds
 * - A value outside the bounds is not in the set
 * - A joint calendar the schema's pattern allows is in the calendar set
 *
 * Run with: pg_prove -d <database> test/reporting_ore_parameter_schema_value_set_test.sql
 */

begin;

select plan(6);

select is(
    (select value_set_code
       from ores_reporting_ore_parameter_vocabularies_tbl
      where xpath = '/CurveConfiguration/BaseCorrelations/BaseCorrelation/StartDate'
        and valid_to = ores_utility_infinity_timestamp_fn()),
    'curve.date',
    'a date leaf takes the set the schema date pattern states');

select is(
    ores_reporting_value_in_set_fn('curve.date', '2024-01-31'),
    true,
    'a date the schema pattern allows is in the set');

-- The schema says its own date pattern "is not super strict, e.g. 2016-17-39
-- would pass, but it catches most errors", so the set refuses a shape the schema
-- refuses rather than a month it admits.
select is(
    ores_reporting_value_in_set_fn('curve.date', '2024-1-31'),
    false,
    'a date whose shape the pattern refuses is not in the set');

select is(
    ores_reporting_value_in_set_fn('convention.day_of_month', '31'),
    true,
    'a whole number bounded by the schema accepts a value inside the bounds');

select is(
    ores_reporting_value_in_set_fn('convention.day_of_month', '32'),
    false,
    'a value outside the bounds is not in the set');

select is(
    ores_reporting_value_in_set_fn('curve.calendar', 'US,MWK'),
    true,
    'a joint calendar the schema pattern allows is in the calendar set');

select * from finish();

rollback;
