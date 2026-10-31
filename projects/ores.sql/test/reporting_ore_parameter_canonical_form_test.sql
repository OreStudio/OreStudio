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
 * pgTAP tests for the canonical form a value set states.
 *
 * ORE reads several renderings for one fact: NONE, INDIFF and NotApplicable all
 * mean Unadjusted, and A360, Actual/360 and Act/360 all mean ACT/360. A set that
 * listed them as separate values offered the same fact many times. A row that
 * names a canonical form is a rendering of one; a row that names none is the
 * form a configuration holds.
 * - No row names itself as its canonical form
 * - Every row that names a canonical form names a row the same set holds
 * - Every canonical form a row names is a row of its own, with no canonical value
 * - The day counter set offers 23 choices and states 52 renderings
 * - The business day convention set offers 7 choices and states 20 renderings
 * - The frequency set offers Once, and Z only as a rendering of it
 * - A setting's choices leave the renderings out
 *
 * Run with: pg_prove -d <database> test/reporting_ore_parameter_canonical_form_test.sql
 */

begin;

select plan(9);

create temporary table t_values on commit drop as
select v.value_set_code, v.value, v.canonical_value
from ores_reporting_ore_parameter_value_set_values_tbl v
where v.valid_to = ores_utility_infinity_timestamp_fn()
  and v.tenant_id = ores_utility_system_tenant_id_fn();

select is_empty(
    $$select value_set_code, value from t_values where canonical_value = value$$,
    'no row names itself as its canonical form');

select is_empty(
    $$select v.value_set_code, v.value, v.canonical_value
        from t_values v
       where v.canonical_value is not null
         and not exists (select 1 from t_values c
                          where c.value_set_code = v.value_set_code
                            and c.value = v.canonical_value)$$,
    'every row that names a canonical form names a row the same set holds');

select is_empty(
    $$select v.value_set_code, v.value, v.canonical_value
        from t_values v
       where v.canonical_value is not null
         and exists (select 1 from t_values c
                      where c.value_set_code = v.value_set_code
                        and c.value = v.canonical_value
                        and c.canonical_value is not null)$$,
    'every canonical form a row names is a row of its own, with no canonical value');

select is(
    (select count(*)::int from t_values
      where value_set_code = 'convention.day_counter'
        and canonical_value is null),
    23,
    'the day counter set offers the 23 forms a configuration holds');

select is(
    (select count(*)::int from t_values
      where value_set_code = 'convention.day_counter'
        and canonical_value is not null),
    52,
    'the day counter set states 52 renderings of them');

select is(
    (select count(*)::int from t_values
      where value_set_code = 'convention.business_day_convention'
        and canonical_value is null),
    7,
    'the business day convention set offers the 7 conventions');

select is(
    (select count(*)::int from t_values
      where value_set_code = 'convention.business_day_convention'
        and canonical_value is not null),
    20,
    'the business day convention set states 20 renderings of them');

select is(
    (select canonical_value from t_values
      where value_set_code = 'convention.frequency'
        and value = 'Z'),
    'Once',
    'the frequency set reads Z as Once, which is the form a configuration holds');

-- A setting's choices are what a control offers, so a rendering is not one. The
-- day counter set governs many settings; this reads the one the contract states.
select is_empty(
    $$select p.name, p.choices
        from ores_reporting_ore_parameter_settings_v p
       where p.value_set_code = 'convention.day_counter'
         and p.choices && array['A360', 'Act/360', 'Actual/360']::text[]$$,
    'a setting''s choices leave the renderings out');

select * from finish();

rollback;
