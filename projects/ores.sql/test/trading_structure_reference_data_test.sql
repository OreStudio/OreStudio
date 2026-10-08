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
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51
 * Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */

/**
 * pgTAP tests for the structure reference data.
 *
 * Tests cover:
 * - The four rungs of the composition ladder and which of them confirms as a
 *   whole
 * - A template names a kind from the catalogue
 * - A role names a template, and a template states each role once
 * - A role's leg counts are not negative and are in order
 *
 * Run with: pg_prove -d <database> test/trading_structure_reference_data_test.sql
 */

begin;

select plan(9);

select set_config('app.current_tenant_id', ores_utility_system_tenant_id_fn()::text, true);

create temp view t_structure_kinds as
select code, confirms_as_whole
from ores_trading_structure_kinds_tbl
where tenant_id = ores_utility_system_tenant_id_fn()
  and valid_to = ores_utility_infinity_timestamp_fn();

create temp view t_template_roles as
select template_code, role, min_legs, max_legs
from ores_trading_structure_template_roles_tbl
where tenant_id = ores_utility_system_tenant_id_fn()
  and valid_to = ores_utility_infinity_timestamp_fn();

create or replace function pg_temp.template(p_code text, p_kind text)
returns void as $$
    insert into ores_trading_structure_templates_tbl (code, tenant_id, version,
        description, kind, modified_by, change_reason_code, change_commentary)
    values (p_code, ores_utility_system_tenant_id_fn(), 0, 'test', p_kind,
        current_user, 'system.new_record', 'test');
$$ language sql;

create or replace function pg_temp.role(p_template text, p_role text, p_min integer,
    p_max integer)
returns void as $$
    insert into ores_trading_structure_template_roles_tbl (template_code, role, tenant_id,
        version, min_legs, max_legs, description, modified_by, change_reason_code,
        change_commentary)
    values (p_template, p_role, ores_utility_system_tenant_id_fn(), 0, p_min, p_max, 'test',
        current_user, 'system.new_record', 'test');
$$ language sql;

-- =============================================================================
-- The ladder
-- =============================================================================

select results_eq(
    $$select code, confirms_as_whole from t_structure_kinds order by code$$,
    $$values ('Dynamic', true), ('Package', false), ('Strategy', true), ('Typed', true)$$,
    'a package confirms trade by trade; the rungs above it confirm as a whole');

select is(
    (select count(*) from t_structure_kinds),
    4::bigint,
    'the seeded ladder has four rungs');

select results_eq(
    $$select role, min_legs, max_legs from t_template_roles
      where template_code = 'Butterfly' order by role$$,
    $$values ('body', 1, 1), ('wing', 2, 2)$$,
    'a butterfly is a body of one and two wings of two');

select is(
    (select count(*) from ores_trading_structure_templates_tbl
     where tenant_id = ores_utility_system_tenant_id_fn()
       and valid_to = ores_utility_infinity_timestamp_fn()),
    5::bigint,
    'the seeded templates are the four strategies and one typed product');

-- =============================================================================
-- What a template and a role refuse
-- =============================================================================

select throws_ok(
    $$select pg_temp.template('TestTemplate', 'Nonsense')$$,
    '23503', null,
    'a template naming an unknown kind is refused');

select throws_ok(
    $$select pg_temp.role('NoSuchTemplate', 'leg', 1, 1)$$,
    '23503', null,
    'a role naming an unknown template is refused');

select throws_ok(
    $$select pg_temp.role('Straddle', 'test', -1, 1)$$,
    '23514', null,
    'a role holding a negative number of legs is refused');

select throws_ok(
    $$select pg_temp.role('Straddle', 'test', 2, 1)$$,
    '23514', null,
    'a role whose maximum is below its minimum is refused');

select throws_ok(
    $$select pg_temp.role('Butterfly', 'body', 1, 1)$$,
    '23505', null,
    'a template states each role once');

select * from finish();

rollback;
