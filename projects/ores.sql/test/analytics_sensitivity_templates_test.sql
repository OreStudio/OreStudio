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
 * pgTAP tests for the metadata a sensitivity template row states.
 *
 * A template key names an asset class and a method, and ORE resolves the key a
 * sensitivity configuration states. The row must therefore hold a valid asset
 * class, a valid method code and one of ORE's pairs, while the spelling of the
 * key itself stays the tenant's.
 *
 * Tests cover:
 * - Every seeded row states a class, a code and a pair the table allows
 * - A key that does not spell the pair is accepted, because a tenant may add one
 * - An asset class that is not ORE's is refused
 * - A method code that is not ORE's is refused
 * - A pair that ORE does not have is refused
 * - A blank key is refused
 *
 * Run with: pg_prove -d <database> test/analytics_sensitivity_templates_test.sql
 */

begin;

select plan(7);

select set_config('app.current_tenant_id', ores_utility_system_tenant_id_fn()::text, true);
select set_config('app.current_actor', 'super_admin', true);

-- Writes a row by copying a seeded one, so the test states only the four
-- columns it is about.
create or replace function pg_temp.write_template(p_code text, p_asset_class text,
                                                  p_method text, p_method_code text)
returns void as $$
declare
    r ores_analytics_sensitivity_templates_tbl;
begin
    select * into r from ores_analytics_sensitivity_templates_tbl
    where tenant_id = ores_utility_system_tenant_id_fn()
      and code = 'IR_Analytical'
      and valid_to = ores_utility_infinity_timestamp_fn();
    r.code := p_code;
    r.asset_class := p_asset_class;
    r.method := p_method;
    r.method_code := p_method_code;
    r.version := 0;
    r.modified_by := 'super_admin';
    r.performed_by := 'super_admin';
    insert into ores_analytics_sensitivity_templates_tbl select (r).*;
end;
$$ language plpgsql;

-- =============================================================================
-- The seed
-- =============================================================================

select is(
    (select count(*) from ores_analytics_sensitivity_templates_tbl
     where tenant_id = ores_utility_system_tenant_id_fn()
       and valid_to = ores_utility_infinity_timestamp_fn()),
    19::bigint,
    'the seed holds the nineteen template keys ORE examples use');

select is(
    (select count(*) from ores_analytics_sensitivity_templates_tbl
     where tenant_id = ores_utility_system_tenant_id_fn()
       and valid_to = ores_utility_infinity_timestamp_fn()
       and (asset_class not in ('IR', 'FX', 'EQ', 'COMM', 'CR', 'ALL')
            or code = ''
            or (method, method_code) not in (
                ('Analytical', 'Analytical'), ('Semianalytical', 'Semianalytical'),
                ('Finite difference', 'FD'), ('Monte Carlo', 'MC'),
                ('Trade level', 'Trade')))),
    0::bigint,
    'every seeded row states a class, a key and a pair the table allows');

-- =============================================================================
-- The key's spelling is the tenant's
-- =============================================================================

select lives_ok(
    $$select pg_temp.write_template('ACME_Grid', 'IR', 'Finite difference', 'FD')$$,
    'a tenant adds a key that does not spell its class and method');

-- =============================================================================
-- The metadata is not
-- =============================================================================

select throws_ok(
    $$select pg_temp.write_template('IR_BadClass', 'RATES', 'Analytical', 'Analytical')$$,
    '23514', null, 'the table refuses an asset class ORE does not have');

select throws_ok(
    $$select pg_temp.write_template('IR_BadCode', 'IR', 'Analytical', 'Closed form')$$,
    '23514', null, 'the table refuses a method code ORE does not have');

select throws_ok(
    $$select pg_temp.write_template('IR_Mismatched', 'IR', 'Finite difference', 'Analytical')$$,
    '23514', null, 'the table refuses a method its code does not state');

select throws_ok(
    $$select pg_temp.write_template('', 'IR', 'Analytical', 'Analytical')$$,
    '23514', null, 'the table refuses a blank key');

select * from finish();

rollback;
