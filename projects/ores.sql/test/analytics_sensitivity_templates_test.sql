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
 * A template key names the class of product it belongs to and the method that
 * product is priced by, and both are lookups: the class is the reference data's
 * asset class code, and the method is one of the five ORE defines. The key's own
 * spelling stays the tenant's, because ORE resolves the key a sensitivity
 * configuration states.
 *
 * The class is absent exactly on a trade-level template, which is the one method
 * ORE defines without a class.
 *
 * Tests cover:
 * - The seed holds nineteen keys, each stating a method and a class the lookups hold
 * - No template states an ALL class, which the reference data does not have
 * - Every seeded class is stated exactly when its method needs one
 * - A tenant key that does not spell its class and method is accepted
 * - A trade-level key states no class
 * - An asset class the reference data does not hold is refused
 * - A method code the methods table does not hold is refused
 * - A class on a trade-level template is refused
 * - A method that needs a class without one is refused
 *
 * Run with: pg_prove -d <database> test/analytics_sensitivity_templates_test.sql
 */

begin;

select plan(9);

select set_config('app.current_tenant_id', ores_utility_system_tenant_id_fn()::text, true);
select set_config('app.current_actor', 'super_admin', true);

-- Writes a row by copying a seeded one, so the test states only the three
-- columns it is about.
create or replace function pg_temp.write_template(p_code text, p_asset_class_code text,
                                                  p_method_code text)
returns void as $$
declare
    r ores_analytics_sensitivity_templates_tbl;
begin
    select * into r from ores_analytics_sensitivity_templates_tbl
    where tenant_id = ores_utility_system_tenant_id_fn()
      and code = 'IR_Analytical'
      and valid_to = ores_utility_infinity_timestamp_fn();
    r.code := p_code;
    r.asset_class_code := p_asset_class_code;
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
    (select count(*) from ores_analytics_sensitivity_templates_tbl t
     where t.tenant_id = ores_utility_system_tenant_id_fn()
       and t.valid_to = ores_utility_infinity_timestamp_fn()
       and (t.code = ''
            or not exists (
                select 1 from ores_analytics_sensitivity_methods_tbl m
                where m.tenant_id = ores_utility_system_tenant_id_fn()
                  and m.code = t.method_code
                  and m.valid_to = ores_utility_infinity_timestamp_fn())
            or (t.asset_class_code is not null and not exists (
                select 1 from ores_analytics_sensitivity_asset_classes_tbl a
                where a.tenant_id = ores_utility_system_tenant_id_fn()
                  and a.asset_class_code = t.asset_class_code
                  and a.valid_to = ores_utility_infinity_timestamp_fn())))),
    0::bigint,
    'every seeded row states a key, a method and a class the lookups hold');

select is(
    (select count(*) from ores_analytics_sensitivity_templates_tbl
     where tenant_id = ores_utility_system_tenant_id_fn()
       and valid_to = ores_utility_infinity_timestamp_fn()
       and asset_class_code = 'ALL'),
    0::bigint,
    'no template states an ALL asset class, because the reference data has none');

select is(
    (select count(*) from ores_analytics_sensitivity_templates_tbl t
     join ores_analytics_sensitivity_methods_tbl m
       on m.tenant_id = ores_utility_system_tenant_id_fn()
      and m.code = t.method_code
      and m.valid_to = ores_utility_infinity_timestamp_fn()
     where t.tenant_id = ores_utility_system_tenant_id_fn()
       and t.valid_to = ores_utility_infinity_timestamp_fn()
       and m.needs_asset_class <> (t.asset_class_code is not null)),
    0::bigint,
    'every seeded row states a class exactly when its method needs one');

-- =============================================================================
-- The key's spelling is the tenant's
-- =============================================================================

select lives_ok(
    $$select pg_temp.write_template('ACME_Grid', 'interest_rates', 'FD')$$,
    'a tenant adds a key that does not spell its class and method');

select lives_ok(
    $$select pg_temp.write_template('ACME_Trade', null, 'Trade')$$,
    'a tenant adds a trade-level key, which states no class');

-- =============================================================================
-- The metadata is not
-- =============================================================================

select throws_ok(
    $$select pg_temp.write_template('IR_BadClass', 'rates', 'Analytical')$$,
    '23503', null, 'the reference data refuses an asset class code it does not hold');

select throws_ok(
    $$select pg_temp.write_template('IR_BadCode', 'interest_rates', 'Closed form')$$,
    '23503', null, 'the methods table refuses a method code it does not hold');

select throws_ok(
    $$select pg_temp.write_template('IR_TradeWithClass', 'interest_rates', 'Trade')$$,
    '23514', null, 'the table refuses a class on a trade-level template');

select throws_ok(
    $$select pg_temp.write_template('IR_NoClass', null, 'Analytical')$$,
    '23514', null, 'the table refuses a method that needs a class without one');

select * from finish();

rollback;
