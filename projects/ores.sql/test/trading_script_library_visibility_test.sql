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
 * pgTAP tests for the script library a tenant reads from the system tenant.
 *
 * The library is the system tenant's reference data. Every tenant reads it, so
 * the trade generator checks a scripted trade against it, and no tenant changes
 * or closes a row of it.
 *
 * The assertions need a tenant other than the system tenant and skip without
 * one. Provision the Acme dataset first, as the test environments do.
 *
 * Run as ORES_TEST_DB_USER, never as the owner role, which bypasses row-level
 * security: pg_prove -d <database> test/trading_script_library_visibility_test.sql
 */

begin;

select plan(5);

create temporary table t_other on commit drop as
select id from ores_iam_tenants_tbl
where id <> ores_utility_system_tenant_id_fn()
  and valid_to = ores_utility_infinity_timestamp_fn()
  and status = 'active'
  and type <> 'automation'
order by id
limit 1;

select set_config('app.current_tenant_id', coalesce((select id::text from t_other),
    ores_utility_system_tenant_id_fn()::text), true);

-- A delete is a rule that closes the row, so the test reads the row back.
delete from ores_trading_scripts_tbl
where tenant_id = ores_utility_system_tenant_id_fn()
  and name = 'AsianBasketOption';
delete from ores_trading_script_inputs_tbl
where tenant_id = ores_utility_system_tenant_id_fn()
  and script_name = 'AsianBasketOption';

select case when not exists(select 1 from t_other)
    then skip('no other active tenant is provisioned', 5)
    else collect_tap(
        ok(exists(select 1 from ores_trading_scripts_tbl
                  where tenant_id = ores_utility_system_tenant_id_fn()
                    and name = 'AsianBasketOption'
                    and valid_to = ores_utility_infinity_timestamp_fn()),
           'a tenant reads a script of the system tenant, and its delete leaves it current'),
        is((select count(*)::int from ores_trading_script_inputs_tbl
            where tenant_id = ores_utility_system_tenant_id_fn()
              and script_name = 'AsianBasketOption'
              and valid_to = ores_utility_infinity_timestamp_fn()),
           10,
           'a tenant reads the ten inputs of the script, and its delete closes none'),
        -- The policy reads the system tenant's rows for every command, and its
        -- check refuses a write that would leave a row outside the tenant.
        throws_ok($$update ores_trading_script_inputs_tbl set data_type = 'Number'
                    where tenant_id = ores_utility_system_tenant_id_fn()
                      and script_name = 'AsianBasketOption'
                      and input_name = 'Underlyings'
                      and valid_to = ores_utility_infinity_timestamp_fn()$$,
                  '42501', null,
                  'a tenant changes no input of the system tenant'),
        is((select data_type from ores_trading_script_inputs_tbl
            where tenant_id = ores_utility_system_tenant_id_fn()
              and script_name = 'AsianBasketOption'
              and input_name = 'Underlyings'
              and valid_to = ores_utility_infinity_timestamp_fn()),
           'Index',
           'the input keeps the type the library states'),
        ok(exists(select 1 from ores_trading_script_variants_tbl
                  where tenant_id = ores_utility_system_tenant_id_fn()),
           'a tenant reads the variants of the system tenant'))
    end;

select * from finish();

rollback;
