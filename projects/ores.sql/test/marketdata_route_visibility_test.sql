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
 * pgTAP tests for the visibility of ores_marketdata_routes_tbl and
 * ores_marketdata_subscriptions_tbl.
 *
 * The adapters are shared by every tenant, so the marketdata service computes one
 * demand for each adapter from the routes and the subscriptions of all tenants.
 * It reads them through a select-only policy for its own database role. A tenant
 * must see its own rows and no others, and a system-tenant session of any other
 * role must see no row of another tenant.
 *
 * The behavioural assertions need a tenant that people set up, with its routes
 * and subscriptions, so provision the Acme dataset first, as the test
 * environments do. Without one they are skipped, and the structural assertions
 * on the policies still run.
 *
 * Run as ORES_TEST_DB_USER, never as the owner role, which bypasses row-level
 * security: pg_prove -d <database> test/marketdata_route_visibility_test.sql
 */

begin;

select plan(11);

-- Structural: the tenant policy holds the own-tenant shape and no system-tenant clause.
select ok(
    (select qual !~ 'ores_utility_system_tenant_id_fn' and with_check !~ 'ores_utility_system_tenant_id_fn'
     from pg_policies
     where tablename = 'ores_marketdata_routes_tbl'
       and policyname = 'routes_tbl_tenant_isolation_policy'),
    'the routes tenant policy does not admit a system-tenant session'
);

select ok(
    (select qual !~ 'ores_utility_system_tenant_id_fn' and with_check !~ 'ores_utility_system_tenant_id_fn'
     from pg_policies
     where tablename = 'ores_marketdata_subscriptions_tbl'
       and policyname = 'subscriptions_tbl_tenant_isolation_policy'),
    'the subscriptions tenant policy does not admit a system-tenant session'
);

-- Structural: one select-only policy that admits every row, for one role.
select ok(
    exists(select 1 from pg_policies
           where tablename = 'ores_marketdata_routes_tbl'
             and policyname ~ 'cross_tenant_read_policy$'
             and cmd = 'SELECT' and qual = 'true' and permissive = 'PERMISSIVE'),
    'the routes have a select-only cross-tenant read policy'
);

select ok(
    exists(select 1 from pg_policies
           where tablename = 'ores_marketdata_subscriptions_tbl'
             and policyname ~ 'cross_tenant_read_policy$'
             and cmd = 'SELECT' and qual = 'true' and permissive = 'PERMISSIVE'),
    'the subscriptions have a select-only cross-tenant read policy'
);

-- Structural: the policy names the marketdata service role and no other role.
select ok(
    not exists(select 1 from pg_policies
               where tablename in ('ores_marketdata_routes_tbl', 'ores_marketdata_subscriptions_tbl')
                 and policyname ~ 'cross_tenant_read_policy$'
                 and (array_length(roles, 1) <> 1 or roles[1] !~ '_marketdata_service$')),
    'the cross-tenant read policies name the marketdata service role alone'
);

create temporary table t_people as
select id from ores_iam_tenants_tbl
where id <> ores_utility_system_tenant_id_fn()
  and valid_to = ores_utility_infinity_timestamp_fn()
  and status = 'active'
  and type <> 'automation'
order by id;

-- A system-tenant session of another role reads no row of another tenant.
select set_config('app.current_tenant_id', ores_utility_system_tenant_id_fn()::text, true);

select case when not exists(select 1 from t_people)
    then skip('no tenant that people set up is provisioned', 2)
    else collect_tap(
        is((select count(*)::int from ores_marketdata_routes_tbl
            where tenant_id in (select id from t_people)),
           0,
           'a system-tenant session reads no route of another tenant'),
        is((select count(*)::int from ores_marketdata_subscriptions_tbl
            where tenant_id in (select id from t_people)),
           0,
           'a system-tenant session reads no subscription of another tenant'))
    end;

-- A tenant reads its own rows and no other tenant's.
select set_config('app.current_tenant_id',
                  coalesce((select id::text from t_people limit 1),
                           ores_utility_system_tenant_id_fn()::text),
                  true);

select case when not exists(select 1 from t_people)
    then skip('no tenant that people set up is provisioned', 4)
    else collect_tap(
        ok(exists(select 1 from ores_marketdata_routes_tbl
                  where tenant_id = (select id from t_people limit 1)
                    and valid_to = ores_utility_infinity_timestamp_fn()),
           'a tenant reads its own routes'),
        is((select count(*)::int from ores_marketdata_routes_tbl
            where tenant_id <> (select id from t_people limit 1)),
           0,
           'a tenant reads no route of another tenant'),
        ok(exists(select 1 from ores_marketdata_subscriptions_tbl
                  where tenant_id = (select id from t_people limit 1)
                    and valid_to = ores_utility_infinity_timestamp_fn()),
           'a tenant reads its own subscriptions'),
        is((select count(*)::int from ores_marketdata_subscriptions_tbl
            where tenant_id <> (select id from t_people limit 1)),
           0,
           'a tenant reads no subscription of another tenant'))
    end;

select * from finish();

rollback;
