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
 * The service reads as the system tenant. Its session must see the rows of every
 * tenant, and a tenant must see its own rows and no others.
 *
 * The behavioural assertions need a tenant that people set up, with its routes
 * and subscriptions, so provision the Acme dataset first, as the test
 * environments do. Without one they are skipped, and the structural assertions
 * on the two policies still run.
 *
 * Run as ORES_TEST_DB_USER, never as the owner role, which bypasses row-level
 * security: pg_prove -d <database> test/marketdata_route_visibility_test.sql
 */

begin;

select plan(8);

-- Structural: each tenant policy admits a system-tenant session.
select ok(
    (select qual ~ 'ores_iam_current_tenant_id_fn\(\)\s*=\s*ores_utility_system_tenant_id_fn\(\)'
     from pg_policies
     where tablename = 'ores_marketdata_routes_tbl'
       and policyname = 'routes_tbl_tenant_isolation_policy'),
    'the routes policy admits a system-tenant session'
);

select ok(
    (select qual ~ 'ores_iam_current_tenant_id_fn\(\)\s*=\s*ores_utility_system_tenant_id_fn\(\)'
     from pg_policies
     where tablename = 'ores_marketdata_subscriptions_tbl'
       and policyname = 'subscriptions_tbl_tenant_isolation_policy'),
    'the subscriptions policy admits a system-tenant session'
);

create temporary table t_people as
select id from ores_iam_tenants_tbl
where id <> ores_utility_system_tenant_id_fn()
  and valid_to = ores_utility_infinity_timestamp_fn()
  and status = 'active'
  and type <> 'automation'
order by id;

-- The system tenant reads the routes and the subscriptions of the tenants that people set up.
select set_config('app.current_tenant_id', ores_utility_system_tenant_id_fn()::text, true);

select case when not exists(select 1 from t_people)
    then skip('no tenant that people set up is provisioned', 2)
    else collect_tap(
        ok(exists(select 1 from ores_marketdata_routes_tbl
                  where tenant_id in (select id from t_people)
                    and valid_to = ores_utility_infinity_timestamp_fn()),
           'the system tenant reads the routes of another tenant'),
        ok(exists(select 1 from ores_marketdata_subscriptions_tbl
                  where tenant_id in (select id from t_people)
                    and valid_to = ores_utility_infinity_timestamp_fn()),
           'the system tenant reads the subscriptions of another tenant'))
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
