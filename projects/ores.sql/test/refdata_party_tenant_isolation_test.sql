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
 * pgTAP tests for the tenant isolation of ores_refdata_parties_tbl.
 *
 * A tenant's parties are read inside the tenant, the system tenant included:
 * each tenant sees its own parties and no others, and no tenant writes a
 * party for another. The system tenant's own reads therefore never list
 * another tenant's parties.
 *
 * The other tenant is one people set up, not an automation tenant, because
 * a test may leave an automation tenant with no system party.
 *
 * Run as ORES_TEST_DB_USER, never as the owner role, which bypasses row-level
 * security: pg_prove -d <database> test/refdata_party_tenant_isolation_test.sql
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

select set_config('app.current_tenant_id', ores_utility_system_tenant_id_fn()::text, true);

select is(
    (select count(*)::int from ores_refdata_parties_tbl
     where tenant_id <> ores_utility_system_tenant_id_fn()),
    0,
    'the system tenant sees no party of another tenant'
);

select ok(
    exists(select 1 from ores_refdata_parties_tbl
           where party_category = 'System'
             and valid_to = ores_utility_infinity_timestamp_fn()),
    'the system tenant sees its own system party'
);

select case when not exists(select 1 from t_other)
    then skip('no other active tenant is provisioned', 1)
    else throws_ok(
        format($$insert into ores_refdata_parties_tbl (
            id, tenant_id, version, full_name, short_code, party_category, party_type,
            parent_party_id, business_center_code, status,
            modified_by, performed_by, change_reason_code, change_commentary
        ) values (
            'a0000000-0000-0000-0000-0000000000f1'::uuid, %L::uuid, 0,
            'Isolation Probe', 'ISOPROBE', 'Operational', 'Bank', NULL, 'WRLD', 'Active',
            current_user, current_user, 'system.test', 'Isolation probe'
        )$$, (select id from t_other)),
        '42501',
        null,
        'the system tenant cannot write a party for another tenant')
    end;

select set_config('app.current_tenant_id', coalesce((select id::text from t_other),
    ores_utility_system_tenant_id_fn()::text), true);

select case when not exists(select 1 from t_other)
    then skip('no other active tenant is provisioned', 2)
    else collect_tap(
        is((select count(*)::int from ores_refdata_parties_tbl
            where tenant_id <> (select id from t_other)),
           0,
           'a tenant sees no party of another tenant'),
        ok(exists(select 1 from ores_refdata_parties_tbl
                  where party_category = 'System'
                    and valid_to = ores_utility_infinity_timestamp_fn()),
           'a tenant sees its own system party'))
    end;

select * from finish();

rollback;
