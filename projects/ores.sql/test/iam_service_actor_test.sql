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
 * pgTAP tests for the service actor rule of the party visibility policy.
 *
 * A service acts inside a tenant and works in no party, so the party rule
 * passes a service actor. The actor is a username, and a username is unique
 * within a tenant only, so a person of a tenant can share a username with a
 * shared service account of the system tenant. That person must not be taken
 * for the service.
 *
 * The assertions need a tenant that people set up, so provision the Acme
 * dataset first, as the test environments do.
 *
 * Run as ORES_TEST_DB_USER, never as the owner role, which bypasses row-level
 * security: pg_prove -d <database> test/iam_service_actor_test.sql
 */

begin;

select plan(4);

-- The assertions need a tenant that people set up, and a rebuilt database has
-- none: provisioning is a runtime act, not part of the build. Provision one
-- here and let the transaction roll it back, so the suite owns its fixture
-- instead of depending on out-of-band setup.
select set_config('app.current_tenant_id', ores_utility_system_tenant_id_fn()::text, true);

create temporary table t_other on commit drop as
select tenant_id as id from ores_iam_provision_tenant_fn(
    'production', 'test_service_actor', 'Service Actor Fixture',
    'test_service_actor');

select ok(exists (select 1 from t_other), 'A tenant that people set up exists');

create temporary table t_service on commit drop as
select username from ores_iam_accounts_tbl
where tenant_id = ores_utility_system_tenant_id_fn()
  and account_type = 'service'
  and shared
  and valid_to = ores_utility_infinity_timestamp_fn()
order by username
limit 1;

select set_config('app.current_tenant_id', (select id::text from t_other), true);
select set_config('app.current_actor', (select username from t_service), true);

select ok(ores_iam_actor_is_service_fn(),
    'The shared service account is a service actor in a tenant');

insert into ores_iam_accounts_tbl (
    id, tenant_id, version, account_type, username, email, shared,
    modified_by, performed_by, change_reason_code, change_commentary
) select
    'a0000000-0000-0000-0000-0000000000b1'::uuid, ores_iam_current_tenant_id_fn(), 0,
    'user', (select username from t_service), 'namesake@tenant.ores', false,
    current_user, current_user, 'system.test', 'Service actor probe';

select is(
    (select account_type from ores_iam_accounts_tbl
     where id = 'a0000000-0000-0000-0000-0000000000b1'::uuid),
    'user',
    'A tenant holds a person that shares the username of a shared service');

select ok(not ores_iam_actor_is_service_fn(),
    'A person that shares the username of a shared service is not a service actor');

select * from finish();

rollback;
