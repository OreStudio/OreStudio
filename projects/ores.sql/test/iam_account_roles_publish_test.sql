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
 * pgTAP tests for the roles an account dataset publishes.
 *
 * The account artefact stages one row per account, so the roles an account
 * holds travel as a comma separated list of names rather than as rows of a
 * second artefact. The publish resolves each name and grants it.
 *
 * The facts asserted are:
 *
 * - an account that names two roles holds both, which is the case the ACME
 *   demonstration needed and had no published path for;
 * - a name no role carries is skipped rather than failing the publish, which
 *   is the behaviour the publish states.
 *
 * The assertions look at the roles the dataset names, not at every role the
 * account holds: a person account also receives =Member= from the trigger that
 * gives it to every person on first write.
 *
 * Run with: pg_prove -d <database> test/iam_account_roles_publish_test.sql
 */

begin;

select plan(3);

select set_config('app.current_tenant_id', ores_utility_system_tenant_id_fn()::text, true);

create temporary table t_ctx on commit drop as
select gen_random_uuid() as dataset_id,
       ores_utility_system_tenant_id_fn() as tenant_id,
       ores_iam_system_party_id_fn() as party_id;

insert into ores_dq_accounts_artefact_tbl (
    dataset_id, tenant_id, id, version, username, full_name, email,
    password_hash, account_type, business_unit_code, roles, job_title,
    reports_to_username, photo_key)
values
    ((select dataset_id from t_ctx), ores_utility_system_tenant_id_fn(),
     gen_random_uuid(), 0, 'pgtap.two.roles', 'pgTAP Two Roles',
     'pgtap.two.roles@example.test', '', 'user', null, 'Trading, Finance',
     'Trader', null, null),
    ((select dataset_id from t_ctx), ores_utility_system_tenant_id_fn(),
     gen_random_uuid(), 0, 'pgtap.one.known.role', 'pgTAP One Known Role',
     'pgtap.one.known.role@example.test', '', 'user', null,
     'Trading, NoSuchRoleAnywhere', 'Trader', null, null);

select * from ores_iam_publish_accounts_from_dq_fn(
    (select dataset_id from t_ctx),
    (select tenant_id from t_ctx),
    'upsert',
    jsonb_build_object('party_id', (select party_id from t_ctx)::text));

select is(
    (select count(*) from ores_iam_account_roles_tbl ar
     join ores_iam_accounts_tbl a on a.id = ar.account_id
     join ores_iam_roles_tbl rl on rl.id = ar.role_id
     where a.username = 'pgtap.two.roles'
       and rl.name in ('Trading', 'Finance')
       and ar.valid_to = ores_utility_infinity_timestamp_fn()),
    2::bigint,
    'an account that names two roles is granted both');

select results_eq(
    $$select rl.name
      from ores_iam_account_roles_tbl ar
      join ores_iam_accounts_tbl a on a.id = ar.account_id
      join ores_iam_roles_tbl rl on rl.id = ar.role_id
      where a.username = 'pgtap.two.roles'
        and rl.name in ('Trading', 'Finance')
        and ar.valid_to = ores_utility_infinity_timestamp_fn()
      order by rl.name$$,
    $$values ('Finance'), ('Trading')$$,
    'the two roles are the two the account named');

select is(
    (select count(*) from ores_iam_account_roles_tbl ar
     join ores_iam_accounts_tbl a on a.id = ar.account_id
     where a.username = 'pgtap.one.known.role'
       and ar.valid_to = ores_utility_infinity_timestamp_fn()),
    2::bigint,
    'a role name no role carries is skipped, and the known name and Member are the only two granted');

select * from finish();

rollback;
