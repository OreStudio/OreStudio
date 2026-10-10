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
 * pgTAP tests for the sandbox portfolios publish.
 *
 * The ORE sample portfolios publish opens a sample sandbox for a party:
 * - The sandbox is anchored at the party's official top portfolio and shared
 * - The publishing actor owns it and holds the right to open sandboxes there
 * - The sample portfolios are root portfolios of the sandbox, not official ones
 * - A repeat publish adds nothing
 * - Without an actor, or without an official portfolio, the publish says so
 *
 * Run with: pg_prove -d <database> test/refdata_publish_sandbox_portfolios_test.sql
 */

begin;

select plan(11);

select set_config('app.current_tenant_id', ores_utility_system_tenant_id_fn()::text, true);
select set_config('app.visible_party_ids',
    (select '{' || string_agg(id::text, ',') || '}' from ores_refdata_parties_tbl), true);

create temp table t_ctx on commit drop as
select (select id from ores_refdata_parties_tbl
        where tenant_id = ores_utility_system_tenant_id_fn() and short_code = 'system_party'
          and valid_to = ores_utility_infinity_timestamp_fn()) as party_id,
       (select id from ores_dq_datasets_tbl
        where code = 'ore.sample_portfolios'
          and valid_to = ores_utility_infinity_timestamp_fn()) as dataset_id,
       (select id from ores_iam_accounts_tbl
        where account_type = 'service' and valid_to = ores_utility_infinity_timestamp_fn()
        order by username limit 1) as owner_id,
       (select username from ores_iam_accounts_tbl
        where account_type = 'service' and valid_to = ores_utility_infinity_timestamp_fn()
        order by username limit 1) as owner_name;

create or replace function pg_temp.publish() returns table (action text, record_count bigint) as $$
    select p.action, p.record_count
    from t_ctx c,
         ores_refdata_publish_sandbox_portfolios_from_dq_fn(
             c.dataset_id, ores_utility_system_tenant_id_fn(),
             'upsert', jsonb_build_object('party_id', c.party_id::text)) p
    order by p.action;
$$ language sql;

-- No actor: the tenant's administrator owns the sandbox, and a tenant with
-- no administrator has no owner. Either way nothing is written without an
-- official portfolio to anchor at.
select results_eq(
    $$select * from pg_temp.publish()$$,
    format($$values (%L::text, 0::bigint)$$,
        case when exists (
            select 1
            from ores_iam_account_roles_tbl ar
            join ores_iam_roles_tbl r on r.id = ar.role_id
              and r.valid_to = ores_utility_infinity_timestamp_fn()
            join ores_iam_accounts_tbl a on a.id = ar.account_id
              and a.valid_to = ores_utility_infinity_timestamp_fn()
            where a.tenant_id = ores_utility_system_tenant_id_fn()
              and a.account_type = 'user'
              and ar.valid_to = ores_utility_infinity_timestamp_fn()
              and r.name in ('TenantAdmin', 'SuperAdmin'))
             then 'skipped_no_anchor' else 'skipped_no_owner' end),
    'without an actor the tenant administrator owns the sandbox, if there is one');

select set_config('app.current_actor', (select owner_name from t_ctx), true);

-- An actor, but the party holds no official portfolio to anchor at.
select results_eq(
    $$select * from pg_temp.publish()$$,
    $$values ('skipped_no_anchor'::text, 0::bigint)$$,
    'the publish does nothing when the party has no official portfolio');

insert into ores_refdata_portfolios_tbl (
    id, tenant_id, version, party_id, name, parent_portfolio_id, purpose_type,
    is_virtual, status, modified_by, performed_by, change_reason_code, change_commentary
)
select '00000000-0000-0000-0000-0000000cf301'::uuid, ores_utility_system_tenant_id_fn(), 0,
    party_id, 'Global Portfolio', null, 'Risk', false, 'Active',
    owner_name, owner_name, 'system.test', 'Sandbox publish pgTAP fixture'
from t_ctx;

select results_eq(
    $$select * from pg_temp.publish()$$,
    $$values ('inserted'::text, 2::bigint)$$,
    'the publish adds the two sample portfolios');

create temp view t_sandbox as
select s.*
from ores_refdata_sandboxes_tbl s, t_ctx c
where s.tenant_id = ores_utility_system_tenant_id_fn()
  and s.anchor_portfolio_id = '00000000-0000-0000-0000-0000000cf301'::uuid
  and s.valid_to = ores_utility_infinity_timestamp_fn();

select is((select count(*) from t_sandbox), 1::bigint, 'the publish opens one sandbox');

select results_eq(
    $$select purpose, visibility, status from t_sandbox$$,
    $$values ('sample'::text, 'shared'::text, 'open'::text)$$,
    'the sandbox is an open, shared sample sandbox');

select is(
    (select s.name from t_sandbox s),
    'ORE Sample Portfolios (system_party)',
    'the sandbox is named after the dataset and the party');

select is(
    (select count(*) from t_sandbox s, t_ctx c where s.owner_account_id = c.owner_id),
    1::bigint,
    'the publishing actor owns the sandbox');

select is(
    (select count(*) from ores_refdata_portfolio_rights_tbl r, t_ctx c
     where r.tenant_id = ores_utility_system_tenant_id_fn()
       and r.account_id = c.owner_id
       and r.portfolio_id = '00000000-0000-0000-0000-0000000cf301'::uuid
       and r.right_code = 'open_sandbox'
       and r.valid_to = ores_utility_infinity_timestamp_fn()),
    1::bigint,
    'the owner holds the right to open sandboxes at the anchor');

select results_eq(
    $$select p.name, p.parent_portfolio_id is null
      from ores_refdata_portfolios_tbl p, t_sandbox s
      where p.sandbox_id = s.id and p.valid_to = ores_utility_infinity_timestamp_fn()
      order by p.name$$,
    $$values ('PF1'::text, true), ('PF2'::text, true)$$,
    'PF1 and PF2 are root portfolios of the sandbox');

select is(
    (select count(*) from ores_refdata_portfolios_tbl p, t_ctx c
     where p.party_id = c.party_id and p.name in ('PF1', 'PF2')
       and p.sandbox_id is null and p.valid_to = ores_utility_infinity_timestamp_fn()),
    0::bigint,
    'no sample portfolio is an official portfolio');

select results_eq(
    $$select * from pg_temp.publish()$$,
    $$values ('skipped'::text, 2::bigint)$$,
    'a second publish adds nothing');

select * from finish();

rollback;
