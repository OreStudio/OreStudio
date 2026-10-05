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
 * The two reads a role grant request makes across into the inbox.
 *
 * Both run as their owner, so IAM's own role needs no grant on the inbox's
 * tables: the inbox keeps the record, and IAM asks it two questions.
 *
 * ores_iam_open_role_grant_requests_fn answers which of the given roles an
 * account has already asked for in a request still open, waiting or held, in
 * the caller's tenant. A second request for the same role while one waits is
 * refused, so a person cannot flood the queue.
 *
 * ores_iam_unapplied_role_grants_fn answers, across every tenant, each role an
 * approved iam.role_grant request asked for that its account does not yet hold,
 * with the username of the approver whose decision closed it. IAM grants each
 * one. It is a reconciliation, not a queue: granting a role takes the row out
 * of the answer, so running it twice grants nothing twice, and an approval made
 * while IAM was down is granted the next time it runs. Only tenants that still
 * exist and are not terminated are read: a removed or terminated tenant has
 * nobody to grant to, and leaving its approvals in the answer would fail every
 * run. A suspended tenant's approvals are still granted, since suspension only
 * stops signing in.
 */

create or replace function ores_iam_open_role_grant_requests_fn(
    p_account_id uuid,
    p_role_ids uuid[]
) returns table (role_id uuid)
as $$
begin
    return query
    select distinct r.role_id
    from ores_iam_role_grant_request_roles_tbl r
    join ores_iam_role_grant_requests_tbl g
        on g.request_id = r.request_id
       and g.tenant_id = r.tenant_id
       and g.valid_to = ores_utility_infinity_timestamp_fn()
    join ores_inbox_approval_requests_tbl q
        on q.id = g.request_id
       and q.tenant_id = g.tenant_id
       and q.valid_to = ores_utility_infinity_timestamp_fn()
    where r.tenant_id = ores_iam_current_tenant_id_fn()
      and r.valid_to = ores_utility_infinity_timestamp_fn()
      and g.account_id = p_account_id
      and r.role_id = any(p_role_ids)
      and q.state_code in ('waiting', 'held');
end;
$$ language plpgsql stable security definer set search_path = public, pg_temp;

create or replace function ores_iam_unapplied_role_grants_fn()
returns table (
    tenant_id uuid,
    request_id uuid,
    account_id uuid,
    role_id uuid,
    approved_by text
)
as $$
begin
    return query
    select g.tenant_id, g.request_id, g.account_id, r.role_id,
        coalesce((
            select a.username
            from ores_inbox_approval_decisions_tbl d
            join ores_iam_accounts_tbl a
                on a.id = d.decided_by
               and a.tenant_id = d.tenant_id
               and a.valid_to = ores_utility_infinity_timestamp_fn()
            where d.request_id = g.request_id
              and d.tenant_id = g.tenant_id
              and d.decision_code = 'approve'
              and d.valid_to = ores_utility_infinity_timestamp_fn()
            order by d.decided_at desc
            limit 1), '')
    from ores_iam_role_grant_requests_tbl g
    join ores_iam_role_grant_request_roles_tbl r
        on r.request_id = g.request_id
       and r.tenant_id = g.tenant_id
       and r.valid_to = ores_utility_infinity_timestamp_fn()
    join ores_inbox_approval_requests_tbl q
        on q.id = g.request_id
       and q.tenant_id = g.tenant_id
       and q.valid_to = ores_utility_infinity_timestamp_fn()
    join ores_iam_tenants_tbl t
        on t.id = g.tenant_id
       and t.status <> 'terminated'
       and t.valid_to = ores_utility_infinity_timestamp_fn()
    where g.valid_to = ores_utility_infinity_timestamp_fn()
      and q.kind_code = 'iam.role_grant'
      and q.state_code = 'approved'
      and not exists (
          select 1 from ores_iam_account_roles_tbl ar
          where ar.tenant_id = g.tenant_id
            and ar.account_id = g.account_id
            and ar.role_id = r.role_id
            and ar.valid_to = ores_utility_infinity_timestamp_fn());
end;
$$ language plpgsql stable security definer set search_path = public, pg_temp;

-- The reconciliation reads every tenant, so PostgreSQL's default grant to
-- PUBLIC is revoked. The IAM service runs it: the registry grants it execute
-- (Execute prefixes in service_registry.org). The read-write role the test
-- suites sign in under keeps it too, so the reconciliation can be tested.
revoke execute on function ores_iam_unapplied_role_grants_fn() from public;
grant execute on function ores_iam_unapplied_role_grants_fn() to :rw_role;
