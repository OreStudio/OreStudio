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
 * ores_iam_unapplied_role_grants_fn answers, across every tenant that still
 * exists and is not terminated, each role an approved iam.role_grant request
 * asked for and IAM has not yet applied, with the approver whose decision
 * closed it and whether the account already holds the role. IAM grants a role
 * not held, then marks every row applied. It is a reconciliation, not a queue:
 * a row leaves the answer once applied, so a second run grants nothing twice,
 * a crash between the grant and the mark converges on the next run, an
 * approval made while IAM was down is granted when it next runs, and a role
 * taken away after it was applied is not granted again. A removed or
 * terminated tenant has nobody to grant to; a suspended tenant's approvals are
 * still applied, since suspension only stops signing in. An approval whose
 * approver account no longer exists is left out, since nobody can be named as
 * the grant's author; an administrator approves a new request instead.
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

-- The answer's columns changed when held and applied_at were added, and a
-- function's result type cannot be replaced in place.
drop function if exists ores_iam_unapplied_role_grants_fn();

create or replace function ores_iam_unapplied_role_grants_fn()
returns table (
    tenant_id uuid,
    request_id uuid,
    account_id uuid,
    role_id uuid,
    approved_by text,
    held boolean
)
as $$
begin
    return query
    select g.tenant_id, g.request_id, g.account_id, r.role_id, approver.username,
        exists (
            select 1 from ores_iam_account_roles_tbl ar
            where ar.tenant_id = g.tenant_id
              and ar.account_id = g.account_id
              and ar.role_id = r.role_id
              and ar.valid_to = ores_utility_infinity_timestamp_fn())
    from ores_iam_role_grant_requests_tbl g
    join ores_iam_role_grant_request_roles_tbl r
        on r.request_id = g.request_id
       and r.tenant_id = g.tenant_id
       and r.valid_to = ores_utility_infinity_timestamp_fn()
       and r.applied_at is null
    join ores_inbox_approval_requests_tbl q
        on q.id = g.request_id
       and q.tenant_id = g.tenant_id
       and q.valid_to = ores_utility_infinity_timestamp_fn()
    join ores_iam_tenants_tbl t
        on t.id = g.tenant_id
       and t.status <> 'terminated'
       and t.valid_to = ores_utility_infinity_timestamp_fn()
    join lateral (
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
        limit 1) approver on true
    where g.valid_to = ores_utility_infinity_timestamp_fn()
      and q.kind_code = 'iam.role_grant'
      and q.state_code = 'approved';
end;
$$ language plpgsql stable security definer set search_path = public, pg_temp;

-- The reconciliation reads every tenant, so PostgreSQL's default grant to
-- PUBLIC is revoked. The IAM service runs it: the registry grants it execute
-- (Execute prefixes in service_registry.org). The read-write role the test
-- suites sign in under keeps it too, so the reconciliation can be tested.
revoke execute on function ores_iam_unapplied_role_grants_fn() from public;
grant execute on function ores_iam_unapplied_role_grants_fn() to :rw_role;
