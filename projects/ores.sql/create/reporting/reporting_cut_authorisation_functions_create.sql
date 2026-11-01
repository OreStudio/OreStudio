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
 * The read a cut's authorisation apply makes across into the inbox.
 *
 * The apply runs as its owner, so the reporting service needs no grant on the
 * inbox's tables: the inbox keeps the record of the ask and the decision, and
 * reporting asks it one question.
 *
 * ores_reporting_unapplied_cut_authorisations_fn answers, across every tenant
 * that still exists and is not terminated, each decided cut-authorisation
 * request whose decision has not reached the cut it authorised: the kind of cut
 * the request answers, the cut, and the decision with the person who took it and
 * the reason they gave. The kind is the cut kind's own approval_kind_code and
 * not a list kept here, so a firm that seeds a third cut kind gets its
 * authorisations applied with no change to this function.
 *
 * It is a reconciliation and not a queue. A row leaves the answer the moment the
 * detail row is stamped applied, so a second run applies nothing twice; a crash
 * between the cut's write and the detail's mark converges on the next run, since
 * the facts the cut is stamped with are the decision's own and not the run's
 * clock; a decision taken while the reporting service was down is applied when
 * it next runs; and a request that is still waiting, held or withdrawn answers
 * nothing, because nobody has decided it. A terminated tenant has no service to
 * be told anything; a suspended tenant's decisions are still applied, since
 * suspension only stops signing in. A decision whose decider account no longer
 * exists is left out, since nobody can be named as the person who signed.
 */

drop function if exists ores_reporting_unapplied_cut_authorisations_fn();

create or replace function ores_reporting_unapplied_cut_authorisations_fn()
returns table (
    tenant_id uuid,
    request_id uuid,
    detail_id uuid,
    cut_id uuid,
    approval_kind_code text,
    decision_code text,
    decided_by text,
    decided_at timestamp with time zone,
    comment text
)
as $$
begin
    return query
    select d.tenant_id, d.request_id, d.id, d.cut_id, q.kind_code,
        decision.decision_code, decision.decided_by, decision.decided_at,
        coalesce(decision.comment, '')
    from ores_reporting_cut_approval_details_tbl d
    join ores_inbox_approval_requests_tbl q
        on q.id = d.request_id
       and q.tenant_id = d.tenant_id
       and q.valid_to = ores_utility_infinity_timestamp_fn()
    join ores_iam_tenants_tbl t
        on t.id = d.tenant_id
       and t.status <> 'terminated'
       and t.valid_to = ores_utility_infinity_timestamp_fn()
    join lateral (
        select dd.decision_code, a.username as decided_by, dd.decided_at, dd.comment
        from ores_inbox_approval_decisions_tbl dd
        join ores_iam_accounts_tbl a
            on a.id = dd.decided_by
           and a.tenant_id = dd.tenant_id
           and a.valid_to = ores_utility_infinity_timestamp_fn()
        where dd.request_id = d.request_id
          and dd.tenant_id = d.tenant_id
          and dd.decision_code in ('approve', 'refuse')
          and dd.valid_to = ores_utility_infinity_timestamp_fn()
        order by dd.decided_at desc
        limit 1) decision on true
    where d.valid_to = ores_utility_infinity_timestamp_fn()
      and d.applied_at is null
      and q.state_code in ('approved', 'refused')
      and exists (
          select 1 from ores_reporting_cut_kinds_tbl k
          where k.approval_kind_code = q.kind_code
            and k.valid_to = ores_utility_infinity_timestamp_fn());
end;
$$ language plpgsql stable security definer set search_path = public, pg_temp;

comment on function ores_reporting_unapplied_cut_authorisations_fn() is
'Every decided cut-authorisation request whose decision the reporting service has
not yet stamped onto the cut it authorised, across every tenant that is not
terminated: the tenant, the request, the detail row, the cut, the approval kind,
and the decision with who took it, when and why. A row leaves the answer once its
detail row is applied, so the read is a reconciliation and not a queue.';

-- The reconciliation reads every tenant, so PostgreSQL's default grant to PUBLIC
-- is revoked. The reporting service runs it: the registry grants it execute
-- (Execute prefixes in service_registry.org). The read-write role the test
-- suites sign in under keeps it too, so the reconciliation can be tested.
revoke execute on function ores_reporting_unapplied_cut_authorisations_fn() from public;
grant execute on function ores_reporting_unapplied_cut_authorisations_fn() to :rw_role;
