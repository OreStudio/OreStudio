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
 * Lists the approved requests whose apply has not finished, in one statement.
 *
 * An approved request whose kind names an apply workflow is waiting for that
 * workflow to apply it. When the apply stops on a failure that can pass, or the
 * start was lost, the request stays approved. The scheduled retry reads those
 * requests here, across every tenant, and resumes each one until the kind's
 * window ends.
 *
 * The window runs from the moment the request became approved, which is when
 * the current row of the request began. over_window says whether it has ended at
 * the moment p_as_of names, so the caller does not repeat the arithmetic and a
 * test can ask about a later moment. A kind that names a workflow but no window
 * has no time to retry, so its request is over the window at once.
 *
 * Kinds are the system tenant's, as the decide function reads them.
 *
 * The read crosses every tenant and touches no row, so this is a
 * security-definer function and the inbox service is the only role granted
 * execute.
 */
create or replace function ores_inbox_stalled_applies_fn(
    p_as_of timestamp with time zone
)
returns table (
    request_id uuid,
    tenant_id uuid,
    kind_code text,
    requested_by uuid,
    apply_workflow_type text,
    approved_at timestamp with time zone,
    over_window boolean
)
as $$
begin
    return query
    select r.id,
           r.tenant_id,
           r.kind_code,
           r.requested_by,
           k.apply_workflow_type,
           r.valid_from,
           (k.apply_window_hours is null
            or r.valid_from + make_interval(hours => k.apply_window_hours) < p_as_of)
    from ores_inbox_approval_requests_tbl r
    join ores_inbox_approval_kinds_tbl k
      on k.tenant_id = ores_utility_system_tenant_id_fn()
     and k.code = r.kind_code
     and k.valid_to = ores_utility_infinity_timestamp_fn()
    where r.valid_to = ores_utility_infinity_timestamp_fn()
      and r.state_code = 'approved'
      and k.apply_workflow_type is not null
      and k.apply_workflow_type <> ''
    order by r.valid_from;
end;
$$ language plpgsql security definer set search_path = public, pg_temp;

-- The read crosses every tenant, so PostgreSQL's default grant to PUBLIC is
-- revoked. The inbox service is the only role that runs it: the registry
-- grants it execute (Execute prefixes in service_registry.org). The read-write
-- role the test suites sign in under keeps it too, so the sweep can be tested.
revoke execute on function ores_inbox_stalled_applies_fn(timestamp with time zone) from public;
grant execute on function ores_inbox_stalled_applies_fn(timestamp with time zone) to :rw_role;
