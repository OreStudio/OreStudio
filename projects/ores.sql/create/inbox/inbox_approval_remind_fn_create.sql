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
 * Reads every open request whose deadline falls inside the reminder window.
 *
 * The other half of the deadline is the warning. A request that closes itself
 * keeps the queue from rotting, but a decider who never looked finds out
 * afterwards that it did; this answers the requests that are about to run out
 * so the service can tell them first.
 *
 * The warning is sent once, and the notice is what says so. A reminder already
 * raised for a request is a notification whose link names it, so there is no
 * second piece of state to keep in step with the first: a column would have to
 * be cleared with the notice and could disagree with it. A request that is
 * answered before its deadline passes simply stops matching, and the sweep that
 * closes what lapsed leaves it out of this one by closing it.
 *
 * The read crosses every tenant, so this is a security-definer function and the
 * inbox service is the only role granted execute.
 */
create or replace function ores_inbox_remind_expiring_approval_requests_fn(
    p_window_seconds double precision
) returns table (
    request_id uuid,
    tenant_id uuid,
    kind_code text,
    requested_by uuid,
    expires_at timestamp with time zone
)
as $$
begin
    return query
        select r.id, r.tenant_id, r.kind_code, r.requested_by, r.expires_at
        from ores_inbox_approval_requests_tbl r
        where r.valid_to = ores_utility_infinity_timestamp_fn()
          and r.state_code in ('waiting', 'held')
          and r.expires_at is not null
          and r.expires_at >= clock_timestamp()
          and r.expires_at < clock_timestamp() + make_interval(secs => p_window_seconds)
          and not exists (
              select 1
              from ores_inbox_notifications_tbl n
              where n.valid_to = ores_utility_infinity_timestamp_fn()
                and n.link_route = 'requests'
                and n.link_id = r.id::text
                and n.kind_code = 'inbox.approval_expiring'
          );
end;
$$ language plpgsql security definer set search_path = public, pg_temp;

-- The sweep reads every tenant, so PostgreSQL's default grant to PUBLIC is
-- revoked. The inbox service is the only role that runs it: the registry
-- grants it execute (Execute prefixes in service_registry.org). The read-write
-- role the test suites sign in under keeps it too, so the sweep can be tested.
revoke execute on function ores_inbox_remind_expiring_approval_requests_fn(double precision) from public;
grant execute on function ores_inbox_remind_expiring_approval_requests_fn(double precision) to :rw_role;
