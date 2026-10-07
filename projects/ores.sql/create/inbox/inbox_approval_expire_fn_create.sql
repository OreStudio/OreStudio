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
 * Closes every approval request that ran out of time, in one statement.
 *
 * An open request past its kind's deadline is a request nobody answered. The
 * decide function already closes one when a decider reaches for it, but a
 * request nobody looks at is exactly the case a queue rots on, so the store
 * closes those on its own too.
 *
 * The rule is the one the decide function applies, read across every tenant at
 * once: an open request, waiting or held, whose expires_at has passed, becomes
 * expired. It states the same tail the decide function states, so a request's
 * history reads the same however it closed.
 *
 * The read crosses every tenant, so this is a security-definer function and
 * the inbox service is the only role granted execute. It answers one row per
 * request it closed, so the caller can tell each person who asked.
 */
create or replace function ores_inbox_expire_approval_requests_fn(
    p_actor text
) returns table (
    request_id uuid,
    tenant_id uuid,
    kind_code text,
    requested_by uuid
)
as $$
declare
    v_request ores_inbox_approval_requests_tbl%rowtype;
begin
    for v_request in
        select *
        from ores_inbox_approval_requests_tbl r
        where r.valid_to = ores_utility_infinity_timestamp_fn()
          and r.state_code in ('waiting', 'held')
          and r.expires_at is not null
          and r.expires_at < clock_timestamp()
        for update
    loop
        v_request.state_code := 'expired';
        v_request.modified_by := p_actor;
        v_request.change_reason_code := 'system.update';
        v_request.change_commentary := 'Closed because nobody answered before its deadline';
        insert into ores_inbox_approval_requests_tbl select v_request.*;

        request_id := v_request.id;
        tenant_id := v_request.tenant_id;
        kind_code := v_request.kind_code;
        requested_by := v_request.requested_by;
        return next;
    end loop;
end;
$$ language plpgsql security definer set search_path = public, pg_temp;

-- The sweep reads every tenant, so PostgreSQL's default grant to PUBLIC is
-- revoked. The inbox service is the only role that runs it: the registry
-- grants it execute (Execute prefixes in service_registry.org). The read-write
-- role the test suites sign in under keeps it too, so the sweep can be tested.
revoke execute on function ores_inbox_expire_approval_requests_fn(text) from public;
grant execute on function ores_inbox_expire_approval_requests_fn(text) to :rw_role;
