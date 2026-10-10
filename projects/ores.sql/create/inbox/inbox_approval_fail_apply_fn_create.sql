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
 * Moves an approved request to apply_failed.
 *
 * An approved request holds changes the owning component applies. When the
 * apply is refused for a reason that cannot change, the request ends here, with
 * the line and the reason in the row's commentary, and a new request is needed.
 * It is not a decision: no person decides it, so no decision row is written.
 *
 * Only an approved request moves. A request already in apply_failed answers ok,
 * so a repeated call changes nothing. Any other state is a conflict.
 *
 * The function runs as the caller, so row-level security scopes the read to the
 * caller's tenant. It answers one row: the outcome (ok, missing, conflict), a
 * message, and the request's state and version afterwards.
 */
create or replace function ores_inbox_fail_apply_fn(
    p_request_id uuid,
    p_reason text,
    p_actor text
) returns table (outcome text, message text, state_code text, version integer)
as $$
declare
    v_request ores_inbox_approval_requests_tbl%rowtype;
begin
    select * into v_request
    from ores_inbox_approval_requests_tbl r
    where r.id = p_request_id
      and r.valid_to = ores_utility_infinity_timestamp_fn()
    for update;

    if not found then
        return query select 'missing'::text, 'No such request.'::text, null::text, null::integer;
        return;
    end if;

    if v_request.state_code = 'apply_failed' then
        return query select 'ok'::text, ''::text, v_request.state_code, v_request.version;
        return;
    end if;

    if v_request.state_code <> 'approved' then
        return query select 'conflict'::text,
            format('The request is %s, and only an approved request can fail to apply.',
                   v_request.state_code),
            v_request.state_code, v_request.version;
        return;
    end if;

    v_request.state_code := 'apply_failed';
    v_request.modified_by := p_actor;
    v_request.change_reason_code := 'system.update';
    v_request.change_commentary := left(coalesce(p_reason, 'Could not apply'), 500);
    insert into ores_inbox_approval_requests_tbl select v_request.*;

    return query select 'ok'::text, ''::text, 'apply_failed'::text, v_request.version + 1;
end;
$$ language plpgsql;
