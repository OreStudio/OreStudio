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
 * The answer a gated write gets from the policy.
 *
 * The function says what the policy requires for an act and whether the request
 * the caller names meets it. It returns one of three words:
 *
 *   free      The policy names no part for the act. The write goes ahead.
 *   approved  The policy names a part, and the named request is approved.
 *   required  The policy names a part, and no approved request is named.
 *
 * The function reads the request only for its state. It does not check that the
 * request holds the write: that is a fact about the entity's own pending table,
 * so the entity's service checks it. A request id that is not a uuid reads as no
 * request.
 */
create or replace function ores_inbox_gate_fn(
    p_entity_type text,
    p_operation text,
    p_columns text[],
    p_request_id text
) returns text as $$
begin
    if not exists (select 1 from ores_inbox_policy_parts_fn(p_entity_type, p_operation, p_columns)) then
        return 'free';
    end if;

    -- The shape is tested in its own statement: SQL does not promise that the
    -- left side of an and runs before the cast on the right.
    if p_request_id is null or p_request_id !~* '^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$' then
        return 'required';
    end if;

    if exists (
        select 1
        from ores_inbox_approval_requests_tbl r
        where r.id = p_request_id::uuid
          and r.tenant_id = ores_iam_current_tenant_id_fn()
          and r.state_code = 'approved'
          and r.valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        return 'approved';
    end if;

    return 'required';
end;
$$ language plpgsql stable;
