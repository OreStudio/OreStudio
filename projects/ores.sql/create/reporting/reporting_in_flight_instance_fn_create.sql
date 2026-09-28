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
 * In-flight Report Instance Query
 *
 * Answers the one question the concurrency policy asks: is an instance of this
 * definition already in flight? The trigger handler needs it before it decides
 * between pending, queued, skipped and failed.
 *
 * "In flight" is read from the machine, not from a list of state names: an
 * instance is in flight while its state is not terminal. A state added to the
 * machine later is then classified correctly without this function changing.
 *
 * The generated repository offers only offset and limit, with no filter by
 * definition and no filter by state, so a caller that wanted this answer
 * through it would have to page the whole tenant and filter in C++. The
 * question is asked on every trigger, so it is asked here instead.
 */

create or replace function ores_reporting_in_flight_instance_fn(
    p_tenant_id     uuid,
    p_definition_id uuid
)
returns uuid as $$
    select i.id
    from ores_reporting_report_instances_tbl i
    join ores_dq_fsm_states_tbl s
      on s.tenant_id = i.tenant_id
     and s.id = i.fsm_state_id
     and s.valid_to = ores_utility_infinity_timestamp_fn()
    where i.tenant_id = p_tenant_id
      and i.definition_id = p_definition_id
      and i.valid_to = ores_utility_infinity_timestamp_fn()
      and s.is_terminal = false
    order by i.valid_from
    limit 1;
$$ language sql stable security definer set search_path = public, pg_temp;

comment on function ores_reporting_in_flight_instance_fn(uuid, uuid) is
'The id of one report instance of this definition that has not reached a
terminal state, or null when none is in flight. The oldest is returned, so the
answer is stable while more than one waits.';

revoke execute on function ores_reporting_in_flight_instance_fn(uuid, uuid) from public;

-- PostgreSQL grants EXECUTE to PUBLIC on every new function, so the revoke
-- above is what makes the restriction real; the grant is what keeps the
-- reporting service able to ask the question. Without it the trigger fails
-- with "permission denied for function".
grant execute on function ores_reporting_in_flight_instance_fn(uuid, uuid)
    to :reporting_service_user;
