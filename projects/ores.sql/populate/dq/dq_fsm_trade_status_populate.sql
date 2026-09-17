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
 * Trade Status FSM Population Script
 *
 * Seeds the trade_status state machine as the Trade Lifecycle knowledge
 * note defines it. That note is the authority: this script follows it and
 * does not restate the reasoning.
 *
 * - 1 machine: trade_status
 * - 4 states: draft, live, expired, cancelled
 *     draft and live are both initial -- there are two ways in. A trade
 *     booked against an agreement already struck enters at live; one
 *     recorded before any agreement exists enters at draft.
 *     No state is terminal: expired and cancelled are reversible, because
 *     either may have been recorded in error.
 * - 8 transitions:
 *     capture   (NULL      -> draft)
 *     new       (NULL      -> live)
 *     execute   (draft     -> live)
 *     discard   (draft     -> cancelled)
 *     expire    (live      -> expired)
 *     cancel    (live      -> cancelled)
 *     unexpire  (expired   -> live)
 *     uncancel  (cancelled -> live)
 *
 * Amend is not seeded. The note draws it as a self-loop on draft, expired
 * and cancelled, and an amendment never changes the state; an activity that
 * names no transition carries the prior status forward, which is that
 * behaviour exactly.
 *
 * Confirmation is not seeded either. It is a separate machine with its own
 * note, and a trade's confirmation status is not its lifecycle status.
 *
 * This script is idempotent: it skips insertion if the machine already exists.
 */

\echo '--- Trade Status FSM ---'

do $$
declare
    v_machine_id uuid;
    v_state_draft uuid;
    v_state_live uuid;
    v_state_expired uuid;
    v_state_cancelled uuid;
    v_sys_tenant uuid := ores_utility_system_tenant_id_fn();
begin
    -- -------------------------------------------------------------------------
    -- Machine
    -- -------------------------------------------------------------------------
    select id into v_machine_id
    from ores_dq_fsm_machines_tbl
    where tenant_id = v_sys_tenant
      and name = 'trade_status'
      and valid_to = ores_utility_infinity_timestamp_fn();

    if found then
        raise debug 'Trade status machine already exists (%), skipping.', v_machine_id;
        return;
    end if;

    v_machine_id := gen_random_uuid();

    insert into ores_dq_fsm_machines_tbl (
        id, tenant_id, version,
        name, description,
        modified_by, change_reason_code, change_commentary
    ) values (
        v_machine_id, v_sys_tenant, 0,
        'trade_status',
        'Where a trade has got to: draft or live, then expired or cancelled, both reversible.',
        current_user, 'system.initial_load', 'Seed trade_status FSM machine'
    );

    raise debug 'Created trade_status machine: %', v_machine_id;

    -- -------------------------------------------------------------------------
    -- States
    -- -------------------------------------------------------------------------
    v_state_draft     := gen_random_uuid();
    v_state_live      := gen_random_uuid();
    v_state_expired   := gen_random_uuid();
    v_state_cancelled := gen_random_uuid();

    insert into ores_dq_fsm_states_tbl (
        id, tenant_id, version,
        machine_id, name, is_initial, is_terminal,
        modified_by, change_reason_code, change_commentary
    ) values
        (v_state_draft, v_sys_tenant, 0,
         v_machine_id, 'draft', 1, 0,
         current_user, 'system.initial_load', 'Seed trade_status state: draft'),
        (v_state_live, v_sys_tenant, 0,
         v_machine_id, 'live', 1, 0,
         current_user, 'system.initial_load', 'Seed trade_status state: live'),
        (v_state_expired, v_sys_tenant, 0,
         v_machine_id, 'expired', 0, 0,
         current_user, 'system.initial_load', 'Seed trade_status state: expired'),
        (v_state_cancelled, v_sys_tenant, 0,
         v_machine_id, 'cancelled', 0, 0,
         current_user, 'system.initial_load', 'Seed trade_status state: cancelled');

    raise debug 'Created 4 trade_status states.';

    -- -------------------------------------------------------------------------
    -- Transitions (8 total)
    -- -------------------------------------------------------------------------
    insert into ores_dq_fsm_transitions_tbl (
        id, tenant_id, version,
        machine_id, from_state_id, to_state_id, name, guard_function,
        modified_by, change_reason_code, change_commentary
    ) values
        -- The two ways in.
        (gen_random_uuid(), v_sys_tenant, 0,
         v_machine_id, null, v_state_draft, 'capture', null,
         current_user, 'system.initial_load', 'NULL -> draft'),
        (gen_random_uuid(), v_sys_tenant, 0,
         v_machine_id, null, v_state_live, 'new', null,
         current_user, 'system.initial_load', 'NULL -> live'),
        -- Out of draft: the agreement is struck, or it never will be.
        (gen_random_uuid(), v_sys_tenant, 0,
         v_machine_id, v_state_draft, v_state_live, 'execute', null,
         current_user, 'system.initial_load', 'draft -> live'),
        (gen_random_uuid(), v_sys_tenant, 0,
         v_machine_id, v_state_draft, v_state_cancelled, 'discard', null,
         current_user, 'system.initial_load', 'draft -> cancelled'),
        -- Out of live.
        (gen_random_uuid(), v_sys_tenant, 0,
         v_machine_id, v_state_live, v_state_expired, 'expire', null,
         current_user, 'system.initial_load', 'live -> expired'),
        (gen_random_uuid(), v_sys_tenant, 0,
         v_machine_id, v_state_live, v_state_cancelled, 'cancel', null,
         current_user, 'system.initial_load', 'live -> cancelled'),
        -- Neither ending is terminal: both correct a record, rather than
        -- reviving an agreement.
        (gen_random_uuid(), v_sys_tenant, 0,
         v_machine_id, v_state_expired, v_state_live, 'unexpire', null,
         current_user, 'system.initial_load', 'expired -> live'),
        (gen_random_uuid(), v_sys_tenant, 0,
         v_machine_id, v_state_cancelled, v_state_live, 'uncancel', null,
         current_user, 'system.initial_load', 'cancelled -> live');

    raise debug 'Created 8 trade_status transitions.';
end;
$$;

-- Summary
select 'FSM Machines' as entity, count(*) as count
from ores_dq_fsm_machines_tbl where valid_to = ores_utility_infinity_timestamp_fn()
union all
select 'FSM States', count(*)
from ores_dq_fsm_states_tbl where valid_to = ores_utility_infinity_timestamp_fn()
union all
select 'FSM Transitions', count(*)
from ores_dq_fsm_transitions_tbl where valid_to = ores_utility_infinity_timestamp_fn();
