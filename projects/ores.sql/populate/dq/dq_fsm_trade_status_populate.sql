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
 * Seeds the trade_status state machine with:
 * - 1 machine: trade_status
 * - 4 states: new, live, expired, cancelled
 * - 5 transitions:
 *     initial_booking  (NULL -> new)
 *     confirm          (new  -> live)
 *     cancel_new       (new  -> cancelled)
 *     expire           (live -> expired)
 *     cancel_live      (live -> cancelled)
 *
 * This script is idempotent: it skips insertion if the machine already exists.
 */

\echo '--- Trade Status FSM ---'

do $$
declare
    v_machine_id uuid;
    v_state_new uuid;
    v_state_live uuid;
    v_state_expired uuid;
    v_state_cancelled uuid;
    v_sys_tenant uuid := ores_iam_system_tenant_id_fn();
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
        raise notice 'Trade status machine already exists (%), skipping.', v_machine_id;
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
        'Operational status of a trade: new → live → expired / cancelled.',
        current_user, 'system.initial_load', 'Seed trade_status FSM machine'
    );

    raise notice 'Created trade_status machine: %', v_machine_id;

    -- -------------------------------------------------------------------------
    -- States
    -- -------------------------------------------------------------------------
    v_state_new       := gen_random_uuid();
    v_state_live      := gen_random_uuid();
    v_state_expired   := gen_random_uuid();
    v_state_cancelled := gen_random_uuid();

    insert into ores_dq_fsm_states_tbl (
        id, tenant_id, version,
        machine_id, name, is_initial, is_terminal,
        modified_by, change_reason_code, change_commentary
    ) values
        (v_state_new, v_sys_tenant, 0,
         v_machine_id, 'new', 1, 0,
         current_user, 'system.initial_load', 'Seed trade_status state: new'),
        (v_state_live, v_sys_tenant, 0,
         v_machine_id, 'live', 0, 0,
         current_user, 'system.initial_load', 'Seed trade_status state: live'),
        (v_state_expired, v_sys_tenant, 0,
         v_machine_id, 'expired', 0, 1,
         current_user, 'system.initial_load', 'Seed trade_status state: expired'),
        (v_state_cancelled, v_sys_tenant, 0,
         v_machine_id, 'cancelled', 0, 1,
         current_user, 'system.initial_load', 'Seed trade_status state: cancelled');

    raise notice 'Created 4 trade_status states.';

    -- -------------------------------------------------------------------------
    -- Transitions (5 total)
    -- -------------------------------------------------------------------------
    insert into ores_dq_fsm_transitions_tbl (
        id, tenant_id, version,
        machine_id, from_state_id, to_state_id, name, guard_function,
        modified_by, change_reason_code, change_commentary
    ) values
        -- Initial booking: no prior state (NULL from_state_id)
        (gen_random_uuid(), v_sys_tenant, 0,
         v_machine_id, null, v_state_new, 'initial_booking', null,
         current_user, 'system.initial_load', 'NULL -> new'),
        -- Confirm: new -> live (trade confirmed by counterparty)
        (gen_random_uuid(), v_sys_tenant, 0,
         v_machine_id, v_state_new, v_state_live, 'confirm', null,
         current_user, 'system.initial_load', 'new -> live'),
        -- Cancel new: new -> cancelled (cancel before confirmation)
        (gen_random_uuid(), v_sys_tenant, 0,
         v_machine_id, v_state_new, v_state_cancelled, 'cancel_new', null,
         current_user, 'system.initial_load', 'new -> cancelled'),
        -- Expire: live -> expired (trade reaches maturity)
        (gen_random_uuid(), v_sys_tenant, 0,
         v_machine_id, v_state_live, v_state_expired, 'expire', null,
         current_user, 'system.initial_load', 'live -> expired'),
        -- Cancel live: live -> cancelled (early termination)
        (gen_random_uuid(), v_sys_tenant, 0,
         v_machine_id, v_state_live, v_state_cancelled, 'cancel_live', null,
         current_user, 'system.initial_load', 'live -> cancelled');

    raise notice 'Created 5 trade_status transitions.';
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
