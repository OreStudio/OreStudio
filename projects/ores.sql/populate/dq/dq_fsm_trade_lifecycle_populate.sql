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
 * Trade Lifecycle FSM Population Script
 *
 * Seeds the trade lifecycle state machine with:
 * - 1 machine: trade_lifecycle
 * - 5 states: New, Amendment, Novation, PartialTermination, FullTermination
 * - 15 transitions covering all legal lifecycle paths
 *
 * This script is idempotent: it skips insertion if the machine already exists.
 */

\echo '--- Trade Lifecycle FSM ---'

do $$
declare
    v_machine_id uuid;
    v_state_new uuid;
    v_state_amendment uuid;
    v_state_novation uuid;
    v_state_partial_termination uuid;
    v_state_full_termination uuid;
    v_sys_tenant uuid := ores_iam_system_tenant_id_fn();
begin
    -- -------------------------------------------------------------------------
    -- Machine
    -- -------------------------------------------------------------------------
    -- Check if already seeded
    select id into v_machine_id
    from ores_dq_fsm_machines_tbl
    where tenant_id = v_sys_tenant
      and name = 'trade_lifecycle'
      and valid_to = ores_utility_infinity_timestamp_fn();

    if found then
        raise notice 'Trade lifecycle machine already exists (%), skipping.', v_machine_id;
        return;
    end if;

    v_machine_id := gen_random_uuid();

    insert into ores_dq_fsm_machines_tbl (
        id, tenant_id, version,
        name, description,
        modified_by, change_reason_code, change_commentary
    ) values (
        v_machine_id, v_sys_tenant, 0,
        'trade_lifecycle',
        'Manages the lifecycle of financial trades from booking through termination.',
        current_user, 'system.initial_load', 'Seed trade lifecycle FSM machine'
    );

    raise notice 'Created trade_lifecycle machine: %', v_machine_id;

    -- -------------------------------------------------------------------------
    -- States
    -- -------------------------------------------------------------------------
    v_state_new               := gen_random_uuid();
    v_state_amendment         := gen_random_uuid();
    v_state_novation          := gen_random_uuid();
    v_state_partial_termination := gen_random_uuid();
    v_state_full_termination  := gen_random_uuid();

    insert into ores_dq_fsm_states_tbl (
        id, tenant_id, version,
        machine_id, name, is_initial, is_terminal,
        modified_by, change_reason_code, change_commentary
    ) values
        (v_state_new, v_sys_tenant, 0,
         v_machine_id, 'New', 1, 0,
         current_user, 'system.initial_load', 'Seed trade lifecycle state: New'),
        (v_state_amendment, v_sys_tenant, 0,
         v_machine_id, 'Amendment', 0, 0,
         current_user, 'system.initial_load', 'Seed trade lifecycle state: Amendment'),
        (v_state_novation, v_sys_tenant, 0,
         v_machine_id, 'Novation', 0, 0,
         current_user, 'system.initial_load', 'Seed trade lifecycle state: Novation'),
        (v_state_partial_termination, v_sys_tenant, 0,
         v_machine_id, 'PartialTermination', 0, 0,
         current_user, 'system.initial_load', 'Seed trade lifecycle state: PartialTermination'),
        (v_state_full_termination, v_sys_tenant, 0,
         v_machine_id, 'FullTermination', 0, 1,
         current_user, 'system.initial_load', 'Seed trade lifecycle state: FullTermination');

    raise notice 'Created 5 trade lifecycle states.';

    -- -------------------------------------------------------------------------
    -- Transitions (15 total - all legal lifecycle paths)
    -- -------------------------------------------------------------------------
    insert into ores_dq_fsm_transitions_tbl (
        id, tenant_id, version,
        machine_id, from_state_id, to_state_id, name, guard_function,
        modified_by, change_reason_code, change_commentary
    ) values
        -- From New
        (gen_random_uuid(), v_sys_tenant, 0,
         v_machine_id, v_state_new, v_state_amendment, 'Amend', null,
         current_user, 'system.initial_load', 'New -> Amendment'),
        (gen_random_uuid(), v_sys_tenant, 0,
         v_machine_id, v_state_new, v_state_novation, 'Novate', null,
         current_user, 'system.initial_load', 'New -> Novation'),
        (gen_random_uuid(), v_sys_tenant, 0,
         v_machine_id, v_state_new, v_state_partial_termination, 'PartiallyTerminate', null,
         current_user, 'system.initial_load', 'New -> PartialTermination'),
        (gen_random_uuid(), v_sys_tenant, 0,
         v_machine_id, v_state_new, v_state_full_termination, 'FullyTerminate', null,
         current_user, 'system.initial_load', 'New -> FullTermination'),
        -- From Amendment
        (gen_random_uuid(), v_sys_tenant, 0,
         v_machine_id, v_state_amendment, v_state_amendment, 'Amend', null,
         current_user, 'system.initial_load', 'Amendment -> Amendment'),
        (gen_random_uuid(), v_sys_tenant, 0,
         v_machine_id, v_state_amendment, v_state_novation, 'Novate', null,
         current_user, 'system.initial_load', 'Amendment -> Novation'),
        (gen_random_uuid(), v_sys_tenant, 0,
         v_machine_id, v_state_amendment, v_state_partial_termination, 'PartiallyTerminate', null,
         current_user, 'system.initial_load', 'Amendment -> PartialTermination'),
        (gen_random_uuid(), v_sys_tenant, 0,
         v_machine_id, v_state_amendment, v_state_full_termination, 'FullyTerminate', null,
         current_user, 'system.initial_load', 'Amendment -> FullTermination'),
        -- From Novation
        (gen_random_uuid(), v_sys_tenant, 0,
         v_machine_id, v_state_novation, v_state_amendment, 'Amend', null,
         current_user, 'system.initial_load', 'Novation -> Amendment'),
        (gen_random_uuid(), v_sys_tenant, 0,
         v_machine_id, v_state_novation, v_state_novation, 'Novate', null,
         current_user, 'system.initial_load', 'Novation -> Novation'),
        (gen_random_uuid(), v_sys_tenant, 0,
         v_machine_id, v_state_novation, v_state_partial_termination, 'PartiallyTerminate', null,
         current_user, 'system.initial_load', 'Novation -> PartialTermination'),
        (gen_random_uuid(), v_sys_tenant, 0,
         v_machine_id, v_state_novation, v_state_full_termination, 'FullyTerminate', null,
         current_user, 'system.initial_load', 'Novation -> FullTermination'),
        -- From PartialTermination
        (gen_random_uuid(), v_sys_tenant, 0,
         v_machine_id, v_state_partial_termination, v_state_amendment, 'Amend', null,
         current_user, 'system.initial_load', 'PartialTermination -> Amendment'),
        (gen_random_uuid(), v_sys_tenant, 0,
         v_machine_id, v_state_partial_termination, v_state_partial_termination, 'PartiallyTerminate', null,
         current_user, 'system.initial_load', 'PartialTermination -> PartialTermination'),
        (gen_random_uuid(), v_sys_tenant, 0,
         v_machine_id, v_state_partial_termination, v_state_full_termination, 'FullyTerminate', null,
         current_user, 'system.initial_load', 'PartialTermination -> FullTermination');

    raise notice 'Created 15 trade lifecycle transitions.';
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
