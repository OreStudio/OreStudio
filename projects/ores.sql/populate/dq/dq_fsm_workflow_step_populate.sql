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
 * Workflow Step FSM Population Script
 *
 * Seeds the workflow_step state machine with:
 * - 1 machine: workflow_step
 * - 5 states: pending, in_progress, completed, failed, compensated
 * - 4 transitions:
 *     initial_start  (NULL         -> in_progress)
 *     complete       (in_progress  -> completed)
 *     fail           (in_progress  -> failed)
 *     compensate     (failed       -> compensated)
 *
 * This script is idempotent: it skips insertion if the machine already exists.
 */

\echo '--- Workflow Step FSM ---'

do $$
declare
    v_machine_id        uuid;
    v_state_pending     uuid;
    v_state_in_progress uuid;
    v_state_completed   uuid;
    v_state_failed      uuid;
    v_state_compensated uuid;
    v_sys_tenant        uuid := ores_iam_system_tenant_id_fn();
begin
    -- -------------------------------------------------------------------------
    -- Machine
    -- -------------------------------------------------------------------------
    select id into v_machine_id
    from ores_dq_fsm_machines_tbl
    where tenant_id = v_sys_tenant
      and name = 'workflow_step'
      and valid_to = ores_utility_infinity_timestamp_fn();

    if found then
        raise notice 'workflow_step machine already exists (%), skipping.', v_machine_id;
        return;
    end if;

    v_machine_id := gen_random_uuid();

    insert into ores_dq_fsm_machines_tbl (
        id, tenant_id, version,
        name, description,
        modified_by, change_reason_code, change_commentary
    ) values (
        v_machine_id, v_sys_tenant, 0,
        'workflow_step',
        'Lifecycle of a single step within a saga workflow execution.',
        current_user, 'system.initial_load', 'Seed workflow_step FSM machine'
    );

    raise notice 'Created workflow_step machine: %', v_machine_id;

    -- -------------------------------------------------------------------------
    -- States
    -- -------------------------------------------------------------------------
    v_state_pending     := gen_random_uuid();
    v_state_in_progress := gen_random_uuid();
    v_state_completed   := gen_random_uuid();
    v_state_failed      := gen_random_uuid();
    v_state_compensated := gen_random_uuid();

    insert into ores_dq_fsm_states_tbl (
        id, tenant_id, version,
        machine_id, name, is_initial, is_terminal,
        modified_by, change_reason_code, change_commentary
    ) values
        (v_state_pending, v_sys_tenant, 0,
         v_machine_id, 'pending', 1, 0,
         current_user, 'system.initial_load', 'Seed workflow_step state: pending'),
        (v_state_in_progress, v_sys_tenant, 0,
         v_machine_id, 'in_progress', 0, 0,
         current_user, 'system.initial_load', 'Seed workflow_step state: in_progress'),
        (v_state_completed, v_sys_tenant, 0,
         v_machine_id, 'completed', 0, 1,
         current_user, 'system.initial_load', 'Seed workflow_step state: completed'),
        (v_state_failed, v_sys_tenant, 0,
         v_machine_id, 'failed', 0, 1,
         current_user, 'system.initial_load', 'Seed workflow_step state: failed'),
        (v_state_compensated, v_sys_tenant, 0,
         v_machine_id, 'compensated', 0, 1,
         current_user, 'system.initial_load', 'Seed workflow_step state: compensated');

    raise notice 'Created 5 workflow_step states.';

    -- -------------------------------------------------------------------------
    -- Transitions (4 total)
    -- -------------------------------------------------------------------------
    insert into ores_dq_fsm_transitions_tbl (
        id, tenant_id, version,
        machine_id, from_state_id, to_state_id, name, guard_function,
        modified_by, change_reason_code, change_commentary
    ) values
        -- Initial start: NULL -> in_progress (step created and immediately started)
        (gen_random_uuid(), v_sys_tenant, 0,
         v_machine_id, null, v_state_in_progress, 'initial_start', null,
         current_user, 'system.initial_load', 'NULL -> in_progress'),
        -- Step completed successfully
        (gen_random_uuid(), v_sys_tenant, 0,
         v_machine_id, v_state_in_progress, v_state_completed, 'complete', null,
         current_user, 'system.initial_load', 'in_progress -> completed'),
        -- Step failed
        (gen_random_uuid(), v_sys_tenant, 0,
         v_machine_id, v_state_in_progress, v_state_failed, 'fail', null,
         current_user, 'system.initial_load', 'in_progress -> failed'),
        -- Step rolled back during saga compensation
        (gen_random_uuid(), v_sys_tenant, 0,
         v_machine_id, v_state_failed, v_state_compensated, 'compensate', null,
         current_user, 'system.initial_load', 'failed -> compensated');

    raise notice 'Created 4 workflow_step transitions.';
end;
$$;
