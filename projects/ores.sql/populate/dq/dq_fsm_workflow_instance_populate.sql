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
 * Workflow Instance FSM Population Script
 *
 * Seeds the workflow_instance state machine with:
 * - 1 machine: workflow_instance
 * - 6 states: pending, in_progress, completed, failed, compensating, compensated
 * - 6 transitions:
 *     initial_start         (NULL          -> in_progress)
 *     complete              (in_progress   -> completed)
 *     fail                  (in_progress   -> failed)
 *     begin_compensation    (in_progress   -> compensating)
 *     finish_compensation   (compensating  -> compensated)
 *     fail_during_compensation (compensating -> failed)
 *
 * This script is idempotent: it skips insertion if the machine already exists.
 */

\echo '--- Workflow Instance FSM ---'

do $$
declare
    v_machine_id         uuid;
    v_state_pending      uuid;
    v_state_in_progress  uuid;
    v_state_completed    uuid;
    v_state_failed       uuid;
    v_state_compensating uuid;
    v_state_compensated  uuid;
    v_sys_tenant         uuid := ores_iam_system_tenant_id_fn();
begin
    -- -------------------------------------------------------------------------
    -- Machine
    -- -------------------------------------------------------------------------
    select id into v_machine_id
    from ores_dq_fsm_machines_tbl
    where tenant_id = v_sys_tenant
      and name = 'workflow_instance'
      and valid_to = ores_utility_infinity_timestamp_fn();

    if found then
        raise notice 'workflow_instance machine already exists (%), skipping.', v_machine_id;
        return;
    end if;

    v_machine_id := gen_random_uuid();

    insert into ores_dq_fsm_machines_tbl (
        id, tenant_id, version,
        name, description,
        modified_by, change_reason_code, change_commentary
    ) values (
        v_machine_id, v_sys_tenant, 0,
        'workflow_instance',
        'Lifecycle of a saga workflow execution: pending -> in_progress -> completed / compensated.',
        current_user, 'system.initial_load', 'Seed workflow_instance FSM machine'
    );

    raise notice 'Created workflow_instance machine: %', v_machine_id;

    -- -------------------------------------------------------------------------
    -- States
    -- -------------------------------------------------------------------------
    v_state_pending      := gen_random_uuid();
    v_state_in_progress  := gen_random_uuid();
    v_state_completed    := gen_random_uuid();
    v_state_failed       := gen_random_uuid();
    v_state_compensating := gen_random_uuid();
    v_state_compensated  := gen_random_uuid();

    insert into ores_dq_fsm_states_tbl (
        id, tenant_id, version,
        machine_id, name, is_initial, is_terminal,
        modified_by, change_reason_code, change_commentary
    ) values
        (v_state_pending, v_sys_tenant, 0,
         v_machine_id, 'pending', 1, 0,
         current_user, 'system.initial_load', 'Seed workflow_instance state: pending'),
        (v_state_in_progress, v_sys_tenant, 0,
         v_machine_id, 'in_progress', 0, 0,
         current_user, 'system.initial_load', 'Seed workflow_instance state: in_progress'),
        (v_state_completed, v_sys_tenant, 0,
         v_machine_id, 'completed', 0, 1,
         current_user, 'system.initial_load', 'Seed workflow_instance state: completed'),
        (v_state_failed, v_sys_tenant, 0,
         v_machine_id, 'failed', 0, 1,
         current_user, 'system.initial_load', 'Seed workflow_instance state: failed'),
        (v_state_compensating, v_sys_tenant, 0,
         v_machine_id, 'compensating', 0, 0,
         current_user, 'system.initial_load', 'Seed workflow_instance state: compensating'),
        (v_state_compensated, v_sys_tenant, 0,
         v_machine_id, 'compensated', 0, 1,
         current_user, 'system.initial_load', 'Seed workflow_instance state: compensated');

    raise notice 'Created 6 workflow_instance states.';

    -- -------------------------------------------------------------------------
    -- Transitions (6 total)
    -- -------------------------------------------------------------------------
    insert into ores_dq_fsm_transitions_tbl (
        id, tenant_id, version,
        machine_id, from_state_id, to_state_id, name, guard_function,
        modified_by, change_reason_code, change_commentary
    ) values
        -- Initial start: NULL -> in_progress (workflow created and immediately started)
        (gen_random_uuid(), v_sys_tenant, 0,
         v_machine_id, null, v_state_in_progress, 'initial_start', null,
         current_user, 'system.initial_load', 'NULL -> in_progress'),
        -- Success path
        (gen_random_uuid(), v_sys_tenant, 0,
         v_machine_id, v_state_in_progress, v_state_completed, 'complete', null,
         current_user, 'system.initial_load', 'in_progress -> completed'),
        -- Unhandled exception: skip compensation
        (gen_random_uuid(), v_sys_tenant, 0,
         v_machine_id, v_state_in_progress, v_state_failed, 'fail', null,
         current_user, 'system.initial_load', 'in_progress -> failed'),
        -- Executor returned false: begin saga rollback
        (gen_random_uuid(), v_sys_tenant, 0,
         v_machine_id, v_state_in_progress, v_state_compensating, 'begin_compensation', null,
         current_user, 'system.initial_load', 'in_progress -> compensating'),
        -- Compensation completed successfully
        (gen_random_uuid(), v_sys_tenant, 0,
         v_machine_id, v_state_compensating, v_state_compensated, 'finish_compensation', null,
         current_user, 'system.initial_load', 'compensating -> compensated'),
        -- Compensation itself threw an exception
        (gen_random_uuid(), v_sys_tenant, 0,
         v_machine_id, v_state_compensating, v_state_failed, 'fail_during_compensation', null,
         current_user, 'system.initial_load', 'compensating -> failed');

    raise notice 'Created 6 workflow_instance transitions.';
end;
$$;
