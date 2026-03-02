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
 * Report Instance Lifecycle FSM Population Script
 *
 * Seeds the report instance lifecycle state machine with:
 *   1 machine  : report_instance_lifecycle
 *   7 states   : pending (initial), queued, running,
 *                completed/failed/cancelled/skipped (all terminal)
 *   7 transitions covering all legal instance lifecycle paths
 *
 * The initial state when an instance is created depends on the parent
 * definition's concurrency_policy and whether another instance is already
 * running (application-layer logic; not encoded in the FSM table itself):
 *   - No running instance          -> pending
 *   - Running + policy = queue     -> queued
 *   - Running + policy = skip      -> skipped (terminal, written directly)
 *   - Running + policy = fail      -> failed  (terminal, written directly)
 *
 * This script is idempotent: it skips insertion if the machine already exists.
 */

\echo '--- Report Instance Lifecycle FSM ---'

do $$
declare
    v_machine_id       uuid;
    v_state_pending    uuid;
    v_state_queued     uuid;
    v_state_running    uuid;
    v_state_completed  uuid;
    v_state_failed     uuid;
    v_state_cancelled  uuid;
    v_state_skipped    uuid;
    v_sys_tenant       uuid := ores_iam_system_tenant_id_fn();
begin
    -- -------------------------------------------------------------------------
    -- Machine
    -- -------------------------------------------------------------------------
    select id into v_machine_id
    from ores_dq_fsm_machines_tbl
    where tenant_id = v_sys_tenant
      and name = 'report_instance_lifecycle'
      and valid_to = ores_utility_infinity_timestamp_fn();

    if found then
        raise notice 'report_instance_lifecycle machine already exists (%), skipping.', v_machine_id;
        return;
    end if;

    v_machine_id := gen_random_uuid();

    insert into ores_dq_fsm_machines_tbl (
        id, tenant_id, version,
        name, description,
        modified_by, change_reason_code, change_commentary
    ) values (
        v_machine_id, v_sys_tenant, 0,
        'report_instance_lifecycle',
        'Manages the lifecycle of a single report execution from creation through completion.',
        current_user, 'system.initial_load', 'Seed report_instance_lifecycle FSM machine'
    );

    raise notice 'Created report_instance_lifecycle machine: %', v_machine_id;

    -- -------------------------------------------------------------------------
    -- States
    -- -------------------------------------------------------------------------
    v_state_pending   := gen_random_uuid();
    v_state_queued    := gen_random_uuid();
    v_state_running   := gen_random_uuid();
    v_state_completed := gen_random_uuid();
    v_state_failed    := gen_random_uuid();
    v_state_cancelled := gen_random_uuid();
    v_state_skipped   := gen_random_uuid();

    insert into ores_dq_fsm_states_tbl (
        id, tenant_id, version,
        machine_id, name, is_initial, is_terminal,
        modified_by, change_reason_code, change_commentary
    ) values
        -- pending: canonical initial state (no concurrent instance running)
        (v_state_pending, v_sys_tenant, 0,
         v_machine_id, 'pending', 1, 0,
         current_user, 'system.initial_load', 'Seed report_instance_lifecycle state: pending'),
        -- queued: initial state under queue concurrency policy
        (v_state_queued, v_sys_tenant, 0,
         v_machine_id, 'queued', 1, 0,
         current_user, 'system.initial_load', 'Seed report_instance_lifecycle state: queued'),
        (v_state_running, v_sys_tenant, 0,
         v_machine_id, 'running', 0, 0,
         current_user, 'system.initial_load', 'Seed report_instance_lifecycle state: running'),
        (v_state_completed, v_sys_tenant, 0,
         v_machine_id, 'completed', 0, 1,
         current_user, 'system.initial_load', 'Seed report_instance_lifecycle state: completed'),
        (v_state_failed, v_sys_tenant, 0,
         v_machine_id, 'failed', 0, 1,
         current_user, 'system.initial_load', 'Seed report_instance_lifecycle state: failed'),
        (v_state_cancelled, v_sys_tenant, 0,
         v_machine_id, 'cancelled', 0, 1,
         current_user, 'system.initial_load', 'Seed report_instance_lifecycle state: cancelled'),
        -- skipped: terminal initial state under skip concurrency policy
        (v_state_skipped, v_sys_tenant, 0,
         v_machine_id, 'skipped', 1, 1,
         current_user, 'system.initial_load', 'Seed report_instance_lifecycle state: skipped');

    raise notice 'Created 7 report_instance_lifecycle states.';

    -- -------------------------------------------------------------------------
    -- Transitions (7 total)
    -- -------------------------------------------------------------------------
    insert into ores_dq_fsm_transitions_tbl (
        id, tenant_id, version,
        machine_id, from_state_id, to_state_id, name, guard_function,
        modified_by, change_reason_code, change_commentary
    ) values
        -- queued -> pending (prior instance finished; this one can now start)
        (gen_random_uuid(), v_sys_tenant, 0,
         v_machine_id, v_state_queued, v_state_pending, 'Promote', null,
         current_user, 'system.initial_load', 'queued -> pending'),
        -- queued -> cancelled (user or archival cancels while waiting)
        (gen_random_uuid(), v_sys_tenant, 0,
         v_machine_id, v_state_queued, v_state_cancelled, 'Cancel', null,
         current_user, 'system.initial_load', 'queued -> cancelled'),
        -- pending -> running (execution starts)
        (gen_random_uuid(), v_sys_tenant, 0,
         v_machine_id, v_state_pending, v_state_running, 'Start', null,
         current_user, 'system.initial_load', 'pending -> running'),
        -- pending -> cancelled (user cancels before execution begins)
        (gen_random_uuid(), v_sys_tenant, 0,
         v_machine_id, v_state_pending, v_state_cancelled, 'Cancel', null,
         current_user, 'system.initial_load', 'pending -> cancelled'),
        -- running -> completed (execution finished successfully)
        (gen_random_uuid(), v_sys_tenant, 0,
         v_machine_id, v_state_running, v_state_completed, 'Complete', null,
         current_user, 'system.initial_load', 'running -> completed'),
        -- running -> failed (execution finished with an error)
        (gen_random_uuid(), v_sys_tenant, 0,
         v_machine_id, v_state_running, v_state_failed, 'Fail', null,
         current_user, 'system.initial_load', 'running -> failed'),
        -- running -> cancelled (user cancels an in-progress execution)
        (gen_random_uuid(), v_sys_tenant, 0,
         v_machine_id, v_state_running, v_state_cancelled, 'Cancel', null,
         current_user, 'system.initial_load', 'running -> cancelled');

    raise notice 'Created 7 report_instance_lifecycle transitions.';
end;
$$;
