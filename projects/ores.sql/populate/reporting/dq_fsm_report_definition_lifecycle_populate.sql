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
 * Report Definition Lifecycle FSM Population Script
 *
 * Seeds the report definition lifecycle state machine with:
 *   1 machine  : report_definition_lifecycle
 *   4 states   : draft (initial), active, suspended, archived (terminal)
 *   6 transitions covering all legal lifecycle paths
 *
 * State semantics:
 *   draft     - Created but not yet scheduled. Editable in full.
 *   active    - Registered with the scheduler. Executes on its cron schedule.
 *   suspended - Temporarily removed from the scheduler. No new instances.
 *   archived  - Terminal. Retained for history; no further execution.
 *
 * This script is idempotent: it skips insertion if the machine already exists.
 */

\echo '--- Report Definition Lifecycle FSM ---'

do $$
declare
    v_machine_id   uuid;
    v_state_draft  uuid;
    v_state_active uuid;
    v_state_suspended uuid;
    v_state_archived  uuid;
    v_sys_tenant   uuid := ores_iam_system_tenant_id_fn();
begin
    -- -------------------------------------------------------------------------
    -- Machine
    -- -------------------------------------------------------------------------
    select id into v_machine_id
    from ores_dq_fsm_machines_tbl
    where tenant_id = v_sys_tenant
      and name = 'report_definition_lifecycle'
      and valid_to = ores_utility_infinity_timestamp_fn();

    if found then
        raise notice 'report_definition_lifecycle machine already exists (%), skipping.', v_machine_id;
        return;
    end if;

    v_machine_id := gen_random_uuid();

    insert into ores_dq_fsm_machines_tbl (
        id, tenant_id, version,
        name, description,
        modified_by, change_reason_code, change_commentary
    ) values (
        v_machine_id, v_sys_tenant, 0,
        'report_definition_lifecycle',
        'Manages the lifecycle of report definitions from draft through archiving.',
        current_user, 'system.initial_load', 'Seed report_definition_lifecycle FSM machine'
    );

    raise notice 'Created report_definition_lifecycle machine: %', v_machine_id;

    -- -------------------------------------------------------------------------
    -- States
    -- -------------------------------------------------------------------------
    v_state_draft     := gen_random_uuid();
    v_state_active    := gen_random_uuid();
    v_state_suspended := gen_random_uuid();
    v_state_archived  := gen_random_uuid();

    insert into ores_dq_fsm_states_tbl (
        id, tenant_id, version,
        machine_id, name, is_initial, is_terminal,
        modified_by, change_reason_code, change_commentary
    ) values
        (v_state_draft, v_sys_tenant, 0,
         v_machine_id, 'draft', 1, 0,
         current_user, 'system.initial_load', 'Seed report_definition_lifecycle state: draft'),
        (v_state_active, v_sys_tenant, 0,
         v_machine_id, 'active', 0, 0,
         current_user, 'system.initial_load', 'Seed report_definition_lifecycle state: active'),
        (v_state_suspended, v_sys_tenant, 0,
         v_machine_id, 'suspended', 0, 0,
         current_user, 'system.initial_load', 'Seed report_definition_lifecycle state: suspended'),
        (v_state_archived, v_sys_tenant, 0,
         v_machine_id, 'archived', 0, 1,
         current_user, 'system.initial_load', 'Seed report_definition_lifecycle state: archived');

    raise notice 'Created 4 report_definition_lifecycle states.';

    -- -------------------------------------------------------------------------
    -- Transitions (6 total)
    -- -------------------------------------------------------------------------
    insert into ores_dq_fsm_transitions_tbl (
        id, tenant_id, version,
        machine_id, from_state_id, to_state_id, name, guard_function,
        modified_by, change_reason_code, change_commentary
    ) values
        -- draft -> active (user activates; scheduler registers cron job)
        (gen_random_uuid(), v_sys_tenant, 0,
         v_machine_id, v_state_draft, v_state_active, 'Activate', null,
         current_user, 'system.initial_load', 'draft -> active'),
        -- draft -> archived (user archives without ever activating)
        (gen_random_uuid(), v_sys_tenant, 0,
         v_machine_id, v_state_draft, v_state_archived, 'Archive', null,
         current_user, 'system.initial_load', 'draft -> archived'),
        -- active -> suspended (user suspends; scheduler removes cron job)
        (gen_random_uuid(), v_sys_tenant, 0,
         v_machine_id, v_state_active, v_state_suspended, 'Suspend', null,
         current_user, 'system.initial_load', 'active -> suspended'),
        -- active -> archived (user archives an active definition)
        (gen_random_uuid(), v_sys_tenant, 0,
         v_machine_id, v_state_active, v_state_archived, 'Archive', null,
         current_user, 'system.initial_load', 'active -> archived'),
        -- suspended -> active (user reactivates; scheduler re-registers cron job)
        (gen_random_uuid(), v_sys_tenant, 0,
         v_machine_id, v_state_suspended, v_state_active, 'Reactivate', null,
         current_user, 'system.initial_load', 'suspended -> active'),
        -- suspended -> archived (user archives a suspended definition)
        (gen_random_uuid(), v_sys_tenant, 0,
         v_machine_id, v_state_suspended, v_state_archived, 'Archive', null,
         current_user, 'system.initial_load', 'suspended -> archived');

    raise notice 'Created 6 report_definition_lifecycle transitions.';
end;
$$;
