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

-- =============================================================================
-- Reporting Helper Functions
-- =============================================================================

/**
 * Returns the UUID of the initial FSM state for the report_definition_lifecycle
 * machine (the "draft" state) from the system tenant.
 *
 * Used by the reporting service when creating new report definitions to assign
 * the correct starting lifecycle state without embedding the UUID in application
 * code.
 *
 * Returns NULL if the state has not been seeded (e.g. on a fresh database
 * before population scripts have run).
 */
create or replace function ores_reporting_initial_definition_state_fn()
returns uuid
language sql
stable
security definer
set search_path = public
as $$
    select s.id
    from ores_dq_fsm_states_tbl s
    join ores_dq_fsm_machines_tbl m on m.id = s.machine_id
    where m.name     = 'report_definition_lifecycle'
      and s.name     = 'draft'
      and s.is_initial = 1
      and s.valid_to = ores_utility_infinity_timestamp_fn()
      and m.valid_to = ores_utility_infinity_timestamp_fn()
      and s.tenant_id = ores_iam_system_tenant_id_fn()
      and m.tenant_id = ores_iam_system_tenant_id_fn()
    limit 1;
$$;

/**
 * Returns the UUID of the "active" FSM state for the report_definition_lifecycle
 * machine from the system tenant.
 *
 * Used by the reporting service when a report definition is scheduled to
 * transition it to the active lifecycle state.
 *
 * Returns NULL if the state has not been seeded.
 */
create or replace function ores_reporting_active_definition_state_fn()
returns uuid
language sql
stable
security definer
set search_path = public
as $$
    select s.id
    from ores_dq_fsm_states_tbl s
    join ores_dq_fsm_machines_tbl m on m.id = s.machine_id
    where m.name     = 'report_definition_lifecycle'
      and s.name     = 'active'
      and s.valid_to = ores_utility_infinity_timestamp_fn()
      and m.valid_to = ores_utility_infinity_timestamp_fn()
      and s.tenant_id = ores_iam_system_tenant_id_fn()
      and m.tenant_id = ores_iam_system_tenant_id_fn()
    limit 1;
$$;

/**
 * Returns the UUID of the "suspended" FSM state for the report_definition_lifecycle
 * machine from the system tenant.
 *
 * Used by the reporting service when a report definition is unscheduled to
 * transition it to the suspended lifecycle state.
 *
 * Returns NULL if the state has not been seeded.
 */
create or replace function ores_reporting_suspended_definition_state_fn()
returns uuid
language sql
stable
security definer
set search_path = public
as $$
    select s.id
    from ores_dq_fsm_states_tbl s
    join ores_dq_fsm_machines_tbl m on m.id = s.machine_id
    where m.name     = 'report_definition_lifecycle'
      and s.name     = 'suspended'
      and s.valid_to = ores_utility_infinity_timestamp_fn()
      and m.valid_to = ores_utility_infinity_timestamp_fn()
      and s.tenant_id = ores_iam_system_tenant_id_fn()
      and m.tenant_id = ores_iam_system_tenant_id_fn()
    limit 1;
$$;
