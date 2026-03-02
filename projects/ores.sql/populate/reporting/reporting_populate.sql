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
 * Reporting Component Population Script
 *
 * Seeds FSM machines for report definition and report instance lifecycles.
 * All scripts are idempotent.
 */

\echo '=== Reporting Component Population ==='
\echo ''

\echo '--- Enum Tables ---'
\ir ./reporting_report_types_populate.sql
\ir ./reporting_concurrency_policies_populate.sql

\echo ''
\echo '--- FSM Machines ---'
\ir ./dq_fsm_report_definition_lifecycle_populate.sql
\ir ./dq_fsm_report_instance_lifecycle_populate.sql

\echo ''
\echo '=== Reporting Component Population Complete ==='

-- Summary
select 'FSM Machines' as entity, count(*) as count
from ores_dq_fsm_machines_tbl
where name in ('report_definition_lifecycle', 'report_instance_lifecycle')
  and valid_to = ores_utility_infinity_timestamp_fn()
union all
select 'FSM States', count(*)
from ores_dq_fsm_states_tbl s
join ores_dq_fsm_machines_tbl m
  on m.tenant_id = s.tenant_id and m.id = s.machine_id
where m.name in ('report_definition_lifecycle', 'report_instance_lifecycle')
  and s.valid_to = ores_utility_infinity_timestamp_fn()
  and m.valid_to = ores_utility_infinity_timestamp_fn()
union all
select 'FSM Transitions', count(*)
from ores_dq_fsm_transitions_tbl t
join ores_dq_fsm_machines_tbl m
  on m.tenant_id = t.tenant_id and m.id = t.machine_id
where m.name in ('report_definition_lifecycle', 'report_instance_lifecycle')
  and t.valid_to = ores_utility_infinity_timestamp_fn()
  and m.valid_to = ores_utility_infinity_timestamp_fn()
order by entity;
