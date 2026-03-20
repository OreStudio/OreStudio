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

-- Seed the compute stale-result reaper job (system scope, runs every minute).
-- Publishes to compute.v1.work.reap which triggers the reaper handler in
-- ores.compute.service to reset stale in-progress results back to Unsent.
insert into ores_scheduler_job_definitions_tbl (
    id, tenant_id, party_id, job_name, description, command,
    schedule_expression, action_type, action_payload, is_active,
    modified_by, performed_by, change_reason_code, change_commentary,
    valid_from, valid_to
)
select
    gen_random_uuid(), NULL, NULL,
    'compute.v1.reap.stale_results',
    'Reset stale in-progress compute results every minute',
    '',
    '* * * * *',
    'nats_publish',
    '{"subject":"compute.v1.work.reap"}',
    1,
    'system', 'system', 'system.new_record', '',
    current_timestamp, ores_utility_infinity_timestamp_fn()
where not exists (
    select 1 from ores_scheduler_job_definitions_tbl
    where job_name = 'compute.v1.reap.stale_results'
      and tenant_id is null
      and valid_to = ores_utility_infinity_timestamp_fn()
);
