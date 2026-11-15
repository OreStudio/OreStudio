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

-- Seed the MQ stats scrape job (system scope, runs every minute)
insert into ores_scheduler_job_definitions_tbl (
    id, tenant_id, party_id, job_name, description, command,
    schedule_expression, action_type, action_payload, is_active,
    modified_by, performed_by, change_reason_code, change_commentary,
    valid_from, valid_to
) values (
    gen_random_uuid(), NULL, NULL,
    'ores.mq.metrics_scrape',
    'Scrape MQ queue stats every minute',
    'SELECT ores_mq_queue_stats_scrape_fn()',
    '* * * * *',
    'execute_sql',
    '{}',
    true,
    'system', 'system', 'system.new_record', '',
    current_timestamp, ores_utility_infinity_timestamp_fn()
);

-- Seed the curve republish job (system scope, every 15 minutes). It bootstraps
-- and publishes every IR curve the system tenant holds a bootstrap config for,
-- so the published curves follow the quotes instead of staying at the
-- provisioning as-of. The cadence is this row's; one job states it for every
-- curve, because the curves share one market.
insert into ores_scheduler_job_definitions_tbl (
    id, tenant_id, party_id, job_name, description, command,
    schedule_expression, action_type, action_payload, is_active,
    modified_by, performed_by, change_reason_code, change_commentary,
    valid_from, valid_to
) values (
    gen_random_uuid(), NULL, NULL,
    'ores.marketdata.curve_republish',
    'Bootstrap and publish every system IR curve every 15 minutes',
    '',
    '*/15 * * * *',
    'nats_request',
    '{"subject": "marketdata.v1.ops.publish_due_curves"}',
    true,
    'system', 'system', 'system.new_record', '',
    current_timestamp, ores_utility_infinity_timestamp_fn()
);
