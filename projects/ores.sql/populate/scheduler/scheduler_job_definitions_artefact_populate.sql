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
 * Job Definitions Artefact Population Script
 *
 * Stages the two jobs the platform declares for housekeeping: the sweep that
 * applies the retention policies, and the sampler that measures the stores. A
 * policy names a table and not a job, so the datasets stand apart and this one
 * declares no dependency.
 *
 * The sweep is fired once a day. Each policy states its own cadence, and the
 * sweep applies the ones that are due, so the schedule here is the tick. The
 * sampler runs every hour, which is the finest cadence the screen's plot needs
 * and the coarsest that still shows a store growing within a day.
 */

\echo '--- Job Definitions Artefacts ---'

select id as v_dataset_id from ores_dq_datasets_tbl
where code = 'scheduler.job_definitions'
  and valid_to = ores_utility_infinity_timestamp_fn() \gset

delete from ores_dq_job_definitions_artefact_tbl
where dataset_id = :'v_dataset_id';

insert into ores_dq_job_definitions_artefact_tbl (
    tenant_id, dataset_id, job_name, description, command,
    schedule_expression, action_type, action_payload, is_active
)
values
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id',
     'ores.mq.metrics_scrape',
     'Scrape MQ queue stats every minute',
     'SELECT ores_mq_queue_stats_scrape_fn()',
     '* * * * *',
     'execute_sql',
     '{}'::jsonb,
     true),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id',
     'ores.marketdata.curve_republish',
     'Bootstrap and publish every system IR curve every 15 minutes',
     '',
     '*/15 * * * *',
     'nats_request',
     '{"subject": "marketdata.v1.ops.publish_due_curves"}'::jsonb,
     true),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id',
     'ores.scheduler.housekeeping_sweep',
     'Apply the retention policies that are due',
     'DO $$ begin perform ores_scheduler_apply_retention_policies_fn(); end $$;',
     '0 3 * * *',
     'execute_sql',
     '{}'::jsonb,
     true),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id',
     'ores.scheduler.store_sampler',
     'Measure the stores the database can see, so their growth can be plotted',
     'DO $$ begin perform ores_scheduler_sample_retained_stores_fn(); end $$;',
     '15 * * * *',
     'execute_sql',
     '{}'::jsonb,
     true),
    (ores_utility_system_tenant_id_fn(), :'v_dataset_id',
     'compute.v1.reap.stale_results',
     'Reset stale in-progress compute results every minute',
     'compute.v1.ops.reap_work',
     '* * * * *',
     'nats_publish',
     '{"subject":"compute.v1.ops.reap_work"}'::jsonb,
     true);
