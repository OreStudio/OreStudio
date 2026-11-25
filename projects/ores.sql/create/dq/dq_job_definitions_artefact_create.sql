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
/*
 * AUTO-GENERATED FILE - DO NOT EDIT MANUALLY
 * Template: sql_schema_domain_entity_artefact_create.mustache
 * To modify, update the template and regenerate.
 */

-- =============================================================================
-- A job the scheduler fires on a cron expression. The row carries the schedule (schedule_expression), what to run when it fires, and whether it is active. action_type selects the behaviour: execute_sql runs the SQL in command, and nats_publish publishes the subject and body carried in action_payload. The table is bi-temporal and audited (see projects/ores.sql/create/scheduler/scheduler_job_definitions_create.sql): it carries version, the four audit columns and the valid_from/valid_to pair with the GIST exclusion and the delete rule, so the model takes the ordinary audited shape and needs no shape flag. tenant_id is nullable. A job may belong to no tenant, because the scheduler fires system jobs from a NULL-tenant row: the MQ statistics scrape and the compute stale-result reaper are both such rows. The model binds uuid-identified-lookup for its UUID surrogate key, its tenant scope and its standard presentation tier, and states nullable_tenant_id itself, which that profile leaves to the model. job_name is the natural key and is unique within its tenant; id is the surrogate. Uniqueness is what the component's upsert path relies on: a job that arrives under an existing name updates that row in place instead of adding a second one. The component's operational views — the global job-instance list and the live scheduler status — are operations rather than entity verbs, and are modelled in ores.scheduler.scheduling_operations. - Artefact Table
-- =============================================================================

create table if not exists "ores_dq_job_definitions_artefact_tbl" (
    "dataset_id" uuid not null,
    "tenant_id" uuid not null,
    "job_name" text not null,
    "description" text not null,
    "command" text not null default '',
    "schedule_expression" text not null,
    "action_type" text not null default 'execute_sql',
    "action_payload" jsonb not null default '{}'::jsonb,
    "is_active" boolean not null default true
);

create index if not exists dq_job_definitions_artefact_dataset_idx
on ores_dq_job_definitions_artefact_tbl (dataset_id);

create index if not exists dq_job_definitions_artefact_tenant_idx
on ores_dq_job_definitions_artefact_tbl (tenant_id);

create index if not exists dq_job_definitions_artefact_job_name_idx
on ores_dq_job_definitions_artefact_tbl (job_name);
