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
-- The persistent template for a report. Describes what to run, when to run it, how to handle concurrent executions, and how much of the processing pipeline a run executes. Type-specific configuration (e.g. risk parameters) lives in a separate table keyed by report_definition_id. The run configuration is held here rather than in the environment so that a run is reproducible from the database alone. pre_processing and post_processing name, for each substitutable phase, whether the phase runs or is replaced by a prepared substitute. Lifecycle is managed through the report_definition_lifecycle FSM machine. fsm_state_id points to the current state in ores_dq_fsm_states_tbl. scheduler_job_id links to ores_scheduler_job_definitions_tbl.id and is set by the scheduler service when the definition is activated (state: active). It is cleared when the definition is suspended or archived. - Artefact Table
-- =============================================================================

create table if not exists "ores_dq_report_definitions_artefact_tbl" (
    "dataset_id" uuid not null,
    "tenant_id" uuid not null,
    "id" uuid not null,
    "version" integer not null,
    "name" text not null,
    "description" text null,
    "report_type" text not null,
    "schedule_expression" text not null,
    "concurrency_policy" text not null,
    "display_order" integer not null,
    "tier" text not null default 'trading',
    "applicable_jurisdiction" text null
);

create index if not exists dq_report_definitions_artefact_dataset_idx
on ores_dq_report_definitions_artefact_tbl (dataset_id);

create index if not exists dq_report_definitions_artefact_tenant_idx
on ores_dq_report_definitions_artefact_tbl (tenant_id);

create index if not exists dq_report_definitions_artefact_id_idx
on ores_dq_report_definitions_artefact_tbl (id);

create index if not exists dq_report_definitions_artefact_name_idx
on ores_dq_report_definitions_artefact_tbl (name);

create index if not exists dq_report_definitions_artefact_report_type_idx
on ores_dq_report_definitions_artefact_tbl (report_type);
