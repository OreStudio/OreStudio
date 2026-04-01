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
 * Template: sql_schema_artefact_create.mustache
 * To modify, update the template and regenerate.
 */

-- =============================================================================
-- Report Definition artefacts - default report templates for the ORE analytics bundle - Artefact Table
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
    "display_order" integer not null
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
