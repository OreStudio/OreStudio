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
-- This is how a report definition reaches its configuration: one row per configuration it binds, where the configuration type is the slot and the configuration fills it. Several reports may fill the same slot with the same configuration, so a configuration is never copied to be shared, and adding a slot is not a column on the report definition. - Artefact Table
-- =============================================================================

create table if not exists "ores_dq_report_configurations_artefact_tbl" (
    "dataset_id" uuid not null,
    "tenant_id" uuid not null,
    "version" integer not null,
    "report_name" text not null,
    "configuration_type_code" text not null,
    "configuration_name" text not null
);

create index if not exists dq_report_configurations_artefact_dataset_idx
on ores_dq_report_configurations_artefact_tbl (dataset_id);

create index if not exists dq_report_configurations_artefact_tenant_idx
on ores_dq_report_configurations_artefact_tbl (tenant_id);

create index if not exists dq_report_configurations_artefact_report_name_idx
on ores_dq_report_configurations_artefact_tbl (report_name);
