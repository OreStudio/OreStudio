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
-- The execution part of a named engine settings configuration. How the engine runs and logs. A change here does not change the numbers. A column that is null means the engine default, and a configuration with no row in this part uses the default for every setting in it. A write to this part bumps the version of the central engine_settings row in the same transaction. - Artefact Table
-- =============================================================================

create table if not exists "ores_dq_engine_settings_execution_artefact_tbl" (
    "dataset_id" uuid not null,
    "tenant_id" uuid not null,
    "id" uuid not null,
    "version" integer not null,
    "engine_settings_id" uuid not null,
    "party_id" uuid not null,
    "n_threads" integer null,
    "log_mask" integer null,
    "gzip_compression_level" integer null
);

create index if not exists dq_engine_settings_execution_artefact_dataset_idx
on ores_dq_engine_settings_execution_artefact_tbl (dataset_id);

create index if not exists dq_engine_settings_execution_artefact_tenant_idx
on ores_dq_engine_settings_execution_artefact_tbl (tenant_id);

create index if not exists dq_engine_settings_execution_artefact_id_idx
on ores_dq_engine_settings_execution_artefact_tbl (id);
