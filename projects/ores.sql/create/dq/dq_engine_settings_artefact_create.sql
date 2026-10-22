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
-- The central row of a named engine settings configuration. A report definition binds the configuration through report_configuration, in the slot of configuration type engine_settings, as it binds its pricing engines. The row holds only the identity. The settings live in three parts, each its own table keyed by this row: engine_settings_fixings, engine_settings_building and engine_settings_execution. A part that has no row means the engine default for every setting in it. A write to any part bumps the version of this row in the same transaction. - Artefact Table
-- =============================================================================

create table if not exists "ores_dq_engine_settings_artefact_tbl" (
    "dataset_id" uuid not null,
    "tenant_id" uuid not null,
    "id" uuid not null,
    "version" integer not null,
    "party_id" uuid not null,
    "name" text not null,
    "description" text null,
    "configuration_id" uuid not null
);

create index if not exists dq_engine_settings_artefact_dataset_idx
on ores_dq_engine_settings_artefact_tbl (dataset_id);

create index if not exists dq_engine_settings_artefact_tenant_idx
on ores_dq_engine_settings_artefact_tbl (tenant_id);

create index if not exists dq_engine_settings_artefact_id_idx
on ores_dq_engine_settings_artefact_tbl (id);
