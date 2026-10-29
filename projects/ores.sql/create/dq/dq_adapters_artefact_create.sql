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
-- The catalogue of adapter modules. An adapter is code, installed by the release; a person configures a session of it and then the feeds that come off the session. The catalogue states the module's version, the asset classes it can serve, the services it offers, and the configuration a session must provide, so the screen can build the session form from data instead of from code. The system tenant owns the rows. Every tenant reads them and only the system tenant writes them. - Artefact Table
-- =============================================================================

create table if not exists "ores_dq_adapters_artefact_tbl" (
    "dataset_id" uuid not null,
    "tenant_id" uuid not null,
    "code" text not null,
    "version" integer not null,
    "name" text not null,
    "module_version" text not null,
    "description" text not null,
    "process" text not null,
    "asset_classes" text not null,
    "services" text not null,
    "configuration_schema" text not null default '{}',
    "display_order" integer not null default 0
);

create index if not exists dq_adapters_artefact_dataset_idx
on ores_dq_adapters_artefact_tbl (dataset_id);

create index if not exists dq_adapters_artefact_tenant_idx
on ores_dq_adapters_artefact_tbl (tenant_id);

create index if not exists dq_adapters_artefact_code_idx
on ores_dq_adapters_artefact_tbl (code);
