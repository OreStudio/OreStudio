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
-- One Configuration block. It is not a collection of entries: it is a bundle of references, each naming one entry of one collection, so that a report can select a coherent set of curves and volatilities by name instead of restating them. It identifies itself by a required id attribute, which is unique within a document. The references it makes live on todays_market_configuration_binding. - Artefact Table
-- =============================================================================

create table if not exists "ores_dq_todays_market_configurations_artefact_tbl" (
    "dataset_id" uuid not null,
    "tenant_id" uuid not null,
    "id" uuid not null,
    "version" integer not null,
    "party_id" uuid not null,
    "todays_market_config_id" uuid not null,
    "configuration_id" text not null,
    "position" integer not null
);

create index if not exists dq_todays_market_configurations_artefact_dataset_idx
on ores_dq_todays_market_configurations_artefact_tbl (dataset_id);

create index if not exists dq_todays_market_configurations_artefact_tenant_idx
on ores_dq_todays_market_configurations_artefact_tbl (tenant_id);

create index if not exists dq_todays_market_configurations_artefact_id_idx
on ores_dq_todays_market_configurations_artefact_tbl (id);
