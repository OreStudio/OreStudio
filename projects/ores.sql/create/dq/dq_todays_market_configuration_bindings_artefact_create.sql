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
-- One reference a configuration makes. A Configuration block writes up to twenty-four of these. Each names a collection kind and the id of one collection of that kind: DiscountingCurvesId inccy selects the DiscountingCurves collection whose id is inccy. The reference is text, not a foreign key to todays_market_collection. ORE resolves it by name when it builds the market, and the export only has to write it back. Whether it should become a real key is left to the work that first resolves it, which is producing the engine input from the entities. - Artefact Table
-- =============================================================================

create table if not exists "ores_dq_todays_market_configuration_bindings_artefact_tbl" (
    "dataset_id" uuid not null,
    "tenant_id" uuid not null,
    "id" uuid not null,
    "version" integer not null,
    "party_id" uuid not null,
    "todays_market_configuration_id" uuid not null,
    "collection" text not null,
    "reference" text not null,
    "position" integer not null
);

create index if not exists dq_todays_market_configuration_bindings_artefact_dataset_idx
on ores_dq_todays_market_configuration_bindings_artefact_tbl (dataset_id);

create index if not exists dq_todays_market_configuration_bindings_artefact_tenant_idx
on ores_dq_todays_market_configuration_bindings_artefact_tbl (tenant_id);

create index if not exists dq_todays_market_configuration_bindings_artefact_id_idx
on ores_dq_todays_market_configuration_bindings_artefact_tbl (id);
