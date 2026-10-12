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
-- One collection — DiscountingCurves, FxSpots, SwaptionVolatilities and the rest. A document writes up to twenty-four of them, each optional, each holding its own ordered list of entries. A document may write the same kind more than once under different ids, for example a DiscountingCurves with id default and another with id inccy. A Configuration selects one of them by that id. So the collection is a thing with an identity, an order and its own entries, and it has a table. Almost every corpus file writes the id. An entry points at its collection rather than repeating the collection's name. - Artefact Table
-- =============================================================================

create table if not exists "ores_dq_todays_market_collections_artefact_tbl" (
    "dataset_id" uuid not null,
    "tenant_id" uuid not null,
    "id" uuid not null,
    "version" integer not null,
    "party_id" uuid not null,
    "todays_market_config_id" uuid not null,
    "collection" text not null,
    "collection_id" text null,
    "position" integer not null
);

create index if not exists dq_todays_market_collections_artefact_dataset_idx
on ores_dq_todays_market_collections_artefact_tbl (dataset_id);

create index if not exists dq_todays_market_collections_artefact_tenant_idx
on ores_dq_todays_market_collections_artefact_tbl (tenant_id);

create index if not exists dq_todays_market_collections_artefact_id_idx
on ores_dq_todays_market_collections_artefact_tbl (id);
