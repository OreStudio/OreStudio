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
-- One entry of one collection. Twenty-three of the document's twenty-four collections are this shape: an entry identifying itself by one attribute and carrying a reference as its text, in a position. The attribute is not one name. It is name for nineteen collections, currency for DiscountingCurves, pair for the two FX collections and the literal string key for SwaptionVolatilities and CapFloorVolatilities. Which one is a fact about the collection's kind, so it is held once on todays_market_collection_kind rather than repeated on every entry. SwaptionVolatilities and CapFloorVolatilities identify by two attributes at once, so key_value_2 carries the second. SwapIndexCurves is the only collection whose entry nests, and discounting carries its one child. - Artefact Table
-- =============================================================================

create table if not exists "ores_dq_todays_market_entries_artefact_tbl" (
    "dataset_id" uuid not null,
    "tenant_id" uuid not null,
    "id" uuid not null,
    "version" integer not null,
    "party_id" uuid not null,
    "todays_market_config_id" uuid not null,
    "todays_market_collection_id" uuid not null,
    "key_value" text null,
    "key_value_2" text null,
    "target" text not null,
    "discounting" text null,
    "position" integer not null
);

create index if not exists dq_todays_market_entries_artefact_dataset_idx
on ores_dq_todays_market_entries_artefact_tbl (dataset_id);

create index if not exists dq_todays_market_entries_artefact_tenant_idx
on ores_dq_todays_market_entries_artefact_tbl (tenant_id);

create index if not exists dq_todays_market_entries_artefact_id_idx
on ores_dq_todays_market_entries_artefact_tbl (id);
