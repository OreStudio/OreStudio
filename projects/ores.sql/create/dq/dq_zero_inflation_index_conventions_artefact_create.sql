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
-- Describes a zero-coupon inflation index: the region it is published for, how often it is published, how far behind its publication date it becomes available, and the currency it is quoted in. Corresponds to the <ZeroInflationIndex> element in ORE conventions.xml. The id field is the natural key (ORE <Id> element). Every field is required. The corpus sets all seven in all fifty-four elements, and ORE's schema holds them by value, so an element without one does not parse. The element carries one more field, RebasingEvents, and this table does not model it: the ORE type holds it as a list of doubles, and the refdata schema has no array column. No shipped file sets it. Rather than drop it silently the mapper counts every element that carries one in conventions_mapper::unmodelled under ZeroInflationIndex.RebasingEvents, so a document that uses it is excluded from the round-trip set and named in the measurement. - Artefact Table
-- =============================================================================

create table if not exists "ores_dq_zero_inflation_index_conventions_artefact_tbl" (
    "dataset_id" uuid not null,
    "tenant_id" uuid not null,
    "id" text not null,
    "version" integer not null,
    "party_id" uuid not null,
    "region_name" text not null,
    "region_code" text not null,
    "revised" boolean not null,
    "frequency" text not null,
    "availability_lag" text not null,
    "currency" text not null,
    "oresmd_uri" text null
);

create index if not exists dq_zero_inflation_index_conventions_artefact_dataset_idx
on ores_dq_zero_inflation_index_conventions_artefact_tbl (dataset_id);

create index if not exists dq_zero_inflation_index_conventions_artefact_tenant_idx
on ores_dq_zero_inflation_index_conventions_artefact_tbl (tenant_id);

create index if not exists dq_zero_inflation_index_conventions_artefact_id_idx
on ores_dq_zero_inflation_index_conventions_artefact_tbl (id);
