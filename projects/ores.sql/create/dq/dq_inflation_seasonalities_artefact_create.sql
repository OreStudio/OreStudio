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
-- ORE's seasonalityType requires BaseDate, Frequency and Factors together. The inflation curve held them as has_seasonality and two nullable columns, which could say a curve has a seasonality with no base date. A row of this part is the seasonality: it exists only where the curve states one, so its presence replaces the flag, and its base date and frequency are required. The factors stay rows of inflation_seasonality_factor, keyed by the same curve entry. The export refuses factors with no seasonality and a seasonality with no factors, as the schema does. A curve entry holds at most one row of this part, so the generated table adds the unique index on the live rows of (tenant_id, curve_definition_id). - Artefact Table
-- =============================================================================

create table if not exists "ores_dq_inflation_seasonalities_artefact_tbl" (
    "dataset_id" uuid not null,
    "tenant_id" uuid not null,
    "id" uuid not null,
    "version" integer not null,
    "party_id" uuid not null,
    "curve_definition_id" uuid not null,
    "base_date" text not null,
    "frequency" text not null
);

create index if not exists dq_inflation_seasonalities_artefact_dataset_idx
on ores_dq_inflation_seasonalities_artefact_tbl (dataset_id);

create index if not exists dq_inflation_seasonalities_artefact_tenant_idx
on ores_dq_inflation_seasonalities_artefact_tbl (tenant_id);

create index if not exists dq_inflation_seasonalities_artefact_id_idx
on ores_dq_inflation_seasonalities_artefact_tbl (id);
