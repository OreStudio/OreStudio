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
-- A commodity curve built as a basis to a base price curve, as ORE's BasisConfiguration: the base price curve and its conventions, the basis conventions, the day counter, the interpolation, whether the basis is added, the month offset, whether the base is averaged and whether a price is a historical fixing. Only the basis construction reads them, and its presence is what selects that construction, so the flag the entry row held is gone. A commodity curve holds at most one row of this part, so the generated table adds the unique index on the live rows of (tenant_id, curve_definition_id). - Artefact Table
-- =============================================================================

create table if not exists "ores_dq_commodity_curve_bases_artefact_tbl" (
    "dataset_id" uuid not null,
    "tenant_id" uuid not null,
    "id" uuid not null,
    "version" integer not null,
    "party_id" uuid not null,
    "curve_definition_id" uuid not null,
    "basis_base_price_curve" text null,
    "basis_base_price_conventions" text null,
    "basis_conventions" text null,
    "basis_day_counter" text null,
    "basis_interpolation_method" text null,
    "basis_add_basis" text null,
    "basis_month_offset" integer null,
    "basis_average_base" text null,
    "basis_price_as_historical_fixing" text null
);

create index if not exists dq_commodity_curve_bases_artefact_dataset_idx
on ores_dq_commodity_curve_bases_artefact_tbl (dataset_id);

create index if not exists dq_commodity_curve_bases_artefact_tenant_idx
on ores_dq_commodity_curve_bases_artefact_tbl (tenant_id);

create index if not exists dq_commodity_curve_bases_artefact_id_idx
on ores_dq_commodity_curve_bases_artefact_tbl (id);
