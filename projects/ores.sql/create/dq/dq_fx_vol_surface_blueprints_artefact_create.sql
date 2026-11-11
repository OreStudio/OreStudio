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
-- Records *how* one FX volatility surface is built, as a named, inspectable recipe, the surface's counterpart of the IR curve bootstrap config. Its pillars, in fx_vol_surface_pillar, name the quotes the calibration reads; the blueprint states what is done with them: the smile model fitted at each expiry, how the fitted expiries are joined, the grid of expiries and strikes the surface is written on, and the series it is written to. See [[id:53D14F31-EE32-493E-BBEB-140AA4906BE4][Constructing a volatility surface: from quotes to a published set]]. output_series_id is a soft, cross-component reference into ores.marketdata's market_series: the fx_vol_surface series the calibration alone writes. As for the IR config, it is chosen and not minted, and it is the sole natural key, so exactly one active blueprint, tenant-wide, writes any given surface series. fx_option_convention_id names the pair's quoting convention in fx_option_convention, a soft reference: its ATM type, delta type, butterfly style and switch tenor decide what each pillar's quote means. smile_model and expiry_interpolation are small, fixed vocabularies intrinsic to the record, held by check constraints, as the IR config's curve_family_role is. The output grid is two lists, the expiries as tenor codes and the strikes in the oresmd strike grammar (ATM, DEL/Spot/Put/25, an absolute level), each separated by commas: the grid is a property of the recipe, read whole by the calibration and never addressed one point at a time, so it has no table of its own. - Artefact Table
-- =============================================================================

create table if not exists "ores_dq_fx_vol_surface_blueprints_artefact_tbl" (
    "dataset_id" uuid not null,
    "tenant_id" uuid not null,
    "id" uuid not null,
    "version" integer not null,
    "output_series_id" uuid not null,
    "party_id" uuid not null,
    "name" text not null,
    "base_currency_code" text not null,
    "quote_currency_code" text not null,
    "fx_option_convention_id" text not null,
    "smile_model" text not null,
    "expiry_interpolation" text not null,
    "output_expiries" text not null,
    "output_strikes" text not null
);

create index if not exists dq_fx_vol_surface_blueprints_artefact_dataset_idx
on ores_dq_fx_vol_surface_blueprints_artefact_tbl (dataset_id);

create index if not exists dq_fx_vol_surface_blueprints_artefact_tenant_idx
on ores_dq_fx_vol_surface_blueprints_artefact_tbl (tenant_id);

create index if not exists dq_fx_vol_surface_blueprints_artefact_id_idx
on ores_dq_fx_vol_surface_blueprints_artefact_tbl (id);
