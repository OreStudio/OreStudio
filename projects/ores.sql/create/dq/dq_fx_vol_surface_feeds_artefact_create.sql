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
-- A synthetic volatility surface of one currency pair, as [[id:D4C7C4A7-CD2D-4307-AB06-DA9BE3C3A334][A synthetic contributor publishes noisy observations of a latent surface]] describes. The surface is SSVI with gamma fixed at one half. Its state is the forward variance of each segment between the feed's expiries, which the fx_vol_surface_feed_expiry rows hold, a skew driver and a curvature driver, each reverting to where the surface started. The correlation of the shocks is three numbers, and the feed builds the matrix from them: spot with every forward variance, spot with the skew, and how strongly neighbouring forward variances move together, which falls by that factor for each segment between them. Every other shock is independent of spot. The surface is quoted by the contributors fx_vol_surface_feed_contributor names. - Artefact Table
-- =============================================================================

create table if not exists "ores_dq_fx_vol_surface_feeds_artefact_tbl" (
    "dataset_id" uuid not null,
    "tenant_id" uuid not null,
    "id" uuid not null,
    "version" integer not null,
    "base_currency_code" text not null,
    "quote_currency_code" text not null,
    "provider_code" text null,
    "ticks_per_hour" integer not null,
    "enabled" boolean not null,
    "auto_start" boolean not null,
    "arrival_mode" text not null,
    "branching_ratio" double precision not null,
    "decay" double precision not null,
    "sensitivity" double precision not null,
    "skew_reversion" double precision not null,
    "skew_volatility" double precision not null,
    "curvature_reversion" double precision not null,
    "curvature_volatility" double precision not null,
    "spot_variance_correlation" double precision not null,
    "spot_skew_correlation" double precision not null,
    "segment_correlation" double precision not null,
    "start_skew" double precision not null,
    "start_curvature" double precision not null
);

create index if not exists dq_fx_vol_surface_feeds_artefact_dataset_idx
on ores_dq_fx_vol_surface_feeds_artefact_tbl (dataset_id);

create index if not exists dq_fx_vol_surface_feeds_artefact_tenant_idx
on ores_dq_fx_vol_surface_feeds_artefact_tbl (tenant_id);

create index if not exists dq_fx_vol_surface_feeds_artefact_id_idx
on ores_dq_fx_vol_surface_feeds_artefact_tbl (id);

create index if not exists dq_fx_vol_surface_feeds_artefact_base_currency_code_idx
on ores_dq_fx_vol_surface_feeds_artefact_tbl (base_currency_code);

create index if not exists dq_fx_vol_surface_feeds_artefact_quote_currency_code_idx
on ores_dq_fx_vol_surface_feeds_artefact_tbl (quote_currency_code);
