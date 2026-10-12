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
-- One way a volatility entry's surface may be built. CDS, equity and commodity volatilities each name one or more volatility configurations, each with an optional priority; ORE builds the first that succeeds. kind names the element the document wrote, and the columns that kind uses are set. An entry writes a kind either directly or inside a VolatilityConfig element, which holds at most one of each kind; is_wrapped says which. The kinds the corpus writes are modelled: Constant, Curve, StrikeSurface, DeltaSurface and ProxySurface. A Curve kind's quotes are rows of curve_quote on the entry, in the list Curve or VolatilityConfig/Curve. MoneynessSurface, ApoFutureSurface and a parametric smile inside a surface appear in no corpus document, and the mapper refuses them. - Artefact Table
-- =============================================================================

create table if not exists "ores_dq_curve_volatility_configs_artefact_tbl" (
    "dataset_id" uuid not null,
    "tenant_id" uuid not null,
    "id" uuid not null,
    "version" integer not null,
    "party_id" uuid not null,
    "curve_definition_id" uuid not null,
    "kind" text not null,
    "is_wrapped" boolean not null,
    "priority" integer null,
    "quote_type" text null,
    "volatility_type" text null,
    "exercise_type" text null,
    "strikes" text null,
    "expiries" text null,
    "time_interpolation" text null,
    "strike_interpolation" text null,
    "extrapolation" text null,
    "time_extrapolation" text null,
    "time_extrapolation_variance" text null,
    "strike_extrapolation" text null,
    "calendar" text null,
    "quote" text null,
    "interpolation" text null,
    "enforce_monotone_variance" boolean null,
    "delta_type" text null,
    "atm_type" text null,
    "atm_delta_type" text null,
    "put_deltas" text null,
    "call_deltas" text null,
    "future_price_correction" text null,
    "proxy_volatility_curve" text null,
    "fx_volatility_curve" text null,
    "correlation_curve" text null,
    "cds_volatility_curve" text null,
    "position" integer not null
);

create index if not exists dq_curve_volatility_configs_artefact_dataset_idx
on ores_dq_curve_volatility_configs_artefact_tbl (dataset_id);

create index if not exists dq_curve_volatility_configs_artefact_tenant_idx
on ores_dq_curve_volatility_configs_artefact_tbl (tenant_id);

create index if not exists dq_curve_volatility_configs_artefact_id_idx
on ores_dq_curve_volatility_configs_artefact_tbl (id);
