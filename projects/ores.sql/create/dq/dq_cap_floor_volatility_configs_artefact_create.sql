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
-- The settings of one CapFloorVolatility entry of a curveconfig.xml, one row per curve_definition in the CapFloorVolatilities section: the cap and floor surface's tenors and strikes, the index and discount curve it is stripped with, and how it interpolates. Its Report is a row of curve_report_configuration, its BootstrapConfig a row of curve_bootstrap_config and its ParametricSmileConfiguration a row of curve_parametric_smile. A ProxyConfig builds the surface from another one; it is one per entry, so it is a row of cap_floor_proxy_config and the entry holds none of its fields. The schema allows both UseEffeciveVolatility, a misspelling kept for old documents, and UseEffectiveVolatility; each has its own column so either round trips. - Artefact Table
-- =============================================================================

create table if not exists "ores_dq_cap_floor_volatility_configs_artefact_tbl" (
    "dataset_id" uuid not null,
    "tenant_id" uuid not null,
    "id" uuid not null,
    "version" integer not null,
    "party_id" uuid not null,
    "curve_definition_id" uuid not null,
    "volatility_type" text null,
    "output_volatility_type" text null,
    "model_shift" double precision null,
    "output_shift" double precision null,
    "extrapolation" text null,
    "interpolation_method" text null,
    "include_atm" text null,
    "day_counter" text null,
    "calendar" text null,
    "business_day_convention" text null,
    "tenors" text null,
    "strikes" text null,
    "optional_quotes" text null,
    "ibor_index" text null,
    "index" text null,
    "rate_computation_period" text null,
    "on_cap_settlement_days" integer null,
    "discount_curve" text null,
    "atm_tenors" text null,
    "settlement_days" integer null,
    "interpolate_on" text null,
    "time_interpolation" text null,
    "strike_interpolation" text null,
    "input_type" text null,
    "quote_includes_index_name" text null,
    "flat_first_period" text null,
    "use_effecive_volatility" text null,
    "use_effective_volatility" text null
);

create index if not exists dq_cap_floor_volatility_configs_artefact_dataset_idx
on ores_dq_cap_floor_volatility_configs_artefact_tbl (dataset_id);

create index if not exists dq_cap_floor_volatility_configs_artefact_tenant_idx
on ores_dq_cap_floor_volatility_configs_artefact_tbl (tenant_id);

create index if not exists dq_cap_floor_volatility_configs_artefact_id_idx
on ores_dq_cap_floor_volatility_configs_artefact_tbl (id);
