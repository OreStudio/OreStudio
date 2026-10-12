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
-- The settings of one InflationCapFloorVolatility entry of a curveconfig.xml, one row per curve_definition in the InflationCapFloorVolatilities section: a zero coupon or year on year inflation cap and floor surface over tenors and strikes, with the index and curves it is built from. Its Report is a row of curve_report_configuration and its BootstrapConfig a row of curve_bootstrap_config, both keyed to the entry. - Artefact Table
-- =============================================================================

create table if not exists "ores_dq_inflation_cap_floor_volatility_configs_artefact_tbl" (
    "dataset_id" uuid not null,
    "tenant_id" uuid not null,
    "id" uuid not null,
    "version" integer not null,
    "party_id" uuid not null,
    "curve_definition_id" uuid not null,
    "inflation_type" text not null,
    "quote_type" text not null,
    "volatility_type" text not null,
    "extrapolation" text not null,
    "tenors" text not null,
    "settlement_days" integer null,
    "cap_strikes" text null,
    "floor_strikes" text null,
    "strikes" text null,
    "calendar" text not null,
    "day_counter" text not null,
    "business_day_convention" text not null,
    "index" text not null,
    "index_curve" text not null,
    "index_interpolated" text null,
    "observation_lag" text not null,
    "yield_term_structure" text not null,
    "quote_index" text null,
    "conventions" text null
);

create index if not exists dq_inflation_cap_floor_volatility_configs_artefact_dataset_idx
on ores_dq_inflation_cap_floor_volatility_configs_artefact_tbl (dataset_id);

create index if not exists dq_inflation_cap_floor_volatility_configs_artefact_tenant_idx
on ores_dq_inflation_cap_floor_volatility_configs_artefact_tbl (tenant_id);

create index if not exists dq_inflation_cap_floor_volatility_configs_artefact_id_idx
on ores_dq_inflation_cap_floor_volatility_configs_artefact_tbl (id);
