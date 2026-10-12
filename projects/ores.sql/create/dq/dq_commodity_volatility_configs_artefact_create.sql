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
-- The settings of one CommodityVolatility entry of a curveconfig.xml, one row per curve_definition in the CommodityVolatilities section: the currency, the instrument the surface is quoted on, and the price curve, yield curve and future convention it is built with. Its volatility configurations, written directly or inside a VolatilityConfig element, are rows of curve_volatility_config. Its Report is a row of curve_report_configuration. The schema also allows a OneDimSolverConfig, which no corpus document writes; the mapper refuses an entry that has one rather than lose it. - Artefact Table
-- =============================================================================

create table if not exists "ores_dq_commodity_volatility_configs_artefact_tbl" (
    "dataset_id" uuid not null,
    "tenant_id" uuid not null,
    "id" uuid not null,
    "version" integer not null,
    "party_id" uuid not null,
    "curve_definition_id" uuid not null,
    "currency" text not null,
    "instrument_type" text null,
    "calendar_spread_offset" integer null,
    "calendar_spread_underlying_name" text null,
    "day_counter" text null,
    "calendar" text null,
    "future_conventions" text null,
    "option_expiry_roll_days" text null,
    "price_curve_id" text null,
    "yield_curve_id" text null,
    "quote_suffix" text null,
    "prefer_out_of_the_money" text null,
    "has_volatility_config" boolean not null
);

create index if not exists dq_commodity_volatility_configs_artefact_dataset_idx
on ores_dq_commodity_volatility_configs_artefact_tbl (dataset_id);

create index if not exists dq_commodity_volatility_configs_artefact_tenant_idx
on ores_dq_commodity_volatility_configs_artefact_tbl (tenant_id);

create index if not exists dq_commodity_volatility_configs_artefact_id_idx
on ores_dq_commodity_volatility_configs_artefact_tbl (id);
