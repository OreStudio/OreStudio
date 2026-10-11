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
-- The settings of one SwaptionVolatility entry of a curveconfig.xml, one row per curve_definition in the SwaptionVolatilities section: the surface's dimension and volatility type, the option and swap tenors of its ATM matrix and smile, and the swap indices it is built on. Its Report is a row of curve_report_configuration and its ParametricSmileConfiguration a row of curve_parametric_smile. A ProxyConfig builds the surface from another one; it is one per entry, so its fields are the proxy_ columns, and has_proxy_config says whether the entry wrote it. - Artefact Table
-- =============================================================================

create table if not exists "ores_dq_swaption_volatility_configs_artefact_tbl" (
    "dataset_id" uuid not null,
    "tenant_id" uuid not null,
    "id" uuid not null,
    "version" integer not null,
    "party_id" uuid not null,
    "curve_definition_id" uuid not null,
    "dimension" text null,
    "volatility_type" text null,
    "interpolation" text null,
    "extrapolation" text null,
    "output_volatility_type" text null,
    "model_shift" text null,
    "output_shift" text null,
    "day_counter" text null,
    "calendar" text null,
    "business_day_convention" text null,
    "option_tenors" text null,
    "swap_tenors" text null,
    "short_swap_index_base" text null,
    "swap_index_base" text null,
    "smile_option_tenors" text null,
    "smile_swap_tenors" text null,
    "smile_spreads" text null,
    "quote_tag" text null,
    "has_proxy_config" boolean not null,
    "proxy_source_curve_id" text null,
    "proxy_source_short_swap_index_base" text null,
    "proxy_source_swap_index_base" text null,
    "proxy_target_short_swap_index_base" text null,
    "proxy_target_swap_index_base" text null
);

create index if not exists dq_swaption_volatility_configs_artefact_dataset_idx
on ores_dq_swaption_volatility_configs_artefact_tbl (dataset_id);

create index if not exists dq_swaption_volatility_configs_artefact_tenant_idx
on ores_dq_swaption_volatility_configs_artefact_tbl (tenant_id);

create index if not exists dq_swaption_volatility_configs_artefact_id_idx
on ores_dq_swaption_volatility_configs_artefact_tbl (id);
