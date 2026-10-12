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
-- The settings of one EquityCurve entry of a curveconfig.xml, one row per curve_definition in the EquityCurves section: the equity's spot quote and forecasting curve, how its forward curve is given, and how its dividends interpolate. The quotes the forward curve is built from are rows of curve_quote. Three corpus entries write an empty Quotes element, so has_quotes holds whether the element is there. 101 write an empty Calendar; the column then holds the empty text the document wrote. A calendar that is not empty is checked by ores_refdata_validate_calendar_fn. - Artefact Table
-- =============================================================================

create table if not exists "ores_dq_equity_curve_configs_artefact_tbl" (
    "dataset_id" uuid not null,
    "tenant_id" uuid not null,
    "id" uuid not null,
    "version" integer not null,
    "party_id" uuid not null,
    "curve_definition_id" uuid not null,
    "currency" text not null,
    "calendar" text null,
    "forecasting_curve" text not null,
    "equity_type" text not null,
    "exercise_style" text null,
    "spot_quote" text not null,
    "has_quotes" boolean not null,
    "day_counter" text null,
    "has_dividend_interpolation" boolean not null,
    "dividend_interpolation_variable" text null,
    "dividend_interpolation_method" text null,
    "dividend_extrapolation" text null,
    "extrapolation" text null
);

create index if not exists dq_equity_curve_configs_artefact_dataset_idx
on ores_dq_equity_curve_configs_artefact_tbl (dataset_id);

create index if not exists dq_equity_curve_configs_artefact_tenant_idx
on ores_dq_equity_curve_configs_artefact_tbl (tenant_id);

create index if not exists dq_equity_curve_configs_artefact_id_idx
on ores_dq_equity_curve_configs_artefact_tbl (id);
