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
-- The document level ReportConfiguration of a curveconfig.xml gives report settings per curve family: FX, equity, commodity, swaption, cap and floor and inflation cap and floor volatilities, yield curves and default curves. Each family element the document writes is one row here, keyed to the document, and has_report says whether the element holds a Report. An entry's own Report overrides these and is a row of curve_report_configuration. The columns are those of curve_report_configuration. The yield curve family's report writes only PillarDates. A ReportConfiguration element that writes no family appears in no corpus document, and the mapper refuses it, because rows cannot record it. - Artefact Table
-- =============================================================================

create table if not exists "ores_dq_curve_global_reports_artefact_tbl" (
    "dataset_id" uuid not null,
    "tenant_id" uuid not null,
    "id" uuid not null,
    "version" integer not null,
    "party_id" uuid not null,
    "curve_configuration_id" uuid not null,
    "family" text not null,
    "has_report" boolean not null,
    "report_on_delta_grid" text null,
    "report_on_moneyness_grid" text null,
    "report_on_strike_grid" text null,
    "report_on_strike_spread_grid" text null,
    "deltas" text null,
    "moneyness" text null,
    "strikes" text null,
    "strike_spreads" text null,
    "expiries" text null,
    "pillar_dates" text null,
    "underlying_tenors" text null,
    "continuation_expiry" text null,
    "position" integer not null
);

create index if not exists dq_curve_global_reports_artefact_dataset_idx
on ores_dq_curve_global_reports_artefact_tbl (dataset_id);

create index if not exists dq_curve_global_reports_artefact_tenant_idx
on ores_dq_curve_global_reports_artefact_tbl (tenant_id);

create index if not exists dq_curve_global_reports_artefact_id_idx
on ores_dq_curve_global_reports_artefact_tbl (id);
