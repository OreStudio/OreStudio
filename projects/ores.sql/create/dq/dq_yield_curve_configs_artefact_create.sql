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
-- The settings of one YieldCurve entry of a curveconfig.xml, one row per curve_definition in the YieldCurves section. The entry's identity is on the definition, its segments in curve_segment, its bootstrap settings in curve_bootstrap_config; this row holds the rest, each as the column its schema type calls for. Extrapolation and ExcludeT0FromInterpolation are ORE booleans, which accept Y, true and True among other spellings. They are held as the spelling the document used, so the export writes it back unchanged. The day counter is held the same way and refers to day_counter. - Artefact Table
-- =============================================================================

create table if not exists "ores_dq_yield_curve_configs_artefact_tbl" (
    "dataset_id" uuid not null,
    "tenant_id" uuid not null,
    "id" uuid not null,
    "version" integer not null,
    "party_id" uuid not null,
    "curve_definition_id" uuid not null,
    "currency" text not null,
    "discount_curve" text not null,
    "interpolation_variable" text null,
    "interpolation_method" text null,
    "mixed_interpolation_cutoff" integer null,
    "day_counter" text null,
    "tolerance" double precision null,
    "extrapolation" text null,
    "extrapolation_method" text null,
    "exclude_t0_from_interpolation" text null,
    "has_report" boolean not null,
    "report_pillar_dates" text null
);

create index if not exists dq_yield_curve_configs_artefact_dataset_idx
on ores_dq_yield_curve_configs_artefact_tbl (dataset_id);

create index if not exists dq_yield_curve_configs_artefact_tenant_idx
on ores_dq_yield_curve_configs_artefact_tbl (tenant_id);

create index if not exists dq_yield_curve_configs_artefact_id_idx
on ores_dq_yield_curve_configs_artefact_tbl (id);
