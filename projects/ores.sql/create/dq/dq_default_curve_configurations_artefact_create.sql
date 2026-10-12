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
-- One way a default curve may be built. A DefaultCurve either writes its settings inline, or lists them as Configurations, each with a priority; ORE builds the first configuration that succeeds. The corpus writes 144 curves inline and 189 with configurations, nine of them with two, and never both. Each inline curve is one row with is_inline set; each listed configuration is one row with its priority. The quotes a configuration is built from are rows of curve_quote, and its BootstrapConfig a row of curve_bootstrap_config, both naming this row. Pillars and States are comma separated lists in ORE's own text, held as written. The schema also allows SourceCurves and SwitchDates lists, which no corpus document writes; the mapper refuses an entry that has them rather than lose them. - Artefact Table
-- =============================================================================

create table if not exists "ores_dq_default_curve_configurations_artefact_tbl" (
    "dataset_id" uuid not null,
    "tenant_id" uuid not null,
    "id" uuid not null,
    "version" integer not null,
    "party_id" uuid not null,
    "curve_definition_id" uuid not null,
    "is_inline" boolean not null,
    "priority" integer null,
    "default_curve_type" text null,
    "discount_curve" text null,
    "day_counter" text null,
    "recovery_rate" text null,
    "start_date" text null,
    "has_quotes" boolean not null,
    "benchmark_curve" text null,
    "reinterpreted_yield_curve" text null,
    "source_curve" text null,
    "pillars" text null,
    "spot_lag" integer null,
    "calendar" text null,
    "conventions" text null,
    "extrapolation" text null,
    "running_spread" numeric(38, 12) null,
    "index_term" text null,
    "imply_default_from_market" text null,
    "allow_negative_rates" text null,
    "price_is_upfront" text null,
    "initial_state" text null,
    "states" text null,
    "position" integer not null
);

create index if not exists dq_default_curve_configurations_artefact_dataset_idx
on ores_dq_default_curve_configurations_artefact_tbl (dataset_id);

create index if not exists dq_default_curve_configurations_artefact_tenant_idx
on ores_dq_default_curve_configurations_artefact_tbl (tenant_id);

create index if not exists dq_default_curve_configurations_artefact_id_idx
on ores_dq_default_curve_configurations_artefact_tbl (id);
