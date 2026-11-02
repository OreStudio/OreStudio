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
-- One way a default curve may be built. A DefaultCurve either writes its settings inline, or lists them as Configurations, each with a priority; ORE builds the first configuration that succeeds. The corpus writes 144 curves inline and 189 with configurations, nine of them with two, and never both. Each inline curve is one row with is_inline set; each listed configuration is one row with its priority. The row holds what every type reads: its type, day counter, recovery rate, extrapolation and whether it allows negative rates. Each type's settings are a part of the configuration, keyed by its id and present only for the types that read them: default_curve_market_terms and default_curve_cds_terms for the CDS spread, hazard rate, price, conventional spread and null types, default_curve_benchmark, default_curve_transition_matrix and default_curve_yield_reinterpretation for theirs. The quotes a configuration is built from are rows of curve_quote, and its BootstrapConfig a row of curve_bootstrap_config, both naming this row. Pillars and States are comma separated lists in ORE's own text, held as written in their parts. The schema also allows SourceCurves and SwitchDates lists, which no corpus document writes; the mapper refuses an entry that has them rather than lose them. - Artefact Table
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
    "day_counter" text null,
    "recovery_rate" text null,
    "extrapolation" text null,
    "allow_negative_rates" text null,
    "position" integer not null
);

create index if not exists dq_default_curve_configurations_artefact_dataset_idx
on ores_dq_default_curve_configurations_artefact_tbl (dataset_id);

create index if not exists dq_default_curve_configurations_artefact_tenant_idx
on ores_dq_default_curve_configurations_artefact_tbl (tenant_id);

create index if not exists dq_default_curve_configurations_artefact_id_idx
on ores_dq_default_curve_configurations_artefact_tbl (id);
