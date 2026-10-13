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
-- A single weighted Gaussian component (mean, standard deviation, weight) of the Gaussian Mixture Model that drives an fx_spot_generation_config's price process. Components belong to a parent FX spot config via fx_spot_config_id; their weights are normalised at generation time. Party- and tenant-scoped. - Artefact Table
-- =============================================================================

create table if not exists "ores_dq_gmm_components_artefact_tbl" (
    "dataset_id" uuid not null,
    "tenant_id" uuid not null,
    "base_currency_code" text not null,
    "quote_currency_code" text not null,
    "component_index" integer not null,
    "description" text null,
    "mean" double precision not null,
    "stdev" double precision not null,
    "weight" double precision not null
);

create index if not exists dq_gmm_components_artefact_dataset_idx
on ores_dq_gmm_components_artefact_tbl (dataset_id);

create index if not exists dq_gmm_components_artefact_tenant_idx
on ores_dq_gmm_components_artefact_tbl (tenant_id);

create index if not exists dq_gmm_components_artefact_base_currency_code_idx
on ores_dq_gmm_components_artefact_tbl (base_currency_code);

create index if not exists dq_gmm_components_artefact_pair_idx
on ores_dq_gmm_components_artefact_tbl (dataset_id, base_currency_code, quote_currency_code);
