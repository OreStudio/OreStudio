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
-- The BootstrapConfig of one curve entry. Five section types carry the same element, so it is one table keyed to the entry rather than columns on each section's row. A row exists exactly when the entry writes the element: fourteen corpus yield curves write it empty, and the export writes them back empty. - Artefact Table
-- =============================================================================

create table if not exists "ores_dq_curve_bootstrap_configs_artefact_tbl" (
    "dataset_id" uuid not null,
    "tenant_id" uuid not null,
    "id" uuid not null,
    "version" integer not null,
    "party_id" uuid not null,
    "curve_definition_id" uuid not null,
    "default_curve_configuration_id" uuid null,
    "accuracy" double precision null,
    "global_accuracy" double precision null,
    "dont_throw" boolean null,
    "max_attempts" integer null,
    "max_factor" double precision null,
    "min_factor" double precision null,
    "dont_throw_steps" integer null,
    "global" boolean null,
    "smoothness_lambda" double precision null
);

create index if not exists dq_curve_bootstrap_configs_artefact_dataset_idx
on ores_dq_curve_bootstrap_configs_artefact_tbl (dataset_id);

create index if not exists dq_curve_bootstrap_configs_artefact_tenant_idx
on ores_dq_curve_bootstrap_configs_artefact_tbl (tenant_id);

create index if not exists dq_curve_bootstrap_configs_artefact_id_idx
on ores_dq_curve_bootstrap_configs_artefact_tbl (id);
