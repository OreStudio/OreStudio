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
-- The ParametricSmileConfiguration of a volatility entry: how the smile's parameters are calibrated, and its residual correction. FX, swaption and cap and floor volatilities write it on the entry, so it is one row per entry, keyed to the entry's curve_definition. Its parameters are rows of curve_parametric_smile_parameter. - Artefact Table
-- =============================================================================

create table if not exists "ores_dq_curve_parametric_smiles_artefact_tbl" (
    "dataset_id" uuid not null,
    "tenant_id" uuid not null,
    "id" uuid not null,
    "version" integer not null,
    "party_id" uuid not null,
    "curve_definition_id" uuid not null,
    "max_calibration_attempts" integer not null,
    "exit_early_error_threshold" double precision not null,
    "max_acceptable_error" double precision not null,
    "residual_correction_dimension" text null
);

create index if not exists dq_curve_parametric_smiles_artefact_dataset_idx
on ores_dq_curve_parametric_smiles_artefact_tbl (dataset_id);

create index if not exists dq_curve_parametric_smiles_artefact_tenant_idx
on ores_dq_curve_parametric_smiles_artefact_tbl (tenant_id);

create index if not exists dq_curve_parametric_smiles_artefact_id_idx
on ores_dq_curve_parametric_smiles_artefact_tbl (id);
