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
-- ORE's fxVolatility is one xs:all with a mandatory Dimension and every smile element optional, so the schema says only that the smile fields may be absent. It cannot say that they are meaningless in one dimension: an ATM surface has no smile, and a smile surface has one. That is the deficiency this part corrects, because a part row exists only where the surface has a smile. The part holds SmileType, SmileInterpolation, SmileDelta, SmileExtrapolation, TimeInterpolation, TimeWeighting and ButterflyErrorTolerance. Dimension stays on the entry, because it does not only decide whether a smile exists: it also decides how the entry's Deltas are read. - Artefact Table
-- =============================================================================

create table if not exists "ores_dq_fx_volatility_smiles_artefact_tbl" (
    "dataset_id" uuid not null,
    "tenant_id" uuid not null,
    "id" uuid not null,
    "version" integer not null,
    "fx_volatility_config_id" uuid not null,
    "party_id" uuid not null,
    "smile_type" text null,
    "smile_interpolation" text null,
    "smile_delta" text null,
    "smile_extrapolation" text null,
    "time_interpolation" text null,
    "time_weighting" text null,
    "butterfly_error_tolerance" double precision null
);

create index if not exists dq_fx_volatility_smiles_artefact_dataset_idx
on ores_dq_fx_volatility_smiles_artefact_tbl (dataset_id);

create index if not exists dq_fx_volatility_smiles_artefact_tenant_idx
on ores_dq_fx_volatility_smiles_artefact_tbl (tenant_id);

create index if not exists dq_fx_volatility_smiles_artefact_id_idx
on ores_dq_fx_volatility_smiles_artefact_tbl (id);
