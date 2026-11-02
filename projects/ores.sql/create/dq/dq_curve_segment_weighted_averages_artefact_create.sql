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
-- The two curves a weighted average segment averages and the weight of each, as ORE's ReferenceCurve1, ReferenceCurve2, Weight1 and Weight2. They hold one invariant: the segment is their weighted sum. ORE requires all four, so every weighted average segment has this part. A segment holds at most one row of this part, so the generated table adds the unique index on the live rows of (tenant_id, curve_segment_id). - Artefact Table
-- =============================================================================

create table if not exists "ores_dq_curve_segment_weighted_averages_artefact_tbl" (
    "dataset_id" uuid not null,
    "tenant_id" uuid not null,
    "id" uuid not null,
    "version" integer not null,
    "party_id" uuid not null,
    "curve_segment_id" uuid not null,
    "reference_curve_1" text not null,
    "reference_curve_2" text not null,
    "weight_1" double precision not null,
    "weight_2" double precision not null
);

create index if not exists dq_curve_segment_weighted_averages_artefact_dataset_idx
on ores_dq_curve_segment_weighted_averages_artefact_tbl (dataset_id);

create index if not exists dq_curve_segment_weighted_averages_artefact_tenant_idx
on ores_dq_curve_segment_weighted_averages_artefact_tbl (tenant_id);

create index if not exists dq_curve_segment_weighted_averages_artefact_id_idx
on ores_dq_curve_segment_weighted_averages_artefact_tbl (id);
