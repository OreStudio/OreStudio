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
-- A yield curve is bootstrapped from one or more segments, and each states its Type: Deposit, Cross Currency Basis Swap, Average OIS and eighteen more. The type fixes the segment element it is written under, so the row holds only the type and refers to curve_segment_type, and a type written under the wrong element cannot be stored. The twelve segment elements share a skeleton and differ in a few settings each: projection curves for simple and tenor basis segments, a discount curve and a spot rate for cross currency ones, a reference curve for zero spread ones. Each setting is its own typed column, null where the segment's element has no such setting. The lists a segment holds are child rows: its quotes in curve_quote, and the index curves and default curves of the bond and default based segments in curve_segment_curve. The convention a segment names may be any of ORE's convention kinds, which are one table each, so a function that looks in all of them checks the reference. - Artefact Table
-- =============================================================================

create table if not exists "ores_dq_curve_segments_artefact_tbl" (
    "dataset_id" uuid not null,
    "tenant_id" uuid not null,
    "id" uuid not null,
    "version" integer not null,
    "party_id" uuid not null,
    "curve_definition_id" uuid not null,
    "segment_type" text not null,
    "position" integer not null,
    "conventions" text null,
    "pillar_choice" text null,
    "priority" integer null,
    "min_distance" integer null,
    "projection_curve" text null,
    "discount_curve" text null,
    "spot_rate" text null,
    "projection_curve_domestic" text null,
    "projection_curve_foreign" text null,
    "projection_curve_pay" text null,
    "projection_curve_receive" text null,
    "projection_curve_long" text null,
    "projection_curve_short" text null,
    "reference_curve" text null,
    "reference_curve_2" text null,
    "weight_1" double precision null,
    "weight_2" double precision null,
    "ibor_index" text null,
    "rfr_curve" text null,
    "rfr_index" text null,
    "spread" numeric(38, 12) null,
    "base_curve" text null,
    "base_curve_currency" text null,
    "numerator_curve" text null,
    "numerator_curve_currency" text null,
    "denominator_curve" text null,
    "denominator_curve_currency" text null,
    "extrapolate_flat" boolean null
);

create index if not exists dq_curve_segments_artefact_dataset_idx
on ores_dq_curve_segments_artefact_tbl (dataset_id);

create index if not exists dq_curve_segments_artefact_tenant_idx
on ores_dq_curve_segments_artefact_tbl (tenant_id);

create index if not exists dq_curve_segments_artefact_id_idx
on ores_dq_curve_segments_artefact_tbl (id);
