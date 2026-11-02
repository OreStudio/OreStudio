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
-- A yield curve is bootstrapped from one or more segments, and each states its Type: Deposit, Cross Currency Basis Swap, Average OIS and eighteen more. The type fixes the segment element it is written under, so the row holds only the type and refers to curve_segment_type, and a type written under the wrong element cannot be stored. The twelve segment elements share a skeleton and differ in their settings, so the row holds only what every element has: its definition, its type and its place. Each component of settings an element states is a part of its own, joined to the segment by its id, and present only for the elements that state it: the convention in curve_segment_convention, the pillar rules in curve_segment_pillar_rule, a simple segment's projection curve in curve_segment_projection, a cross currency segment's terms in curve_segment_cross_currency_terms, and six more, one per component. The lists a segment holds are child rows too: its quotes in curve_quote, and the index curves and default curves of the bond and default based segments in curve_segment_curve. - Artefact Table
-- =============================================================================

create table if not exists "ores_dq_curve_segments_artefact_tbl" (
    "dataset_id" uuid not null,
    "tenant_id" uuid not null,
    "id" uuid not null,
    "version" integer not null,
    "party_id" uuid not null,
    "curve_definition_id" uuid not null,
    "segment_type" text not null,
    "position" integer not null
);

create index if not exists dq_curve_segments_artefact_dataset_idx
on ores_dq_curve_segments_artefact_tbl (dataset_id);

create index if not exists dq_curve_segments_artefact_tenant_idx
on ores_dq_curve_segments_artefact_tbl (tenant_id);

create index if not exists dq_curve_segments_artefact_id_idx
on ores_dq_curve_segments_artefact_tbl (id);
