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
-- One curve entry of one section of a curveconfig.xml, held as the identity every part of the entry hangs off: the document it belongs to, its section, its CurveId and CurveDescription, and its place in the document. The settings that follow differ by section and live on a detail row per section family, as typed columns: yield_curve_config for YieldCurves. The lists an entry holds are child rows keyed to this one: its segments in curve_segment, its quotes in curve_quote, its BootstrapConfig in curve_bootstrap_config. So one table of quotes serves every section, and a reference to a curve is a reference to this row whatever its section. The natural key is the document, the section and the curve id together: EUR-ESTR names a curve inside one section of one document, another section may name its own, and two documents may define the same curve differently. - Artefact Table
-- =============================================================================

create table if not exists "ores_dq_curve_definitions_artefact_tbl" (
    "dataset_id" uuid not null,
    "tenant_id" uuid not null,
    "id" uuid not null,
    "version" integer not null,
    "party_id" uuid not null,
    "curve_configuration_id" uuid not null,
    "section_code" text not null,
    "curve_id" text not null,
    "description" text null,
    "position" integer not null
);

create index if not exists dq_curve_definitions_artefact_dataset_idx
on ores_dq_curve_definitions_artefact_tbl (dataset_id);

create index if not exists dq_curve_definitions_artefact_tenant_idx
on ores_dq_curve_definitions_artefact_tbl (tenant_id);

create index if not exists dq_curve_definitions_artefact_id_idx
on ores_dq_curve_definitions_artefact_tbl (id);
