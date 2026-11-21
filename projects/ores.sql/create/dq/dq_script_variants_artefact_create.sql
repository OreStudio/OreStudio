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
-- One variant of a library script. Every script has a default variant, and a few add one for another purpose, such as a finite-difference engine. The variant holds the payoff code and the variable the NPV is read from. - Artefact Table
-- =============================================================================

create table if not exists "ores_dq_script_variants_artefact_tbl" (
    "dataset_id" uuid not null,
    "tenant_id" uuid not null,
    "script_name" text not null,
    "purpose" text not null,
    "version" integer not null,
    "code" text not null,
    "npv_variable" text not null
);

create index if not exists dq_script_variants_artefact_dataset_idx
on ores_dq_script_variants_artefact_tbl (dataset_id);

create index if not exists dq_script_variants_artefact_tenant_idx
on ores_dq_script_variants_artefact_tbl (tenant_id);

create index if not exists dq_script_variants_artefact_script_name_idx
on ores_dq_script_variants_artefact_tbl (script_name);
