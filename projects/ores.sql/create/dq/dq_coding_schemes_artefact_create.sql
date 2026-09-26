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
-- A published code list that a subject area's values come from, such as an ISO currency list or an FpML codelist. The scheme names the authority that publishes it and the subject area it applies to, and may point at the document that defines it. The authority and the subject-area columns are plain text rather than declared foreign keys, because the table has never carried a constraint and adding one would refuse rows the platform accepts today. - Artefact Table
-- =============================================================================

create table if not exists "ores_dq_coding_schemes_artefact_tbl" (
    "dataset_id" uuid not null,
    "tenant_id" uuid not null,
    "code" text not null,
    "version" integer not null,
    "name" text not null,
    "authority_type" text not null,
    "subject_area_name" text not null,
    "domain_name" text not null,
    "uri" text null,
    "description" text not null
);

create index if not exists dq_coding_schemes_artefact_dataset_idx
on ores_dq_coding_schemes_artefact_tbl (dataset_id);

create index if not exists dq_coding_schemes_artefact_tenant_idx
on ores_dq_coding_schemes_artefact_tbl (tenant_id);

create index if not exists dq_coding_schemes_artefact_code_idx
on ores_dq_coding_schemes_artefact_tbl (code);
