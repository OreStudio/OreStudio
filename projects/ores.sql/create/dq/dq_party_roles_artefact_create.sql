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
-- A party role is the part a party plays in a transaction or a relationship, such as the guarantor or the beneficiary. The scheme can be extended with custom roles. Each row is one value of an FpML coding scheme: a code, unique within its coding scheme, with the source that defines it and a description. The rows arrive through DQ from the FpML codelists. - Artefact Table
-- =============================================================================

create table if not exists "ores_dq_party_roles_artefact_tbl" (
    "dataset_id" uuid not null,
    "tenant_id" uuid not null,
    "code" text not null,
    "version" integer not null,
    "source" text null,
    "description" text null,
    "coding_scheme_code" text not null
);

create index if not exists dq_party_roles_artefact_dataset_idx
on ores_dq_party_roles_artefact_tbl (dataset_id);

create index if not exists dq_party_roles_artefact_tenant_idx
on ores_dq_party_roles_artefact_tbl (tenant_id);

create index if not exists dq_party_roles_artefact_code_idx
on ores_dq_party_roles_artefact_tbl (code);

create index if not exists dq_party_roles_artefact_coding_scheme_idx
on ores_dq_party_roles_artefact_tbl (coding_scheme_code);
