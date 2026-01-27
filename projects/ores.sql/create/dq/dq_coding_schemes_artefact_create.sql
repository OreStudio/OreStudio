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
set schema 'metadata';

-- =============================================================================
-- Coding Schemes Artefact Table
--
-- Staging table for coding/identification schemes before publication to
-- dq_coding_schemes_tbl. Supports the DQ artefact pipeline pattern.
-- =============================================================================

create table if not exists "metadata"."dq_coding_schemes_artefact_tbl" (
    "dataset_id" uuid not null,
    "code" text not null,
    "version" integer not null default 0,
    "name" text not null,
    "authority_type" text not null,
    "subject_area_name" text not null,
    "domain_name" text not null,
    "uri" text,
    "description" text not null
);

create index if not exists dq_coding_schemes_artefact_dataset_idx
on "metadata"."dq_coding_schemes_artefact_tbl" (dataset_id);

create index if not exists dq_coding_schemes_artefact_code_idx
on "metadata"."dq_coding_schemes_artefact_tbl" (code);

create index if not exists dq_coding_schemes_artefact_authority_type_idx
on "metadata"."dq_coding_schemes_artefact_tbl" (authority_type);
