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
set schema 'ores';

-- =============================================================================
-- Contains a code representing a related party role. This can be extended to provide custom roles. - Artefact Table
-- =============================================================================

create table if not exists "ores"."dq_party_roles_artefact_tbl" (
    "dataset_id" uuid not null,
    "code" text not null,
    "version" integer not null,
    "source" text null,
    "description" text null
    ,"coding_scheme_code" text not null
);

create index if not exists dq_party_roles_artefact_dataset_idx
on "ores"."dq_party_roles_artefact_tbl" (dataset_id);

create index if not exists dq_party_roles_artefact_code_idx
on "ores"."dq_party_roles_artefact_tbl" (code);

create index if not exists dq_party_roles_artefact_coding_scheme_idx
on "ores"."dq_party_roles_artefact_tbl" (coding_scheme_code);
