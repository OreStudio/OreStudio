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
-- A name a source system uses for a netting set, such as the CPTY_A an ORE document puts in a trade's envelope, staged with the identifier scheme it belongs to and the code of the set it names. Publishing resolves the code in the target tenant and writes the alias as a netting set identifier. - Artefact Table
-- =============================================================================

create table if not exists "ores_dq_netting_set_aliases_artefact_tbl" (
    "dataset_id" uuid not null,
    "tenant_id" uuid not null,
    "id_value" text not null,
    "version" integer not null,
    "id_scheme" text not null,
    "netting_set_code" text not null,
    "description" text null
);

create index if not exists dq_netting_set_aliases_artefact_dataset_idx
on ores_dq_netting_set_aliases_artefact_tbl (dataset_id);

create index if not exists dq_netting_set_aliases_artefact_tenant_idx
on ores_dq_netting_set_aliases_artefact_tbl (tenant_id);

create index if not exists dq_netting_set_aliases_artefact_id_value_idx
on ores_dq_netting_set_aliases_artefact_tbl (id_value);

create index if not exists dq_netting_set_aliases_artefact_netting_set_code_idx
on ores_dq_netting_set_aliases_artefact_tbl (netting_set_code);
