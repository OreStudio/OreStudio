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
-- A page of ORE's knowledge base, exactly as it was loaded: a user-facing org file, its org id, where it lives in the repository, and its text, unchanged. A page is user-facing when its frontmatter says #+audience: user. The pages are installation data: they are published from DQ into the system tenant at bring-up, and every tenant reads them there. Nothing here is parsed; the indexing step reads the text and writes the sections the search reads. - Artefact Table
-- =============================================================================

create table if not exists "ores_dq_knowledge_pages_artefact_tbl" (
    "dataset_id" uuid not null,
    "tenant_id" uuid not null,
    "id" text not null,
    "version" integer not null,
    "path" text not null,
    "org" text not null
);

create index if not exists dq_knowledge_pages_artefact_dataset_idx
on ores_dq_knowledge_pages_artefact_tbl (dataset_id);

create index if not exists dq_knowledge_pages_artefact_tenant_idx
on ores_dq_knowledge_pages_artefact_tbl (tenant_id);

create index if not exists dq_knowledge_pages_artefact_id_idx
on ores_dq_knowledge_pages_artefact_tbl (id);
