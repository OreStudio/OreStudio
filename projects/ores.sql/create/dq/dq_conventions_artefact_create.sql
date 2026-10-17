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
-- The central row of the atomised convention. Every convention kind -- a swap, a deposit, a tenor basis swap -- is one row here plus the parts its family names. The row carries the key, the party that owns the row, the kind the row is, and the name it carries in the source document. The name is the identifier the source document gives the convention, the <Id> element of that document. It is kept so a round trip through the ORE XML is lossless, and it is part of the convention's identity: the party, the kind and the name together are what make a convention the one a caller means. Two kinds may hold the same name, because the kind is part of that identity. A convention's kind-specific fields live in that kind's parts, reached from this row by its key. The central row carries no kind-specific column. - Artefact Table
-- =============================================================================

create table if not exists "ores_dq_conventions_artefact_tbl" (
    "dataset_id" uuid not null,
    "tenant_id" uuid not null,
    "kind_code" text not null,
    "name" text not null,
    "oresmd_uri" text null
);

create index if not exists dq_conventions_artefact_dataset_idx
on ores_dq_conventions_artefact_tbl (dataset_id);

create index if not exists dq_conventions_artefact_tenant_idx
on ores_dq_conventions_artefact_tbl (tenant_id);

create index if not exists dq_conventions_artefact_name_idx
on ores_dq_conventions_artefact_tbl (name);
