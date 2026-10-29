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
-- The legal entities a trade may reference, keyed by LEI. A credit trade states the legal entity its risk is on, and until now that reference was a string a caller typed: a credit curve named ACME in an engine document and an LEI were the same column, so nothing could be joined to either. The LEI is the key because it is the identifier the market uses and the one the DQ dataset carries. A published dataset of 38,000 entities becomes readable by every party through this table, which is what world data means: a legal entity is not one tenant's private row. The name, the jurisdiction and the status travel with the entity because they are facts about it, and a screen that shows only an LEI shows nothing a person can read. This model holds the vocabulary only. Which entity a given trade references, and whether that reference is an LEI or an internal identifier, is the trade's statement and lives on the trade. - Artefact Table
-- =============================================================================

create table if not exists "ores_dq_legal_entities_artefact_tbl" (
    "dataset_id" uuid not null,
    "tenant_id" uuid not null,
    "lei" text not null,
    "version" integer not null,
    "legal_name" text not null,
    "jurisdiction" text null,
    "entity_status" text null,
    "entity_category" text null,
    "display_order" integer not null default 0
);

create index if not exists dq_legal_entities_artefact_dataset_idx
on ores_dq_legal_entities_artefact_tbl (dataset_id);

create index if not exists dq_legal_entities_artefact_tenant_idx
on ores_dq_legal_entities_artefact_tbl (tenant_id);

create index if not exists dq_legal_entities_artefact_lei_idx
on ores_dq_legal_entities_artefact_tbl (lei);
