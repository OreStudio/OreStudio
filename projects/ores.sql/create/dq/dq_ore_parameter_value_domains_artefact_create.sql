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
-- Every configuration parameter holds one value, and this says what that value is: a currency, a date, a boolean, an integer, a tenor, a document reference, or a list. Where the value names something in the model, referenced_entity names the entity it points at, so the mapper knows what to write into the document and a reader knows what the value means. A malformed org header makes this loader fail in a misleading way: a line that does not start at column zero, or a stray character before a #+ keyword, leaves the entity type unresolved and the generator writes files called unknown_* rather than reporting the bad line. Check the header first when a model generates unknown. - Artefact Table
-- =============================================================================

create table if not exists "ores_dq_ore_parameter_value_domains_artefact_tbl" (
    "dataset_id" uuid not null,
    "tenant_id" uuid not null,
    "code" text not null,
    "version" integer not null,
    "name" text not null,
    "storage_kind" text not null,
    "referenced_entity" text not null
);

create index if not exists dq_ore_parameter_value_domains_artefact_dataset_idx
on ores_dq_ore_parameter_value_domains_artefact_tbl (dataset_id);

create index if not exists dq_ore_parameter_value_domains_artefact_tenant_idx
on ores_dq_ore_parameter_value_domains_artefact_tbl (tenant_id);

create index if not exists dq_ore_parameter_value_domains_artefact_code_idx
on ores_dq_ore_parameter_value_domains_artefact_tbl (code);
