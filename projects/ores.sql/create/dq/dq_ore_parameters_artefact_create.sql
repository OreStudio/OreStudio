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
-- The vocabulary of ORE XML. A row documents one leaf of one ORE document: an attribute or an element that holds a value. schema_name names the XML schema of the document, for example ore.xsd, and xpath is the path to the leaf in a document of that schema. The two together say which leaf this is. ORE writes the parameters of ore.xml as a name and a value inside a block. For them scope names the block (Setup, Markets, an analytic), subtype narrows it, and name is the parameter, so the three give the same path as xpath. position keeps the order the document wrote, because a document that is not reproducible in order is not reproducible. 165 of the 218 parameter names observed in the shipped corpus belong to exactly one analytic kind, which is why the name alone is not the key. - Artefact Table
-- =============================================================================

create table if not exists "ores_dq_ore_parameters_artefact_tbl" (
    "dataset_id" uuid not null,
    "tenant_id" uuid not null,
    "id" uuid not null,
    "version" integer not null,
    "scope" text not null,
    "subtype" text not null,
    "name" text not null,
    "schema_name" text not null,
    "xpath" text not null,
    "position" integer not null,
    "ore_parameter_value_domain_code" text not null,
    "is_required" boolean not null,
    "label" text not null,
    "help" text not null,
    "description" text not null,
    "default_text" text not null,
    "format" text not null,
    "introduced_in" text not null,
    "removed_in" text null
);

create index if not exists dq_ore_parameters_artefact_dataset_idx
on ores_dq_ore_parameters_artefact_tbl (dataset_id);

create index if not exists dq_ore_parameters_artefact_tenant_idx
on ores_dq_ore_parameters_artefact_tbl (tenant_id);

create index if not exists dq_ore_parameters_artefact_id_idx
on ores_dq_ore_parameters_artefact_tbl (id);
