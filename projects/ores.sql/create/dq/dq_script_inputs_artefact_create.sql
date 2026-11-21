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
-- One input a library script reads from its trade: a variable the code uses and does not declare. The type is the one ORE's scripted trade states for it (Number, Event, Index, Currency or Daycounter), and an array input states a list. A value rule, where the library states one, says which values the script accepts. The rows are derived from the code by ORE Studio's parser, as the ore.script_library dataset's methodology states, not typed by hand. - Artefact Table
-- =============================================================================

create table if not exists "ores_dq_script_inputs_artefact_tbl" (
    "dataset_id" uuid not null,
    "tenant_id" uuid not null,
    "script_name" text not null,
    "sequence_number" integer not null,
    "version" integer not null,
    "input_name" text not null,
    "data_type" text not null,
    "is_array" boolean not null,
    "value_rule" text null,
    "description" text null
);

create index if not exists dq_script_inputs_artefact_dataset_idx
on ores_dq_script_inputs_artefact_tbl (dataset_id);

create index if not exists dq_script_inputs_artefact_tenant_idx
on ores_dq_script_inputs_artefact_tbl (tenant_id);

create index if not exists dq_script_inputs_artefact_script_name_idx
on ores_dq_script_inputs_artefact_tbl (script_name);
