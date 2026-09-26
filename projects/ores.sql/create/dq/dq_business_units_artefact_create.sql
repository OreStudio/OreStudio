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
-- Represents internal organizational units (e.g., desks, departments, branches). Supports hierarchical structure via self-referencing parent_business_unit_id. Each unit belongs to a top-level legal entity (party). - Artefact Table
-- =============================================================================

create table if not exists "ores_dq_business_units_artefact_tbl" (
    "dataset_id" uuid not null,
    "tenant_id" uuid not null,
    "id" uuid not null,
    "version" integer not null,
    "unit_name" text not null,
    "parent_business_unit_id" uuid null,
    "unit_code" text null,
    "business_centre_code" text null,
    "unit_type_code" text null
);

create index if not exists dq_business_units_artefact_dataset_idx
on ores_dq_business_units_artefact_tbl (dataset_id);

create index if not exists dq_business_units_artefact_tenant_idx
on ores_dq_business_units_artefact_tbl (tenant_id);

create index if not exists dq_business_units_artefact_id_idx
on ores_dq_business_units_artefact_tbl (id);

create index if not exists dq_business_units_artefact_id_idx
on ores_dq_business_units_artefact_tbl (id);

create index if not exists dq_business_units_artefact_id_idx
on ores_dq_business_units_artefact_tbl (id);

create index if not exists dq_business_units_artefact_id_idx
on ores_dq_business_units_artefact_tbl (id);

create index if not exists dq_business_units_artefact_id_idx
on ores_dq_business_units_artefact_tbl (id);

create index if not exists dq_business_units_artefact_id_idx
on ores_dq_business_units_artefact_tbl (id);

create index if not exists dq_business_units_artefact_id_idx
on ores_dq_business_units_artefact_tbl (id);
