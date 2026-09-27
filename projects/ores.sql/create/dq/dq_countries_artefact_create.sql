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
-- ISO 3166-1 country definitions used for reference data. Countries use alpha-2, alpha-3, and numeric codes per the ISO standard. - Artefact Table
-- =============================================================================

create table if not exists "ores_dq_countries_artefact_tbl" (
    "dataset_id" uuid not null,
    "tenant_id" uuid not null,
    "alpha2_code" text not null,
    "version" integer not null,
    "alpha3_code" text not null,
    "numeric_code" text not null,
    "name" text not null,
    "official_name" text not null,
    "image_id" uuid null
);

create index if not exists dq_countries_artefact_dataset_idx
on ores_dq_countries_artefact_tbl (dataset_id);

create index if not exists dq_countries_artefact_tenant_idx
on ores_dq_countries_artefact_tbl (tenant_id);

create index if not exists dq_countries_artefact_alpha2_code_idx
on ores_dq_countries_artefact_tbl (alpha2_code);
