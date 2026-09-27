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
-- ISO 4217 currency definitions used for reference data. Currencies are managed per-tenant and drive financial calculations, rounding, and display formatting across the system. - Artefact Table
-- =============================================================================

create table if not exists "ores_dq_currencies_artefact_tbl" (
    "dataset_id" uuid not null,
    "tenant_id" uuid not null,
    "iso_code" text not null,
    "version" integer not null,
    "name" text not null,
    "numeric_code" text not null,
    "symbol" text not null,
    "fraction_symbol" text not null,
    "fractions_per_unit" integer not null,
    "rounding_type" text not null,
    "rounding_precision" integer not null,
    "format" text not null,
    "monetary_nature" text not null,
    "market_tier" text not null,
    "image_id" uuid null,
    "spot_days" integer not null default 2,
    "day_basis" text not null default 'ACT/360',
    "base_precedence" integer not null default 100,
    "holiday_calendar" text null
);

create index if not exists dq_currencies_artefact_dataset_idx
on ores_dq_currencies_artefact_tbl (dataset_id);

create index if not exists dq_currencies_artefact_tenant_idx
on ores_dq_currencies_artefact_tbl (tenant_id);

create index if not exists dq_currencies_artefact_iso_code_idx
on ores_dq_currencies_artefact_tbl (iso_code);
