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
-- Defines the fixed-leg and overnight floating-leg parameters for an OIS. OIS are used to bootstrap risk-free overnight discount curves (e.g. EONIA, SOFR, SONIA). Corresponds to the <OIS> element in ORE conventions.xml. - Artefact Table
-- =============================================================================

create table if not exists "ores_dq_ois_conventions_artefact_tbl" (
    "dataset_id" uuid not null,
    "tenant_id" uuid not null,
    "id" text not null,
    "version" integer not null,
    "party_id" uuid not null,
    "spot_lag" integer not null,
    "index" text not null,
    "fixed_day_count_fraction" text not null,
    "fixed_calendar" text null,
    "payment_lag" integer null,
    "end_of_month" boolean null,
    "fixed_frequency" text null,
    "fixed_convention" text null,
    "fixed_payment_convention" text null,
    "rule" text null,
    "payment_calendar" text null,
    "rate_cutoff" integer null,
    "oresmd_uri" text null
);

create index if not exists dq_ois_conventions_artefact_dataset_idx
on ores_dq_ois_conventions_artefact_tbl (dataset_id);

create index if not exists dq_ois_conventions_artefact_tenant_idx
on ores_dq_ois_conventions_artefact_tbl (tenant_id);

create index if not exists dq_ois_conventions_artefact_id_idx
on ores_dq_ois_conventions_artefact_tbl (id);
