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
-- Describes how ORE averages the floating leg of an overnight index swap over a tenor, and how the fixed leg pays against it. Corresponds to the <AverageOIS> element in ORE conventions.xml. The id field is the natural key (ORE <Id> element). - Artefact Table
-- =============================================================================

create table if not exists "ores_dq_average_ois_conventions_artefact_tbl" (
    "dataset_id" uuid not null,
    "tenant_id" uuid not null,
    "id" text not null,
    "version" integer not null,
    "party_id" uuid not null,
    "spot_lag" integer not null,
    "fixed_tenor" text not null,
    "fixed_day_count_fraction" text not null,
    "fixed_calendar" text null,
    "fixed_convention" text null,
    "fixed_payment_convention" text null,
    "fixed_frequency" text null,
    "index" text not null,
    "on_tenor" text not null,
    "rate_cutoff" text not null,
    "oresmd_uri" text null
);

create index if not exists dq_average_ois_conventions_artefact_dataset_idx
on ores_dq_average_ois_conventions_artefact_tbl (dataset_id);

create index if not exists dq_average_ois_conventions_artefact_tenant_idx
on ores_dq_average_ois_conventions_artefact_tbl (tenant_id);

create index if not exists dq_average_ois_conventions_artefact_id_idx
on ores_dq_average_ois_conventions_artefact_tbl (id);
