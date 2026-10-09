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
-- Defines the settlement lag, premium payment schedule, and accrual rules for a vanilla CDS. Corresponds to the <CDS> element in ORE conventions.xml. The rule field governs IMM dates and standard CDS roll dates. - Artefact Table
-- =============================================================================

create table if not exists "ores_dq_cds_conventions_artefact_tbl" (
    "dataset_id" uuid not null,
    "tenant_id" uuid not null,
    "id" text not null,
    "version" integer not null,
    "party_id" uuid not null,
    "settlement_days" integer not null,
    "calendar" text not null,
    "frequency" text not null,
    "payment_convention" text not null,
    "rule" text not null,
    "day_count_fraction" text not null,
    "settles_accrual" boolean not null,
    "pays_at_default_time" boolean not null,
    "upfront_settlement_days" integer null,
    "last_period_day_count_fraction" text null,
    "oresmd_uri" text null
);

create index if not exists dq_cds_conventions_artefact_dataset_idx
on ores_dq_cds_conventions_artefact_tbl (dataset_id);

create index if not exists dq_cds_conventions_artefact_tenant_idx
on ores_dq_cds_conventions_artefact_tbl (tenant_id);

create index if not exists dq_cds_conventions_artefact_id_idx
on ores_dq_cds_conventions_artefact_tbl (id);
