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
-- Reference data defining desk-style currency groupings. Unlike currency.market_tier (single-valued primary liquidity tier), a currency can belong to any number of groups simultaneously via [[id:579032D2-3637-4188-859E-6C17C1D144F7][ores.refdata.currency_currency_group_junction]] (e.g. NOK: G11 *and* SCANDIES *and* COMMODITY). Seeded with G11, SCANDIES, ANTIPODEANS, COMMODITY, ASIANS, LATAMS — extensible by inserting a row, no schema change needed for a new group. - Artefact Table
-- =============================================================================

create table if not exists "ores_dq_currency_groups_artefact_tbl" (
    "dataset_id" uuid not null,
    "tenant_id" uuid not null,
    "code" text not null,
    "version" integer not null,
    "name" text not null,
    "description" text not null,
    "display_order" integer not null default 0
);

create index if not exists dq_currency_groups_artefact_dataset_idx
on ores_dq_currency_groups_artefact_tbl (dataset_id);

create index if not exists dq_currency_groups_artefact_tenant_idx
on ores_dq_currency_groups_artefact_tbl (tenant_id);

create index if not exists dq_currency_groups_artefact_code_idx
on ores_dq_currency_groups_artefact_tbl (code);
