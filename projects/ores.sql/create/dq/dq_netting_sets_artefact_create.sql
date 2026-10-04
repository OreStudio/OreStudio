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
-- A netting set of the party a publish targets. It names its master agreement by number and its counterparty by LEI; publishing resolves both in the target tenant. A set with no agreement holds trades that do not net. - Artefact Table
-- =============================================================================

create table if not exists "ores_dq_netting_sets_artefact_tbl" (
    "dataset_id" uuid not null,
    "tenant_id" uuid not null,
    "code" text not null,
    "version" integer not null,
    "agreement_number" text null,
    "counterparty_lei" text null,
    "call_type" text null,
    "initial_margin_type" text null,
    "risk_weight" double precision null,
    "description" text null
);

create index if not exists dq_netting_sets_artefact_dataset_idx
on ores_dq_netting_sets_artefact_tbl (dataset_id);

create index if not exists dq_netting_sets_artefact_tenant_idx
on ores_dq_netting_sets_artefact_tbl (tenant_id);

create index if not exists dq_netting_sets_artefact_code_idx
on ores_dq_netting_sets_artefact_tbl (code);

create index if not exists dq_netting_sets_artefact_agreement_number_idx
on ores_dq_netting_sets_artefact_tbl (agreement_number);
