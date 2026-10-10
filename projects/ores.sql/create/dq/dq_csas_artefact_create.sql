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
-- The credit support annex of a netting set, with the terms ORE reads from a netting set definition. The set is named by code; publishing resolves it in the target tenant. The eligible collateral currencies are a comma separated list in posting order. - Artefact Table
-- =============================================================================

create table if not exists "ores_dq_csas_artefact_tbl" (
    "dataset_id" uuid not null,
    "tenant_id" uuid not null,
    "netting_set_code" text not null,
    "version" integer not null,
    "is_active" boolean not null,
    "bilateral" text null,
    "csa_currency" text null,
    "index_name" text null,
    "threshold_pay" double precision null,
    "threshold_receive" double precision null,
    "minimum_transfer_amount_pay" numeric(38, 12) null,
    "minimum_transfer_amount_receive" numeric(38, 12) null,
    "independent_amount_held" numeric(38, 12) null,
    "independent_amount_type" text null,
    "call_frequency" text null,
    "post_frequency" text null,
    "margin_period_of_risk" text null,
    "collateral_compounding_spread_receive" numeric(38, 12) null,
    "collateral_compounding_spread_pay" numeric(38, 12) null,
    "apply_initial_margin" boolean null,
    "initial_margin_type" text null,
    "calculate_im_amount" boolean null,
    "calculate_vm_amount" boolean null,
    "non_exempt_im_regulations" text null,
    "eligible_currencies" text null
);

create index if not exists dq_csas_artefact_dataset_idx
on ores_dq_csas_artefact_tbl (dataset_id);

create index if not exists dq_csas_artefact_tenant_idx
on ores_dq_csas_artefact_tbl (tenant_id);

create index if not exists dq_csas_artefact_netting_set_code_idx
on ores_dq_csas_artefact_tbl (netting_set_code);
