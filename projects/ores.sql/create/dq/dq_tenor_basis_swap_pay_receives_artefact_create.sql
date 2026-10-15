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
-- The paying and receiving leg form of a tenor basis swap. ORE writes a tenor basis swap as PayIndex and ReceiveIndex with an optional frequency each, plus the averaging and sub-periods fields. A row of this part holds that form. The long and short form lives in tenor_basis_swap_long_short, and a convention fills exactly one of the two. - Artefact Table
-- =============================================================================

create table if not exists "ores_dq_tenor_basis_swap_pay_receives_artefact_tbl" (
    "dataset_id" uuid not null,
    "tenant_id" uuid not null,
    "convention_name" text not null,
    "kind_code" text not null,
    "pay_index" text null,
    "pay_frequency" text null,
    "receive_index" text null,
    "receive_frequency" text null,
    "spread_on_rec" boolean null,
    "include_spread" boolean null,
    "sub_periods_coupon_type" text null,
    "pay_is_averaged" boolean null,
    "rec_is_averaged" boolean null
);

create index if not exists dq_tenor_basis_swap_pay_receives_artefact_dataset_idx
on ores_dq_tenor_basis_swap_pay_receives_artefact_tbl (dataset_id);

create index if not exists dq_tenor_basis_swap_pay_receives_artefact_tenant_idx
on ores_dq_tenor_basis_swap_pay_receives_artefact_tbl (tenant_id);

create index if not exists dq_tenor_basis_swap_pay_receives_artefact_convention_name_idx
on ores_dq_tenor_basis_swap_pay_receives_artefact_tbl (convention_name);
