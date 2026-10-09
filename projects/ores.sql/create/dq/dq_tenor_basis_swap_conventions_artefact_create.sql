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
-- Describes how ORE builds the two legs of a tenor basis swap, where each leg pays a different tenor off its own index and the two legs may differ in more than the tenor. Corresponds to the <TenorBasisSwap> element in ORE conventions.xml. The id field is the natural key (ORE <Id> element). ORE lets a tenor basis swap be written two ways, and the corpus uses both. The paying and receiving form names PayIndex and ReceiveIndex with an optional frequency each, and adds the averaging and sub-periods fields. The long and short form names LongIndex and ShortIndex with an optional payment tenor each, and adds the spread flags. The two forms do not share a field, so the table carries every field of both and each row fills one form. - Artefact Table
-- =============================================================================

create table if not exists "ores_dq_tenor_basis_swap_conventions_artefact_tbl" (
    "dataset_id" uuid not null,
    "tenant_id" uuid not null,
    "id" text not null,
    "version" integer not null,
    "party_id" uuid not null,
    "pay_index" text null,
    "pay_frequency" text null,
    "receive_index" text null,
    "receive_frequency" text null,
    "spread_on_rec" boolean null,
    "include_spread" boolean null,
    "sub_periods_coupon_type" text null,
    "pay_is_averaged" boolean null,
    "rec_is_averaged" boolean null,
    "long_index" text null,
    "long_pay_tenor" text null,
    "short_index" text null,
    "short_pay_tenor" text null,
    "spread_on_short" boolean null,
    "oresmd_uri" text null
);

create index if not exists dq_tenor_basis_swap_conventions_artefact_dataset_idx
on ores_dq_tenor_basis_swap_conventions_artefact_tbl (dataset_id);

create index if not exists dq_tenor_basis_swap_conventions_artefact_tenant_idx
on ores_dq_tenor_basis_swap_conventions_artefact_tbl (tenant_id);

create index if not exists dq_tenor_basis_swap_conventions_artefact_id_idx
on ores_dq_tenor_basis_swap_conventions_artefact_tbl (id);
