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
-- The leg of a cross-currency basis swap that pays its index flat, as ORE's FlatIndex with the Flat* fixing rules: whether the index is resettable, the tenor, the payment lag, whether the spread is included in the coupon, the lookback, the fixing days, the rate cutoff, whether coupons are averaged, and whether the observation is shifted. They are a part because each states how this one leg builds its coupon; the spread leg beside it states its own. Each column keeps the flat_ prefix of ORE's element, because the screen shows both legs of one convention on one row, and the two legs' columns would otherwise share their names. - Artefact Table
-- =============================================================================

create table if not exists "ores_dq_cross_currency_basis_flat_legs_artefact_tbl" (
    "dataset_id" uuid not null,
    "tenant_id" uuid not null,
    "convention_name" text not null,
    "kind_code" text not null,
    "flat_index" text not null,
    "flat_index_is_resettable" boolean null,
    "flat_tenor" text null,
    "flat_payment_lag" integer null,
    "flat_include_spread" boolean null,
    "flat_lookback" text null,
    "flat_fixing_days" integer null,
    "flat_rate_cutoff" integer null,
    "flat_is_averaged" boolean null,
    "flat_observation_shift" boolean null
);

create index if not exists dq_cross_currency_basis_flat_legs_artefact_dataset_idx
on ores_dq_cross_currency_basis_flat_legs_artefact_tbl (dataset_id);

create index if not exists dq_cross_currency_basis_flat_legs_artefact_tenant_idx
on ores_dq_cross_currency_basis_flat_legs_artefact_tbl (tenant_id);

create index if not exists dq_cross_currency_basis_flat_legs_artefact_convention_name_idx
on ores_dq_cross_currency_basis_flat_legs_artefact_tbl (convention_name);
