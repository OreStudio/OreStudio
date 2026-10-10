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
-- Describes how ORE builds both legs of a cross-currency basis swap: the two indices, their tenors, their payment lags and their fixing and cutoff rules, and whether the spread is included in a leg's coupons. Corresponds to the <CrossCurrencyBasis> element in ORE conventions.xml. The id field is the natural key (ORE <Id> element). - Artefact Table
-- =============================================================================

create table if not exists "ores_dq_cross_currency_basis_conventions_artefact_tbl" (
    "dataset_id" uuid not null,
    "tenant_id" uuid not null,
    "id" text not null,
    "version" integer not null,
    "party_id" uuid not null,
    "settlement_days" integer not null,
    "settlement_calendar" text null,
    "roll_convention" text not null,
    "flat_index" text not null,
    "spread_index" text not null,
    "eom" boolean null,
    "is_resettable" boolean null,
    "flat_index_is_resettable" boolean null,
    "flat_tenor" text null,
    "spread_tenor" text null,
    "spread_payment_lag" integer null,
    "flat_payment_lag" integer null,
    "spread_include_spread" boolean null,
    "spread_lookback" text null,
    "spread_fixing_days" integer null,
    "spread_rate_cutoff" integer null,
    "spread_is_averaged" boolean null,
    "spread_observation_shift" boolean null,
    "flat_include_spread" boolean null,
    "flat_lookback" text null,
    "flat_fixing_days" integer null,
    "flat_rate_cutoff" integer null,
    "flat_is_averaged" boolean null,
    "flat_observation_shift" boolean null,
    "oresmd_uri" text null
);

create index if not exists dq_cross_currency_basis_conventions_artefact_dataset_idx
on ores_dq_cross_currency_basis_conventions_artefact_tbl (dataset_id);

create index if not exists dq_cross_currency_basis_conventions_artefact_tenant_idx
on ores_dq_cross_currency_basis_conventions_artefact_tbl (tenant_id);

create index if not exists dq_cross_currency_basis_conventions_artefact_id_idx
on ores_dq_cross_currency_basis_conventions_artefact_tbl (id);
