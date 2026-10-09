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
-- Describes how ORE builds a cross-currency swap whose fixed leg pays one currency and whose floating leg pays an index in another. Corresponds to the <CrossCurrencyFixFloat> element in ORE conventions.xml. The id field is the natural key (ORE <Id> element). Nine fields are required and nine are optional. The corpus sets all nine required in all forty-one elements and sets none of the optional ones, so the optional half of the mapping is proven by a case that builds the element rather than by the corpus walk. - Artefact Table
-- =============================================================================

create table if not exists "ores_dq_cross_currency_fix_float_conventions_artefact_tbl" (
    "dataset_id" uuid not null,
    "tenant_id" uuid not null,
    "id" text not null,
    "version" integer not null,
    "party_id" uuid not null,
    "settlement_days" integer not null,
    "settlement_calendar" text not null,
    "settlement_convention" text not null,
    "fixed_currency" text not null,
    "fixed_frequency" text not null,
    "fixed_convention" text not null,
    "fixed_day_count_fraction" text not null,
    "index" text not null,
    "eom" boolean null,
    "is_resettable" boolean null,
    "float_index_is_resettable" boolean null,
    "include_spread" boolean null,
    "lookback" text null,
    "fixing_days" integer null,
    "rate_cutoff" integer null,
    "is_averaged" boolean null,
    "observation_shift" boolean null,
    "oresmd_uri" text null
);

create index if not exists dq_cross_currency_fix_float_conventions_artefact_dataset_idx
on ores_dq_cross_currency_fix_float_conventions_artefact_tbl (dataset_id);

create index if not exists dq_cross_currency_fix_float_conventions_artefact_tenant_idx
on ores_dq_cross_currency_fix_float_conventions_artefact_tbl (tenant_id);

create index if not exists dq_cross_currency_fix_float_conventions_artefact_id_idx
on ores_dq_cross_currency_fix_float_conventions_artefact_tbl (id);
