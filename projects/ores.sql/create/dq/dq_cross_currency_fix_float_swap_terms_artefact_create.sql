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
-- The terms a cross-currency fix-float swap states once for both legs, as ORE's SettlementDays, SettlementCalendar, SettlementConvention, EOM and IsResettable. It is a part because its fields hold one invariant: each applies to the whole swap at once, so neither leg can hold it alone. The settlement fields fix the settlement date, the end of month rule rolls the coupons of both legs, and the resettable flag resets the notional of the swap. The fixed and floating legs beside it hold none of them. - Artefact Table
-- =============================================================================

create table if not exists "ores_dq_cross_currency_fix_float_swap_terms_artefact_tbl" (
    "dataset_id" uuid not null,
    "tenant_id" uuid not null,
    "convention_name" text not null,
    "kind_code" text not null,
    "settlement_days" integer not null,
    "settlement_calendar" text not null,
    "settlement_convention" text not null,
    "eom" boolean null,
    "is_resettable" boolean null
);

create index if not exists dq_cross_currency_fix_float_swap_terms_artefact_dataset_idx
on ores_dq_cross_currency_fix_float_swap_terms_artefact_tbl (dataset_id);

create index if not exists dq_cross_currency_fix_float_swap_terms_artefact_tenant_idx
on ores_dq_cross_currency_fix_float_swap_terms_artefact_tbl (tenant_id);

create index if not exists dq_cross_currency_fix_float_swap_terms_artefact_convention_name_idx
on ores_dq_cross_currency_fix_float_swap_terms_artefact_tbl (convention_name);
