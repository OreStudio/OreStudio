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
-- Reference data table for how often a leg's cashflows occur, sourced from ORE's authoritative frequencyType enumeration (external/ore/xsd/ore_types.xsd) -- Once, Annual, Semiannual, Quarterly, Bimonthly, Monthly, Lunarmonth, Weekly, Daily (the canonical long-form names; ORE also accepts single-letter aliases -- Z/A/S/Q/B/M/L/W/D -- not modelled here since this table's code is the long form other components store and display). Each row carries a period_unit/period_multiplier pair reusing [[id:01E76440-B9A5-4D0A-A32C-B0C4A7484B26][tenor_unit]]'s own vocabulary (DAY/WEEK/MONTH/YEAR, plus the NONE sentinel for Once, which has no periodic step at all -- a single payment at termination), so a caller building a payment schedule (e.g. the IR Curve Template's swap fixed leg) can reuse the exact same period-stepping arithmetic ores::refdata::domain::resolve_end_date() already implements for tenors, rather than re-deriving month/day counts from the code string. Not scoped to any single consumer -- this table exists as reusable reference data, the same category as day_count_fraction_type or business_day_convention_type. Managed by the system tenant. This table replaces ores.trading's older payment_frequency_type entity outright -- there is no case for two types modelling the same ORE enumeration ("payment frequency conventions" vs "payment frequencies" is a flimsy distinction); every ores.trading column that stored a payment-frequency code (swap_leg, credit_instrument, commodity_instrument, equity_swap_instrument) now validates against this table instead. See the parent story's * Decisions for the full reasoning. - Artefact Table
-- =============================================================================

create table if not exists "ores_dq_payment_frequencies_artefact_tbl" (
    "dataset_id" uuid not null,
    "tenant_id" uuid not null,
    "code" text not null,
    "version" integer not null,
    "name" text not null,
    "description" text not null,
    "period_unit" text not null,
    "period_multiplier" integer null,
    "display_order" integer not null default 0
);

create index if not exists dq_payment_frequencies_artefact_dataset_idx
on ores_dq_payment_frequencies_artefact_tbl (dataset_id);

create index if not exists dq_payment_frequencies_artefact_tenant_idx
on ores_dq_payment_frequencies_artefact_tbl (tenant_id);

create index if not exists dq_payment_frequencies_artefact_code_idx
on ores_dq_payment_frequencies_artefact_tbl (code);
