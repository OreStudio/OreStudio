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
-- The expiry schedule of the options written on a commodity future, as ORE's Option* fields: the month lag, the contract frequency, the offset, the calendar and business days before, the expiry day, the nth weekday, the last weekday, the weekly day and the business day convention, with the future convention an option names as its underlying and the option continuation mappings. It is a part because its fields hold one invariant: each applies only when the convention states options on the future. A convention with no options fills none of it, so it fills this part at most once. - Artefact Table
-- =============================================================================

create table if not exists "ores_dq_commodity_future_option_expiries_artefact_tbl" (
    "dataset_id" uuid not null,
    "tenant_id" uuid not null,
    "convention_name" text not null,
    "kind_code" text not null,
    "option_expiry_month_lag" integer null,
    "option_contract_frequency" text null,
    "option_expiry_offset" integer null,
    "option_calendar_days_before" integer null,
    "option_min_business_days_before" integer null,
    "option_expiry_day" integer null,
    "option_nth_nth" integer null,
    "option_nth_weekday" text null,
    "option_expiry_last_weekday_of_month" text null,
    "option_expiry_weekly_day_of_the_week" text null,
    "option_business_day_convention" text null,
    "option_underlying_future_convention" text null,
    "option_continuation_mappings" text null
);

create index if not exists dq_commodity_future_option_expiries_artefact_dataset_idx
on ores_dq_commodity_future_option_expiries_artefact_tbl (dataset_id);

create index if not exists dq_commodity_future_option_expiries_artefact_tenant_idx
on ores_dq_commodity_future_option_expiries_artefact_tbl (tenant_id);

create index if not exists dq_commodity_future_option_expiries_artefact_convention_name_idx
on ores_dq_commodity_future_option_expiries_artefact_tbl (convention_name);
