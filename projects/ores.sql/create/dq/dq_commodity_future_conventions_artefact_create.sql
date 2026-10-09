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
-- Describes how ORE builds a commodity future: when it expires, which contract months it lists, how its expiry anchors to a day of the month or a weekday, and the schedule of the options written on it. Corresponds to the <CommodityFuture> element in ORE conventions.xml. The id field is the natural key (ORE <Id> element). Thirty of the element's thirty-four fields are modelled here, as forty columns, because the element's nested structs are flattened rather than given tables of their own. AnchorDay becomes seven columns, OptionNthWeekday two more, and OffPeakPowerIndexData four. The corpus uses all six of AnchorDay's variants, so flattening it is what lets a commodity future round trip at all. Five files carry the element, in eight hundred and eight elements, and two of them carried nothing else unmodelled when it landed. Four more fields hold lists, and each is written as a text column rather than counted: AveragingData becomes eight columns, and ProhibitedExpiries, FutureContinuationMappings and OptionContinuationMappings become one text column each, holding comma-separated dates and from:to pairs. A column is a weaker home than a table, and it is what keeps the field in the element's round trip rather than excluding the file from it. - Artefact Table
-- =============================================================================

create table if not exists "ores_dq_commodity_future_conventions_artefact_tbl" (
    "dataset_id" uuid not null,
    "tenant_id" uuid not null,
    "id" text not null,
    "version" integer not null,
    "party_id" uuid not null,
    "contract_frequency" text not null,
    "calendar" text not null,
    "expiry_calendar" text null,
    "expiry_month_lag" integer null,
    "one_contract_month" text null,
    "offset_days" integer null,
    "business_day_convention" text null,
    "adjust_before_offset" boolean null,
    "is_averaging" boolean null,
    "valid_contract_months" text null,
    "anchor_day_of_month" integer null,
    "anchor_calendar_days_before" integer null,
    "anchor_business_days_after" integer null,
    "anchor_nth_nth" integer null,
    "anchor_nth_weekday" text null,
    "anchor_last_weekday" text null,
    "anchor_weekly_day_of_the_week" text null,
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
    "hours_per_day" integer null,
    "off_peak_index" text null,
    "peak_index" text null,
    "off_peak_hours" double precision null,
    "peak_calendar" text null,
    "index_name" text null,
    "savings_time" text null,
    "delivery_location" text null,
    "balance_of_the_month" boolean null,
    "balance_of_the_month_pricing_calendar" text null,
    "option_underlying_future_convention" text null,
    "averaging_commodity_name" text null,
    "averaging_period" text null,
    "averaging_pricing_calendar" text null,
    "averaging_conventions" text null,
    "averaging_use_business_days" boolean null,
    "averaging_delivery_roll_days" integer null,
    "averaging_future_month_offset" integer null,
    "averaging_daily_expiry_offset" integer null,
    "prohibited_expiries" text null,
    "future_continuation_mappings" text null,
    "option_continuation_mappings" text null,
    "oresmd_uri" text null
);

create index if not exists dq_commodity_future_conventions_artefact_dataset_idx
on ores_dq_commodity_future_conventions_artefact_tbl (dataset_id);

create index if not exists dq_commodity_future_conventions_artefact_tenant_idx
on ores_dq_commodity_future_conventions_artefact_tbl (tenant_id);

create index if not exists dq_commodity_future_conventions_artefact_id_idx
on ores_dq_commodity_future_conventions_artefact_tbl (id);
