/** -*- mode: typescript-ts-mode; tab-width: 4; indent-tabs-mode: nil -*-
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
/**
 * AUTO-GENERATED FILE - DO NOT EDIT MANUALLY
 * Template: domain_types.ts.mustache
 * To modify, update the template and regenerate.
 */
/**
 * The commodity future convention wire shape.
 *
 * Field names are the C++ member names, because they are the keys rfl::json
 * writes. Renaming them breaks the wire silently, so they are not renamed.
 *
 * See the sibling protocol module for the messages that carry this type.
 */
export interface CommodityFutureConvention {
    version: number;
    tenant_id: string;
    workspace_id: string;
    id: string;
    contract_frequency: string;
    calendar: string;
    expiry_calendar: string | null;
    expiry_month_lag: number | null;
    one_contract_month: string | null;
    offset_days: number | null;
    business_day_convention: string | null;
    adjust_before_offset: boolean | null;
    is_averaging: boolean | null;
    valid_contract_months: string | null;
    anchor_day_of_month: number | null;
    anchor_calendar_days_before: number | null;
    anchor_business_days_after: number | null;
    anchor_nth_nth: number | null;
    anchor_nth_weekday: string | null;
    anchor_last_weekday: string | null;
    anchor_weekly_day_of_the_week: string | null;
    option_expiry_month_lag: number | null;
    option_contract_frequency: string | null;
    option_expiry_offset: number | null;
    option_calendar_days_before: number | null;
    option_min_business_days_before: number | null;
    option_expiry_day: number | null;
    option_nth_nth: number | null;
    option_nth_weekday: string | null;
    option_expiry_last_weekday_of_month: string | null;
    option_expiry_weekly_day_of_the_week: string | null;
    option_business_day_convention: string | null;
    hours_per_day: number | null;
    off_peak_index: string | null;
    peak_index: string | null;
    off_peak_hours: number | null;
    peak_calendar: string | null;
    index_name: string | null;
    savings_time: string | null;
    delivery_location: string | null;
    balance_of_the_month: boolean | null;
    balance_of_the_month_pricing_calendar: string | null;
    option_underlying_future_convention: string | null;
    modified_by: string;
    performed_by: string;
    change_reason_code: string;
    change_commentary: string;
    recorded_at: string;
}
