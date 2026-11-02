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
 * The commodity future contract terms wire shape.
 *
 * Field names are the C++ member names, because they are the keys rfl::json
 * writes. Renaming them breaks the wire silently, so they are not renamed.
 *
 * See the sibling protocol module for the messages that carry this type.
 */
export interface CommodityFutureContractTerms {
    version: number;
    tenant_id: string;
    id: string;
    convention_id: string;
    party_id: string;
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
    hours_per_day: number | null;
    index_name: string | null;
    savings_time: string | null;
    delivery_location: string | null;
    balance_of_the_month: boolean | null;
    balance_of_the_month_pricing_calendar: string | null;
    prohibited_expiries: string | null;
    future_continuation_mappings: string | null;
    modified_by: string;
    performed_by: string;
    change_reason_code: string;
    change_commentary: string;
    recorded_at: string;
}
