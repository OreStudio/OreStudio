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
import type { SwapLegIdentity } from './swap_leg_identity.js';
import type { AuditRecord } from '../../dq/domain/audit_record.js';
/**
 * The swap leg wire shape.
 *
 * Field names are the C++ member names, because they are the keys rfl::json
 * writes. Renaming them breaks the wire silently, so they are not renamed.
 *
 * See the sibling protocol module for the messages that carry this type.
 */
export interface SwapLeg {
    identity: SwapLegIdentity;
    payer: boolean | null;
    leg_type_code: string;
    day_count_fraction_code: string;
    business_day_convention_code: string;
    payment_frequency_code: string;
    floating_index_code: string;
    currency: string;
    leg_group: string | null;
    fixing_days: number | null;
    is_in_arrears: boolean | null;
    include_spread: boolean | null;
    is_averaged: boolean | null;
    has_sub_periods: boolean | null;
    notional_initial_exchange: boolean | null;
    notional_final_exchange: boolean | null;
    payment_lag: string | null;
    payment_calendar: string | null;
    commodity_price_type: string | null;
    future_month_offset: number | null;
    daily_expiry_offset: number | null;
    pricing_calendar: string | null;
    audit: AuditRecord;
}
