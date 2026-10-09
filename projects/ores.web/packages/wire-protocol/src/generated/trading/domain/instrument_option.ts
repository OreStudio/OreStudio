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
 * The instrument option wire shape.
 *
 * Field names are the C++ member names, because they are the keys rfl::json
 * writes. Renaming them breaks the wire silently, so they are not renamed.
 *
 * See the sibling protocol module for the messages that carry this type.
 */
export interface InstrumentOption {
    version: number;
    tenant_id: string;
    trade_id: string;
    trade_activity_id: string;
    long_short: string;
    option_type: string | null;
    payoff_type: string | null;
    payoff_type_2: string | null;
    style: string | null;
    notice_period: string | null;
    notice_calendar: string | null;
    notice_convention: string | null;
    mid_coupon_exercise: string | null;
    settlement: string | null;
    settlement_method: string | null;
    pay_off_at_expiry: string | null;
    premium_amount: string | null;
    premium_currency: string | null;
    premium_pay_date: string | null;
    exercise_fee_settlement_period: string | null;
    exercise_fee_settlement_calendar: string | null;
    exercise_fee_settlement_convention: string | null;
    automatic_exercise: string | null;
    has_exercise_data: boolean;
    exercise_date: string | null;
    exercise_price: string | null;
    has_payment_data: boolean;
    payment_lag: number | null;
    payment_calendar: string | null;
    payment_convention: string | null;
    payment_relative_to: string | null;
    has_settlement_data: boolean;
    settlement_pay_currency: string | null;
    settlement_fx_index: string | null;
    settlement_fixing_date: string | null;
    modified_by: string;
    performed_by: string;
    change_reason_code: string;
    change_commentary: string;
    recorded_at: string;
}
