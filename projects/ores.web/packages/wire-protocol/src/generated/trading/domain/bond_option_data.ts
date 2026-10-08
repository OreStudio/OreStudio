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
import type { BondOptionExercise } from './bond_option_exercise.js';
import type { BondOptionExerciseFee } from './bond_option_exercise_fee.js';
import type { BondOptionPaymentData } from './bond_option_payment_data.js';
import type { BondOptionPremium } from './bond_option_premium.js';
import type { BondOptionSettlement } from './bond_option_settlement.js';

/**
 * The BondOptionData wire shape.
 *
 * Field names are the C++ member names, because they are the keys rfl::json
 * writes. Renaming them breaks the wire silently, so they are not renamed.
 */
export interface BondOptionData {
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
    premiums: BondOptionPremium[];
    exercise_prices: string | null;
    exercise_fees: BondOptionExerciseFee[];
    exercise_fee_settlement_period: string | null;
    exercise_fee_settlement_calendar: string | null;
    exercise_fee_settlement_convention: string | null;
    automatic_exercise: string | null;
    exercise_data: BondOptionExercise | null;
    payment_data: BondOptionPaymentData | null;
    settlement_data: BondOptionSettlement | null;
}
