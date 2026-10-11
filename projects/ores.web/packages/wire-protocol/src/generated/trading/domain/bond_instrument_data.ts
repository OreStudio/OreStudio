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
import type { Ascot } from './ascot.js';
import type { BondForwardPremium } from './bond_forward_premium.js';
import type { BondForwardSettlement } from './bond_forward_settlement.js';
import type { BondFuture } from './bond_future.js';
import type { BondInstrument } from './bond_instrument.js';
import type { BondIssue } from './bond_issue.js';
import type { BondIssueCallDate } from './bond_issue_call_date.js';
import type { BondIssueConversionTarget } from './bond_issue_conversion_target.js';
import type { BondLegData } from './bond_leg_data.js';
import type { BondOption } from './bond_option.js';
import type { BondOptionData } from './bond_option_data.js';
import type { BondRepo } from './bond_repo.js';
import type { BondScheduleData } from './bond_schedule_data.js';
import type { BondStrikeData } from './bond_strike_data.js';
import type { BondTrs } from './bond_trs.js';

/**
 * The BondInstrumentData wire shape.
 *
 * Field names are the C++ member names, because they are the keys rfl::json
 * writes. Renaming them breaks the wire silently, so they are not renamed.
 */
export interface BondInstrumentData {
    instrument: BondInstrument;
    issue: BondIssue;
    call_dates: BondIssueCallDate[];
    conversion_targets: BondIssueConversionTarget[];
    option: BondOption | null;
    trs: BondTrs | null;
    repo: BondRepo | null;
    future: BondFuture | null;
    ascot_row: Ascot | null;
    option_exercise_dates: string[];
    option_data: BondOptionData | null;
    strike_data: BondStrikeData | null;
    option_exercise_schedule: BondScheduleData | null;
    option_redemption: string | null;
    option_price_type: string | null;
    option_knocks_out: string | null;
    trs_price_type: string;
    forward_long_in_forward: string | null;
    forward_settlement: BondForwardSettlement | null;
    forward_premium: BondForwardPremium | null;
    trs_payer: string | null;
    trs_initial_price: string | null;
    trs_schedule: BondScheduleData;
    bond_legs: BondLegData[];
    trs_funding_leg: BondLegData;
    repo_leg: BondLegData;
    ascot_swap_leg: BondLegData;
}
