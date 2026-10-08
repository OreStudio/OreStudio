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
import type { BondFloatData } from './bond_float_data.js';
import type { BondScheduleData } from './bond_schedule_data.js';
import type { BondStubInterpolation } from './bond_stub_interpolation.js';

/**
 * The BondFloatingLegData wire shape.
 *
 * Field names are the C++ member names, because they are the keys rfl::json
 * writes. Renaming them breaks the wire silently, so they are not renamed.
 */
export interface BondFloatingLegData {
    index: string;
    is_in_arrears: boolean | null;
    last_recent_period: string | null;
    last_recent_period_calendar: string | null;
    fixing_days: number | null;
    lookback: string | null;
    rate_cutoff: number | null;
    is_averaged: boolean | null;
    has_sub_periods: boolean | null;
    include_spread: boolean | null;
    is_not_resetting_xccy: boolean | null;
    spreads: BondFloatData[];
    caps: BondFloatData[];
    floors: BondFloatData[];
    gearings: BondFloatData[];
    naked_option: boolean | null;
    local_cap_floor: boolean | null;
    fixing_schedule: BondScheduleData;
    reset_schedule: BondScheduleData;
    front_stub_interpolation: BondStubInterpolation | null;
    back_stub_interpolation: BondStubInterpolation | null;
    stub_use_original_curve: boolean | null;
    observation_shift: boolean | null;
}
