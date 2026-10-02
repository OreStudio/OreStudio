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
 * The bond issue leg rate wire shape.
 *
 * Field names are the C++ member names, because they are the keys rfl::json
 * writes. Renaming them breaks the wire silently, so they are not renamed.
 *
 * See the sibling protocol module for the messages that carry this type.
 */
export interface BondIssueLegRate {
    version: number;
    tenant_id: string;
    issue_id: string;
    leg_number: number;
    rate_kind: string;
    index: string | null;
    is_in_arrears: boolean | null;
    fixing_days: number | null;
    fixing_calendar: string | null;
    last_recent_period: string | null;
    last_recent_period_calendar: string | null;
    lookback: string | null;
    rate_cutoff: number | null;
    is_averaged: boolean | null;
    has_sub_periods: boolean | null;
    include_spread: boolean | null;
    is_not_resetting_xccy: boolean | null;
    naked_option: boolean | null;
    local_cap_floor: boolean | null;
    stub_use_original_curve: boolean | null;
    observation_shift: boolean | null;
    front_stub_short_index: string | null;
    front_stub_long_index: string | null;
    front_stub_rounding_type: string | null;
    front_stub_rounding_precision: number | null;
    back_stub_short_index: string | null;
    back_stub_long_index: string | null;
    back_stub_rounding_type: string | null;
    back_stub_rounding_precision: number | null;
    modified_by: string;
    performed_by: string;
    change_reason_code: string;
    change_commentary: string;
    recorded_at: string;
}
