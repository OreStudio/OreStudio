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
 * The cross-currency basis convention wire shape.
 *
 * Field names are the C++ member names, because they are the keys rfl::json
 * writes. Renaming them breaks the wire silently, so they are not renamed.
 *
 * See the sibling protocol module for the messages that carry this type.
 */
export interface CrossCurrencyBasisConvention {
    version: number;
    tenant_id: string;
    workspace_id: string;
    id: string;
    party_id: string;
    settlement_days: number;
    settlement_calendar: string | null;
    roll_convention: string;
    flat_index: string;
    spread_index: string;
    eom: boolean | null;
    is_resettable: boolean | null;
    flat_index_is_resettable: boolean | null;
    flat_tenor: string | null;
    spread_tenor: string | null;
    spread_payment_lag: number | null;
    flat_payment_lag: number | null;
    spread_include_spread: boolean | null;
    spread_lookback: string | null;
    spread_fixing_days: number | null;
    spread_rate_cutoff: number | null;
    spread_is_averaged: boolean | null;
    spread_observation_shift: boolean | null;
    flat_include_spread: boolean | null;
    flat_lookback: string | null;
    flat_fixing_days: number | null;
    flat_rate_cutoff: number | null;
    flat_is_averaged: boolean | null;
    flat_observation_shift: boolean | null;
    modified_by: string;
    performed_by: string;
    change_reason_code: string;
    change_commentary: string;
    recorded_at: string;
}
