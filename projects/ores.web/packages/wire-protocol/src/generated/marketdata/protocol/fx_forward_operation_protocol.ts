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
 * Template: ts_protocol.ts.mustache
 * To modify, update the template and regenerate.
 */
/**
 * @brief The published funding curve one side of the pair was read from.
 */
export interface FxForwardCurveSource {
    currency: string;
    curve_name: string;
    output_series_id: string;
    publish_id: string;
    as_of: string;
    published_by: string;
}

/**
 * @brief One tenor of the ladder.
 *
 * Forward points run from spot to the value date. Swap points run from the near
 * date to the value date, so a reader can recompute them for another near date
 * from the forward rates alone.
 */
export interface FxForwardLadderRow {
    tenor: string;
    near_date: string;
    value_date: string;
    base_discount: number;
    quote_discount: number;
    forward_rate: number;
    forward_points: number;
    swap_points: number;
}

/**
 * @brief The forward ladder of one USD pair at one as-of.
 */
export interface GetFxForwardLadderRequest {
    pair_code: string;
    as_of: string;
    party_id: string;
}

export interface GetFxForwardLadderResponse {
    success: boolean;
    message: string;
    pair_code: string;
    as_of: string;
    spot: number;
    spot_observed_at: string;
    spot_date: string;
    spot_days: number;
    pip_factor: number;
    decimal_places: number;
    base_curve: FxForwardCurveSource;
    quote_curve: FxForwardCurveSource;
    rows: FxForwardLadderRow[];
}

export const subjects = {
    get_fx_forward_ladder_request: 'marketdata.v1.ops.get_fx_forward_ladder',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    get_fx_forward_ladder_request: true,
} as const;
