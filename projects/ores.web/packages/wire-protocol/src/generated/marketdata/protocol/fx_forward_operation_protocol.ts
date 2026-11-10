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
    /**
     * @brief The curve publish's state now: published, withdrawn or superseded. A set built from a withdrawn curve says so.
     */
    state_code: string;
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
 * @brief One spot observation a published set read: its own for a USD pair, or one USD leg of a cross.
 */
export interface FxForwardSpotSource {
    leg_code: string;
    pair_code: string;
    series_id: string;
    observed_at: string;
    value: number;
}

/**
 * @brief Raises, or applies, the publish of one pair's forward set for the caller's party. A call that names no approved request computes the set from the party's spot and published funding curves, raises a marketdata.fx_forward_publish request and stores the set as its candidate. A call that names the approved request publishes that candidate, unchanged. A set whose spot or curve is missing is refused, naming the input, and nothing is written.
 */
export interface PublishFxForwardRequest {
    /**
     * @brief The pair, such as EUR/USD.
     */
    pair_code: string;
    /**
     * @brief The instant the set reads the market at.
     */
    as_of: string;
    /**
     * @brief The approved request this call applies, or empty to raise one.
     */
    approval_request_id: string;
    /**
     * @brief Why the set should be published, for the decider to read.
     */
    reason: string;
}

/**
 * @brief What the call did: raised a request, published a set, or refused and why.
 */
export interface PublishFxForwardResponse {
    success: boolean;
    message: string;
    approval_request_id: string;
    state_code: string;
    published_at: string | null;
    published_by: string;
}

/**
 * @brief Reads the caller's party's published forward set of a pair: the newest one at or before the as-of, exactly as it was published, with where each number came from.
 */
export interface GetPublishedFxForwardRequest {
    pair_code: string;
    as_of: string;
}

/**
 * @brief One published forward set and its provenance, or why there is none.
 */
export interface GetPublishedFxForwardResponse {
    success: boolean;
    message: string;
    publish_id: string;
    request_id: string;
    party_id: string;
    pair_code: string;
    as_of: string;
    spot_date: string;
    spot_rate: number;
    published_by: string;
    published_at: string | null;
    convention_version: number;
    calendar_codes: string;
    method_code: string;
    base_curve: FxForwardCurveSource;
    quote_curve: FxForwardCurveSource;
    /**
     * @brief Business days from the trade date to spot, from the convention.
     */
    spot_days: number;
    /**
     * @brief The size of one pip of the pair, from the convention.
     */
    pip_factor: number;
    /**
     * @brief The decimal places the pair is quoted to, from the convention.
     */
    decimal_places: number;
    /**
     * @brief Who raised the publish.
     */
    requested_by: string;
    /**
     * @brief The state of the request that authorised the publish.
     */
    approval_state_code: string;
    /**
     * @brief The rule that closed the request without a person, or empty when a person decided it.
     */
    auto_approval_code: string;
    /**
     * @brief Who decided the request: a person, or the account that closed it under the rule.
     */
    decided_by: string;
    /**
     * @brief The decision's comment. Under auto-approval it names the rule and who turned it on.
     */
    decision_comment: string;
    spots: FxForwardSpotSource[];
    rows: FxForwardLadderRow[];
}

export const subjects = {
    publish_fx_forward_request: 'marketdata.v1.ops.publish_fx_forward',
    get_published_fx_forward_request: 'marketdata.v1.ops.get_published_fx_forward',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    publish_fx_forward_request: true,
    get_published_fx_forward_request: true,
} as const;
