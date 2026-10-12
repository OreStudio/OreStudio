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
 * @brief Request a mixture in the terms of the simple view, or the other way.
 *
 * Stateless: the caller names the values as they stand on its screen.
 */
export interface ConvertFxSimpleRequest {
    /** @brief The engine the mixture is read by: geometric, arithmetic or ornstein_uhlenbeck. */
    process_type: string;
    /** @brief True to read the mixture as the simple view; false to write the simple view as a mixture. */
    to_simple: boolean;
    /** @brief The price that scales between log-returns and price units. */
    initial_price: number;
    /** @brief The feed's rate, which turns a figure per hour into one per update. */
    ticks_per_hour: number;
    /** @brief Component means, when reading a mixture; means, stdevs and weights have equal size. */
    gmm_means: number[];
    /** @brief Component standard deviations, when reading a mixture. */
    gmm_stdevs: number[];
    /** @brief Component weights, when reading a mixture; the first is κ under the Ornstein-Uhlenbeck engine. */
    gmm_weights: number[];
    /** @brief The simple view's drift, as a percent of the price over an hour. */
    trend_pct_per_hour: number;
    /** @brief The simple view's volatility, as a percent of the price over an hour. */
    volatility_pct_per_hour: number;
    /** @brief The percent of updates that come from the jump regime. */
    jump_share_pct: number;
    /** @brief Minutes to come half way back to the long-run price; read by Ornstein-Uhlenbeck only. */
    half_life_minutes: number;
}

/**
 * @brief The converted values, and why there are none when there are none.
 */
export interface ConvertFxSimpleResponse {
    success: boolean;
    message: string;
    /** @brief False when the simple view only approximates the mixture; message then says why. */
    has_simple_form: boolean;
    /** @brief The simple view's drift, as a percent of the price over an hour. */
    trend_pct_per_hour: number;
    /** @brief The simple view's volatility, as a percent of the price over an hour. */
    volatility_pct_per_hour: number;
    /** @brief The percent of updates that come from the jump regime. */
    jump_share_pct: number;
    /** @brief Minutes to come half way back to the long-run price. */
    half_life_minutes: number;
    /** @brief The mixture's means. */
    gmm_means: number[];
    /** @brief The mixture's standard deviations. */
    gmm_stdevs: number[];
    /** @brief The mixture's weights, κ first under the Ornstein-Uhlenbeck engine. */
    gmm_weights: number[];
}

export const subjects = {
    convert_fx_simple_request: 'synthetic.v1.ops.convert_fx_simple',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    convert_fx_simple_request: true,
} as const;
