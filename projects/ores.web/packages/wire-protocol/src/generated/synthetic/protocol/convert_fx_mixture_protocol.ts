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
 * @brief Request the mixture an engine change leaves.
 *
 * Stateless: the caller names the mixture as it stands on its screen, so an
 * unsaved mixture converts as readily as a saved one.
 */
export interface ConvertFxMixtureRequest {
    /** @brief Component means; means, stdevs and weights have equal size. */
    gmm_means: number[];
    /** @brief Component standard deviations. */
    gmm_stdevs: number[];
    /** @brief Component weights; the first is κ under the Ornstein-Uhlenbeck engine. */
    gmm_weights: number[];
    /** @brief The engine the mixture is written for. */
    from_process_type: string;
    /** @brief The engine the mixture is converted for. */
    to_process_type: string;
    /** @brief The price that scales between log-returns and price units. */
    initial_price: number;
    /** @brief The feed's rate, which turns the default half-life into κ. */
    ticks_per_hour: number;
}

/**
 * @brief The converted mixture, and why there is none when there is none.
 */
export interface ConvertFxMixtureResponse {
    success: boolean;
    message: string;
    /** @brief The converted means, one per component the new engine reads. */
    gmm_means: number[];
    /** @brief The converted standard deviations. */
    gmm_stdevs: number[];
    /** @brief The converted weights, κ first under the Ornstein-Uhlenbeck engine. */
    gmm_weights: number[];
}

export const subjects = {
    convert_fx_mixture_request: 'synthetic.v1.ops.convert_fx_mixture',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    convert_fx_mixture_request: true,
} as const;
