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
 * @brief Request a batch (dry-run) simulation of FX spot sample paths.
 *
 * Stateless: nothing is persisted or published, and the caller names the
 * process parameters rather than a stored config, so an unsaved editor can
 * preview what it is about to save.
 */
export interface SimulateFxSpotPathsRequest {
    /** @brief GMM component means; means, stdevs and weights have equal size. */
    gmm_means: number[];
    /** @brief GMM component standard deviations. */
    gmm_stdevs: number[];
    /** @brief GMM component weights. */
    gmm_weights: number[];
    /**
     * @brief Price-process engine: "geometric" (GBM, log-returns) or
     * "arithmetic" (arithmetic Brownian motion, absolute increments).
     */
    process_type: string;
    /** @brief Starting spot price for every path. */
    initial_price: number;
    /** @brief Number of ticks (steps) to generate per path. */
    num_ticks: number;
    /** @brief Number of independent sample paths to generate (for contrast). */
    num_paths: number;
    /** @brief Base RNG seed; path i uses seed + i. Change to reseed. */
    seed: number;
}

/**
 * @brief The generated batch, and why it is empty when it is.
 */
export interface SimulateFxSpotPathsResponse {
    success: boolean;
    message: string;
    /** @brief num_paths series, each of num_ticks prices. */
    paths: number[][];
}

export const subjects = {
    simulate_fx_spot_paths_request: 'synthetic.v1.ops.simulate_fx_spot_paths',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    simulate_fx_spot_paths_request: true,
} as const;
