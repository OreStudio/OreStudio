/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
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
#ifndef ORES_SYNTHETIC_API_MESSAGING_SIMULATE_FX_SPOT_PATHS_PROTOCOL_HPP
#define ORES_SYNTHETIC_API_MESSAGING_SIMULATE_FX_SPOT_PATHS_PROTOCOL_HPP

#include <cstdint>
#include <string>
#include <vector>

namespace ores::synthetic::messaging {

/**
 * @brief Request a batch (dry-run) simulation of FX spot sample paths.
 *
 * Unlike the live feed, which streams ticks over time, this generates the whole
 * batch in one go using the real GMM price process and returns it, so the UI
 * can preview and contrast the configured price behaviour before persisting or
 * starting a feed. Stateless: nothing is persisted or published.
 */
struct simulate_fx_spot_paths_request {
    using response_type = struct simulate_fx_spot_paths_response;
    static constexpr std::string_view nats_subject = "synthetic.v1.fx_spot.simulate";

    /** @brief Shared batch-size limits, applied by the service clamp and the UI spinners. */
    static constexpr int max_num_ticks = 5000;
    static constexpr int max_num_paths = 50;

    /** @brief GMM component parameters (means, stdevs, weights of equal size). */
    std::vector<double> gmm_means;
    std::vector<double> gmm_stdevs;
    std::vector<double> gmm_weights;

    /**
     * @brief Price-process engine: "geometric" (GBM, log-returns) or
     * "arithmetic" (arithmetic Brownian motion, absolute increments).
     */
    std::string process_type = "geometric";

    /** @brief Starting spot price for every path. */
    double initial_price = 1.0;

    /** @brief Number of ticks (steps) to generate per path. */
    int num_ticks = 100;

    /** @brief Number of independent sample paths to generate (for contrast). */
    int num_paths = 5;

    /** @brief Base RNG seed; path i uses seed + i. Change to reseed. */
    std::uint32_t seed = 1;
};

struct simulate_fx_spot_paths_response {
    bool success = false;
    std::string message;

    /** @brief num_paths series, each of num_ticks prices. */
    std::vector<std::vector<double>> paths;
};

}

#endif
