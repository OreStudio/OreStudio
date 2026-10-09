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
/**
 * AUTO-GENERATED FILE - DO NOT EDIT MANUALLY
 * Template: cpp_protocol.hpp.mustache
 * To modify, update the template and regenerate.
 */
#ifndef ORES_SYNTHETIC_API_MESSAGING_SIMULATE_FX_SPOT_PATHS_PROTOCOL_HPP
#define ORES_SYNTHETIC_API_MESSAGING_SIMULATE_FX_SPOT_PATHS_PROTOCOL_HPP

#include <cstdint>
#include <string>
#include <string_view>
#include <vector>

namespace ores::synthetic::messaging {

/**
 * @brief Request a batch (dry-run) simulation of FX spot sample paths.
 *
 * Stateless: nothing is persisted or published, and the caller names the
 * process parameters rather than a stored config, so an unsaved editor can
 * preview what it is about to save.
 */
struct simulate_fx_spot_paths_request {
    using response_type = struct simulate_fx_spot_paths_response;
    static constexpr std::string_view nats_subject = "synthetic.v1.ops.simulate_fx_spot_paths";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    /** @brief GMM component means; means, stdevs and weights have equal size. */
    std::vector<double> gmm_means;
    /** @brief GMM component standard deviations. */
    std::vector<double> gmm_stdevs;
    /** @brief GMM component weights. */
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

/**
 * @brief The generated batch, and why it is empty when it is.
 */
struct simulate_fx_spot_paths_response {
    bool success = false;
    std::string message;
    /** @brief num_paths series, each of num_ticks prices. */
    std::vector<std::vector<double>> paths;
};

}

#endif
