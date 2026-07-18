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
#ifndef ORES_SYNTHETIC_API_MESSAGING_SIMULATE_IR_CURVE_PATHS_PROTOCOL_HPP
#define ORES_SYNTHETIC_API_MESSAGING_SIMULATE_IR_CURVE_PATHS_PROTOCOL_HPP

#include <cstdint>
#include <string>
#include <vector>

namespace ores::synthetic::messaging {

/**
 * @brief Request a batch (dry-run) simulation of short-rate sample paths.
 *
 * The IR curve analogue of simulate_fx_spot_paths_request: builds the real short-rate process
 * (Vasicek/CIR/Hull-White) and steps it to generate the whole batch in one response, so an IR
 * curve editor can preview the configured process behaviour before persisting or starting a feed.
 * Stateless: nothing is persisted or published.
 */
struct simulate_ir_curve_paths_request {
    using response_type = struct simulate_ir_curve_paths_response;
    static constexpr std::string_view nats_subject = "synthetic.v1.ir_curve.simulate_paths";

    /** @brief Shared batch-size limits, applied by the service clamp and the UI spinners. */
    static constexpr int max_num_ticks = 5000;
    static constexpr int max_num_paths = 50;

    /** @brief Short-rate process engine: "vasicek", "cir", or "hull_white". */
    std::string process_type = "vasicek";

    /** @brief Mean-reversion speed. */
    double kappa = 0.0;

    /** @brief Constant mean-reversion target level. */
    double theta = 0.0;

    /** @brief Volatility. */
    double sigma = 0.0;

    /** @brief Starting short rate. */
    double initial_rate = 0.0;

    /** @brief Number of ticks (steps) to generate per path. */
    int num_ticks = 100;

    /** @brief Number of independent sample paths to generate (for contrast). */
    int num_paths = 5;

    /** @brief Base RNG seed; path i uses seed + i. Change to reseed. */
    std::uint32_t seed = 1;
};

struct simulate_ir_curve_paths_response {
    bool success = false;
    std::string message;

    /** @brief num_paths series, each of num_ticks short-rate values. */
    std::vector<std::vector<double>> paths;
};

}

#endif
