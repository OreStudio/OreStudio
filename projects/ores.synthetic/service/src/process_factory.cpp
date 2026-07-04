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
#include "process_factory.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.synthetic.api/domain/process_parameter_validation.hpp"
#include "processes/arithmetic_gmm_process.hpp"
#include "processes/gmm_process.hpp"
#include "processes/ou_process.hpp"
#include <stdexcept>

namespace ores::synthetic::service {

namespace {
auto& lg() {
    static auto instance = ores::logging::make_logger("ores.synthetic.service.process_factory");
    return instance;
}
}

std::unique_ptr<ores::marketdata::domain::IStochasticProcess>
process_factory::make_process(const std::string& process_type,
                              std::vector<double> means,
                              std::vector<double> stdevs,
                              std::vector<double> weights,
                              double initial_price,
                              std::uint32_t seed) {
    // Single source of truth for "are these parameters good?" — shared with the
    // Qt client (ores.synthetic.api::domain::validate_process_parameters), so
    // both surfaces enforce identical rules without duplicating them.
    const auto validation = ores::synthetic::domain::validate_process_parameters(
        process_type, means, stdevs, weights, initial_price);
    if (!validation.valid)
        throw std::invalid_argument(validation.message);

    if (process_type == "arithmetic")
        return std::make_unique<arithmetic_gmm_process>(
            std::move(means), std::move(stdevs), std::move(weights), initial_price, seed);
    if (process_type == "ou") {
        const double kappa = weights.front();
        const double sigma = stdevs.front();
        return std::make_unique<ou_process>(kappa, initial_price, sigma, initial_price, seed);
    }
    if (process_type != "geometric") {
        using namespace ores::logging;
        BOOST_LOG_SEV(lg(), warn) << "Unknown process_type '" << process_type
                                  << "'; defaulting to geometric engine.";
    }
    // Default to the geometric engine for "geometric" and any unknown value.
    return std::make_unique<gmm_process>(
        std::move(means), std::move(stdevs), std::move(weights), initial_price, seed);
}

}
