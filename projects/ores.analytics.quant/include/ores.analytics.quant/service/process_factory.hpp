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
#ifndef ORES_ANALYTICS_QUANT_SERVICE_PROCESS_FACTORY_HPP
#define ORES_ANALYTICS_QUANT_SERVICE_PROCESS_FACTORY_HPP

#include "ores.analytics.quant/domain/i_stochastic_process.hpp"
#include "ores.analytics.quant/export.hpp"
#include <cstdint>
#include <memory>
#include <string>
#include <vector>

namespace ores::analytics::quant::service {

/**
 * @brief Factory for IStochasticProcess implementations.
 *
 * Kept minimal at the scaffold stage; will grow as more process types
 * are added in later implementation steps.
 */
class ORES_ANALYTICS_QUANT_EXPORT process_factory {
public:
    /**
     * @brief Build the price process for the given engine.
     *
     * @param process_type "geometric" (GBM), "arithmetic" (additive), or "ou"
     *        (Ornstein-Uhlenbeck, mean-reverting). Any unrecognised value
     *        falls back to geometric.
     *
     * For "ou" the GMM component channels are repurposed as scalar
     * parameters rather than a mixture (OU is a single-regime process, not a
     * mixture): weights[0] = kappa (reversion speed), stdevs[0] = sigma
     * (volatility), and initial_price doubles as theta (long-run mean) — the
     * process reverts toward its own starting level by default. means is
     * unused for "ou".
     */
    static std::unique_ptr<ores::analytics::quant::domain::IStochasticProcess>
    make_process(const std::string& process_type,
                 std::vector<double> means,
                 std::vector<double> stdevs,
                 std::vector<double> weights,
                 double initial_price,
                 std::uint32_t seed = 42);
};

}

#endif
