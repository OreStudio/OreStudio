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
#include "ores.analytics.quant/service/process_factory.hpp"
#include "ores.analytics.quant/domain/process_parameter_validation.hpp"
#include "ores.analytics.quant/service/processes/arithmetic_gmm_process.hpp"
#include "ores.analytics.quant/service/processes/cir_process.hpp"
#include "ores.analytics.quant/service/processes/gmm_process.hpp"
#include "ores.analytics.quant/service/processes/hull_white_process.hpp"
#include "ores.analytics.quant/service/processes/ou_process.hpp"
#include "ores.analytics.quant/service/processes/vasicek_process.hpp"
#include <stdexcept>

namespace ores::analytics::quant::service {

std::unique_ptr<ores::analytics::quant::domain::IStochasticProcess>
process_factory::make_process(const std::string& process_type,
                              std::vector<double> means,
                              std::vector<double> stdevs,
                              std::vector<double> weights,
                              double initial_price,
                              std::uint32_t seed) {
    // Single source of truth for "are these parameters good?" — shared with the
    // Qt client (ores.analytics.quant::domain::validate_process_parameters), so
    // both surfaces enforce identical rules without duplicating them.
    const auto validation = ores::analytics::quant::domain::validate_process_parameters(
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
    // Default to the geometric engine for "geometric" and any unknown value.
    // ores.analytics.quant deliberately carries no logging dependency (it stays
    // consumable standalone); callers that care about diagnosing an unknown
    // process_type should log around this call.
    return std::make_unique<gmm_process>(
        std::move(means), std::move(stdevs), std::move(weights), initial_price, seed);
}

std::unique_ptr<ores::analytics::quant::domain::IYieldCurveProcess>
process_factory::make_yield_curve_process(const std::string& process_type,
                                          double kappa,
                                          std::vector<double> theta_path,
                                          double sigma,
                                          double initial_rate,
                                          std::uint32_t seed) {
    const auto validation = ores::analytics::quant::domain::validate_yield_curve_process_parameters(
        process_type, kappa, theta_path, sigma, initial_rate);
    if (!validation.valid)
        throw std::invalid_argument(validation.message);

    if (process_type == "vasicek")
        return std::make_unique<vasicek_process>(
            kappa, theta_path.front(), sigma, initial_rate, seed);
    if (process_type == "cir")
        return std::make_unique<cir_process>(kappa, theta_path.front(), sigma, initial_rate, seed);
    if (process_type == "hull_white")
        return std::make_unique<hull_white_process>(
            kappa, std::move(theta_path), sigma, initial_rate, seed);

    throw std::invalid_argument(
        "process_factory::make_yield_curve_process: unrecognised process_type '" + process_type +
        "'");
}

}
