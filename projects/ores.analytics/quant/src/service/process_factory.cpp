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
#include "ores.analytics.quant/service/processes/arithmetic_gaussian_mixture_model_process.hpp"
#include "ores.analytics.quant/service/processes/cox_ingersoll_ross_process.hpp"
#include "ores.analytics.quant/service/processes/gaussian_mixture_model_process.hpp"
#include "ores.analytics.quant/service/processes/hawkes_arrival_process.hpp"
#include "ores.analytics.quant/service/processes/hull_white_process.hpp"
#include "ores.analytics.quant/service/processes/ornstein_uhlenbeck_process.hpp"
#include "ores.analytics.quant/service/processes/periodic_arrival_process.hpp"
#include "ores.analytics.quant/service/processes/poisson_arrival_process.hpp"
#include "ores.analytics.quant/service/processes/vasicek_process.hpp"
#include <cmath>
#include <stdexcept>

namespace ores::analytics::quant::service {

std::unique_ptr<ores::analytics::quant::domain::i_stochastic_process>
process_factory::make_process(const std::string& process_type,
                              std::vector<double> means,
                              std::vector<double> stdevs,
                              std::vector<double> weights,
                              double initial_price,
                              std::uint32_t seed,
                              double dt) {
    // Single source of truth for "are these parameters good?" — shared with the
    // Qt client (ores.analytics.quant::domain::validate_process_parameters), so
    // both surfaces enforce identical rules without duplicating them.
    const auto validation = ores::analytics::quant::domain::validate_process_parameters(
        process_type, means, stdevs, weights, initial_price);
    if (!validation.valid)
        throw std::invalid_argument(validation.message);

    if (process_type == "arithmetic")
        return std::make_unique<arithmetic_gaussian_mixture_model_process>(
            std::move(means), std::move(stdevs), std::move(weights), initial_price, seed);
    if (process_type == "ornstein_uhlenbeck") {
        const double kappa = weights.front();
        const double sigma = stdevs.front();
        return std::make_unique<ornstein_uhlenbeck_process>(
            kappa, initial_price, sigma, initial_price, seed, dt);
    }
    // Default to the geometric engine for "geometric" and any unknown value.
    // ores.analytics.quant deliberately carries no logging dependency (it stays
    // consumable standalone); callers that care about diagnosing an unknown
    // process_type should log around this call.
    return std::make_unique<gaussian_mixture_model_process>(
        std::move(means), std::move(stdevs), std::move(weights), initial_price, seed);
}

std::unique_ptr<ores::analytics::quant::domain::i_yield_curve_process>
process_factory::make_yield_curve_process(const std::string& process_type,
                                          double kappa,
                                          std::vector<double> theta_path,
                                          double sigma,
                                          double initial_rate,
                                          std::uint32_t seed,
                                          double dt) {
    const auto validation = ores::analytics::quant::domain::validate_yield_curve_process_parameters(
        process_type, kappa, theta_path, sigma, initial_rate);
    if (!validation.valid)
        throw std::invalid_argument(validation.message);

    if (process_type == "vasicek")
        return std::make_unique<vasicek_process>(
            kappa, theta_path.front(), sigma, initial_rate, seed, dt);
    if (process_type == "cox_ingersoll_ross")
        return std::make_unique<cox_ingersoll_ross_process>(
            kappa, theta_path.front(), sigma, initial_rate, seed, dt);
    if (process_type == "hull_white")
        return std::make_unique<hull_white_process>(
            kappa, std::move(theta_path), sigma, initial_rate, seed, dt);

    throw std::invalid_argument(
        "process_factory::make_yield_curve_process: unrecognised process_type '" + process_type +
        "'");
}

std::unique_ptr<ores::analytics::quant::domain::i_arrival_process>
process_factory::make_arrival_process(const std::string& mode,
                                      double ticks_per_hour,
                                      double branching_ratio,
                                      double decay,
                                      std::uint32_t seed) {

    const auto validation = ores::analytics::quant::domain::validate_arrival_process_parameters(
        mode, ticks_per_hour, branching_ratio, decay);
    if (!validation.valid)
        throw std::invalid_argument("process_factory::make_arrival_process: " + validation.message);

    // One conversion, here: the config speaks ticks per hour, every arrival
    // process speaks arrivals per second.
    constexpr double seconds_per_hour = 3600.0;
    const double mean_rate = ticks_per_hour / seconds_per_hour;

    if (mode == "periodic")
        return std::make_unique<periodic_arrival_process>(mean_rate);
    if (mode == "poisson")
        return std::make_unique<poisson_arrival_process>(mean_rate, seed);
    if (mode == "hawkes")
        return std::make_unique<hawkes_arrival_process>(mean_rate, branching_ratio, decay, seed);

    throw std::invalid_argument("process_factory::make_arrival_process: unrecognised mode '" +
                                mode + "'");
}

}
