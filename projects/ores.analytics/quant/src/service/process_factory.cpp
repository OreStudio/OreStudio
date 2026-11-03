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
#include "ores.analytics.quant/service/processes/gjr_garch_variance_process.hpp"
#include "ores.analytics.quant/service/processes/hull_white_process.hpp"
#include "ores.analytics.quant/service/processes/ornstein_uhlenbeck_process.hpp"
#include "ores.analytics.quant/service/processes/vasicek_process.hpp"
#include <cmath>
#include <stdexcept>
#include <utility>

namespace ores::analytics::quant::service {

std::unique_ptr<ores::analytics::quant::domain::i_stochastic_process>
process_factory::make_process(const std::string& process_type,
                              std::vector<double> means,
                              std::vector<double> stdevs,
                              std::vector<double> weights,
                              double initial_price,
                              math::rng_stream stream,
                              double dt) {
    // Single source of truth for "are these parameters good?" — shared with the
    // Qt client (ores.analytics.quant::domain::validate_process_parameters), so
    // both surfaces enforce identical rules without duplicating them.
    const auto validation = ores::analytics::quant::domain::validate_process_parameters(
        process_type, means, stdevs, weights, initial_price);
    if (!validation.valid)
        throw std::invalid_argument(validation.message);

    if (process_type == "arithmetic")
        return std::make_unique<arithmetic_gaussian_mixture_model_process>(std::move(means),
                                                                           std::move(stdevs),
                                                                           std::move(weights),
                                                                           initial_price,
                                                                           std::move(stream));
    if (process_type == "ornstein_uhlenbeck") {
        const double kappa = weights.front();
        const double sigma = stdevs.front();
        return std::make_unique<ornstein_uhlenbeck_process>(
            kappa, initial_price, sigma, initial_price, std::move(stream), dt);
    }
    // Default to the geometric engine for "geometric" and any unknown value.
    // ores.analytics.quant deliberately carries no logging dependency (it stays
    // consumable standalone); callers that care about diagnosing an unknown
    // process_type should log around this call.
    return std::make_unique<gaussian_mixture_model_process>(
        std::move(means), std::move(stdevs), std::move(weights), initial_price, std::move(stream));
}

std::unique_ptr<ores::analytics::quant::domain::i_stochastic_process>
process_factory::make_gjr_garch_process(const std::string& process_type,
                                        std::vector<double> means,
                                        std::vector<double> stdevs,
                                        std::vector<double> weights,
                                        double initial_price,
                                        math::rng_stream stream,
                                        const math::gjr_garch_shape& shape,
                                        double ticks_per_day,
                                        double initial_variance_ratio) {
    if (process_type != "geometric" && process_type != "arithmetic")
        throw std::invalid_argument("process_factory::make_gjr_garch_process: the engine '" +
                                    process_type +
                                    "' cannot scale its volatility; use geometric or arithmetic");
    if (!(ticks_per_day > 0.0) || !std::isfinite(ticks_per_day))
        throw std::invalid_argument(
            "process_factory::make_gjr_garch_process: ticks_per_day must be positive and finite");
    if (!(initial_variance_ratio > 0.0) || !std::isfinite(initial_variance_ratio))
        throw std::invalid_argument("process_factory::make_gjr_garch_process: the initial "
                                    "variance ratio must be positive and finite");

    double total_weight = 0.0;
    for (const double w : weights)
        total_weight += w;
    math::mixture per_tick{.means = means, .stdevs = stdevs, .weights = weights};
    if (total_weight > 0.0)
        for (auto& w : per_tick.weights)
            w /= total_weight;
    const auto moments = math::moments_of(per_tick);
    const auto measure =
        process_type == "geometric" ? return_measure::log_return : return_measure::increment;
    const daily_variance_frame frame{.ticks_per_day = ticks_per_day,
                                     .inner_daily_variance = moments.variance * ticks_per_day,
                                     .inner_daily_drift = moments.mean * ticks_per_day,
                                     .measure = measure};
    const auto model =
        math::gjr_garch_model::make(shape,
                                    frame.inner_daily_variance,
                                    math::daily_negative_second_moment(per_tick, ticks_per_day));
    if (!model)
        throw std::invalid_argument("process_factory::make_gjr_garch_process: " + model.error());

    auto inner = make_process(process_type,
                              std::move(means),
                              std::move(stdevs),
                              std::move(weights),
                              initial_price,
                              std::move(stream));
    return std::make_unique<gjr_garch_variance_process>(
        std::move(inner), *model, frame, initial_variance_ratio * frame.inner_daily_variance);
}

std::unique_ptr<ores::analytics::quant::domain::i_yield_curve_process>
process_factory::make_yield_curve_process(const std::string& process_type,
                                          double kappa,
                                          std::vector<double> theta_path,
                                          double sigma,
                                          double initial_rate,
                                          math::rng_stream stream,
                                          double dt) {
    const auto validation = ores::analytics::quant::domain::validate_yield_curve_process_parameters(
        process_type, kappa, theta_path, sigma, initial_rate);
    if (!validation.valid)
        throw std::invalid_argument(validation.message);

    if (process_type == "vasicek")
        return std::make_unique<vasicek_process>(
            kappa, theta_path.front(), sigma, initial_rate, std::move(stream), dt);
    if (process_type == "cox_ingersoll_ross")
        return std::make_unique<cox_ingersoll_ross_process>(
            kappa, theta_path.front(), sigma, initial_rate, std::move(stream), dt);
    if (process_type == "hull_white")
        return std::make_unique<hull_white_process>(
            kappa, std::move(theta_path), sigma, initial_rate, std::move(stream), dt);

    throw std::invalid_argument(
        "process_factory::make_yield_curve_process: unrecognised process_type '" + process_type +
        "'");
}

}
