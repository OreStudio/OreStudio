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
#include "ores.analytics.quant/service/processes/arithmetic_gaussian_mixture_model_process.hpp"
#include <cmath>
#include <stdexcept>

namespace ores::analytics::quant::service {

arithmetic_gaussian_mixture_model_process::arithmetic_gaussian_mixture_model_process(
    std::vector<double> means,
    std::vector<double> stdevs,
    std::vector<double> weights,
    double initial_price,
    std::uint32_t seed)
    : means_(std::move(means))
    , stdevs_(std::move(stdevs))
    , weights_(std::move(weights))
    , price_(initial_price)
    , rng_(seed)
    , component_dist_(weights_.begin(), weights_.end()) {

    if (means_.size() != stdevs_.size() || means_.size() != weights_.size())
        throw std::invalid_argument("arithmetic_gaussian_mixture_model_process: means, stdevs, "
                                    "weights must have equal size");
    if (means_.empty())
        throw std::invalid_argument(
            "arithmetic_gaussian_mixture_model_process: at least one component required");
    if (initial_price <= 0.0)
        throw std::invalid_argument(
            "arithmetic_gaussian_mixture_model_process: initial_price must be positive");
}

double arithmetic_gaussian_mixture_model_process::next() {
    return next(1.0);
}

double arithmetic_gaussian_mixture_model_process::next(double elapsed_ticks) {
    if (elapsed_ticks <= 0.0)
        throw std::invalid_argument(
            "arithmetic_gaussian_mixture_model_process: elapsed_ticks must be strictly positive");

    // One draw per tick, because the component is drawn per tick: scaling one
    // component over the whole step would grow the between-component variance
    // with the square of the step, and the spread of a span would then depend on
    // how the span was cut. A whole step of one tick is exactly the old draw.
    const auto whole = static_cast<long long>(std::floor(elapsed_ticks));
    for (long long i = 0; i < whole; ++i)
        step_one_tick();
    if (const double part = elapsed_ticks - static_cast<double>(whole); part > 0.0)
        step_partial_tick(part);
    return price_;
}

void arithmetic_gaussian_mixture_model_process::step_one_tick() {
    const int k = component_dist_(rng_);
    // std::normal_distribution requires stddev > 0 (a libstdc++ assertion aborts
    // the process on σ <= 0 in debug builds). A zero-variance component is
    // degenerate: the draw is exactly the mean, so handle it directly.
    const double sd = stdevs_[k];
    const double increment =
        sd > 0.0 ? std::normal_distribution<double>(means_[k], sd)(rng_) : means_[k];
    price_ += increment;
}

void arithmetic_gaussian_mixture_model_process::step_partial_tick(double part) {
    const int k = component_dist_(rng_);
    const double sd = stdevs_[k] * std::sqrt(part);
    const double mean = means_[k] * part;
    const double increment = sd > 0.0 ? std::normal_distribution<double>(mean, sd)(rng_) : mean;
    price_ += increment;
}

bool arithmetic_gaussian_mixture_model_process::supports_variable_step() const {
    return true;
}

double arithmetic_gaussian_mixture_model_process::current() const {
    return price_;
}

}
