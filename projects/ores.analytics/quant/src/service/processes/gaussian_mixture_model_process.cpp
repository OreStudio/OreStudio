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
#include "ores.analytics.quant/service/processes/gaussian_mixture_model_process.hpp"
#include <cmath>
#include <cstddef>
#include <stdexcept>
#include <utility>

namespace ores::analytics::quant::service {

gaussian_mixture_model_process::gaussian_mixture_model_process(std::vector<double> means,
                                                               std::vector<double> stdevs,
                                                               std::vector<double> weights,
                                                               double initial_price,
                                                               math::rng_stream stream)
    : means_(std::move(means))
    , stdevs_(std::move(stdevs))
    , weights_(std::move(weights))
    , price_(initial_price)
    , rng_(std::move(stream))
    , component_dist_(weights_.begin(), weights_.end()) {

    if (means_.size() != stdevs_.size() || means_.size() != weights_.size())
        throw std::invalid_argument(
            "gaussian_mixture_model_process: means, stdevs, weights must have equal size");
    if (means_.empty())
        throw std::invalid_argument(
            "gaussian_mixture_model_process: at least one component required");
    if (initial_price <= 0.0)
        throw std::invalid_argument(
            "gaussian_mixture_model_process: initial_price must be positive");
    base_ = {.means = means_, .stdevs = stdevs_, .weights = weights_};
    double total_weight = 0.0;
    for (std::size_t k = 0; k < weights_.size(); ++k) {
        mixture_mean_ += weights_[k] * means_[k];
        total_weight += weights_[k];
    }
    if (total_weight > 0.0)
        mixture_mean_ /= total_weight;
}

double gaussian_mixture_model_process::next() {
    return next(1.0);
}

double gaussian_mixture_model_process::next(double elapsed_ticks) {
    if (elapsed_ticks <= 0.0)
        throw std::invalid_argument(
            "gaussian_mixture_model_process: elapsed_ticks must be strictly positive");

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

void gaussian_mixture_model_process::step_one_tick() {
    const int k = component_dist_(rng_);
    // std::normal_distribution requires stddev > 0 (a libstdc++ assertion aborts
    // the process on σ <= 0 in debug builds). A zero-variance component is
    // degenerate: the draw is exactly the mean, so handle it directly.
    const double sd = multiplier_ * stdevs_[k];
    const double mean = mean_of(k);
    const double log_return = sd > 0.0 ? std::normal_distribution<double>(mean, sd)(rng_) : mean;
    price_ *= std::exp(log_return);
}

void gaussian_mixture_model_process::step_partial_tick(double part) {
    // A part of a tick has no component count of its own, so it scales one draw:
    // the fraction of the mean and the square root of the fraction of the
    // variance, which keeps the spread additive to first order.
    const int k = component_dist_(rng_);
    const double sd = multiplier_ * stdevs_[k] * std::sqrt(part);
    const double mean = mean_of(k) * part;
    const double log_return = sd > 0.0 ? std::normal_distribution<double>(mean, sd)(rng_) : mean;
    price_ *= std::exp(log_return);
}

double gaussian_mixture_model_process::mean_of(int k) const {
    // At a multiplier of one the configured mean is drawn as it is, so a path that
    // never scales is the path it always was, to the last bit.
    if (multiplier_ == 1.0)
        return means_[k];
    return mixture_mean_ + multiplier_ * (means_[k] - mixture_mean_);
}

void gaussian_mixture_model_process::set_volatility_multiplier(double multiplier) {
    if (!(multiplier > 0.0) || !std::isfinite(multiplier))
        throw std::invalid_argument("gaussian_mixture_model_process: the volatility multiplier "
                                    "must be positive and finite");
    multiplier_ = multiplier;
}

bool gaussian_mixture_model_process::supports_volatility_multiplier() const {
    return true;
}

bool gaussian_mixture_model_process::supports_variable_step() const {
    return true;
}

void gaussian_mixture_model_process::set_tail_tilt(double tilt) {
    if (tilt == tilt_)
        return;
    tilt_ = tilt;
    stdevs_ = math::tilt(base_, tilt).stdevs;
}

double gaussian_mixture_model_process::current() const {
    return price_;
}

}
