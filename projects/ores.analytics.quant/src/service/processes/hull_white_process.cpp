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
#include "ores.analytics.quant/service/processes/hull_white_process.hpp"
#include <cmath>
#include <limits>
#include <stdexcept>

namespace ores::analytics::quant::service {

hull_white_process::hull_white_process(double kappa,
                                       std::vector<double> theta_path,
                                       double sigma,
                                       double initial_rate,
                                       std::uint32_t seed,
                                       double dt)
    : kappa_(kappa)
    , theta_path_(std::move(theta_path))
    , sigma_(sigma)
    , rate_(initial_rate)
    , dt_(dt)
    , rng_(seed)
    , normal_(0.0, 1.0) {

    if (theta_path_.empty())
        throw std::invalid_argument("hull_white_process: theta_path must not be empty");
    if (sigma_ < 0.0)
        throw std::invalid_argument("hull_white_process: sigma must be non-negative");
    if (dt_ <= 0.0)
        throw std::invalid_argument("hull_white_process: dt must be strictly positive");
}

double hull_white_process::theta_at(std::size_t tick) const {
    const std::size_t i = tick < theta_path_.size() ? tick : theta_path_.size() - 1;
    return theta_path_[i];
}

namespace {
// QuantLib's OrnsteinUhlenbeckProcess branches at this same threshold
// (speed < sqrt(QL_EPSILON)) rather than a bare kappa <= 0 check -- kappa
// values positive but too small to divide by safely still need the
// algebraic small-kappa limit, not just kappa == 0 exactly.
const double small_kappa_threshold = std::sqrt(std::numeric_limits<double>::epsilon());
}

double hull_white_process::next() {
    const double z = normal_(rng_);
    const double theta_i = theta_at(tick_);
    if (kappa_ > small_kappa_threshold) {
        const double decay = std::exp(-kappa_ * dt_);
        const double var = (1.0 - decay * decay) / (2.0 * kappa_);
        rate_ = theta_i + (rate_ - theta_i) * decay + sigma_ * std::sqrt(var) * z;
    } else {
        // kappa <= 0 (or too small to divide by safely): no mean reversion --
        // same degenerate treatment as ou_process's driftless-random-walk
        // fallback, and QuantLib's own small-speed algebraic limit
        // (variance = sigma^2*dt).
        rate_ += sigma_ * std::sqrt(dt_) * z;
    }
    ++tick_;
    return rate_;
}

double hull_white_process::current() const {
    return rate_;
}

double hull_white_process::discount_factor(std::size_t ticks_ahead) const {
    // Backward recursion from the bond's own maturity (tick tick_ +
    // ticks_ahead, where B = A = 0) down to now (tick_), one simulation
    // tick at a time -- see the class docstring for the derivation. Using
    // std::size_t loop counters here to index theta_at() directly.
    double b = 0.0;
    double a = 0.0;
    for (std::size_t steps_remaining = ticks_ahead; steps_remaining > 0; --steps_remaining) {
        const std::size_t i = tick_ + steps_remaining - 1;
        const double theta_i = theta_at(i);
        if (kappa_ > small_kappa_threshold) {
            const double decay = std::exp(-kappa_ * dt_);
            const double var = (1.0 - decay * decay) / (2.0 * kappa_);
            const double next_b = dt_ + b * decay;
            a = a - b * theta_i * (1.0 - decay) + 0.5 * b * b * sigma_ * sigma_ * var;
            b = next_b;
        } else {
            // kappa <= 0 (or too small to divide by safely) mirrors next()'s
            // degenerate branch: no mean-reversion pull, so theta plays no
            // role, and the per-tick variance is simply sigma^2*dt. b
            // accumulates dt (not a flat 1) per tick -- see the class
            // docstring for why this is the actual bug this dt parameter
            // fixes.
            a = a + 0.5 * b * b * sigma_ * sigma_ * dt_;
            b = b + dt_;
        }
    }
    return std::exp(a - b * rate_);
}

}
