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
#include "ores.analytics.quant/service/processes/ou_process.hpp"
#include <cmath>
#include <limits>
#include <stdexcept>

namespace ores::analytics::quant::service {

namespace {
// Mirrors hull_white_process's own threshold and QuantLib's
// OrnsteinUhlenbeckProcess::variance() branch point.
const double small_kappa_threshold = std::sqrt(std::numeric_limits<double>::epsilon());
}

ou_process::ou_process(
    double kappa, double theta, double sigma, double initial_price, std::uint32_t seed, double dt)
    : kappa_(kappa)
    , theta_(theta)
    , sigma_(sigma)
    , price_(initial_price)
    , dt_(dt)
    , rng_(seed)
    , normal_(0.0, 1.0) {

    if (sigma_ < 0.0)
        throw std::invalid_argument("ou_process: sigma must be non-negative");
    if (initial_price <= 0.0)
        throw std::invalid_argument("ou_process: initial_price must be positive");
    if (dt_ <= 0.0)
        throw std::invalid_argument("ou_process: dt must be strictly positive");
}

double ou_process::next() {
    const double z = normal_(rng_);
    if (kappa_ > small_kappa_threshold) {
        const double decay = std::exp(-kappa_ * dt_);
        const double var = (1.0 - decay * decay) / (2.0 * kappa_);
        price_ = theta_ + (price_ - theta_) * decay + sigma_ * std::sqrt(var) * z;
    } else {
        // kappa <= 0 (or too small to divide by safely): no mean reversion —
        // the kappa -> 0 limit of the exact discretisation above is a
        // driftless random walk with variance sigma^2*dt.
        price_ += sigma_ * std::sqrt(dt_) * z;
    }
    return price_;
}

double ou_process::current() const {
    return price_;
}

}
