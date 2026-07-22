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
#include "ores.analytics.quant/service/processes/cir_process.hpp"
#include <cmath>
#include <stdexcept>

namespace ores::analytics::quant::service {

cir_process::cir_process(
    double kappa, double theta, double sigma, double initial_rate, std::uint32_t seed, double dt)
    : kappa_(kappa)
    , theta_(theta)
    , sigma_(sigma)
    , rate_(initial_rate)
    , dt_(dt)
    , rng_(seed) {

    if (kappa_ <= 0.0)
        throw std::invalid_argument("cir_process: kappa must be strictly positive");
    if (theta_ <= 0.0)
        throw std::invalid_argument("cir_process: theta must be strictly positive");
    if (sigma_ < 0.0)
        throw std::invalid_argument("cir_process: sigma must be non-negative");
    if (initial_rate < 0.0)
        throw std::invalid_argument("cir_process: initial_rate must be non-negative");
    if (dt_ <= 0.0)
        throw std::invalid_argument("cir_process: dt must be strictly positive");
}

double cir_process::next_stochastic() {
    const double decay = std::exp(-kappa_ * dt_);
    const double c = sigma_ * sigma_ * (1.0 - decay) / (4.0 * kappa_);
    const double d = 4.0 * kappa_ * theta_ / (sigma_ * sigma_);
    const double lambda = rate_ * decay / c;

    // std::poisson_distribution requires a strictly positive mean (asserts
    // in libstdc++); lambda == 0 (r_t == 0, the boundary CIR must be able
    // to reach) means N == 0 deterministically, so skip the draw rather
    // than constructing a distribution with mean 0.
    const int n = lambda > 0.0 ? std::poisson_distribution<int>(lambda / 2.0)(rng_) : 0;
    std::chi_squared_distribution<double> chi2(d + 2.0 * n);
    const double x = chi2(rng_);

    return c * x;
}

double cir_process::next() {
    if (sigma_ == 0.0) {
        // Deterministic mean-reversion ODE: dr = kappa*(theta-r)*dt. The
        // general stochastic formulas above have a sigma-in-the-denominator
        // singularity at sigma == 0, so this is a genuinely separate branch,
        // not just an optimisation.
        rate_ = theta_ + (rate_ - theta_) * std::exp(-kappa_ * dt_);
    } else {
        rate_ = next_stochastic();
    }
    return rate_;
}

double cir_process::current() const {
    return rate_;
}

double cir_process::discount_factor(std::size_t ticks_ahead) const {
    // tau is real elapsed years, not a raw tick count -- see the class
    // docstring for why omitting the *dt_ scaling here silently
    // over-discounts by roughly 1/dt_ at fine tick granularities.
    const double tau = static_cast<double>(ticks_ahead) * dt_;
    if (sigma_ == 0.0) {
        // Exact deterministic path: P = exp(-integral of r(s) ds, 0..tau),
        // r(s) = theta + (r_0 - theta) * e^{-kappa*s}.
        const double integral =
            theta_ * tau + (rate_ - theta_) * (1.0 - std::exp(-kappa_ * tau)) / kappa_;
        return std::exp(-integral);
    }

    // Numerically stable form: the textbook formula divides through by
    // e^{gamma*tau}, which overflows to +inf for large tau (e.g. a
    // daily-tick 10Y+ tenor), collapsing b/a_base to inf/inf = NaN.
    // Dividing numerator and denominator by e^{gamma*tau} up front keeps
    // every exponent non-positive (gamma > kappa always, for sigma > 0),
    // so this is exact and overflow-free for any tau.
    const double gamma = std::sqrt(kappa_ * kappa_ + 2.0 * sigma_ * sigma_);
    const double e_neg_gamma_tau = std::exp(-gamma * tau);
    const double denom = (gamma + kappa_) * (1.0 - e_neg_gamma_tau) + 2.0 * gamma * e_neg_gamma_tau;
    const double b = 2.0 * (1.0 - e_neg_gamma_tau) / denom;
    const double a_base = (2.0 * gamma * std::exp(-(gamma - kappa_) * tau / 2.0)) / denom;
    const double exponent = 2.0 * kappa_ * theta_ / (sigma_ * sigma_);
    const double a = std::pow(a_base, exponent);
    return a * std::exp(-b * rate_);
}

}
