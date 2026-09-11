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
#include "ores.analytics.quant/service/processes/two_factor_gaussian_process.hpp"
#include <cmath>
#include <limits>
#include <stdexcept>

namespace ores::analytics::quant::service {

namespace {

const double small_kappa_threshold = std::sqrt(std::numeric_limits<double>::epsilon());

/**
 * @brief Exact OU transition variance factor: (1 - exp(-2*kappa*dt)) / (2*kappa).
 *
 * Falls back to dt when kappa is too small to safely divide.
 */
double ou_variance(double kappa, double dt) {
    if (kappa > small_kappa_threshold) {
        const double decay = std::exp(-2.0 * kappa * dt);
        return (1.0 - decay) / (2.0 * kappa);
    }
    return dt;
}

/**
 * @brief Exact OU B-factor: (1 - exp(-kappa * dt)) / kappa.
 *
 * This is the factor that multiplies the current OU state in the
 * one-step Gaussian transition: x_{i+1} = theta + (x_i - theta) * exp(-kappa*dt) + sigma *
 * sqrt(var) * Z. But here for B accumulation in discount_factor: B_i = dt + B_{i+1} *
 * exp(-kappa*dt).
 */
double ou_decay(double kappa, double dt) {
    return std::exp(-kappa * dt);
}

} // anonymous namespace

two_factor_gaussian_process::two_factor_gaussian_process(double kappa_x,
                                                         double kappa_y,
                                                         double sigma_x,
                                                         double sigma_y,
                                                         double rho,
                                                         double initial_rate,
                                                         std::uint32_t seed,
                                                         double dt)
    : kappa_x_(kappa_x)
    , kappa_y_(kappa_y)
    , sigma_x_(sigma_x)
    , sigma_y_(sigma_y)
    , rho_(rho)
    , factor_x_(initial_rate)
    , factor_y_(0.0)
    , dt_(dt)
    , rng_(seed)
    , normal_(0.0, 1.0) {

    if (kappa_x_ < 0.0 || kappa_y_ < 0.0)
        throw std::invalid_argument("two_factor_gaussian_process: kappas must be non-negative");
    if (sigma_x_ < 0.0 || sigma_y_ < 0.0)
        throw std::invalid_argument("two_factor_gaussian_process: sigmas must be non-negative");
    if (rho_ < -1.0 || rho_ > 1.0)
        throw std::invalid_argument("two_factor_gaussian_process: rho must be in [-1, 1]");
    if (dt_ <= 0.0)
        throw std::invalid_argument("two_factor_gaussian_process: dt must be strictly positive");
}

two_factor_gaussian_process::two_factor_gaussian_process(const two_factor_gaussian_params& params,
                                                         std::uint32_t seed,
                                                         double dt)
    : two_factor_gaussian_process(params.kappa_x,
                                  params.kappa_y,
                                  params.sigma_x,
                                  params.sigma_y,
                                  params.rho,
                                  params.initial_rate,
                                  seed,
                                  dt) {}

double two_factor_gaussian_process::next() {
    // Correlated normals via Cholesky: Z_x = U1, Z_y = rho*U1 + sqrt(1-rho^2)*U2
    const double u1 = normal_(rng_);
    const double u2 = normal_(rng_);
    const double z_x = u1;
    const double z_y = rho_ * u1 + std::sqrt(1.0 - rho_ * rho_) * u2;

    // Evolve each factor using the exact one-step Gaussian transition.
    // Unlike Hull-White, there is no theta pull here — each factor mean-reverts
    // toward zero (the OU process form, not the Hull-White target-level form).
    const double decay_x = ou_decay(kappa_x_, dt_);
    const double decay_y = ou_decay(kappa_y_, dt_);
    const double var_x = ou_variance(kappa_x_, dt_);
    const double var_y = ou_variance(kappa_y_, dt_);

    factor_x_ = factor_x_ * decay_x + sigma_x_ * std::sqrt(var_x) * z_x;
    factor_y_ = factor_y_ * decay_y + sigma_y_ * std::sqrt(var_y) * z_y;

    ++tick_;
    return current();
}

double two_factor_gaussian_process::current() const {
    return factor_x_ + factor_y_;
}

double two_factor_gaussian_process::discount_factor(std::size_t ticks_ahead) const {
    // Backward recursion for the two-factor affine bond price:
    //   P = exp(A - B_x * x - B_y * y)
    // Start at maturity (B_x = B_y = A = 0) and step back to now.
    //
    // All per-tick coefficients are constants independent of the step
    // index — hoist them outside the loop.

    const double decay_x = ou_decay(kappa_x_, dt_);
    const double decay_y = ou_decay(kappa_y_, dt_);
    const double var_x = ou_variance(kappa_x_, dt_);
    const double var_y = ou_variance(kappa_y_, dt_);

    // Cross-term covariance per tick for correlated factors:
    //   Cov(x_{t+1}, y_{t+1} | x_t, y_t) =
    //     rho * sigma_x * sigma_y * (1 - exp(-(kappa_x + kappa_y) * dt)) / (kappa_x + kappa_y)
    double cov_xy;
    const double kappa_sum = kappa_x_ + kappa_y_;
    if (kappa_sum > small_kappa_threshold) {
        cov_xy = rho_ * sigma_x_ * sigma_y_ * (1.0 - std::exp(-kappa_sum * dt_)) / kappa_sum;
    } else {
        cov_xy = rho_ * sigma_x_ * sigma_y_ * dt_;
    }

    double b_x = 0.0;
    double b_y = 0.0;
    double a = 0.0;

    for (std::size_t steps = ticks_ahead; steps > 0; --steps) {
        const double next_b_x = dt_ + b_x * decay_x;
        const double next_b_y = dt_ + b_y * decay_y;

        a = a + 0.5 * b_x * b_x * sigma_x_ * sigma_x_ * var_x +
            0.5 * b_y * b_y * sigma_y_ * sigma_y_ * var_y + b_x * b_y * cov_xy;

        b_x = next_b_x;
        b_y = next_b_y;
    }

    return std::exp(a - b_x * factor_x_ - b_y * factor_y_);
}

} // namespace ores::analytics::quant::service
