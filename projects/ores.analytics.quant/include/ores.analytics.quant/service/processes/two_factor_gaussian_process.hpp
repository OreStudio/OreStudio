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
#ifndef ORES_ANALYTICS_QUANT_SERVICE_PROCESSES_TWO_FACTOR_GAUSSIAN_PROCESS_HPP
#define ORES_ANALYTICS_QUANT_SERVICE_PROCESSES_TWO_FACTOR_GAUSSIAN_PROCESS_HPP

#include "ores.analytics.quant/domain/i_yield_curve_process.hpp"
#include "ores.analytics.quant/export.hpp"
#include <cstdint>
#include <random>

namespace ores::analytics::quant::service {

/**
 * @brief G2++ two-factor Gaussian short-rate process.
 *
 * r(t) = x(t) + y(t), where x and y are correlated Ornstein-Uhlenbeck
 * processes:
 *   dx = -kappa_x * x * dt + sigma_x * dW_x
 *   dy = -kappa_y * y * dt + sigma_y * dW_y
 *   Corr(dW_x, dW_y) = rho
 *
 * Each factor uses the exact one-step Gaussian transition (not an Euler
 * approximation), matching the hull_white_process convention. The
 * correlation is applied via Cholesky decomposition of the 2×2
 * covariance matrix at each step.
 *
 * discount_factor() prices a zero-coupon bond via backward recursion
 * for the two-factor affine bond price:
 *   P(t,T) = A(t,T) * exp(-B_x(t,T) * x(t) - B_y(t,T) * y(t))
 *
 * Derivation follows Brigo & Mercurio, "Interest Rate Models — Theory
 * and Practice", ch. 4 (two-factor Gaussian G2++ model).
 *
 * At construction the entire initial_rate is placed in factor_x_ (the
 * factor that persists under the one-factor reduction below), with
 * factor_y_ starting at zero — this asymmetry is deliberate so that
 * the one-factor reduction preserves the initial short rate.
 *
 * One-factor reduction: when sigma_y = 0 and kappa_y -> infinity,
 * factor_y is pinned to zero and the model reduces to a one-factor
 * Gaussian (Vasicek) process for factor_x alone. The constructor's
 * initial-state split (all initial_rate in factor_x) ensures this
 * limit starts at the user's specified rate, not zero.
 *
 * Parameters are annualised; dt is the year-fraction per tick, matching
 * hull_white_process. Callers never pre-scale parameters for finer
 * granularity — they pass the real dt instead.
 */
class ORES_ANALYTICS_QUANT_EXPORT two_factor_gaussian_process final
    : public ores::analytics::quant::domain::IYieldCurveProcess {
public:
    two_factor_gaussian_process(double kappa_x,
                 double kappa_y,
                 double sigma_x,
                 double sigma_y,
                 double rho,
                 double initial_rate,
                 std::uint32_t seed = 42,
                 double dt = 1.0);

    double next() override;
    double current() const override;
    double discount_factor(std::size_t ticks_ahead) const override;

private:
    double kappa_x_;
    double kappa_y_;
    double sigma_x_;
    double sigma_y_;
    double rho_;
    double factor_x_;
    double factor_y_;
    double dt_;
    std::size_t tick_ = 0;
    std::mt19937 rng_;
    std::normal_distribution<double> normal_;
};

} // namespace ores::analytics::quant::service

#endif
