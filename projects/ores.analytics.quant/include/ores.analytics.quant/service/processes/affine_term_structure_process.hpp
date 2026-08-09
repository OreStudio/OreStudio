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
#ifndef ORES_ANALYTICS_QUANT_SERVICE_PROCESSES_AFFINE_TERM_STRUCTURE_PROCESS_HPP
#define ORES_ANALYTICS_QUANT_SERVICE_PROCESSES_AFFINE_TERM_STRUCTURE_PROCESS_HPP

#include "ores.analytics.quant/domain/i_yield_curve_process.hpp"
#include "ores.analytics.quant/export.hpp"
#include "ores.analytics.quant/service/processes/affine_term_structure_params.hpp"
#include <cstdint>
#include <random>

namespace ores::analytics::quant::service {

/**
 * @brief N-factor Gaussian affine term-structure process.
 *
 * The short rate is an affine function of N Gaussian mean-reverting
 * factors with full covariance:
 *
 *     r(t) = delta_0 + deltas' * X(t)
 *     dX(t) = diag(kappas) * (theta - X(t)) * dt + sigma * dW(t)
 *
 * with sigma the instantaneous covariance of the factor Brownian
 * motions. The factor dynamics use the exact one-step Gaussian
 * transition (not an Euler approximation), matching the
 * hull_white_process convention; the correlated shocks are applied via
 * the Cholesky factor of the exact per-tick covariance matrix.
 *
 * discount_factor() prices a zero-coupon bond via the per-tick affine
 * recursion:
 *
 *     P(t,T) = exp(A - B' * X(t))
 *     B_i = deltas * dt + diag(exp(-kappas*dt)) * B_{i+1}
 *     A_i = A_{i+1} - B_{i+1}' * (I - diag(exp(-kappas*dt))) * theta
 *           + 1/2 * B_{i+1}' * Sigma_dt * B_{i+1}
 *
 * with Sigma_dt the exact per-tick factor covariance
 *
 *     Sigma_dt[i][j] = sigma[i][j] * (1 - exp(-(kappas_i+kappas_j)*dt))
 *                      / (kappas_i + kappas_j)
 *
 * (dt in the zero-sum limit). This is the direct generalisation of the
 * one-factor hull_white_process recursion and the two-factor
 * two_factor_gaussian_process recursion; both, and vasicek_process,
 * are exact special cases (verified by tests).
 *
 * The factors are Gaussian, so the short rate is unbounded below:
 * negative rates occur with positive probability. The square-root
 * (CIR) factor dynamics, which keep rates positive, are a separate
 * model and deliberately out of scope here.
 *
 * Parameters are annualised; dt is the year-fraction per tick,
 * matching hull_white_process. Callers never pre-scale parameters for
 * finer granularity — they pass the real dt instead.
 */
class ORES_ANALYTICS_QUANT_EXPORT affine_term_structure_process final
    : public ores::analytics::quant::domain::IYieldCurveProcess {
public:
    affine_term_structure_process(Eigen::VectorXd kappas,
                                  Eigen::MatrixXd sigma,
                                  Eigen::VectorXd theta,
                                  double delta_0,
                                  Eigen::VectorXd deltas,
                                  Eigen::VectorXd initial_factors,
                                  std::uint32_t seed = 42,
                                  double dt = 1.0);

    /**
     * @brief Construct from the strongly-typed parameter struct.
     *
     * The row-based parameter architecture (ores.synthetic) stores
     * parameters as {name, value} pairs; the mapping layer materialises
     * this struct from those rows and constructs the process through this
     * overload.
     */
    explicit affine_term_structure_process(const affine_term_structure_params& params,
                                           std::uint32_t seed = 42,
                                           double dt = 1.0);

    double next() override;
    double current() const override;
    double discount_factor(std::size_t ticks_ahead) const override;

    /**
     * @brief The current factor vector.
     *
     * The interface contract exposes only the scalar short rate via
     * current(); the multi-factor state is available through this
     * accessor for simulation statistics and cross-checks.
     */
    const Eigen::VectorXd& factors() const;

private:
    double dt_;
    double delta_0_;
    Eigen::VectorXd deltas_;
    Eigen::VectorXd theta_;
    Eigen::VectorXd lambda_;
    Eigen::VectorXd theta_shift_;
    Eigen::VectorXd deltas_dt_;
    Eigen::MatrixXd sigma_dt_;
    Eigen::MatrixXd cholesky_;
    Eigen::VectorXd z_;
    Eigen::VectorXd factors_;
    std::size_t tick_ = 0;
    std::mt19937 rng_;
    std::normal_distribution<double> normal_;
};

} // namespace ores::analytics::quant::service

#endif
