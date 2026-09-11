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
#ifndef ORES_ANALYTICS_QUANT_SERVICE_PROCESSES_QUADRATIC_GAUSSIAN_PROCESS_HPP
#define ORES_ANALYTICS_QUANT_SERVICE_PROCESSES_QUADRATIC_GAUSSIAN_PROCESS_HPP

#include "ores.analytics.quant/domain/i_yield_curve_process.hpp"
#include "ores.analytics.quant/export.hpp"
#include "ores.analytics.quant/service/processes/quadratic_gaussian_params.hpp"
#include <cstdint>
#include <random>

namespace ores::analytics::quant::service {

/**
 * @brief N-factor quadratic-Gaussian term-structure process.
 *
 * The short rate is a quadratic form of N Gaussian mean-reverting
 * factors with full covariance:
 *
 *     r(t) = delta_0 + deltas' * X(t) + X(t)' * gamma * X(t)
 *     dX(t) = diag(kappas) * (theta - X(t)) * dt + sigma * dW(t)
 *
 * with sigma the instantaneous covariance of the factor Brownian
 * motions (Ahn-Dittmar-Gallant / Cheng-Scorletti quadratic term
 * structure). The factor dynamics use the exact one-step Gaussian
 * transition, matching the affine_term_structure_process convention,
 * and the Cholesky of the exact per-tick covariance realises the
 * correlated shocks. A positive-semidefinite gamma keeps the rate
 * bounded below: the quadratic term dominates the linear one at large
 * |X|.
 *
 * The quadratic term breaks the affine bond-price ansatz, but the
 * bond price is still exponential-quadratic: integrating out each
 * tick's Gaussian shock by completion of squares gives
 *
 *     P(t, T) = exp(A - B' * X(t) - X(t)' * C * X(t))
 *
 * with the per-tick backward recursion (C, B, A for the i+1-th tick
 * remaining; V = Sigma_dt the exact per-tick factor covariance,
 * Lambda = diag(exp(-kappas*dt)) and m = (I - Lambda) * theta):
 *
 *     C~ = C (I + 2 V C)^-1
 *     P  = V (I + 2 C V)^-1 = V - 2 V C~ V
 *     C_i  = gamma * dt + Lambda' * C~ * Lambda
 *     B_i  = deltas * dt + Lambda' * (B_{i+1} + 2 C~ (m - V * B_{i+1}))
 *     A_i  = A_{i+1} - delta_0 * dt - m' * C * m - B_{i+1}' * m
 *            + 1/2 (2 C m + B_{i+1})' * P * (2 C m + B_{i+1})
 *            - 1/2 ln det(I + 2 V C)
 *
 * The first three terms of A_i are the affine recursion; the quadratic
 * correction is the expectation E[exp(-eps' C eps - (2 C m + B)'
 * eps)] of the shock eps ~ N(0, V), whose determinant piece appears in
 * A and whose covariance dressing enters C and B through C~ and P.
 * The C recursion is a discrete matrix Riccati equation: in the
 * continuous limit it is C' = gamma - 2 C Sigma C - kappa' C - C
 * kappa, which is exactly the exponential-quadratic bond ODE of the
 * literature. At gamma == 0 the recursion reduces to the affine one
 * exactly (verified by tests), so the model prices the affine,
 * hull-white, g2++ and vasicek cases identically.
 *
 * The C matrix is symmetric but not sparse in general; the recursion
 * costs one dense matrix inverse per tick, trivial at the factor
 * counts (one to three) this model targets.
 *
 * Parameters are annualised; dt is the year-fraction per tick,
 * matching the other processes. Callers never pre-scale parameters
 * for finer granularity — they pass the real dt instead.
 */
class ORES_ANALYTICS_QUANT_EXPORT quadratic_gaussian_process final
    : public ores::analytics::quant::domain::IYieldCurveProcess {
public:
    quadratic_gaussian_process(Eigen::VectorXd kappas,
                               Eigen::MatrixXd sigma,
                               Eigen::VectorXd theta,
                               double delta_0,
                               Eigen::VectorXd deltas,
                               Eigen::MatrixXd gamma,
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
    explicit quadratic_gaussian_process(const quadratic_gaussian_params& params,
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
    Eigen::MatrixXd gamma_;
    Eigen::MatrixXd gamma_dt_;
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
