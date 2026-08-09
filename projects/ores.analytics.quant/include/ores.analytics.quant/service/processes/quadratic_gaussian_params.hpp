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
#ifndef ORES_ANALYTICS_QUANT_SERVICE_PROCESSES_QUADRATIC_GAUSSIAN_PARAMS_HPP
#define ORES_ANALYTICS_QUANT_SERVICE_PROCESSES_QUADRATIC_GAUSSIAN_PARAMS_HPP

#include <Eigen/Dense>

namespace ores::analytics::quant::service {

/**
 * @brief Strongly-typed parameters of the quadratic_gaussian_process.
 *
 * The row-based parameter architecture stores process parameters as
 * {name, value} pairs (see ores.synthetic's parameter definition and
 * value entities); this struct is the strongly-typed "view" of those
 * rows that the mapping layer materialises before constructing the
 * process. It is deliberately a plain aggregate -- no methods, no
 * invariants (the process constructor and the mapping layer validate).
 *
 * The short rate is a quadratic form of N Gaussian mean-reverting
 * factors:
 *
 *     r = delta_0 + deltas' * X + X' * gamma * X
 *     dX = diag(kappas) * (theta - X) * dt + sigma * dW
 *
 * with gamma the symmetric, positive-semidefinite matrix of quadratic
 * sensitivities (the short rate is bounded below exactly when gamma is
 * positive semidefinite). The parameters are the affine process's,
 * plus gamma: at gamma == 0 the model is the affine term-structure
 * process and prices exactly as it does (verified by tests).
 *
 * All parameters are annualised; dt is a separate argument to the
 * process, never folded into these values.
 */
struct quadratic_gaussian_params final {
    /// Mean-reversion speed of each factor. Must be non-negative.
    Eigen::VectorXd kappas;
    /// Instantaneous covariance of the factor Brownian motions: the
    /// square, symmetric, positive-definite matrix sigma with
    /// sigma[i][j] the covariance between factor i and factor j.
    Eigen::MatrixXd sigma;
    /// Long-run mean-reversion level of each factor.
    Eigen::VectorXd theta;
    /// Constant short-rate offset: r = delta_0 + deltas' * X + X' * gamma * X.
    double delta_0 = 0.0;
    /// Linear sensitivities of the short rate to each factor.
    Eigen::VectorXd deltas;
    /// Quadratic sensitivities of the short rate to each factor pair:
    /// symmetric, positive semidefinite, one row and column per factor.
    Eigen::MatrixXd gamma;
    /// Factor values at the start of the simulation.
    Eigen::VectorXd initial_factors;
};

} // namespace ores::analytics::quant::service

#endif
