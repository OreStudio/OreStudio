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
#ifndef ORES_ANALYTICS_QUANT_SERVICE_PROCESSES_LIBOR_MARKET_MODEL_PARAMS_HPP
#define ORES_ANALYTICS_QUANT_SERVICE_PROCESSES_LIBOR_MARKET_MODEL_PARAMS_HPP

#include <Eigen/Dense>

namespace ores::analytics::quant::service {

/**
 * @brief Strongly-typed parameters of the libor_market_model_process.
 *
 * The row-based parameter architecture stores process parameters as
 * {name, value} pairs (see ores.synthetic's parameter definition and
 * value entities); this struct is the strongly-typed "view" of those
 * rows that the mapping layer materialises before constructing the
 * process. It is deliberately a plain aggregate -- no methods, no
 * invariants (the process constructor and the mapping layer validate).
 *
 * The model is the displaced-lognormal LIBOR market model under the
 * spot measure: rate L_i follows
 *
 *     dL_i = mu_i * dt + sigma_i * (L_i + s_i) * dW_i
 *
 * with the spot-measure no-arbitrage drift mu (see
 * lmm_spot_measure_drift) and dW_i correlated per the correlation
 * matrix. All parameters are annualised; dt is a separate argument to
 * the process, never folded into these values.
 */
struct libor_market_model_params final {
    /// Forward LIBOR rates at the start of the simulation, one per
    /// accrual period. Each L_i + displacements[i] must be positive.
    Eigen::VectorXd initial_forward_rates;
    /// Volatility of ln(L_i + s_i) for each rate, one entry per rate.
    Eigen::VectorXd volatilities;
    /// Instantaneous correlation of the rate Brownian motions: the
    /// square, symmetric, positive-semidefinite matrix with unit
    /// diagonal and entries in [-1, 1].
    Eigen::MatrixXd correlation;
    /// Displacement of each rate, non-negative. Shifts the log-normal
    /// domain: L_i + s_i > 0 is what the volatility applies to.
    Eigen::VectorXd displacements;
    /// Accrual period of each rate, the year-fraction between its
    /// fixing and its payment. Must be strictly positive, one entry
    /// per rate.
    Eigen::VectorXd tenor_spacings;
};

} // namespace ores::analytics::quant::service

#endif
