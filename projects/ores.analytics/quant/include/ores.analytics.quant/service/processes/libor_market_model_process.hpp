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
#ifndef ORES_ANALYTICS_QUANT_SERVICE_PROCESSES_LIBOR_MARKET_MODEL_PROCESS_HPP
#define ORES_ANALYTICS_QUANT_SERVICE_PROCESSES_LIBOR_MARKET_MODEL_PROCESS_HPP

#include "ores.analytics.quant/domain/i_yield_curve_process.hpp"
#include "ores.analytics.quant/export.hpp"
#include "ores.analytics.quant/service/processes/libor_market_model_params.hpp"
#include <cstdint>
#include <random>

namespace ores::analytics::quant::service {

/**
 * @brief The spot-measure no-arbitrage drift of the LIBOR market model.
 *
 * The displaced-lognormal LMM dynamics for the forward LIBOR rate L_i
 * are
 *
 *     dL_i = mu_i * dt + sigma_i * (L_i + s_i) * dW_i
 *
 * where sigma_i is the volatility of ln(L_i + s_i) and dW_i are
 * Brownian motions correlated per the instantaneous covariance matrix
 * C. Under the spot measure (numeraire the rolled money-market
 * account) the no-arbitrage drift is
 *
 *     mu_i = sum_{j <= i} C[i][j] * tau_j * (L_j + s_j) / (1 + tau_j * L_j)
 *
 * (Jamshidian 1997; identical to QuantLib's LMMDriftCalculator with
 * numeraire = 0, which sums over j in [0, i] and applies no sign
 * flip). Each term is the covariance between rate i and rate j times
 * the weight tau_j * (L_j + s_j) / (1 + tau_j * L_j) that rate j puts
 * on the money-market numeraire over its accrual period. The drift is
 * state dependent: it must be recomputed from the current rates at
 * every tick.
 *
 * The function validates its inputs and is also the pricing core used
 * by libor_market_model_process. The rates are not required to keep
 * L_i + s_i positive: the formula is well defined for any rates away
 * from the denominator pole L_i = -1/tau_i, and the rate-Euler
 * discretisation can step outside the log domain under a high
 * volatility. The positive log domain is the model's support and is
 * enforced on the initial rates by the process constructor.
 *
 * @param forward_rates The current forward LIBOR rates, one per
 *        accrual period.
 * @param displacements The per-rate displacements, one per rate.
 * @param tenor_spacings The accrual period of each rate, one per rate.
 * @param covariance The instantaneous covariance matrix of the rate
 *        Brownian motions: C = diag(sigma) * rho * diag(sigma) for
 *        per-rate volatilities sigma and correlation rho.
 * @return The drift vector mu, one entry per rate.
 */
ORES_ANALYTICS_QUANT_EXPORT Eigen::VectorXd
lmm_spot_measure_drift(const Eigen::VectorXd& forward_rates,
                       const Eigen::VectorXd& displacements,
                       const Eigen::VectorXd& tenor_spacings,
                       const Eigen::MatrixXd& covariance);

/**
 * @brief Displaced-lognormal LIBOR market model under the spot
 * measure, on a discrete grid of accrual periods.
 *
 * The state is the vector of forward LIBOR rates L_i for the accrual
 * periods tau_i, i = 0..N-1. Rate L_i fixes at time t_i and is paid at
 * t_{i+1}; the tenors are fixed relative to "now", so L_0 is the rate
 * fixing immediately. Each tick advances every rate by one Euler step
 * of the displaced-lognormal dynamics:
 *
 *     L_i += mu_i * dt + sigma_i * (L_i + s_i) * sqrt(dt) * Z_i
 *
 * with mu = lmm_spot_measure_drift(...) recomputed from the current
 * rates and Z the correlated Gaussian draws (Cholesky of the
 * correlation matrix). The volatility applies to the displaced rate
 * L_i + s_i: the model is log-normal in the displacement, so a
 * positive displacement keeps the log domain L_i + s_i > 0. The Euler
 * discretisation on the rates themselves carries the usual O(dt)
 * error (QuantLib's log-normal evolver discretises ln(L_i + s_i)
 * instead); with a fine dt the two agree.
 *
 * current() returns L_0, the rate fixing for the next period. The
 * rates keep evolving past their fixing time (their fixing values are
 * what the numeraire uses; a fixed rate's later values are never
 * consumed).
 *
 * discount_factor() prices a zero-coupon bond from the current
 * forward curve under the spot measure: the product of the current
 * simple discount factors,
 *
 *     product_{k < ticks_ahead} 1 / (1 + tau_k * L_k)
 *
 * the inverse of the rolled money-market account over the ticks
 * ahead. Discounting beyond the last rate's payment date is not
 * defined (no rates exist past the grid) and throws.
 *
 * Parameters are annualised; dt is the year-fraction per tick,
 * matching hull_white_process. Callers never pre-scale parameters for
 * finer granularity -- they pass the real dt instead.
 */
class ORES_ANALYTICS_QUANT_EXPORT libor_market_model_process final
    : public ores::analytics::quant::domain::IYieldCurveProcess {
public:
    libor_market_model_process(Eigen::VectorXd initial_forward_rates,
                               Eigen::VectorXd volatilities,
                               Eigen::MatrixXd correlation,
                               Eigen::VectorXd displacements,
                               Eigen::VectorXd tenor_spacings,
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
    explicit libor_market_model_process(const libor_market_model_params& params,
                                        std::uint32_t seed = 42,
                                        double dt = 1.0);

    double next() override;
    double current() const override;
    double discount_factor(std::size_t ticks_ahead) const override;

    /**
     * @brief The current forward rates at the fixed relative tenors.
     *
     * The interface contract exposes only the scalar short rate via
     * current(); the full rate vector is available through this
     * accessor for simulation statistics and cross-checks.
     */
    const Eigen::VectorXd& forward_rates() const;

private:
    double dt_;
    Eigen::VectorXd forward_rates_;
    Eigen::VectorXd displacements_;
    Eigen::VectorXd tenor_spacings_;
    Eigen::MatrixXd covariance_;
    Eigen::MatrixXd cholesky_;
    Eigen::VectorXd vol_sqrt_dt_;
    Eigen::VectorXd z_;
    Eigen::VectorXd correlated_;
    std::size_t tick_ = 0;
    std::mt19937 rng_;
    std::normal_distribution<double> normal_;
};

} // namespace ores::analytics::quant::service

#endif
