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
#ifndef ORES_ANALYTICS_QUANT_SERVICE_PROCESSES_HEATH_JARROW_MORTON_PROCESS_HPP
#define ORES_ANALYTICS_QUANT_SERVICE_PROCESSES_HEATH_JARROW_MORTON_PROCESS_HPP

#include "ores.analytics.quant/domain/i_yield_curve_process.hpp"
#include "ores.analytics.quant/export.hpp"
#include "ores.analytics.quant/service/processes/heath_jarrow_morton_params.hpp"
#include <cstdint>
#include <random>

namespace ores::analytics::quant::service {

/**
 * @brief The no-arbitrage drift of the single-factor HJM model.
 *
 * The single-factor HJM dynamics for the instantaneous forward rate
 * f(t, T) are
 *
 *     df(t, T) = sigma(T) * (integral_t^T sigma(s) ds) * dt + sigma(T) * dW
 *
 * (Heath, Jarrow & Morton 1992). With a time-homogeneous volatility
 * function sigma(T) and the relative-tenor grid tau_0 = 0 < tau_1 <
 * ... < tau_N, the drift of the rate at tenor tau_i discretises to
 *
 *     mu_i = sigma_i * sum_{j<i} sigma_{j+1} * (tau_{j+1} - tau_j)
 *
 * where sigma_j is the volatility at tenor tau_j and the sum is the
 * integral of the volatility curve from the front (tau_0) to tau_i.
 * The front rate therefore has zero drift: the short rate f(t, t) is a
 * martingale under the risk-neutral measure.
 *
 * The function validates its inputs and is also the pricing core used
 * by heath_jarrow_morton_process.
 *
 * @param volatilities The per-tenor volatilities, one per grid point.
 * @param tenor_spacings The grid spacing between consecutive tenors;
 *        one entry fewer than volatilities.
 * @return The drift vector mu, one entry per grid point.
 */
ORES_ANALYTICS_QUANT_EXPORT Eigen::VectorXd
hjm_no_arbitrage_drift(const Eigen::VectorXd& volatilities, const Eigen::VectorXd& tenor_spacings);

/**
 * @brief Single-factor Heath-Jarrow-Morton forward-curve process on a
 * discrete tenor grid.
 *
 * The state is the instantaneous forward curve f(t, t + tau_i) at the
 * fixed relative tenors tau_0 = 0 < tau_1 < ... < tau_N, advanced by
 * the single-factor HJM dynamics with a time-homogeneous per-tenor
 * volatility sigma(T_i):
 *
 *     f_i += mu_i * dt + sigma_i * sqrt(dt) * Z
 *
 * with mu = hjm_no_arbitrage_drift(sigma, spacings) the no-arbitrage
 * drift computed on the grid. Each tick advances every grid rate
 * simultaneously with one shared shock Z, then the curve re-anchors:
 * the tenors stay fixed relative to the new "now", so the front rate
 * f_0 is always the short rate.
 *
 * current() returns the front rate f_0, the short rate. The front rate
 * has zero drift and is therefore a martingale under the risk-neutral
 * measure -- a defining HJM property, exploited by the tests.
 *
 * discount_factor() prices a zero-coupon bond from the current curve:
 * exp(-integral_0^horizon f(t, t+s) ds). The curve is treated as
 * piecewise-linear between grid points (the trapezoid rule), and held
 * flat at the last tenor beyond the grid. This grid discretisation is
 * the model's approximation: the simulated forward curve is linear
 * between tenors, not the true instantaneous curve. The drift above is
 * the discrete analogue of the continuum no-arbitrage drift: with the
 * volatility piecewise constant on the tenor grid, the covariance sum
 * to each tenor is the trapezoid rule for the integrated volatility,
 * so the discretisation is first-order consistent with the continuum
 * model.
 *
 * Parameters are annualised; dt is the year-fraction per tick,
 * matching hull_white_process. Callers never pre-scale parameters for
 * finer granularity -- they pass the real dt instead.
 */
class ORES_ANALYTICS_QUANT_EXPORT heath_jarrow_morton_process final
    : public ores::analytics::quant::domain::IYieldCurveProcess {
public:
    heath_jarrow_morton_process(Eigen::VectorXd initial_forward_rates,
                                Eigen::VectorXd volatilities,
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
    explicit heath_jarrow_morton_process(const heath_jarrow_morton_params& params,
                                         std::uint32_t seed = 42,
                                         double dt = 1.0);

    double next() override;
    double current() const override;
    double discount_factor(std::size_t ticks_ahead) const override;

    /**
     * @brief The current forward curve at the fixed relative tenors.
     *
     * The interface contract exposes only the scalar short rate via
     * current(); the full curve is available through this accessor for
     * simulation statistics and cross-checks.
     */
    const Eigen::VectorXd& forward_rates() const;

private:
    double dt_;
    Eigen::VectorXd forward_rates_;
    Eigen::VectorXd drift_dt_;
    Eigen::VectorXd vol_sqrt_dt_;
    Eigen::VectorXd tenors_;
    std::size_t tick_ = 0;
    std::mt19937 rng_;
    std::normal_distribution<double> normal_;
};

} // namespace ores::analytics::quant::service

#endif
