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
#ifndef ORES_ANALYTICS_QUANT_SERVICE_PROCESSES_BLACK_KARASINSKI_PROCESS_HPP
#define ORES_ANALYTICS_QUANT_SERVICE_PROCESSES_BLACK_KARASINSKI_PROCESS_HPP

#include "ores.analytics.quant/domain/i_yield_curve_process.hpp"
#include "ores.analytics.quant/export.hpp"
#include "ores.analytics.quant/math/rate_tree.hpp"
#include "ores.analytics.quant/service/processes/black_karasinski_params.hpp"
#include <cstddef>
#include <cstdint>
#include <random>

namespace ores::analytics::quant::service {

/**
 * @brief Build the centered adaptive trinomial lattice of ln r for the
 * Black-Karasinski model.
 *
 * The lattice approximates the Gaussian OU process of ln r (the model's
 * per-tick transition law, see black_karasinski_process) with a
 * recombining trinomial tree. Level i has 2i+1 nodes; node (i, j), j in
 * [0, 2i], carries the log rate
 *
 *     m_i + (j - i) * b
 *
 * where the level center m_i = theta + (log_rate - theta) * e^{-kappa*i*dt}
 * follows the deterministic mean-reversion path of the process (so each
 * level is centered on where the process actually is, not on a fixed
 * origin), and the spacing b = sqrt(3v) is fixed across levels, with v the
 * exact per-tick variance of the OU transition
 *
 *     v = sigma^2 * (1 - e^{-2*kappa*dt}) / (2*kappa)      (kappa > 0)
 *     v = sigma^2 * dt                                     (kappa <= 0)
 *
 * Every node branches to the three nodes of the next level nearest its
 * conditional mean: with offset o = j - i and decay f = e^{-kappa*dt}
 * (f = 1 for kappa <= 0, mirroring the process's driftless branch), the
 * conditional mean offset is o*f, the middle branch sits at
 * temp = round(o*f), and the residual e = o*f - temp lies in [-1/2, 1/2].
 * The Hull-White moment-matching probabilities
 *
 *     p_down = (1 + 3e^2 - 3e) / 6
 *     p_mid  = (2 - 3e^2) / 3
 *     p_up   = (1 + 3e^2 + 3e) / 6
 *
 * (in grid units -- the same formulas as QuantLib's TrinomialTree,
 * p1 = (1 + e^2/v - sqrt(3)*e/sqrt(v)) / 6 in rate units) reproduce the
 * conditional mean o*f*b and the conditional variance v exactly at every
 * node, so the tree's one-tick transition law is exact in mean and
 * variance. Unlike a drift-matched binomial, the construction is valid
 * for any horizon: mean reversion always pulls the branch targets back
 * inside the symmetric grid -- the middle branch offset temp never
 * exceeds the level's own range, so no clamping or range tracking (the
 * jMin/jMax bookkeeping of QuantLib's TrinomialTree) is ever needed.
 * The last level is terminal: it stores no branches, exactly the
 * contract of the generic rate_tree.
 *
 * The tree converges weakly to the continuous-time Black-Karasinski
 * model as dt -> 0; at a fixed dt it prices exactly the same discrete
 * per-tick transition law that the process's next() simulates.
 *
 * @param log_rate Current ln r; the root of the lattice.
 * @param kappa    Mean-reversion speed of ln r (any value; kappa <= 0
 *                 selects the driftless branch, as in the process).
 * @param theta    Long-run level of ln r.
 * @param sigma    Volatility of ln r; must be non-negative.
 * @param dt       Year fraction per tick; must be strictly positive.
 * @param steps    Number of ticks the lattice covers; the tree stores
 *                 steps + 1 levels (0..steps).
 * @return The lattice, ready for the rate_tree walk and state-price
 *         propagation.
 */
ORES_ANALYTICS_QUANT_EXPORT ores::analytics::quant::math::rate_tree build_black_karasinski_tree(
    double log_rate, double kappa, double theta, double sigma, double dt, std::size_t steps);

/**
 * @brief One-factor Black-Karasinski short-rate process: the logarithm
 * of the short rate is an Ornstein-Uhlenbeck process.
 *
 *     d ln r = kappa * (theta - ln r) * dt + sigma * dW
 *
 * Writing the model on the logarithm guarantees r > 0 for all time,
 * unlike a Gaussian model on the rate itself; the price paid is that the
 * model is log-normal, so the bond price has no closed form and
 * discount_factor() below evaluates it on a lattice instead of
 * analytically (this is also how QuantLib's BlackKarasinski class works,
 * through TrinomialTree).
 *
 * next() advances ln r one tick with the *exact* one-step Gaussian
 * transition, the same discretisation as ornstein_uhlenbeck_process:
 *
 *     ln r_{i+1} = theta + (ln r_i - theta) * e^{-kappa*dt} +
 *                  sigma * sqrt((1 - e^{-2*kappa*dt}) / (2*kappa)) * Z,  Z ~ N(0, 1)
 *
 * and returns r = exp(ln r). kappa <= 0 (or too small to divide by
 * safely) takes the same driftless branch as ornstein_uhlenbeck_process:
 * ln r_{i+1} = ln r_i + sigma * sqrt(dt) * Z.
 *
 * discount_factor() prices a zero-coupon bond by constructing, per call,
 * the centered adaptive trinomial lattice of ln r described in
 * build_black_karasinski_tree and propagating state prices forward
 * through it with the rate_tree utility. The lattice is the model's own
 * price: it uses exactly the same per-tick transition moments as next(),
 * so both always price the same discrete-time model, and it converges to
 * the continuous-time price as dt -> 0. sigma == 0 short-circuits to the
 * deterministic closed form exp(-sum_i exp(m_i) * dt) along the exact
 * mean-reversion path, an O(ticks_ahead) result instead of a degenerate
 * zero-spacing lattice.
 *
 * dt is the year-fraction one tick represents (default 1.0 -- one tick
 * per year, i.e. the class's unscaled behaviour); kappa/theta/sigma stay
 * in their natural annualised units always -- callers never pre-scale
 * them for a finer tick granularity, they pass the real dt instead.
 */
class ORES_ANALYTICS_QUANT_EXPORT black_karasinski_process final
    : public ores::analytics::quant::domain::IYieldCurveProcess {
public:
    black_karasinski_process(double kappa,
                             double theta,
                             double sigma,
                             double initial_rate,
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
    explicit black_karasinski_process(const black_karasinski_params& params,
                                      std::uint32_t seed = 42,
                                      double dt = 1.0);

    double next() override;
    double current() const override;
    double discount_factor(std::size_t ticks_ahead) const override;

private:
    double kappa_;
    double theta_;
    double sigma_;
    double log_rate_;
    double dt_;
    std::size_t tick_ = 0;
    std::mt19937 rng_;
    std::normal_distribution<double> normal_;
};

} // namespace ores::analytics::quant::service

#endif
