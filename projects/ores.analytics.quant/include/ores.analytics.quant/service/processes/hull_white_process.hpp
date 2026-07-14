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
#ifndef ORES_ANALYTICS_QUANT_SERVICE_PROCESSES_HULL_WHITE_PROCESS_HPP
#define ORES_ANALYTICS_QUANT_SERVICE_PROCESSES_HULL_WHITE_PROCESS_HPP

#include "ores.analytics.quant/domain/i_yield_curve_process.hpp"
#include "ores.analytics.quant/export.hpp"
#include <cstdint>
#include <random>
#include <vector>

namespace ores::analytics::quant::service {

/**
 * @brief One-factor Hull-White short-rate process with a piecewise-constant,
 * time-varying mean-reversion level.
 *
 * dr = kappa * (theta(t) - r) * dt + sigma * dW
 *
 * Written in "target level" form -- kappa*(theta(t)-r), the same shape as
 * ou_process -- rather than Hull & White's original "drift intercept" form
 * (dr = [phi(t) - a*r]dt + sigma*dW, phi(t) = kappa*theta(t)); the two are
 * equivalent up to relabelling theta(t). This form is chosen deliberately:
 * it makes the degenerate kappa <= 0 case and the Vasicek special case
 * (theta(t) held constant) reduce to *exactly* ou_process's formula, not an
 * approximation of it.
 *
 * theta(t) is supplied as theta_path, a piecewise-constant function of
 * tick: theta at tick i is theta_path[i], or theta_path.back() for any
 * i >= theta_path.size() (the path is held flat once it runs out). A
 * single-element path makes this exactly the constant-theta (Vasicek)
 * case -- see vasicek_process, which composes this class that way rather
 * than duplicating its math.
 *
 * Fitting theta(t) to reproduce an observed market curve exactly (Hull &
 * White's original motivation) is out of scope here: that requires curve
 * bootstrapping, which this story deliberately does not build. theta_path
 * is a caller-supplied input; deriving a realistic one from real market
 * data is future work.
 *
 * next() advances the short rate one tick using the *exact* one-step
 * Gaussian transition (not an Euler approximation) for that tick's local
 * theta:
 *   r_{i+1} = theta_i + (r_i - theta_i) * e^{-kappa} +
 *             sigma * sqrt((1 - e^{-2*kappa}) / (2*kappa)) * Z,  Z ~ N(0, 1)
 *
 * discount_factor() prices a zero-coupon bond off the *same* state and the
 * *same* per-tick transition law, via the standard backward recursion for
 * a discrete-time Gaussian affine short-rate model (Brigo & Mercurio,
 * "Interest Rate Models -- Theory and Practice", ch. 3): assuming
 * P_i = exp(A_i - B_i * r_i), matching moments of
 * P_i = exp(-r_i) * E_i[P_{i+1}] gives, per tick (kappa > 0 case):
 *   B_i = 1 + B_{i+1} * e^{-kappa}
 *   A_i = A_{i+1} - B_{i+1} * theta_i * (1 - e^{-kappa}) +
 *         0.5 * B_{i+1}^2 * sigma^2 * (1 - e^{-2*kappa}) / (2*kappa)
 * with B_N = A_N = 0 at the bond's own maturity (tick N). This recursion
 * is exact and, by construction, consistent with next()'s simulation --
 * not a separate continuous-time approximation of it.
 */
class ORES_ANALYTICS_QUANT_EXPORT hull_white_process final
    : public ores::analytics::quant::domain::IYieldCurveProcess {
public:
    hull_white_process(double kappa,
                       std::vector<double> theta_path,
                       double sigma,
                       double initial_rate,
                       std::uint32_t seed = 42);

    double next() override;
    double current() const override;
    double discount_factor(std::size_t ticks_ahead) const override;

private:
    double theta_at(std::size_t tick) const;

    double kappa_;
    std::vector<double> theta_path_;
    double sigma_;
    double rate_;
    std::size_t tick_ = 0;
    std::mt19937 rng_;
    std::normal_distribution<double> normal_;
};

}

#endif
