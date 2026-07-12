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
#ifndef ORES_ANALYTICS_QUANT_SERVICE_PROCESSES_CIR_PROCESS_HPP
#define ORES_ANALYTICS_QUANT_SERVICE_PROCESSES_CIR_PROCESS_HPP

#include "ores.analytics.quant/domain/i_yield_curve_process.hpp"
#include "ores.analytics.quant/export.hpp"
#include <cstdint>
#include <random>

namespace ores::analytics::quant::service {

/**
 * @brief Cox-Ingersoll-Ross (1985) mean-reverting, non-negative short-rate
 * process.
 *
 * dr = kappa * (theta - r) * dt + sigma * sqrt(r) * dW
 *
 * Unlike Vasicek/Hull-White (Gaussian, dr driven by dW alone), CIR's
 * sqrt(r) volatility term keeps r non-negative (strictly positive if the
 * Feller condition 2*kappa*theta >= sigma^2 holds; otherwise r can touch
 * zero but not go negative) and gives r_{t+1} | r_t a non-central
 * chi-squared distribution rather than a Gaussian one -- there is no
 * Euler-discretisation approximation here: next() draws the *exact*
 * transition via the standard Poisson-mixture-of-central-chi-squared
 * construction (Glasserman, "Monte Carlo Methods in Financial
 * Engineering", 2003, section 3.4):
 *
 *   c = sigma^2 * (1 - e^{-kappa}) / (4 * kappa)
 *   d = 4 * kappa * theta / sigma^2               (degrees of freedom)
 *   lambda = r_t * e^{-kappa} / c                  (non-centrality)
 *   N ~ Poisson(lambda / 2)
 *   r_{t+1} = c * X,  X ~ chi-squared(d + 2*N)
 *
 * kappa must be strictly positive (the sqrt(r) volatility term makes
 * kappa <= 0 ill-posed -- c above divides by kappa), unlike Vasicek/
 * Hull-White, which tolerate kappa <= 0 as a degenerate driftless case.
 * sigma == 0 is handled separately as the deterministic mean-reversion
 * ODE (dr = kappa*(theta-r)*dt), since the general formulas above and the
 * closed-form bond price both have a sigma-in-the-denominator singularity
 * at sigma == 0.
 *
 * discount_factor() uses the standard CIR closed-form affine bond price
 * (Cox, Ingersoll & Ross, 1985, "A Theory of the Term Structure of
 * Interest Rates", Econometrica 53(2)):
 *   gamma = sqrt(kappa^2 + 2*sigma^2)
 *   B(tau) = 2*(e^{gamma*tau} - 1) / ((gamma+kappa)*(e^{gamma*tau}-1) + 2*gamma)
 *   A(tau) = [2*gamma*e^{(kappa+gamma)*tau/2} /
 *             ((gamma+kappa)*(e^{gamma*tau}-1) + 2*gamma)] ^ (2*kappa*theta/sigma^2)
 *   P(tau) = A(tau) * e^{-B(tau)*r_t},  tau = ticks_ahead
 */
class ORES_ANALYTICS_QUANT_EXPORT cir_process final
    : public ores::analytics::quant::domain::IYieldCurveProcess {
public:
    cir_process(
        double kappa, double theta, double sigma, double initial_rate, std::uint32_t seed = 42);

    double next() override;
    double current() const override;
    double discount_factor(std::size_t ticks_ahead) const override;

private:
    double kappa_;
    double theta_;
    double sigma_;
    double rate_;
    std::mt19937 rng_;

    /// True Feller-consistent step (sigma > 0); sigma == 0 uses the
    /// deterministic ODE path instead via next().
    double next_stochastic();
};

}

#endif
