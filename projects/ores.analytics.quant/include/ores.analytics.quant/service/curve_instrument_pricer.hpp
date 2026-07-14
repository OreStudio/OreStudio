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
#ifndef ORES_ANALYTICS_QUANT_SERVICE_CURVE_INSTRUMENT_PRICER_HPP
#define ORES_ANALYTICS_QUANT_SERVICE_CURVE_INSTRUMENT_PRICER_HPP

#include "ores.analytics.quant/export.hpp"
#include <vector>

namespace ores::analytics::quant::service {

/**
 * @brief Derives a Curve Template entry's published rate from a short-rate
 * process's discount_factor()s, never from an independently-noised value.
 *
 * Every function here is a pure closed-form calculation over discount
 * factors and year fractions already evaluated from one
 * IYieldCurveProcess::discount_factor() at the tick-batch's single
 * current state -- this is what guarantees the tick batch a Curve
 * Template publishes is, by construction, a slice of one internally
 * consistent latent curve rather than an incoherent grid (see the IR
 * Rates synthetic data generation story's Analysis section). Mapping a
 * tenor label to a tick count/year fraction, and evaluating
 * discount_factor() at the resulting ticks, is the caller's
 * responsibility.
 */
class ORES_ANALYTICS_QUANT_EXPORT curve_instrument_pricer {
public:
    /**
     * @brief Simple deposit rate implied by a discount factor.
     *
     * (1 + r * year_fraction) = 1 / df, solved for r.
     *
     * @param discount_factor P(0, tau); must be > 0.
     * @param year_fraction Accrual fraction from now to tau; must be > 0.
     */
    static double deposit_rate(double discount_factor, double year_fraction);

    /**
     * @brief Forward rate agreement rate implied between two discount
     * factors.
     *
     * (1 + F * accrual_fraction) = P(0, start) / P(0, end), solved for F.
     *
     * @param discount_factor_start P(0, start); must be > 0.
     * @param discount_factor_end P(0, end); must be > 0.
     * @param accrual_fraction Accrual fraction from start to end; must be
     *        > 0.
     */
    static double fra_rate(double discount_factor_start,
                           double discount_factor_end,
                           double accrual_fraction);

    /**
     * @brief Par swap rate implied by a set of fixed-leg discount factors.
     *
     * S = (P(0, start) - P(0, end)) / sum_i(accrual_fraction_i * P(0, t_i))
     * -- the standard par-rate identity: floating leg PV telescopes to
     * P(start) - P(end) (unit notional), so S is the fixed rate making
     * the fixed leg's PV equal to that.
     *
     * @param discount_factor_start P(0, start) (the swap's effective
     *        date); must be > 0.
     * @param discount_factor_end P(0, end) (the swap's maturity date,
     *        the last fixed-leg discount factor's date); must be > 0.
     * @param fixed_leg_discount_factors P(0, t_i) for each fixed-leg
     *        payment date; must be non-empty, same size as
     *        accrual_fractions, and each entry > 0.
     * @param accrual_fractions Accrual fraction for each fixed-leg
     *        period; must be non-empty, same size as
     *        fixed_leg_discount_factors, and each entry > 0.
     */
    static double swap_par_rate(double discount_factor_start,
                                double discount_factor_end,
                                const std::vector<double>& fixed_leg_discount_factors,
                                const std::vector<double>& accrual_fractions);
};

}

#endif
