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
#ifndef ORES_ANALYTICS_QUANT_DOMAIN_I_YIELD_CURVE_PROCESS_HPP
#define ORES_ANALYTICS_QUANT_DOMAIN_I_YIELD_CURVE_PROCESS_HPP

#include "ores.analytics.quant/domain/i_stochastic_process.hpp"
#include <cstddef>

namespace ores::analytics::quant::domain {

/**
 * @brief A short-rate process that also exposes the model's implied
 * zero-coupon bond price -- the "latent curve" a single process state
 * implies at every maturity, not just the next scalar tick.
 *
 * IStochasticProcess::next()/current() still drive the short rate itself
 * (r_t) tick by tick. discount_factor() answers, from the *current* state,
 * what P(t, t+ticks_ahead) the model implies -- so every instrument a
 * caller derives from one process (deposit, FRA, swap, at whatever tenor)
 * comes from evaluating the same formula at the same r_t, guaranteeing the
 * whole set is a slice of one internally consistent curve rather than
 * independently-noised quotes.
 */
class IYieldCurveProcess : public IStochasticProcess {
public:
    /**
     * @brief The model-implied zero-coupon bond price from the current
     * state to ticks_ahead ticks in the future.
     *
     * @param ticks_ahead Number of ticks from now to the bond's maturity;
     *        0 returns exactly 1.0 (a bond maturing now is worth its
     *        notional). Ticks are this codebase's unit of simulation
     *        time throughout (see ou_process, gmm_process) -- mapping a
     *        calendar tenor (e.g. "3M") to a tick count is the caller's
     *        responsibility.
     */
    virtual double discount_factor(std::size_t ticks_ahead) const = 0;
};

} // namespace ores::analytics::quant::domain

#endif
