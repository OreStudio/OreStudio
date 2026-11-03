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
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51
 * Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */
#ifndef ORES_ANALYTICS_QUANT_MATH_FX_DELTA_HPP
#define ORES_ANALYTICS_QUANT_MATH_FX_DELTA_HPP

#include "ores.analytics.quant/export.hpp"
#include <expected>
#include <string>

namespace ores::analytics::quant::math {

enum class option_side { call, put };

/**
 * @brief How an FX option's delta is stated.
 *
 * Spot delta is the hedge in spot, discounted at the foreign rate; forward delta
 * the hedge in the forward. The premium-adjusted forms subtract the premium, paid
 * in the foreign currency, from the hedge, so they use d2 and the strike over the
 * forward (Clark, Foreign Exchange Option Pricing, 2011, section 3.5).
 */
enum class fx_delta_type { spot, forward, premium_adjusted_spot, premium_adjusted_forward };

/// Where an FX option quote puts its at-the-money strike.
enum class fx_atm_type {
    /// The strike at which a call and a put have deltas that sum to zero.
    delta_neutral,
    /// The forward.
    forward
};

/// The spot and the two discount factors to an expiry, which fix its forward.
struct fx_market final {
    double spot;
    double domestic_discount;
    double foreign_discount;

    [[nodiscard]] double forward() const noexcept {
        return spot * foreign_discount / domestic_discount;
    }
};

/// The undiscounted Black price of a call or put on the forward, at a total stdev.
ORES_ANALYTICS_QUANT_EXPORT double
black_forward_price(option_side side, double forward, double strike, double stdev) noexcept;

/// The delta of an option at a strike and a positive total stdev, in the stated form; NaN at a
/// stdev that is not positive, where a delta jumps.
ORES_ANALYTICS_QUANT_EXPORT double fx_delta(fx_delta_type type,
                                            option_side side,
                                            const fx_market& market,
                                            double stdev,
                                            double strike) noexcept;

/**
 * @brief The strike whose delta at a total stdev is the given delta.
 *
 * Closed form for the two unadjusted forms. A premium-adjusted put is solved on a
 * delta that is monotone in the strike. A premium-adjusted call's delta rises and
 * then falls, so a delta below its maximum has two strikes; the market's is the
 * one above the maximum, and that is the one returned. Refuses a delta of the
 * wrong sign, or one no strike reaches.
 */
ORES_ANALYTICS_QUANT_EXPORT std::expected<double, std::string> fx_strike_from_delta(
    fx_delta_type type, option_side side, const fx_market& market, double stdev, double delta);

/**
 * @brief The at-the-money strike at a total stdev.
 *
 * The forward, or the delta-neutral strike, F exp(stdev^2 / 2) for an unadjusted
 * delta and F exp(-stdev^2 / 2) for a premium-adjusted one.
 */
ORES_ANALYTICS_QUANT_EXPORT double
fx_atm_strike(fx_atm_type atm, fx_delta_type type, const fx_market& market, double stdev) noexcept;

/// Whether the delta form subtracts the premium.
[[nodiscard]] constexpr bool premium_adjusted(fx_delta_type type) noexcept {
    return type == fx_delta_type::premium_adjusted_spot ||
           type == fx_delta_type::premium_adjusted_forward;
}

}

#endif
