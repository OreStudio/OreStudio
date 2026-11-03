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
#include "ores.analytics.quant/math/fx_delta.hpp"
#include "ores.analytics.quant/math/normal.hpp"
#include "ores.analytics.quant/math/root.hpp"
#include <algorithm>
#include <cmath>
#include <limits>

namespace ores::analytics::quant::math {

namespace {

double sign_of(option_side side) noexcept {
    return side == option_side::call ? 1.0 : -1.0;
}

/// The discount that turns a forward hedge into a spot hedge: one for a forward delta.
double hedge_discount(fx_delta_type type, const fx_market& market) noexcept {
    return type == fx_delta_type::spot || type == fx_delta_type::premium_adjusted_spot ?
               market.foreign_discount :
               1.0;
}

double d1_of(double forward, double strike, double stdev) noexcept {
    return (std::log(forward / strike) + 0.5 * stdev * stdev) / stdev;
}

}

double black_forward_price(option_side side, double forward, double strike, double stdev) noexcept {
    const double phi = sign_of(side);
    if (!(stdev > 0.0))
        return std::max(phi * (forward - strike), 0.0);
    const double d1 = d1_of(forward, strike, stdev);
    const double d2 = d1 - stdev;
    return phi * (forward * normal_cdf(phi * d1) - strike * normal_cdf(phi * d2));
}

double fx_delta(fx_delta_type type,
                option_side side,
                const fx_market& market,
                double stdev,
                double strike) noexcept {
    if (!(stdev > 0.0))
        return std::numeric_limits<double>::quiet_NaN();
    const double phi = sign_of(side);
    const double forward = market.forward();
    const double d1 = d1_of(forward, strike, stdev);
    const double discount = hedge_discount(type, market);
    if (premium_adjusted(type))
        return phi * discount * (strike / forward) * normal_cdf(phi * (d1 - stdev));
    return phi * discount * normal_cdf(phi * d1);
}

std::expected<double, std::string> fx_strike_from_delta(
    fx_delta_type type, option_side side, const fx_market& market, double stdev, double delta) {
    const double phi = sign_of(side);
    if (!(stdev > 0.0) || !std::isfinite(stdev))
        return std::unexpected("the stdev must be positive and finite");
    if (!(phi * delta > 0.0))
        return std::unexpected("a call delta must be positive and a put delta negative");
    const double forward = market.forward();
    const double discount = hedge_discount(type, market);
    const double scaled = phi * delta / discount;

    if (!premium_adjusted(type)) {
        if (!(scaled > 0.0 && scaled < 1.0))
            return std::unexpected("the delta is beyond what any strike reaches");
        const double d1 = phi * normal_quantile(scaled);
        return forward * std::exp(-d1 * stdev + 0.5 * stdev * stdev);
    }

    // In x = log(K / F), d2 = -x / stdev - stdev / 2.
    const auto delta_at = [&](double x) {
        const double d2 = -x / stdev - 0.5 * stdev;
        return phi * discount * std::exp(x) * normal_cdf(phi * d2) - delta;
    };
    double lo = 0.0;
    if (side == option_side::call) {
        // The maximum of exp(x) N(d2) is where stdev N(d2) = n(d2).
        const auto peak = find_root(
            [&](double d2) { return stdev * normal_cdf(d2) - normal_pdf(d2); }, -10.0, 10.0);
        if (!peak)
            return std::unexpected("the strike of the largest delta was not found: " +
                                   peak.error());
        lo = -*peak * stdev - 0.5 * stdev * stdev;
        if (!(delta_at(lo) >= 0.0))
            return std::unexpected("the delta is above the largest a premium-adjusted call has");
    } else {
        lo = -40.0 * stdev;
    }
    double hi = lo + stdev;
    while (delta_at(hi) * delta_at(lo) > 0.0) {
        hi += 2.0 * (hi - lo);
        if (hi - lo > 80.0 * stdev + 80.0)
            return std::unexpected("the delta is beyond what any strike reaches");
    }
    const auto x = find_root(delta_at, lo, hi);
    if (!x)
        return std::unexpected(x.error());
    return forward * std::exp(*x);
}

double
fx_atm_strike(fx_atm_type atm, fx_delta_type type, const fx_market& market, double stdev) noexcept {
    const double forward = market.forward();
    if (atm == fx_atm_type::forward)
        return forward;
    const double half = 0.5 * stdev * stdev;
    return forward * std::exp(premium_adjusted(type) ? -half : half);
}

}
