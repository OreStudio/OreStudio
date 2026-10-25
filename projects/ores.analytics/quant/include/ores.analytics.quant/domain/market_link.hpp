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
#ifndef ORES_ANALYTICS_QUANT_DOMAIN_MARKET_LINK_HPP
#define ORES_ANALYTICS_QUANT_DOMAIN_MARKET_LINK_HPP

#include "ores.analytics.quant/domain/market_regime.hpp"
#include "ores.analytics.quant/domain/shared_intensity.hpp"

namespace ores::analytics::quant::domain {

/**
 * @brief One series' tie to the market: what it follows, and how hard.
 *
 * A series' intensity is
 *
 *   lambda = sensitivity * (mean_rate * S(t) + C(t))
 *
 * which is a rate, while a process is built with a mean rate and re-rated by a
 * multiplier. Rewriting the same expression as
 *
 *   lambda = mean_rate * sensitivity * (S(t) + C(t) / mean_rate)
 *
 * gives the multiplier a process takes, so the four numbers travel together and
 * a call site does not grow a fifth argument. Passive: it holds no state and
 * reads the two published values, each one atomic load.
 */
struct market_link final {
    /// The state every series shares, or null for a run with no market.
    const market_regime* market = nullptr;
    /// The heat every event feeds, or null for a run with no endogenous factor.
    shared_intensity* intensity = nullptr;
    /// How strongly this series follows the market, one being the average.
    double sensitivity = 1.0;
    /// The mean rate the row configures, in arrivals per second.
    double mean_rate = 0.0;

    /**
     * @brief The multiplier to rate this series' process at, now.
     *
     * With neither factor, it is one and the process keeps the rate it was built
     * with.
     */
    [[nodiscard]] double rate_multiplier() const {
        double factor = 0.0;
        if (market != nullptr)
            factor += market->multiplier();
        if (intensity != nullptr && mean_rate > 0.0)
            factor += intensity->value() / mean_rate;
        if (market == nullptr && intensity == nullptr)
            return 1.0;
        return sensitivity * factor;
    }
};

/**
 * @brief The same link, with its mean rate taken from the arrival spec.
 *
 * The spec is the row's own statement of its rate, so a feed reads it from there
 * rather than from a second caller who could disagree.
 */
[[nodiscard]] inline market_link with_mean_rate(market_link link, double ticks_per_hour) {
    link.mean_rate = ticks_per_hour / 3600.0;
    return link;
}

}

#endif
