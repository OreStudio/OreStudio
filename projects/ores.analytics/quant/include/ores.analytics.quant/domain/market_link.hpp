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
 *   lambda = sensitivity * mean_rate * (S(t) + C(t) / R(t))
 *
 * where R is the summed mean rate of the series enrolled with the heat (see
 * shared_intensity), so each series takes the share of C its rate is of R. The
 * bracket is the multiplier a process is re-rated by. Passive: it holds no state
 * and reads the published values, each one atomic load.
 */
struct market_link final {
    /// The state every series shares, or null for a run with no market.
    const market_regime* market = nullptr;
    /// The heat every event feeds, or null for a run with no endogenous factor.
    shared_intensity* intensity = nullptr;
    /// How strongly this series follows the market, one being the average.
    double sensitivity = 1.0;

    /**
     * @brief The multiplier to rate this series' process at, now.
     *
     * With neither factor, it is one and the process keeps the rate it was built
     * with.
     */
    [[nodiscard]] double rate_multiplier() const {
        // The market factor is one when there is no market, and the heat adds to
        // it: a run with heat but no market must not lose its baseline.
        double factor = market != nullptr ? market->multiplier() : 1.0;
        if (intensity != nullptr)
            factor += intensity->rate_share();
        return sensitivity * factor;
    }
};

/**
 * @brief Enrols a series that runs at @p ticks_per_hour with the heat @p link
 * feeds, or returns an empty enrolment when the run has no heat.
 */
[[nodiscard]] inline shared_intensity::enrolment enrol_hourly(const market_link& link,
                                                              double ticks_per_hour) {
    if (link.intensity == nullptr)
        return {};
    return link.intensity->enrol(ticks_per_hour / 3600.0);
}
}

#endif
