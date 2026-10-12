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
#ifndef ORES_SYNTHETIC_API_FEEDS_FX_SIMPLE_CONVERSION_HPP
#define ORES_SYNTHETIC_API_FEEDS_FX_SIMPLE_CONVERSION_HPP

#include "ores.synthetic.api/export.hpp"
#include "ores.synthetic.api/feeds/fx_mixture_conversion.hpp"
#include <string>

namespace ores::synthetic::feed {

/**
 * @brief The questions the simple view of an FX feed asks.
 *
 * Every number is per hour, so it does not depend on how many updates the feed
 * makes in an hour: the trend and the volatility are a percent of the price over
 * one hour, and the jump share is the percent of updates that come from the
 * jump regime. The half-life, in minutes, is read only by the
 * Ornstein-Uhlenbeck engine; the trend and the jump share are read only by the
 * others.
 */
struct fx_simple_view {
    double trend_pct_per_hour = 0.0;
    double volatility_pct_per_hour = 0.0;
    double jump_share_pct = 0.0;
    double half_life_minutes = 0.0;
};

/**
 * @brief What the simple view makes of a mixture.
 *
 * @p view is always the closest simple view. @p exact is true when the simple
 * view stands for the mixture without loss: one primary component and at most
 * one jump component of the shape the simple view writes, or, for the
 * Ornstein-Uhlenbeck engine, one component with no drift and a reversion speed
 * above zero. Otherwise @p reason says why, and writing the view back replaces
 * the mixture with an approximation.
 */
struct fx_simple_reading {
    fx_simple_view view;
    bool exact = false;
    std::string reason;
};

/**
 * @brief The mixture a simple view stands for, read by the given engine.
 *
 * The geometric engine reads the trend and the volatility as log-returns per
 * update; the arithmetic engine reads them in price units, so they scale by
 * @p initial_price. A trend per hour is spread over the updates of the hour,
 * and a volatility per hour over their square root. A jump share above zero
 * adds a second component with no drift and six times the volatility. The
 * Ornstein-Uhlenbeck engine gets one component with κ from the half-life.
 *
 * Throws std::invalid_argument for an unknown engine, a price or a rate that is
 * not positive, a negative volatility, a jump share outside 0 to 100, or, for
 * Ornstein-Uhlenbeck, a half-life that is not positive.
 */
ORES_SYNTHETIC_API_EXPORT fx_mixture fx_mixture_from_simple(const fx_simple_view& view,
                                                            const std::string& process_type,
                                                            double initial_price,
                                                            int ticks_per_hour);

/**
 * @brief The simple view of a mixture, and whether it is exact.
 *
 * The inverse of fx_mixture_from_simple. The primary component is the one with
 * the most weight; components with four times its deviation or more are the jump
 * regime. Throws std::invalid_argument for an unknown engine, vectors of
 * different sizes, or a price or rate that is not positive.
 */
ORES_SYNTHETIC_API_EXPORT fx_simple_reading fx_simple_from_mixture(const fx_mixture& mixture,
                                                                   const std::string& process_type,
                                                                   double initial_price,
                                                                   int ticks_per_hour);

}

#endif
