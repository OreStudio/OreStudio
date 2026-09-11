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
#ifndef ORES_ANALYTICS_QUANT_SERVICE_FORWARD_RATE_CALCULATOR_HPP
#define ORES_ANALYTICS_QUANT_SERVICE_FORWARD_RATE_CALCULATOR_HPP

#include "ores.analytics.quant/export.hpp"
#include "ores.analytics.quant/service/curve_bootstrap_engine.hpp"
#include "ores.analytics.quant/service/day_count_calculator.hpp"
#include <chrono>
#include <vector>

namespace ores::analytics::quant::service {

/**
 * @brief One instantaneous forward rate spanning a consecutive pair of curve points.
 */
struct ORES_ANALYTICS_QUANT_EXPORT forward_rate_point {
    /// Echoed from the pair's own end point -- the pillar this forward rate leads into.
    std::string point_id;
    std::chrono::year_month_day start_date{};
    std::chrono::year_month_day end_date{};
    /// Continuously compounded, annualised: ln(df_start / df_end) / year_fraction(start, end).
    double instantaneous_forward_rate = 0.0;
};

/**
 * @brief Derives instantaneous forward rates from a bootstrapped curve's discount factors --
 * building-block math for a curve's own "health check" (a jagged or negative forward-rate series
 * is the standard visual tell for a flawed bootstrap composition), not tied to any one caller.
 *
 * Deliberately data-oriented: takes any span of already-known curve points (not just one
 * config's own pillar list -- there is nothing bootstrap-specific about deriving a forward rate
 * from two discount factors) and returns one forward rate per consecutive pair. Same
 * static/pure/vector-in-vector-out shape as
 * curve_bootstrap_engine::interpolate_discount_factor() -- deliberately not built on
 * std::adjacent_difference, whose "copy first element into the output" semantics don't fit an
 * N-points-in/N-1-out transform.
 */
class ORES_ANALYTICS_QUANT_EXPORT forward_rate_calculator {
public:
    /**
     * @brief Derives one forward rate per consecutive pair in @p points.
     *
     * @param points Curve points in strictly increasing date order (as
     * curve_bootstrap_engine::bootstrap() already returns them). Fewer than two points yields an
     * empty result -- not an error, since there is no pair to derive a rate from.
     * @param day_count_convention Applied uniformly to every pair, matching the convention the
     * curve was itself bootstrapped under.
     * @throws std::invalid_argument if any consecutive pair does not strictly increase in date.
     */
    static std::vector<forward_rate_point>
    calculate(const std::vector<bootstrapped_point>& points,
              day_count_convention_code day_count_convention);
};

}

#endif
