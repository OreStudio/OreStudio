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
#ifndef ORES_MARKETDATA_SERVICE_PUBLISHED_CURVE_HPP
#define ORES_MARKETDATA_SERVICE_PUBLISHED_CURVE_HPP

#include "ores.analytics.quant/service/curve_bootstrap_engine.hpp"
#include <chrono>
#include <vector>

namespace ores::marketdata::service {

/**
 * @brief Adds the curve's reference date at a discount factor of one, which a
 * stored curve leaves out.
 *
 * Every builder that reads a published set back faces the same gap: the set holds
 * the points its pillars produced and none at the reference date its own value
 * date anchors. A reader that needs the span from the reference date -- a forward
 * ladder from spot, a projection whose first pillar is shorter than the funding
 * curve's first one -- puts the anchor back with this, once.
 */
inline std::vector<analytics::quant::service::bootstrapped_point>
anchor_curve(std::vector<analytics::quant::service::bootstrapped_point> points,
             std::chrono::year_month_day reference_date) {
    if (points.empty() ||
        std::chrono::sys_days(points.front().date) > std::chrono::sys_days(reference_date))
        points.insert(points.begin(), {"", reference_date, 1.0});
    return points;
}

}

#endif
