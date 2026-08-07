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
#include "ores.analytics.quant/service/forward_rate_calculator.hpp"
#include <cmath>
#include <stdexcept>

namespace ores::analytics::quant::service {

std::vector<forward_rate_point>
forward_rate_calculator::calculate(const std::vector<bootstrapped_point>& points,
                                   day_count_convention_code day_count_convention) {
    std::vector<forward_rate_point> out;
    if (points.size() < 2)
        return out;

    out.reserve(points.size() - 1);
    for (std::size_t i = 1; i < points.size(); ++i) {
        const auto& start = points[i - 1];
        const auto& end = points[i];
        if (std::chrono::sys_days(end.date) <= std::chrono::sys_days(start.date))
            throw std::invalid_argument(
                "forward_rate_calculator: points must strictly increase in date -- '" +
                end.point_id + "' does not mature after '" + start.point_id + "'");

        const double yf =
            day_count_calculator::year_fraction(start.date, end.date, day_count_convention);

        forward_rate_point fp;
        fp.point_id = end.point_id;
        fp.start_date = start.date;
        fp.end_date = end.date;
        fp.instantaneous_forward_rate = std::log(start.discount_factor / end.discount_factor) / yf;
        out.push_back(fp);
    }

    return out;
}

}
