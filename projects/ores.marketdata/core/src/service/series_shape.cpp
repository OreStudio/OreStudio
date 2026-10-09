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
#include "ores.marketdata.core/service/series_shape.hpp"
#include "ores.marketdata.core/repository/series_axis_repository.hpp"
#include "ores.marketdata.core/repository/series_axis_value_repository.hpp"
#include <algorithm>
#include <stdexcept>
#include <string>
#include <utility>
#include <vector>

namespace ores::marketdata::service {

series_shape read_series_shape(ores::database::context ctx, const std::string& series_id) {
    using namespace ores::marketdata;
    const std::vector<std::string> ids{series_id};

    std::vector<std::pair<int, datum::field>> ordered_axes;
    for (const auto& a : repository::series_axis_repository{}.read_latest_for_series(ctx, ids)) {
        const auto named = datum::field_named(a.axis_field);
        if (!named)
            throw std::runtime_error("the shape declares '" + a.axis_field +
                                     "', which is not an oresmd field name");
        ordered_axes.emplace_back(a.sequence, *named);
    }
    std::ranges::sort(ordered_axes);

    std::map<datum::field, std::vector<std::pair<int, std::string>>> ordered_values;
    for (const auto& v :
         repository::series_axis_value_repository{}.read_latest_for_series(ctx, ids)) {
        const auto named = datum::field_named(v.axis_field);
        if (!named)
            throw std::runtime_error("the shape declares a value of '" + v.axis_field +
                                     "', which is not an oresmd field name");
        ordered_values[*named].emplace_back(v.sequence, v.value);
    }

    series_shape result;
    result.axes.reserve(ordered_axes.size());
    for (auto& [sequence, axis] : ordered_axes) {
        result.axes.push_back(axis);
        auto& values = ordered_values[axis];
        std::ranges::sort(values);
        auto& declared = result.values[axis];
        declared.reserve(values.size());
        for (auto& [position, value] : values)
            declared.push_back(std::move(value));
    }
    return result;
}

}
