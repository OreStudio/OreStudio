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
#include "ores.marketdata.core/service/series_tick_reader.hpp"
#include "ores.database/domain/outcome_code.hpp"
#include "ores.marketdata.core/repository/tick_repository.hpp"
#include <map>
#include <string>

namespace ores::marketdata::service {

using ores::database::domain::outcome_code;
using ores::database::domain::refuse;

messaging::get_series_ticks_response
series_tick_reader::read(ores::database::context ctx,
                         const messaging::get_series_ticks_request& request,
                         std::chrono::system_clock::time_point now) {
    messaging::get_series_ticks_response response;

    // The party policy of the tick table shows a session with no party no tick, so an empty
    // reply would look like a series with no ticks.
    if (ctx.visible_party_ids().empty()) {
        response.result = refuse(outcome_code::party_required, {.entity = "ticks"});
        return response;
    }

    const auto to = request.to_instant.value_or(now);
    if (to < request.from_instant) {
        response.result = refuse(outcome_code::range_inverted, {.entity = "ticks"});
        return response;
    }

    // One tick more than the ceiling tells a span over it from a span at it.
    const auto ticks = repository::tick_repository{}.read_range(
        ctx, request.series_id, request.from_instant, to, request.coordinates, max_ticks + 1);
    if (ticks.size() > max_ticks) {
        response.result = refuse(outcome_code::range_too_large,
                                 {.entity = "ticks", .limit = std::to_string(max_ticks)});
        return response;
    }

    std::map<std::string, int> index_of;
    response.gaps_us.reserve(ticks.size());
    response.coordinate_indexes.reserve(ticks.size());
    response.values.reserve(ticks.size());
    auto previous = ticks.empty() ? request.from_instant : ticks.front().at;
    if (!ticks.empty())
        response.first_instant = previous;
    for (const auto& t : ticks) {
        const auto found = index_of.try_emplace(t.oresmd_uri, static_cast<int>(index_of.size()));
        if (found.second)
            response.coordinates.push_back(t.oresmd_uri);
        response.gaps_us.push_back(
            std::chrono::duration_cast<std::chrono::microseconds>(t.at - previous).count());
        response.coordinate_indexes.push_back(found.first->second);
        response.values.push_back(t.value);
        previous = t.at;
    }
    return response;
}

}
