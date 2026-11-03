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
#include "ores.marketdata.core/service/observation_reader.hpp"
#include "ores.database/domain/outcome_code.hpp"
#include "ores.marketdata.core/repository/observation_read_repository.hpp"
#include "ores.marketdata.core/service/series_resolution.hpp"
#include <chrono>
#include <string>
#include <variant>

namespace ores::marketdata::service {

using ores::database::domain::outcome_code;
using ores::database::domain::refuse;
using ores::utility::domain::result;

messaging::get_observation_window_response
observation_reader::window(ores::database::context ctx,
                           const messaging::get_observation_window_request& request) {
    messaging::get_observation_window_response response;
    response.days = request.days;
    if (!request.to.ok()) {
        response.result =
            refuse(outcome_code::filter_range_invalid, {.field = "to", .value = "date"});
        return response;
    }
    if (request.days < 1) {
        response.result = refuse(outcome_code::range_inverted, {.entity = "observations"});
        return response;
    }
    if (request.days > max_days) {
        response.result =
            refuse(outcome_code::range_too_large,
                   {.entity = "observations", .limit = std::to_string(max_days) + " days"});
        return response;
    }
    const auto resolved = resolve_series(
        ctx, request.item, request.provider_code, request.party_id, "observations", "series");
    if (const auto* refused = std::get_if<result>(&resolved)) {
        response.result = *refused;
        return response;
    }
    const auto& series = std::get<repository::fixing_series>(resolved);
    response.provider_code = series.provider_code;

    const auto from = std::chrono::year_month_day(std::chrono::sys_days(request.to) -
                                                  std::chrono::days(request.days - 1));
    // One more than the ceiling is read, so a window that holds more is told apart from
    // one that holds exactly the ceiling.
    const auto found = repository::observation_read_repository{}.between(
        ctx, series.id, from, request.to, max_observations + 1);
    if (found.size() > max_observations) {
        response.result = refuse(outcome_code::range_too_large,
                                 {.entity = "observations",
                                  .limit = std::to_string(max_observations) + " observations"});
        return response;
    }
    for (const auto& o : found) {
        response.observed_at.push_back(o.observed_at);
        response.coordinates.push_back(o.coordinate);
        response.values.push_back(o.value);
        response.sources.push_back(o.source);
    }
    response.used = static_cast<int>(found.size());
    return response;
}

}
