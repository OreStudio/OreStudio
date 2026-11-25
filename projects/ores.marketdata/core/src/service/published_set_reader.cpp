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
#include "ores.marketdata.core/service/published_set_reader.hpp"
#include "ores.database/domain/outcome_code.hpp"
#include "ores.marketdata.core/repository/published_set_read_repository.hpp"
#include <string>

namespace ores::marketdata::service {

using ores::database::domain::outcome_code;
using ores::database::domain::refuse;

messaging::get_latest_published_sets_response
published_set_reader::latest(const ores::database::context& ctx,
                             const messaging::get_latest_published_sets_request& request) {
    messaging::get_latest_published_sets_response response;
    if (request.series_ids.size() > max_batch) {
        response.result = refuse(outcome_code::filter_too_large,
                                 {.field = "series_ids", .limit = std::to_string(max_batch)});
        return response;
    }
    const auto found =
        repository::published_set_read_repository{}.latest_publishes(ctx, request.series_ids);
    response.series_ids.reserve(found.size());
    response.as_ofs.reserve(found.size());
    response.state_codes.reserve(found.size());
    for (const auto& one : found) {
        response.series_ids.push_back(one.series_id);
        response.as_ofs.push_back(one.as_of);
        response.state_codes.push_back(one.state_code);
    }
    response.result.outcome = ores::utility::domain::outcome::ok;
    return response;
}

}
