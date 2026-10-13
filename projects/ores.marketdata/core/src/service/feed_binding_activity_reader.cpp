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
#include "ores.marketdata.core/service/feed_binding_activity_reader.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.marketdata.core/repository/feed_binding_repository.hpp"
#include "ores.marketdata.core/repository/market_observation_repository.hpp"
#include "ores.platform/time/datetime.hpp"
#include <boost/uuid/uuid_io.hpp>
#include <algorithm>
#include <string>
#include <string_view>
#include <vector>

namespace ores::marketdata::service {

using namespace ores::logging;

namespace {

inline std::string_view logger_name = "ores.marketdata.core.feed_binding_activity_reader";

[[nodiscard]] auto& lg() {
    static auto instance = make_logger(logger_name);
    return instance;
}

}

messaging::feed_binding_activity
feed_binding_activity_reader::activity_of(const std::string& source_name,
                                          const std::optional<domain::market_observation>& newest) {
    messaging::feed_binding_activity activity;
    activity.source_name = source_name;
    if (newest) {
        activity.series_id = boost::uuids::to_string(newest->series_id);
        activity.last_tick_at =
            ores::platform::time::datetime::to_iso8601_utc(newest->observation_datetime);
    }
    return activity;
}

messaging::list_feed_binding_activity_response
feed_binding_activity_reader::read(ores::database::context ctx) {
    messaging::list_feed_binding_activity_response response;

    auto bindings = repository::feed_binding_repository{}.read_latest(ctx);
    std::ranges::sort(bindings,
                      [](const auto& a, const auto& b) { return a.source_name < b.source_name; });

    repository::market_observation_repository observations;
    response.activities.reserve(bindings.size());
    for (const auto& binding : bindings) {
        response.activities.push_back(activity_of(
            binding.source_name, observations.read_newest_for_source(ctx, binding.source_name)));
    }
    BOOST_LOG_SEV(lg(), debug) << "Read the activity of " << response.activities.size()
                               << " feed bindings.";
    response.success = true;
    return response;
}

}
