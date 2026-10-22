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

messaging::feed_binding_activity feed_binding_activity_reader::activity_of(
    const std::string& source_name,
    const std::optional<repository::feed_binding_activity_record>& recorded) {
    messaging::feed_binding_activity activity;
    activity.source_name = source_name;
    if (recorded) {
        activity.series_id = recorded->series_id;
        activity.last_tick_at = recorded->last_tick_at;
    }
    return activity;
}

std::vector<domain::feed_binding>
feed_binding_activity_reader::bindings_of_party(std::vector<domain::feed_binding> bindings,
                                                const boost::uuids::uuid& party_id) {
    std::erase_if(bindings, [&](const auto& b) { return b.party_id != party_id; });
    return bindings;
}

messaging::list_feed_binding_activity_response
feed_binding_activity_reader::read(ores::database::context ctx) {
    messaging::list_feed_binding_activity_response response;

    // The bindings and the activity rows are isolated by tenant alone, so the caller's
    // party is stated here. A context that names no party has no bindings of its own.
    const auto party_id = ctx.party_id();
    if (!party_id) {
        response.success = false;
        response.message = "The request names no party, so it has no feed bindings.";
        return response;
    }

    // One read for each binding, which the primary key serves. A party holds tens of
    // bindings, so the round trips stay few; one read for all of them is the change if
    // that stops being true.
    auto bindings =
        bindings_of_party(repository::feed_binding_repository{}.read_latest(ctx), *party_id);
    std::ranges::sort(bindings,
                      [](const auto& a, const auto& b) { return a.source_name < b.source_name; });

    repository::feed_binding_activity_repository recorded;
    response.activities.reserve(bindings.size());
    for (const auto& binding : bindings) {
        response.activities.push_back(
            activity_of(binding.source_name, recorded.read(ctx, *party_id, binding.source_name)));
    }
    BOOST_LOG_SEV(lg(), debug) << "Read the activity of " << response.activities.size()
                               << " feed bindings.";
    response.success = true;
    return response;
}

}
