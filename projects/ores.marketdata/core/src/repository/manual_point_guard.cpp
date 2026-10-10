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
#include "ores.marketdata.core/repository/manual_point_guard.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.marketdata.core/repository/observation_lineage_repository.hpp"
#include <boost/log/sources/severity_feature.hpp>
#include <boost/uuid/uuid.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <chrono>
#include <map>
#include <set>
#include <string>
#include <utility>
#include <vector>

namespace ores::marketdata::repository {

using namespace ores::logging;

namespace {

auto& guard_lg() {
    static auto instance = make_logger("ores.marketdata.repository.manual_point_guard");
    return instance;
}

using owned_point = std::pair<std::string, std::chrono::system_clock::time_point>;

}

std::vector<domain::market_observation>
manual_point_guard::unshadowed(ores::database::context ctx,
                               const std::vector<domain::market_observation>& observations) {
    if (observations.empty())
        return observations;

    // The manual annex is cold and small, and a series nobody over-keyed has no
    // row in it, so the guard costs one narrow read per series of the batch.
    std::map<boost::uuids::uuid, std::set<owned_point>> owned;
    for (const auto& obs : observations) {
        if (owned.contains(obs.series_id))
            continue;
        auto& points = owned[obs.series_id];
        for (const auto& annex :
             observation_lineage_repository{}.read_manual_for_series(ctx, obs.series_id))
            points.emplace(annex.oresmd_uri, annex.observation_datetime);
    }

    std::vector<domain::market_observation> kept;
    kept.reserve(observations.size());
    for (const auto& obs : observations) {
        if (owned.at(obs.series_id).contains({obs.oresmd_uri, obs.observation_datetime})) {
            BOOST_LOG_SEV(guard_lg(), debug)
                << "Dropped an automatic write to a manual point. Series: " << obs.series_id
                << " coordinate: " << obs.oresmd_uri;
            continue;
        }
        kept.push_back(obs);
    }
    return kept;
}

}
