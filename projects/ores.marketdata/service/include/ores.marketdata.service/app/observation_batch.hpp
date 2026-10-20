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
#ifndef ORES_MARKETDATA_SERVICE_APP_OBSERVATION_BATCH_HPP
#define ORES_MARKETDATA_SERVICE_APP_OBSERVATION_BATCH_HPP

#include "ores.marketdata.api/domain/market_observation.hpp"
#include <boost/uuid/uuid_io.hpp>
#include <algorithm>
#include <chrono>
#include <set>
#include <string>
#include <tuple>
#include <vector>

namespace ores::marketdata::service::app {

/**
 * @brief The rows of @p rows with one row for each key, the last one winning.
 *
 * The insert trigger of the observation table closes the current row of a key
 * only when that row began before the current transaction, and the current
 * timestamp is the start of the transaction. Two rows of one key in one batch
 * would therefore both stay current and break the unique index. A batch keeps
 * the newest row of each key and keeps the order of the survivors.
 */
[[nodiscard]] inline std::vector<domain::market_observation>
keep_last_per_key(std::vector<domain::market_observation> rows) {
    using key = std::
        tuple<std::string, std::string, std::string, std::chrono::system_clock::rep, std::string>;
    std::set<key> seen;
    std::vector<domain::market_observation> kept;
    kept.reserve(rows.size());
    for (auto it = rows.rbegin(); it != rows.rend(); ++it) {
        key k{it->tenant_id.to_string(),
              boost::uuids::to_string(it->party_id),
              boost::uuids::to_string(it->series_id),
              it->observation_datetime.time_since_epoch().count(),
              it->oresmd_uri};
        if (seen.insert(std::move(k)).second)
            kept.push_back(std::move(*it));
    }
    std::reverse(kept.begin(), kept.end());
    return kept;
}

}

#endif
