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
#ifndef ORES_MARKETDATA_CORE_REPOSITORY_MARKET_OBSERVATIONS_REPOSITORY_HPP
#define ORES_MARKETDATA_CORE_REPOSITORY_MARKET_OBSERVATIONS_REPOSITORY_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.marketdata.api/domain/market_observation.hpp"
#include "ores.marketdata.core/export.hpp"
#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <sqlgen/postgres.hpp>
#include <string>
#include <vector>

namespace ores::marketdata::repository {

/**
 * @brief Reads and writes market data observations to data storage.
 *
 * Backed by a TimescaleDB hypertable partitioned by observation_datetime.
 * Bulk writes are the primary ingestion path; individual reads filter
 * by series and optionally by datetime range.
 */
class ORES_MARKETDATA_CORE_EXPORT market_observations_repository {
private:
    inline static std::string_view logger_name =
        "ores.marketdata.repository.market_observations_repository";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    std::string sql();

    void write(context ctx, const domain::market_observation& v);
    void write(context ctx, const std::vector<domain::market_observation>& v);

    std::vector<domain::market_observation> read_latest(context ctx,
                                                        const boost::uuids::uuid& series_id);

    std::vector<domain::market_observation>
    read_latest(context ctx,
                const boost::uuids::uuid& series_id,
                const std::chrono::system_clock::time_point& from_datetime,
                const std::chrono::system_clock::time_point& to_datetime);

    void remove(context ctx, const boost::uuids::uuid& series_id);
};

}

#endif
