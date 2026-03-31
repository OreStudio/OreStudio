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
#ifndef ORES_MARKETDATA_CORE_REPOSITORY_MARKET_FIXINGS_REPOSITORY_HPP
#define ORES_MARKETDATA_CORE_REPOSITORY_MARKET_FIXINGS_REPOSITORY_HPP

#include <chrono>
#include <string>
#include <vector>
#include <boost/uuid/uuid.hpp>
#include <sqlgen/postgres.hpp>
#include "ores.logging/make_logger.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.marketdata.api/domain/market_fixing.hpp"

namespace ores::marketdata::repository {

/**
 * @brief Reads and writes market data fixings to data storage.
 *
 * Backed by a TimescaleDB hypertable partitioned by fixing_date.
 */
class market_fixings_repository {
private:
    inline static std::string_view logger_name =
        "ores.marketdata.repository.market_fixings_repository";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    std::string sql();

    void write(context ctx, const domain::market_fixing& v);
    void write(context ctx, const std::vector<domain::market_fixing>& v);

    std::vector<domain::market_fixing>
    read_latest(context ctx, const boost::uuids::uuid& series_id);

    std::vector<domain::market_fixing>
    read_latest(context ctx, const boost::uuids::uuid& series_id,
        const std::chrono::year_month_day& from_date,
        const std::chrono::year_month_day& to_date);

    void remove(context ctx, const boost::uuids::uuid& series_id);
};

}

#endif
