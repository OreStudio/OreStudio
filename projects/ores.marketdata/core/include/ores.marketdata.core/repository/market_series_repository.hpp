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
#ifndef ORES_MARKETDATA_CORE_REPOSITORY_MARKET_SERIES_REPOSITORY_HPP
#define ORES_MARKETDATA_CORE_REPOSITORY_MARKET_SERIES_REPOSITORY_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.marketdata.api/domain/market_series.hpp"
#include "ores.marketdata.core/export.hpp"
#include <cstdint>
#include <sqlgen/postgres.hpp>
#include <string>
#include <vector>

namespace ores::marketdata::repository {

/**
 * @brief Reads and writes market series to data storage.
 */
class ORES_MARKETDATA_CORE_EXPORT market_series_repository {
private:
    inline static std::string_view logger_name =
        "ores.marketdata.repository.market_series_repository";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Returns the SQL created by sqlgen to construct the table.
     */
    std::string sql();

    /**
     * @brief Writes market series to database.
     */
    /**@{*/
    void write(context ctx, const domain::market_series& v);
    void write(context ctx, const std::vector<domain::market_series>& v);
    /**@}*/

    /**
     * @brief Reads latest market series, possibly filtered by id.
     */
    /**@{*/
    std::vector<domain::market_series> read_latest(context ctx);
    std::vector<domain::market_series> read_latest(context ctx, const std::string& id);
    /**@}*/

    /**
     * @brief Reads all market series, possibly filtered by id.
     */
    std::vector<domain::market_series> read_all(context ctx, const std::string& id);

    /**
     * @brief Reads latest market series with pagination support.
     * @param ctx Repository context with database connection
     * @param offset Number of records to skip
     * @param limit Maximum number of records to return
     */
    std::vector<domain::market_series>
    read_latest(context ctx, std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active market series.
     * @param ctx Repository context with database connection
     * @return Total number of active market series
     */
    std::uint32_t get_total_market_series_count(context ctx);

    /**
     * @brief Deletes a market series by closing its temporal validity.
     */
    void remove(context ctx, const std::string& id);

    /**
     * @brief Deletes market series by closing their temporal validity.
     */
    void remove(context ctx, const std::vector<std::string>& ids);

    std::vector<domain::market_series> read_latest_by_type(context ctx,
                                                           const std::string& series_type,
                                                           const std::string& metric,
                                                           const std::string& qualifier,
                                                           const std::string& party_id = {});
};

}

#endif
