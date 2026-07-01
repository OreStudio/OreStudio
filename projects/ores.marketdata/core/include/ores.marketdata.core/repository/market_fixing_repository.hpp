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
#ifndef ORES_MARKETDATA_CORE_REPOSITORY_MARKET_FIXING_REPOSITORY_HPP
#define ORES_MARKETDATA_CORE_REPOSITORY_MARKET_FIXING_REPOSITORY_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.marketdata.api/domain/market_fixing.hpp"
#include "ores.marketdata.core/export.hpp"
#include <cstdint>
#include <sqlgen/postgres.hpp>
#include <string>
#include <vector>

namespace ores::marketdata::repository {

/**
 * @brief Reads and writes market fixings to data storage.
 */
class ORES_MARKETDATA_CORE_EXPORT market_fixing_repository {
private:
    inline static std::string_view logger_name =
        "ores.marketdata.repository.market_fixing_repository";

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
     * @brief Writes market fixings to database.
     */
    /**@{*/
    void write(context ctx, const domain::market_fixing& v);
    void write(context ctx, const std::vector<domain::market_fixing>& v);
    /**@}*/

    /**
     * @brief Reads latest market fixings, possibly filtered by id.
     */
    /**@{*/
    std::vector<domain::market_fixing> read_latest(context ctx);
    std::vector<domain::market_fixing> read_latest(context ctx, const std::string& id);
    /**@}*/

    /**
     * @brief Reads all market fixings, possibly filtered by id.
     */
    std::vector<domain::market_fixing> read_all(context ctx, const std::string& id);

    /**
     * @brief Reads latest market fixings with pagination support.
     * @param ctx Repository context with database connection
     * @param offset Number of records to skip
     * @param limit Maximum number of records to return
     */
    std::vector<domain::market_fixing>
    read_latest(context ctx, std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active market fixings.
     * @param ctx Repository context with database connection
     * @return Total number of active market fixings
     */
    std::uint32_t get_total_market_fixing_count(context ctx);

    /**
     * @brief Deletes a market fixing by closing its temporal validity.
     */
    void remove(context ctx, const std::string& id);

    /**
     * @brief Deletes market fixings by closing their temporal validity.
     */
    void remove(context ctx, const std::vector<std::string>& ids);
};

}

#endif
