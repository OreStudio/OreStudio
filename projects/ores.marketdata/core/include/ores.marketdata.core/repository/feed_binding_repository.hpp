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
#ifndef ORES_MARKETDATA_CORE_REPOSITORY_FEED_BINDING_REPOSITORY_HPP
#define ORES_MARKETDATA_CORE_REPOSITORY_FEED_BINDING_REPOSITORY_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.marketdata.api/domain/feed_binding.hpp"
#include "ores.marketdata.core/export.hpp"
#include <cstdint>
#include <sqlgen/postgres.hpp>
#include <string>
#include <vector>

namespace ores::marketdata::repository {

/**
 * @brief Reads and writes feed bindings to data storage.
 */
class ORES_MARKETDATA_CORE_EXPORT feed_binding_repository {
private:
    inline static std::string_view logger_name =
        "ores.marketdata.repository.feed_binding_repository";

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
     * @brief Writes feed bindings to database.
     */
    /**@{*/
    void write(context ctx, const domain::feed_binding& v);
    void write(context ctx, const std::vector<domain::feed_binding>& v);
    /**@}*/

    /**
     * @brief Reads latest feed bindings, possibly filtered by id.
     */
    /**@{*/
    std::vector<domain::feed_binding> read_latest(context ctx);
    std::vector<domain::feed_binding> read_latest(context ctx, const std::string& id);
    /**@}*/

    /**
     * @brief Reads all feed bindings, possibly filtered by id.
     */
    std::vector<domain::feed_binding> read_all(context ctx, const std::string& id);

    /**
     * @brief Reads latest feed bindings with pagination support.
     * @param ctx Repository context with database connection
     * @param offset Number of records to skip
     * @param limit Maximum number of records to return
     */
    std::vector<domain::feed_binding>
    read_latest(context ctx, std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active feed bindings.
     * @param ctx Repository context with database connection
     * @return Total number of active feed bindings
     */
    std::uint32_t get_total_feed_binding_count(context ctx);

    /**
     * @brief Deletes a feed binding by closing its temporal validity.
     */
    void remove(context ctx, const std::string& id);

    /**
     * @brief Deletes feed bindings by closing their temporal validity.
     */
    void remove(context ctx, const std::vector<std::string>& ids);

    std::vector<domain::feed_binding> read_latest_all_tenants(context ctx);
};

}

#endif
