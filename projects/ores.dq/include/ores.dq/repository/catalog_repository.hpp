/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2025 Marco Craveiro <marco.craveiro@gmail.com>
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
#ifndef ORES_DQ_REPOSITORY_CATALOG_REPOSITORY_HPP
#define ORES_DQ_REPOSITORY_CATALOG_REPOSITORY_HPP

#include <string>
#include <vector>
#include <sqlgen/postgres.hpp>
#include "ores.logging/make_logger.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.dq/domain/catalog.hpp"

namespace ores::dq::repository {

/**
 * @brief Reads and writes catalogs to data storage.
 */
class catalog_repository {
private:
    inline static std::string_view logger_name =
        "ores.dq.repository.catalog_repository";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    explicit catalog_repository(context ctx);

    /**
     * @brief Returns the SQL created by sqlgen to construct the table.
     */
    std::string sql();

    /**
     * @brief Writes catalogs to database.
     */
    /**@{*/
    void write(const domain::catalog& catalog);
    void write(const std::vector<domain::catalog>& catalogs);
    /**@}*/

    /**
     * @brief Reads latest catalogs, possibly filtered by name.
     */
    /**@{*/
    std::vector<domain::catalog> read_latest();
    std::vector<domain::catalog> read_latest(const std::string& name);
    /**@}*/

    /**
     * @brief Reads latest catalogs with pagination support.
     * @param offset Number of records to skip
     * @param limit Maximum number of records to return
     * @return Vector of catalogs within the specified range
     */
    std::vector<domain::catalog>
    read_latest(std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active catalogs.
     * @return Total number of catalogs with valid_to == max_timestamp
     */
    std::uint32_t get_total_count();

    /**
     * @brief Reads all historical versions of a catalog by name.
     * @param name The catalog name to look up
     * @return Vector of all versions, ordered by version descending
     */
    std::vector<domain::catalog> read_all(const std::string& name);

    /**
     * @brief Deletes a catalog by closing its temporal validity.
     */
    void remove(const std::string& name);

private:
    context ctx_;
};

}

#endif
