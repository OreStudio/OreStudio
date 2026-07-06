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
#ifndef ORES_REFDATA_CORE_REPOSITORY_PURPOSE_TYPE_REPOSITORY_HPP
#define ORES_REFDATA_CORE_REPOSITORY_PURPOSE_TYPE_REPOSITORY_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.refdata.api/domain/purpose_type.hpp"
#include "ores.refdata.core/export.hpp"
#include <cstdint>
#include <sqlgen/postgres.hpp>
#include <string>
#include <vector>

namespace ores::refdata::repository {

/**
 * @brief Reads and writes purpose types to data storage.
 */
class ORES_REFDATA_CORE_EXPORT purpose_type_repository {
private:
    inline static std::string_view logger_name = "ores.refdata.repository.purpose_type_repository";

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
     * @brief Writes purpose types to database.
     */
    /**@{*/
    void write(context ctx, const domain::purpose_type& v);
    void write(context ctx, const std::vector<domain::purpose_type>& v);
    /**@}*/

    /**
     * @brief Reads latest purpose types, possibly filtered by code.
     */
    /**@{*/
    std::vector<domain::purpose_type> read_latest(context ctx);
    std::vector<domain::purpose_type> read_latest(context ctx, const std::string& code);
    /**@}*/

    /**
     * @brief Reads all purpose types, possibly filtered by code.
     */
    std::vector<domain::purpose_type> read_all(context ctx, const std::string& code);


    /**
     * @brief Reads latest purpose types with pagination support.
     * @param ctx Repository context with database connection
     * @param offset Number of records to skip
     * @param limit Maximum number of records to return
     */
    std::vector<domain::purpose_type>
    read_latest(context ctx, std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active purpose types.
     * @param ctx Repository context with database connection
     * @return Total number of active purpose types
     */
    std::uint32_t get_total_type_count(context ctx);

    /**
     * @brief Deletes a purpose type by closing its temporal validity.
     */
    void remove(context ctx, const std::string& code);

    /**
     * @brief Deletes purpose types by closing their temporal validity.
     */
    void remove(context ctx, const std::vector<std::string>& codes);
};

}

#endif
