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
#ifndef ORES_REFDATA_CORE_REPOSITORY_MONETARY_NATURE_REPOSITORY_HPP
#define ORES_REFDATA_CORE_REPOSITORY_MONETARY_NATURE_REPOSITORY_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.refdata.api/domain/monetary_nature.hpp"
#include "ores.refdata.core/export.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <sqlgen/postgres.hpp>
#include <string>
#include <vector>

namespace ores::refdata::repository {

/**
 * @brief Reads and writes monetary natures to data storage.
 */
class ORES_REFDATA_CORE_EXPORT monetary_nature_repository {
private:
    inline static std::string_view logger_name =
        "ores.refdata.repository.monetary_nature_repository";

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
     * @brief Writes monetary natures to database.
     */
    /**@{*/
    void write(context ctx, const domain::monetary_nature& v);
    void write(context ctx, const std::vector<domain::monetary_nature>& v);
    /**@}*/

    /**
     * @brief Reads latest monetary natures, possibly filtered by code.
     */
    /**@{*/
    std::vector<domain::monetary_nature> read_latest(context ctx);
    std::vector<domain::monetary_nature> read_latest(context ctx, const std::string& code);
    /**@}*/

    /**
     * @brief Reads all monetary natures, possibly filtered by code.
     */
    std::vector<domain::monetary_nature> read_all(context ctx, const std::string& code);

    /**
     * @brief Reads a single monetary nature as it stood at a specific
     * version — the version's own [valid_from, valid_to) window is returned
     * verbatim, so the caller can compose child entities "as of" the same
     * window. See the "Temporal composite entity versioning" architecture
     * doc.
     * @param ctx Repository context with database connection
     * @param code The code to look up
     * @param version The version to fetch
     */
    std::optional<domain::monetary_nature>
    read_at_version(context ctx, const std::string& code, std::uint32_t version);

    /**
     * @brief Reads latest monetary natures with pagination support.
     * @param ctx Repository context with database connection
     * @param offset Number of records to skip
     * @param limit Maximum number of records to return
     */
    std::vector<domain::monetary_nature>
    read_latest(context ctx, std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active monetary natures.
     * @param ctx Repository context with database connection
     * @return Total number of active monetary natures
     */
    std::uint32_t get_total_type_count(context ctx);

    /**
     * @brief Deletes a monetary nature by closing its temporal validity.
     */
    void remove(context ctx, const std::string& code);

    /**
     * @brief Deletes monetary natures by closing their temporal validity.
     */
    void remove(context ctx, const std::vector<std::string>& codes);
};

}

#endif
