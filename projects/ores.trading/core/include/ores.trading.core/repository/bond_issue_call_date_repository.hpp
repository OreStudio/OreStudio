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
#ifndef ORES_TRADING_CORE_REPOSITORY_BOND_ISSUE_CALL_DATE_REPOSITORY_HPP
#define ORES_TRADING_CORE_REPOSITORY_BOND_ISSUE_CALL_DATE_REPOSITORY_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.trading.api/domain/bond_issue_call_date.hpp"
#include "ores.trading.core/export.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <sqlgen/postgres.hpp>
#include <string>
#include <vector>

namespace ores::trading::repository {

/**
 * @brief Reads and writes bond issue call dates to data storage.
 */
class ORES_TRADING_CORE_EXPORT bond_issue_call_date_repository {
private:
    inline static std::string_view logger_name =
        "ores.trading.repository.bond_issue_call_date_repository";

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
     * @brief Writes bond issue call dates to database.
     */
    /**@{*/
    void write(context ctx, const domain::bond_issue_call_date& v);
    void write(context ctx, const std::vector<domain::bond_issue_call_date>& v);
    /**@}*/

    /**
     * @brief Reads latest bond issue call dates, possibly filtered by primary key.
     */
    /**@{*/
    std::vector<domain::bond_issue_call_date> read_latest(context ctx);
    std::vector<domain::bond_issue_call_date>
    read_latest(context ctx, const std::string& issue_id, const std::string& sequence_number);
    /**@}*/

    /**
     * @brief Reads all bond issue call dates, possibly filtered by primary key.
     */
    std::vector<domain::bond_issue_call_date>
    read_all(context ctx, const std::string& issue_id, const std::string& sequence_number);

    /**
     * @brief Reads a single bond issue call date as it stood at a specific
     * version — the version's own [valid_from, valid_to) window is returned
     * verbatim, so the caller can compose child entities "as of" the same
     * window. See the "Temporal composite entity versioning" architecture
     * doc.
     * @param ctx Repository context with database connection
     * @param version The version to fetch
     */
    std::optional<domain::bond_issue_call_date> read_at_version(context ctx,
                                                                const std::string& issue_id,
                                                                const std::string& sequence_number,
                                                                std::uint32_t version);

    /**
     * @brief Reads latest bond issue call dates with pagination support.
     * @param ctx Repository context with database connection
     * @param offset Number of records to skip
     * @param limit Maximum number of records to return
     */
    std::vector<domain::bond_issue_call_date>
    read_latest(context ctx, std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active bond issue call dates.
     * @param ctx Repository context with database connection
     * @return Total number of active bond issue call dates
     */
    std::uint32_t get_total_call_date_count(context ctx);

    /**
     * @brief Deletes a bond issue call date by closing its temporal validity.
     */
    void remove(context ctx, const std::string& issue_id, const std::string& sequence_number);

    /**
     * @brief Deletes bond issue call dates by closing their temporal validity.
     */
    void remove(context ctx,
                const std::vector<std::string>& issue_ids,
                const std::vector<std::string>& sequence_numbers);
};

}

#endif
