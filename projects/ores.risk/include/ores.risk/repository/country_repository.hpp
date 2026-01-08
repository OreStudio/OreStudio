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
#ifndef ORES_RISK_REPOSITORY_COUNTRY_REPOSITORY_HPP
#define ORES_RISK_REPOSITORY_COUNTRY_REPOSITORY_HPP

#include <string>
#include <vector>
#include <sqlgen/postgres.hpp>
#include "ores.telemetry/log/make_logger.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.risk/domain/country.hpp"

namespace ores::risk::repository {

/**
 * @brief Reads and writes countries off of data storage.
 */
class country_repository {
private:
    inline static std::string_view logger_name =
        "ores.risk.repository.country_repository";

    static auto& lg() {
        using namespace ores::telemetry::log;
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
     * @brief Writes countries to database. Expects the country set to have
     * unique alpha-2 codes.
     */
    /**@{*/
    void write(context ctx, const domain::country& country);
    void write(context ctx, const std::vector<domain::country>& countries);
    /**@}*/

    /**
     * @brief Reads latest countries, possibly filtered by alpha-2 code.
     */
    /**@{*/
    std::vector<domain::country> read_latest(context ctx);
    std::vector<domain::country>
    read_latest(context ctx, const std::string& alpha2_code);
    /**@}*/

    /**
     * @brief Reads latest countries with pagination support.
     * @param ctx Repository context with database connection
     * @param offset Number of records to skip
     * @param limit Maximum number of records to return
     * @return Vector of countries within the specified range
     */
    std::vector<domain::country> read_latest(context ctx,
                                              std::uint32_t offset,
                                              std::uint32_t limit);

    /**
     * @brief Gets the total count of active countries.
     * @param ctx Repository context with database connection
     * @return Total number of countries with valid_to == max_timestamp
     */
    std::uint32_t get_total_country_count(context ctx);

    /**
     * @brief Reads countries at the supplied time point, possibly filtered by
     * alpha-2 code.
     */
    /**@{*/
    std::vector<domain::country>
    read_at_timepoint(context ctx, const std::string& as_of);
    std::vector<domain::country>
    read_at_timepoint(context ctx, const std::string& as_of,
        const std::string& alpha2_code);
    /**@}*/

    /**
     * @brief Reads all countries, possibly filtered by alpha-2 code.
     */
    /**@{*/
    std::vector<domain::country> read_all(context ctx);
    std::vector<domain::country>
    read_all(context ctx, const std::string& alpha2_code);
    /**@}*/

    /**
     * @brief Deletes a country by closing its temporal validity.
     *
     * Sets the valid_to timestamp to now, effectively "deleting" the country
     * from the current point in time onwards while preserving history.
     */
    void remove(context ctx, const std::string& alpha2_code);
};

}

#endif
