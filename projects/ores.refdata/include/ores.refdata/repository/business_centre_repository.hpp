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
#ifndef ORES_REFDATA_REPOSITORY_BUSINESS_CENTRE_REPOSITORY_HPP
#define ORES_REFDATA_REPOSITORY_BUSINESS_CENTRE_REPOSITORY_HPP

#include <string>
#include <vector>
#include <sqlgen/postgres.hpp>
#include "ores.logging/make_logger.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.refdata/domain/business_centre.hpp"

namespace ores::refdata::repository {

/**
 * @brief Reads and writes business centres off of data storage.
 */
class business_centre_repository {
private:
    inline static std::string_view logger_name =
        "ores.refdata.repository.business_centre_repository";

    static auto& lg() {
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
     * @brief Writes business centres to database.
     */
    /**@{*/
    void write(context ctx, const domain::business_centre& bc);
    void write(context ctx, const std::vector<domain::business_centre>& bcs);
    /**@}*/

    /**
     * @brief Reads latest business centres, possibly filtered by code.
     */
    /**@{*/
    std::vector<domain::business_centre> read_latest(context ctx);
    std::vector<domain::business_centre>
    read_latest(context ctx, const std::string& code);
    /**@}*/

    /**
     * @brief Reads latest business centres with pagination support.
     */
    std::vector<domain::business_centre> read_latest(context ctx,
                                                      std::uint32_t offset,
                                                      std::uint32_t limit);

    /**
     * @brief Gets the total count of active business centres.
     */
    std::uint32_t get_total_business_centre_count(context ctx);

    /**
     * @brief Reads business centres at the supplied time point, possibly
     * filtered by code.
     */
    /**@{*/
    std::vector<domain::business_centre>
    read_at_timepoint(context ctx, const std::string& as_of);
    std::vector<domain::business_centre>
    read_at_timepoint(context ctx, const std::string& as_of,
        const std::string& code);
    /**@}*/

    /**
     * @brief Reads all business centres, possibly filtered by code.
     */
    /**@{*/
    std::vector<domain::business_centre> read_all(context ctx);
    std::vector<domain::business_centre>
    read_all(context ctx, const std::string& code);
    /**@}*/

    /**
     * @brief Deletes a business centre by closing its temporal validity.
     */
    void remove(context ctx, const std::string& code);
};

}

#endif
