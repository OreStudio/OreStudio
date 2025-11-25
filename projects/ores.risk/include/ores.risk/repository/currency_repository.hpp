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
#ifndef ORES_RISK_REPOSITORY_CURRENCY_REPOSITORY_HPP
#define ORES_RISK_REPOSITORY_CURRENCY_REPOSITORY_HPP

#include <string>
#include <vector>
#include <sqlgen/postgres.hpp>
#include "ores.utility/log/make_logger.hpp"
#include "ores.utility/repository/context.hpp"
#include "ores.utility/repository/helpers.hpp"
#include "ores.risk/domain/currency.hpp"

namespace ores::risk::repository {

/**
 * @brief Reads and writes currencies off of data storage.
 */
class currency_repository {
private:
    static auto& lg() {
        using namespace ores::utility::log;
        static auto instance = make_logger(
            "ores.risk.repository.currency_repository");
        return instance;
    }

public:
    using context = ores::utility::repository::context;

    /**
     * @brief Returns the SQL created by sqlgen to construct the table.
     */
    std::string sql();

    /**
     * @brief Writes currencies to database. Expects the currency set to have
     * unique ISO codes.
     */
    /**@{*/
    void write(context ctx, const domain::currency& currencies);
    void write(context ctx, const std::vector<domain::currency>& currencies);
    /**@}*/

    /**
     * @brief Reads latest currencies, possibly filtered by ISO code.
     */
    /**@{*/
    std::vector<domain::currency> read_latest(context ctx);
    std::vector<domain::currency>
    read_latest(context ctx, const std::string& iso_code);
    /**@}*/

    /**
     * @brief Reads currencies at the supplied time point, possibly filtered by
     * ISO code.
     */
    /**@{*/
    std::vector<domain::currency>
    read_at_timepoint(context ctx, const std::string& as_of);
    std::vector<domain::currency>
    read_at_timepoint(context ctx, const std::string& as_of,
        const std::string& iso_code);
    /**@}*/

    /**
     * @brief Reads all currencies, possibly filtered by ISO code.
     */
    /**@{*/
    std::vector<domain::currency> read_all(context ctx);
    std::vector<domain::currency>
    read_all(context ctx, const std::string& iso_code);
    /**@}*/

    /**
     * @brief Deletes a currency by closing its temporal validity.
     *
     * Sets the valid_to timestamp to now, effectively "deleting" the currency
     * from the current point in time onwards while preserving history.
     */
    void remove(context ctx, const std::string& iso_code);
};

}

#endif
