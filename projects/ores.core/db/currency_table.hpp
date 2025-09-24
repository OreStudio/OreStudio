/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2024 Marco Craveiro <marco.craveiro@gmail.com>
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
#ifndef ORES_CORE_DB_CURRENCY_TABLE_HPP
#define ORES_CORE_DB_CURRENCY_TABLE_HPP

#if defined(_MSC_VER) && (_MSC_VER >= 1200)
#pragma once
#endif

#include <string>
#include <vector>
#include "ores.core/risk/currency.hpp"

namespace ores::core::db {

class currency_table {
private:
    std::vector<risk::currency> read_internal(const std::string& query);

public:
    /**
     * @brief Writes currencies to database. Expects the currency set to have
     * unique ISO codes.
     */
    void write(const std::vector<risk::currency>& currencies);

    /**
     * @brief Reads latest currencies, possibly filtered by ISO code.
     */
    std::vector<risk::currency>
    read_latest(const std::string& iso_code = std::string());

    /**
     * @brief Reads currencies at the supplied time point, possibly filtered by
     * ISO code.
     */
    std::vector<risk::currency>
    read_at_timepoint(const std::string& as_of,
        const std::string& iso_code = std::string());

    /**
     * @brief Reads all currencies, possibly filtered by ISO code.
     */
    std::vector<risk::currency>
    read_all(const std::string& iso_code = std::string());
};

}

#endif
