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
#ifndef ORES_REFDATA_API_DOMAIN_PARTY_TYPE_HPP
#define ORES_REFDATA_API_DOMAIN_PARTY_TYPE_HPP

#include <chrono>
#include <string>

namespace ores::refdata::domain {

/**
 * @brief Classification of legal entities participating in financial transactions.
 *
 * Reference data table defining valid party type classifications.
 * Examples: 'Bank', 'Corporate', 'HedgeFund', 'Government'.
 *
 * Party types are managed by the system tenant and are used to
 * categorise parties and counterparties.
 */
struct party_type final {
    /**
     * @brief Unique type code.
     *
     * Examples: 'Bank', 'Corporate', 'HedgeFund'.
     */
    std::string code;

    /**
     * @brief Human-readable name for the party type.
     */
    std::string name;

    /**
     * @brief Detailed description of the party type.
     */
    std::string description;

    /**
     * @brief Order for UI display purposes.
     */
    int display_order = 0;

    /**
     * @brief Timestamp when this record was created.
     */
    std::chrono::system_clock::time_point created_at;
};

}

#endif
