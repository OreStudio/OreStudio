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
#ifndef ORES_DQ_DOMAIN_CHANGE_REASON_CATEGORY_HPP
#define ORES_DQ_DOMAIN_CHANGE_REASON_CATEGORY_HPP

#include <chrono>
#include <string>

namespace ores::dq::domain {

/**
 * @brief Groups change reasons into logical categories.
 *
 * Change reason categories define the namespace for change reasons and
 * determine which reasons apply to which entity types. Categories provide
 * a way to organize change reasons by domain or purpose.
 *
 * Examples of categories:
 * - static_data: For changes to reference data like currencies, countries
 * - trade: For trade-related modifications
 * - market_data: For market data corrections
 * - system: For system-level changes like initial creation
 *
 * @note This type only includes change_commentary (not change_reason_code)
 * because this entity IS a change reason category. Self-referential tracking
 * is handled at the database level.
 */
struct change_reason_category final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Unique code identifying this category.
     *
     * This is the natural key for the category. Examples: "static_data",
     * "trade", "market_data", "system".
     */
    std::string code;

    /**
     * @brief Human-readable description of the category's purpose.
     */
    std::string description;

    /**
     * @brief Username of the person who last modified this category.
     */
    std::string recorded_by;

    /**
     * @brief Free-text commentary explaining the change.
     */
    std::string change_commentary;

    /**
     * @brief Timestamp when this version of the record was recorded.
     */
    std::chrono::system_clock::time_point recorded_at;
};

/**
 * @brief Well-known category codes used throughout the system.
 */
namespace change_reason_categories {
    constexpr auto system = "system";
    constexpr auto static_data = "static_data";
    constexpr auto trade = "trade";
    constexpr auto market_data = "market_data";
}

}

#endif
