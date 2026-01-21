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
#ifndef ORES_DQ_DOMAIN_CHANGE_REASON_HPP
#define ORES_DQ_DOMAIN_CHANGE_REASON_HPP

#include <chrono>
#include <string>

namespace ores::dq::domain {

/**
 * @brief Defines a specific reason for record changes.
 *
 * Change reasons provide a controlled vocabulary for documenting why records
 * are modified. Each reason belongs to a category (e.g., "static_data",
 * "trade") and can be configured to apply to specific operations (amend,
 * delete) and whether commentary is required when using this reason.
 *
 * Reasons follow a namespaced format: "category.reason_name"
 * Examples:
 * - system.new: Initial record creation
 * - static_data.front_office_error: Correcting front office mistake
 * - static_data.back_office_error: Correcting back office mistake
 * - trade.price_correction: Fixing trade price
 *
 * @note This type only includes change_commentary (not change_reason_code)
 * because this entity IS a change reason. Self-referential tracking
 * is handled at the database level.
 */
struct change_reason final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Unique code identifying this reason.
     *
     * This is the natural key for the reason. Format is "category.reason".
     * Examples: "system.new", "static_data.front_office_error".
     */
    std::string code;

    /**
     * @brief Human-readable description of when to use this reason.
     */
    std::string description;

    /**
     * @brief Category code this reason belongs to.
     *
     * References change_reason_categories.code (soft FK).
     */
    std::string category_code;

    /**
     * @brief Whether this reason can be used for amend operations.
     */
    bool applies_to_amend = true;

    /**
     * @brief Whether this reason can be used for delete operations.
     */
    bool applies_to_delete = true;

    /**
     * @brief Whether commentary is mandatory when using this reason.
     */
    bool requires_commentary = false;

    /**
     * @brief Display order for UI presentation.
     *
     * Lower numbers appear first in dropdown lists.
     */
    int display_order = 0;

    /**
     * @brief Username of the person who last modified this reason.
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
 * @brief Well-known reason codes used throughout the system.
 */
namespace change_reasons {
    // System reasons
    constexpr auto system_new = "system.new";
    constexpr auto system_migration = "system.migration";

    // Static data reasons
    constexpr auto static_data_front_office_error = "static_data.front_office_error";
    constexpr auto static_data_back_office_error = "static_data.back_office_error";
    constexpr auto static_data_regulatory_change = "static_data.regulatory_change";
    constexpr auto static_data_corporate_action = "static_data.corporate_action";
}

}

#endif
