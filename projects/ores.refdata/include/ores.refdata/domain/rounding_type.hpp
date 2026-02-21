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
#ifndef ORES_REFDATA_DOMAIN_ROUNDING_TYPE_HPP
#define ORES_REFDATA_DOMAIN_ROUNDING_TYPE_HPP

#include <chrono>
#include <string>

namespace ores::refdata::domain {

/**
 * @brief Valid rounding methods per ORE XML schema.
 *
 * Reference data table defining valid rounding method classifications.
 * Values match xs:enumeration in ORE's currencyconfig.xsd.
 * 
 * Rounding types are managed by the system tenant and are used to
 * specify how currency amounts are rounded in financial calculations.
 */
struct rounding_type final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Unique rounding type code.
     *
     * Examples: 'Up', 'Down', 'Closest', 'Floor', 'Ceiling'.
     */
    std::string code;

    /**
     * @brief Human-readable name for the rounding type.
     */
    std::string name;

    /**
     * @brief Detailed description of the rounding type with numeric examples.
     */
    std::string description;

    /**
     * @brief Order for UI display purposes.
     */
    int display_order;

    /**
     * @brief Username of the person who last modified this rounding type.
     */
    std::string modified_by;

    /**
     * @brief Username of the account that performed this action.
     */
    std::string performed_by;

    /**
     * @brief Code identifying the reason for the change.
     *
     * References change_reasons table (soft FK).
     */
    std::string change_reason_code;

    /**
     * @brief Free-text commentary explaining the change.
     */
    std::string change_commentary;

    /**
     * @brief Timestamp when this version of the record was recorded.
     */
    std::chrono::system_clock::time_point recorded_at;
};

}

#endif
