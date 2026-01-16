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
#ifndef ORES_DQ_DOMAIN_NATURE_DIMENSION_HPP
#define ORES_DQ_DOMAIN_NATURE_DIMENSION_HPP

#include <chrono>
#include <string>

namespace ores::dq::domain {

/**
 * @brief Classifies datasets by their data nature (actual vs. synthetic).
 *
 * The nature dimension distinguishes between real production data and
 * synthetic test data, helping ensure appropriate handling and segregation.
 *
 * Standard nature values:
 * - Actual: Real production data from live systems
 * - Synthetic: Generated test data for development/testing
 * - Historical: Archived data from past periods
 */
struct nature_dimension final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Unique code identifying this nature type.
     *
     * This is the natural key for the dimension.
     * Examples: "Actual", "Synthetic", "Historical".
     */
    std::string code;

    /**
     * @brief Human-readable name for display purposes.
     */
    std::string name;

    /**
     * @brief Detailed description of this nature type.
     */
    std::string description;

    /**
     * @brief Username of the person who last modified this nature dimension.
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

}

#endif
