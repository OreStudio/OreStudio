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
#ifndef ORES_DQ_DOMAIN_TREATMENT_DIMENSION_HPP
#define ORES_DQ_DOMAIN_TREATMENT_DIMENSION_HPP

#include <chrono>
#include <string>

namespace ores::dq::domain {

/**
 * @brief Classifies datasets by their processing treatment level.
 *
 * The treatment dimension indicates how much processing or transformation
 * has been applied to the data, from raw untouched data to fully processed.
 *
 * Standard treatment values:
 * - Raw: Unprocessed data as received from source
 * - Cleansed: Data that has undergone validation and cleaning
 * - Enriched: Data augmented with additional derived fields
 * - Aggregated: Summarized or rolled-up data
 */
struct treatment_dimension final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    std::string tenant_id;

    /**
     * @brief Unique code identifying this treatment type.
     *
     * This is the natural key for the dimension.
     * Examples: "Raw", "Cleansed", "Enriched", "Aggregated".
     */
    std::string code;

    /**
     * @brief Human-readable name for display purposes.
     */
    std::string name;

    /**
     * @brief Detailed description of this treatment type.
     */
    std::string description;

    /**
     * @brief Username of the person who last modified this treatment dimension.
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
