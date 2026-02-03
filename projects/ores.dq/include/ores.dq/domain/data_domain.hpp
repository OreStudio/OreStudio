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
#ifndef ORES_DQ_DOMAIN_DATA_DOMAIN_HPP
#define ORES_DQ_DOMAIN_DATA_DOMAIN_HPP

#include <chrono>
#include <string>

namespace ores::dq::domain {

/**
 * @brief Represents a high-level classification of data within the system.
 *
 * Data domains define broad categories of data (e.g., "Reference Data",
 * "Market Data", "Trade Data") that help organize and manage datasets.
 * Each data domain can contain multiple subject areas.
 *
 * Examples of data domains:
 * - Reference Data: Static reference data like currencies, countries
 * - Market Data: Real-time and historical market prices
 * - Trade Data: Transaction and trade information
 */
struct data_domain final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    std::string tenant_id;

    /**
     * @brief Unique name identifying this data domain.
     *
     * This is the natural key for the data domain.
     * Examples: "Reference Data", "Market Data", "Trade Data".
     */
    std::string name;

    /**
     * @brief Human-readable description of the data domain's purpose.
     */
    std::string description;

    /**
     * @brief Username of the person who last modified this data domain.
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
