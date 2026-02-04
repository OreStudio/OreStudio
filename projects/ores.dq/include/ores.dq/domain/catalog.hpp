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
#ifndef ORES_DQ_DOMAIN_CATALOG_HPP
#define ORES_DQ_DOMAIN_CATALOG_HPP

#include <chrono>
#include <optional>
#include <string>

namespace ores::dq::domain {

/**
 * @brief Represents a logical grouping of related datasets.
 *
 * Catalogs provide a way to organize datasets into logical collections,
 * similar to how a library catalog organizes books. Examples include
 * "ISO Standards", "Cryptocurrency", "FpML Standards".
 *
 * Catalogs are optional for datasets but provide useful organizational
 * structure for managing large numbers of datasets.
 */
struct catalog final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    std::string tenant_id;

    /**
     * @brief Unique name identifying this catalog.
     *
     * This is the natural key for the catalog.
     * Examples: "ISO Standards", "Cryptocurrency", "FpML Standards".
     */
    std::string name;

    /**
     * @brief Human-readable description of the catalog's purpose.
     */
    std::string description;

    /**
     * @brief Optional owner or responsible party for this catalog.
     */
    std::optional<std::string> owner;

    /**
     * @brief Username of the person who last modified this catalog.
     */
    std::string recorded_by;

    /**
     * @brief Free-text commentary explaining the change.
     */
    std::string change_commentary;

    /**
     * @brief ID of the service account that performed this operation.
     *
     * Null when operation was performed directly by a user.
     * Set when operation was triggered by a service, algorithm, or LLM.
     * Contains the UUID as a string for serialization compatibility.
     */
    std::string performed_by;

    /**
     * @brief Timestamp when this version of the record was recorded.
     */
    std::chrono::system_clock::time_point recorded_at;
};

}

#endif
