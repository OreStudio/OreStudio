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
#ifndef ORES_DQ_DOMAIN_SUBJECT_AREA_HPP
#define ORES_DQ_DOMAIN_SUBJECT_AREA_HPP

#include <chrono>
#include <optional>
#include <string>

namespace ores::dq::domain {

/**
 * @brief Represents a subdivision within a data domain.
 *
 * Subject areas provide finer-grained categorization within a data domain.
 * For example, within "Reference Data" domain, subject areas might include
 * "Currencies", "Countries", "Parties", etc.
 *
 * Subject areas are uniquely identified by the combination of their name
 * and the data domain they belong to (composite key).
 */
struct subject_area final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    std::string tenant_id;

    /**
     * @brief Name of this subject area within its domain.
     *
     * Part of the composite key (name + domain_name).
     * Examples: "Currencies", "Countries", "Parties".
     */
    std::string name;

    /**
     * @brief Name of the data domain this subject area belongs to.
     *
     * Part of the composite key. References data_domain.name (soft FK).
     */
    std::string domain_name;

    /**
     * @brief Human-readable description of the subject area's purpose.
     */
    std::string description;

    /**
     * @brief Username of the person who last modified this subject area.
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
