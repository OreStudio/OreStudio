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
#ifndef ORES_DQ_DOMAIN_CODING_SCHEME_HPP
#define ORES_DQ_DOMAIN_CODING_SCHEME_HPP

#include <chrono>
#include <optional>
#include <string>

namespace ores::dq::domain {

/**
 * @brief Defines a coding or identification standard used to identify entities.
 *
 * A scheme defines a coding or identification standard used to identify
 * entities such as parties, currencies, or other domain objects. Based on
 * the FPML coding-scheme concept.
 */
struct coding_scheme final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    std::string tenant_id;

    /**
     * @brief Unique code identifying this coding scheme.
     *
     * This is the natural key for the scheme.
     * Examples: "ISO_4217", "SWIFT_BIC", "LEI".
     */
    std::string code;

    /**
     * @brief Human-readable name for the coding scheme.
     */
    std::string name;

    /**
     * @brief Type of authority that maintains this scheme.
     *
     * Links to coding_scheme_authority_type for categorization.
     */
    std::string authority_type;

    /**
     * @brief Subject area this scheme belongs to.
     *
     * Links to subject_area for organizational structure.
     */
    std::string subject_area_name;

    /**
     * @brief Data domain this scheme applies to.
     *
     * Links to data_domain for domain categorization.
     */
    std::string domain_name;

    /**
     * @brief Optional URI pointing to the official scheme definition.
     */
    std::optional<std::string> uri;

    /**
     * @brief Human-readable description of the coding scheme's purpose.
     */
    std::string description;

    /**
     * @brief Username of the person who last modified this scheme.
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
    std::optional<std::string> performed_by;

    /**
     * @brief Timestamp when this version of the record was recorded.
     */
    std::chrono::system_clock::time_point recorded_at;
};

}

#endif
