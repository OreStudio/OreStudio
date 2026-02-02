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
#ifndef ORES_IAM_DOMAIN_TENANT_HPP
#define ORES_IAM_DOMAIN_TENANT_HPP

#include <chrono>
#include <string>
#include <optional>
#include <boost/uuid/uuid.hpp>

namespace ores::iam::domain {

/**
 * @brief A tenant representing an isolated organisation or the system platform.
 *
 * Core entity for multi-tenancy support. Each tenant represents an isolated
 * organisation with its own users, roles, and data. The system tenant (UUID all zeros)
 * is a special tenant used for shared reference data and system administration.
 * 
 * Tenants are identified by:
 * - tenant_id: UUID primary key
 * - code: Unique text code for stable referencing (e.g., 'system', 'acme')
 * - hostname: Unique hostname for tenant routing during login
 */
struct tenant final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief UUID uniquely identifying this tenant.
     *
     * The system tenant has UUID 00000000-0000-0000-0000-000000000000.
     */
    boost::uuids::uuid tenant_id;

    /**
     * @brief Unique code for stable referencing.
     *
     * Examples: 'system', 'acme', 'demo'.
     */
    std::string code;

    /**
     * @brief Human-readable name for the tenant.
     */
    std::string name;

    /**
     * @brief Tenant type classification (FK to tenant_types).
     */
    std::string type;

    /**
     * @brief Detailed description of the tenant.
     */
    std::string description;

    /**
     * @brief Unique hostname for tenant routing.
     */
    std::string hostname;

    /**
     * @brief Tenant lifecycle status (FK to tenant_statuses).
     */
    std::string status;

    /**
     * @brief Username of the person who last modified this tenant.
     */
    std::string recorded_by;

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
