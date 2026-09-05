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
#ifndef ORES_IAM_API_DOMAIN_TENANT_HPP
#define ORES_IAM_API_DOMAIN_TENANT_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <optional>
#include <string>
#include <string_view>

namespace ores::iam::domain {

/**
 * @brief A tenant representing an isolated organisation or the system platform.
 *
 * Core entity for multi-tenancy support. Each tenant represents an isolated
 * organisation with its own users, roles, and data. The system tenant (UUID all zeros)
 * is a special tenant used for shared reference data and system administration.
 *
 * Tenants are identified by:
 * - id: UUID primary key (SQL also has tenant_id = id for self-reference)
 * - code: Unique text code for stable referencing (e.g., 'system', 'acme')
 * - hostname: Unique hostname for tenant routing during login
 */
struct tenant final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief UUID uniquely identifying this tenant.
     *
     * The system tenant has UUID 00000000-0000-0000-0000-000000000000. In SQL, tenant_id = id for
     * tenant records.
     */
    boost::uuids::uuid id;

    /**
     * @brief Unique code for stable referencing.
     *
     * Examples: 'system', 'acme', 'demo'.
     */
    std::string code;

    /**
     * @brief Human-readable display name for the tenant.
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

/**
 * @brief Dispatch-key identifier for tenant, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const tenant&) {
    return "ores.iam.tenant";
}

}

#endif
