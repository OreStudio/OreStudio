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
#ifndef ORES_DATABASE_SERVICE_TENANT_CONTEXT_HPP
#define ORES_DATABASE_SERVICE_TENANT_CONTEXT_HPP

#include <string>
#include "ores.database/domain/context.hpp"
#include "ores.utility/uuid/tenant_id.hpp"

namespace ores::database::service {

/**
 * @brief Manages tenant context for multi-tenant database operations.
 *
 * This class provides utilities for setting and managing the current tenant
 * context on database connections. In a multi-tenant environment, all database
 * operations must be performed within a tenant context to ensure proper
 * row-level security enforcement.
 */
class tenant_context final {
public:
    /**
     * @brief The UUID of the system tenant (max UUID per RFC 9562).
     *
     * Uses max UUID instead of nil UUID to prevent confusion with
     * uninitialized UUIDs (boost::uuids::uuid default-constructs to nil).
     */
    static constexpr const char* system_tenant_id =
        "ffffffff-ffff-ffff-ffff-ffffffffffff";

    tenant_context() = delete;

    /**
     * @brief Creates a new context with the specified tenant.
     *
     * Accepts either a tenant code (e.g., "system", "acme") or a tenant UUID.
     * If a code is provided, looks up the corresponding tenant_id from the
     * database. The returned context will set the session variable
     * `app.current_tenant_id` on connection acquisition, which is used by
     * row-level security policies.
     *
     * @param ctx The source context (provides pool and credentials).
     * @param tenant Tenant code or UUID string.
     * @return A new context configured for the specified tenant.
     * @throws std::runtime_error if tenant not found.
     */
    [[nodiscard]] static context with_tenant(const context& ctx,
        const std::string& tenant);

    /**
     * @brief Creates a new context with the system tenant.
     *
     * Convenience method that creates a context for the system tenant
     * (ffffffff-ffff-ffff-ffff-ffffffffffff, the max UUID).
     *
     * @param ctx The source context (provides pool and credentials).
     * @return A new context configured for the system tenant.
     */
    [[nodiscard]] static context with_system_tenant(const context& ctx);

    /**
     * @brief Looks up a tenant ID by its code.
     *
     * @param ctx The database context to query.
     * @param code The tenant code (e.g., "system", "acme").
     * @return The tenant ID.
     * @throws std::runtime_error if tenant not found.
     */
    static utility::uuid::tenant_id lookup_by_code(const context& ctx,
        const std::string& code);

    /**
     * @brief Looks up a tenant ID by its hostname.
     *
     * @param ctx The database context to query.
     * @param hostname The tenant hostname (e.g., "localhost", "acme.example.com").
     * @return The tenant ID.
     * @throws std::runtime_error if tenant not found.
     */
    static utility::uuid::tenant_id lookup_by_hostname(const context& ctx,
        const std::string& hostname);

    /**
     * @brief Looks up a tenant name by its ID.
     *
     * Returns "System" for the system tenant (max UUID), otherwise queries
     * the database for the tenant name.
     *
     * @param ctx The database context to query.
     * @param tenant_id The tenant ID.
     * @return The tenant name.
     * @throws std::runtime_error if tenant not found.
     */
    static std::string lookup_name(const context& ctx,
        const utility::uuid::tenant_id& tenant_id);

    /**
     * @brief Checks if a string is a valid UUID format.
     *
     * @param str The string to check.
     * @return true if the string matches UUID format, false otherwise.
     */
    static bool is_uuid(const std::string& str);
};

}

#endif
