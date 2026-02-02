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
     * @brief The UUID of the system tenant (all zeros).
     */
    static constexpr const char* system_tenant_id =
        "00000000-0000-0000-0000-000000000000";

    tenant_context() = delete;

    /**
     * @brief Sets the tenant context on a database connection.
     *
     * Accepts either a tenant code (e.g., "system", "acme") or a tenant UUID.
     * If a code is provided, looks up the corresponding tenant_id from the
     * database. Sets the session variable `app.current_tenant_id` which is
     * used by row-level security policies.
     *
     * @param ctx The database context to set tenant on.
     * @param tenant Tenant code or UUID string.
     * @throws std::runtime_error if tenant not found or context cannot be set.
     */
    static void set(context& ctx, const std::string& tenant);

    /**
     * @brief Sets the system tenant context on a database connection.
     *
     * Convenience method that sets the context to the system tenant
     * (00000000-0000-0000-0000-000000000000).
     *
     * @param ctx The database context to set tenant on.
     */
    static void set_system_tenant(context& ctx);

    /**
     * @brief Looks up a tenant ID by its code.
     *
     * @param ctx The database context to query.
     * @param code The tenant code (e.g., "system", "acme").
     * @return The tenant UUID as a string.
     * @throws std::runtime_error if tenant not found.
     */
    static std::string lookup_by_code(context& ctx, const std::string& code);

    /**
     * @brief Looks up a tenant ID by its hostname.
     *
     * @param ctx The database context to query.
     * @param hostname The tenant hostname (e.g., "localhost", "acme.example.com").
     * @return The tenant UUID as a string.
     * @throws std::runtime_error if tenant not found.
     */
    static std::string lookup_by_hostname(context& ctx, const std::string& hostname);

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
