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
#ifndef ORES_IAM_DOMAIN_PERMISSION_HPP
#define ORES_IAM_DOMAIN_PERMISSION_HPP

#include <string>
#include <boost/uuid/uuid.hpp>

namespace ores::iam::domain {

/**
 * @brief Represents an atomic permission that can be granted to roles.
 *
 * Permissions follow a hierarchical naming convention using colons as
 * separators: "resource:action" (e.g., "accounts:create", "currencies:read").
 * The special code "*" represents all permissions (superuser).
 */
struct permission final {
    /**
     * @brief Unique identifier for the permission.
     */
    boost::uuids::uuid id;

    /**
     * @brief Permission code following the format "resource:action".
     *
     * Examples: "accounts:create", "currencies:read", "flags:update".
     * Use "*" for wildcard (all permissions).
     */
    std::string code;

    /**
     * @brief Human-readable description of what this permission allows.
     */
    std::string description;
};

/**
 * @brief Well-known permission codes used throughout the system.
 */
namespace permissions {
    // Account management
    constexpr auto accounts_create = "accounts:create";
    constexpr auto accounts_read = "accounts:read";
    constexpr auto accounts_update = "accounts:update";
    constexpr auto accounts_delete = "accounts:delete";
    constexpr auto accounts_lock = "accounts:lock";
    constexpr auto accounts_unlock = "accounts:unlock";
    constexpr auto accounts_reset_password = "accounts:reset_password";

    // Currency management
    constexpr auto currencies_create = "currencies:create";
    constexpr auto currencies_read = "currencies:read";
    constexpr auto currencies_update = "currencies:update";
    constexpr auto currencies_delete = "currencies:delete";
    constexpr auto currencies_history = "currencies:history";

    // Feature flags management
    constexpr auto flags_create = "flags:create";
    constexpr auto flags_read = "flags:read";
    constexpr auto flags_update = "flags:update";
    constexpr auto flags_delete = "flags:delete";

    // Login info (read-only audit data)
    constexpr auto login_info_read = "login_info:read";

    // Roles management
    constexpr auto roles_create = "roles:create";
    constexpr auto roles_read = "roles:read";
    constexpr auto roles_update = "roles:update";
    constexpr auto roles_delete = "roles:delete";
    constexpr auto roles_assign = "roles:assign";
    constexpr auto roles_revoke = "roles:revoke";

    // Wildcard - grants all permissions
    constexpr auto all = "*";
}

}

#endif
