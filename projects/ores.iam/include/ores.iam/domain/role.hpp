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
#ifndef ORES_IAM_DOMAIN_ROLE_HPP
#define ORES_IAM_DOMAIN_ROLE_HPP

#include <string>
#include <vector>
#include <boost/uuid/uuid.hpp>

namespace ores::iam::domain {

/**
 * @brief Represents a named collection of permissions that can be assigned
 * to accounts.
 *
 * Roles provide a way to group related permissions together for easier
 * management. For example, a "Trading" role might include permissions to
 * read and execute trades, while a "Support" role might have read-only
 * access to most resources.
 */
struct role final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Unique identifier for the role.
     */
    boost::uuids::uuid id;

    /**
     * @brief Unique name for the role (e.g., "Trading", "Sales", "Admin").
     */
    std::string name;

    /**
     * @brief Human-readable description of the role's purpose and scope.
     */
    std::string description;

    /**
     * @brief Username of the person who last modified this role.
     */
    std::string recorded_by;

    /**
     * @brief Timestamp when this version of the record was recorded.
     */
    std::string recorded_at;

    /**
     * @brief Permission codes granted to this role.
     *
     * This is a denormalized list of permission codes for convenience.
     * The authoritative source is the role_permissions junction table.
     */
    std::vector<std::string> permission_codes;
};

/**
 * @brief Well-known role names used throughout the system.
 */
namespace roles {
    constexpr auto admin = "Admin";
    constexpr auto trading = "Trading";
    constexpr auto sales = "Sales";
    constexpr auto operations = "Operations";
    constexpr auto support = "Support";
}

}

#endif
