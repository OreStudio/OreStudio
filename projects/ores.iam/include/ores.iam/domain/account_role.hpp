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
#ifndef ORES_ACCOUNTS_DOMAIN_ACCOUNT_ROLE_HPP
#define ORES_ACCOUNTS_DOMAIN_ACCOUNT_ROLE_HPP

#include <string>
#include <chrono>
#include <boost/uuid/uuid.hpp>

namespace ores::iam::domain {

/**
 * @brief Represents the assignment of a role to an account.
 *
 * This is a junction entity that links accounts to roles, supporting
 * many-to-many relationships. An account can have multiple roles, and
 * a role can be assigned to multiple accounts.
 */
struct account_role final {
    /**
     * @brief The account to which the role is assigned.
     */
    boost::uuids::uuid account_id;

    /**
     * @brief The role being assigned to the account.
     */
    boost::uuids::uuid role_id;

    /**
     * @brief Username of the person who assigned this role.
     */
    std::string assigned_by;

    /**
     * @brief Timestamp when this role assignment was created.
     */
    std::string assigned_at;
};

}

#endif
