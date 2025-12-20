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
#ifndef ORES_ACCOUNTS_REPOSITORY_ACCOUNT_ROLE_ENTITY_HPP
#define ORES_ACCOUNTS_REPOSITORY_ACCOUNT_ROLE_ENTITY_HPP

#include <string>
#include "sqlgen/Timestamp.hpp"

namespace ores::iam::repository {

/**
 * @brief Represents an account-role assignment in the database.
 *
 * This is a junction table linking accounts to roles.
 * Note: The composite primary key (account_id, role_id, valid_from) is defined
 * at the database schema level since sqlgen doesn't support composite keys.
 */
struct account_role_entity {
    constexpr static const char* schema = "oresdb";
    constexpr static const char* tablename = "account_roles";

    std::string account_id;
    std::string role_id;
    std::string assigned_by;
    sqlgen::Timestamp<"%Y-%m-%d %H:%M:%S"> assigned_at = "9999-12-31 23:59:59";
    sqlgen::Timestamp<"%Y-%m-%d %H:%M:%S"> valid_from = "9999-12-31 23:59:59";
    sqlgen::Timestamp<"%Y-%m-%d %H:%M:%S"> valid_to = "9999-12-31 23:59:59";
};

std::ostream& operator<<(std::ostream& s, const account_role_entity& v);

}

#endif
