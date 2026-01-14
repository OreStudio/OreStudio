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
#ifndef ORES_IAM_REPOSITORY_ROLE_PERMISSION_ENTITY_HPP
#define ORES_IAM_REPOSITORY_ROLE_PERMISSION_ENTITY_HPP

#include <string>
#include "sqlgen/Timestamp.hpp"

namespace ores::iam::repository {

/**
 * @brief Represents a role-permission assignment in the database.
 *
 * This is a junction table linking roles to permissions in a many-to-many
 * relationship. A role can have multiple permissions, and a permission can
 * be assigned to multiple roles.
 *
 * Note: The actual primary key is a composite key (role_id, permission_id,
 * valid_from) defined at the database schema level. sqlgen does not support
 * composite primary keys, so this entity omits the PrimaryKey wrapper.
 */
struct role_permission_entity {
    constexpr static const char* schema = "ores";
    constexpr static const char* tablename = "iam_role_permissions_tbl";

    std::string role_id;
    std::string permission_id;
    sqlgen::Timestamp<"%Y-%m-%d %H:%M:%S"> valid_from = "9999-12-31 23:59:59";
    sqlgen::Timestamp<"%Y-%m-%d %H:%M:%S"> valid_to = "9999-12-31 23:59:59";
};

std::ostream& operator<<(std::ostream& s, const role_permission_entity& v);

}

#endif
