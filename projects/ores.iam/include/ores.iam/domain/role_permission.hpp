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
#ifndef ORES_ACCOUNTS_DOMAIN_ROLE_PERMISSION_HPP
#define ORES_ACCOUNTS_DOMAIN_ROLE_PERMISSION_HPP

#include <boost/uuid/uuid.hpp>

namespace ores::iam::domain {

/**
 * @brief Represents the assignment of a permission to a role.
 *
 * This is a junction entity that links roles to permissions, supporting
 * many-to-many relationships. A role can have multiple permissions, and
 * a permission can be assigned to multiple roles.
 */
struct role_permission final {
    /**
     * @brief The role to which the permission is granted.
     */
    boost::uuids::uuid role_id;

    /**
     * @brief The permission being granted to the role.
     */
    boost::uuids::uuid permission_id;
};

}

#endif
