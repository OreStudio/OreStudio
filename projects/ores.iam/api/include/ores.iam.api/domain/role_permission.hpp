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
#ifndef ORES_IAM_DOMAIN_ROLE_PERMISSION_HPP
#define ORES_IAM_DOMAIN_ROLE_PERMISSION_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <string>

namespace ores::iam::domain {

/**
 * @brief Represents the assignment of a permission to a role.
 *
 * This is a junction entity that links roles to permissions, supporting
 * many-to-many relationships. A role can have multiple permissions, and
 * a permission can be assigned to multiple roles.
 *
 * The tail records who wrote the link, when and why, the same as an
 * account-role assignment. The store stamps =assigned_at= and the actor; the
 * writer states the reason and commentary.
 */
struct role_permission final {
    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief The role to which the permission is granted.
     */
    boost::uuids::uuid role_id;

    /**
     * @brief The permission being granted to the role.
     */
    boost::uuids::uuid permission_id;

    /**
     * @brief The account that wrote the link, by username.
     */
    std::string assigned_by;

    /**
     * @brief When the store wrote the link.
     */
    std::chrono::system_clock::time_point assigned_at;

    /**
     * @brief Why the link was written.
     */
    std::string change_reason_code;

    /**
     * @brief The sentence that goes with the reason.
     */
    std::string change_commentary;
};

}

#endif
