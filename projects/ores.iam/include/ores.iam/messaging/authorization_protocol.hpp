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
#ifndef ORES_IAM_MESSAGING_AUTHORIZATION_PROTOCOL_HPP
#define ORES_IAM_MESSAGING_AUTHORIZATION_PROTOCOL_HPP

#include <optional>
#include <string>
#include <vector>
#include <boost/uuid/uuid.hpp>
#include "ores.iam/domain/role.hpp"
#include "ores.iam/domain/permission.hpp"

namespace ores::iam::messaging {

struct list_roles_request {};

struct list_roles_response {
    std::vector<ores::iam::domain::role> roles;
};

struct list_permissions_request {};

struct list_permissions_response {
    std::vector<ores::iam::domain::permission> permissions;
};

struct get_role_request {
    std::string identifier;
};

struct get_role_response {
    bool found = false;
    std::optional<ores::iam::domain::role> role;
    std::string error_message;
};

struct assign_role_request {
    boost::uuids::uuid account_id;
    boost::uuids::uuid role_id;
};

struct assign_role_response {
    bool success = false;
    std::string error_message;
};

struct assign_role_by_name_request {
    std::string principal;
    std::string role_name;
};

struct revoke_role_request {
    boost::uuids::uuid account_id;
    boost::uuids::uuid role_id;
};

struct revoke_role_response {
    bool success = false;
    std::string error_message;
};

struct revoke_role_by_name_request {
    std::string principal;
    std::string role_name;
};

struct get_account_roles_request {
    boost::uuids::uuid account_id;
};

struct get_account_roles_response {
    std::vector<ores::iam::domain::role> roles;
};

struct get_account_permissions_request {
    boost::uuids::uuid account_id;
};

struct get_account_permissions_response {
    std::vector<std::string> permission_codes;
};

struct suggest_role_commands_request {
    std::string username;
    std::string tenant_id;
    std::string hostname;
};

struct suggest_role_commands_response {
    std::vector<std::string> commands;
};

}

#endif
