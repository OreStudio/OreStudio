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
#include <string_view>
#include <vector>
#include "ores.iam.api/domain/role.hpp"
#include "ores.iam.api/domain/permission.hpp"

namespace ores::iam::messaging {

struct list_roles_request {
    using response_type = struct list_roles_response;
    static constexpr std::string_view nats_subject = "iam.v1.roles.list";
};

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
    using response_type = struct assign_role_response;
    static constexpr std::string_view nats_subject = "iam.v1.roles.assign";
    std::string account_id;
    std::string role_id;
};

struct assign_role_response {
    bool success = false;
    std::string error_message;
};

struct assign_role_by_name_response {
    bool success = false;
    std::string error_message;
};

struct assign_role_by_name_request {
    using response_type = struct assign_role_by_name_response;
    static constexpr std::string_view nats_subject = "iam.v1.roles.assign-by-name";
    std::string principal;
    std::string role_name;
};

struct revoke_role_request {
    using response_type = struct revoke_role_response;
    static constexpr std::string_view nats_subject = "iam.v1.roles.revoke";
    std::string account_id;
    std::string role_id;
};

struct revoke_role_response {
    bool success = false;
    std::string error_message;
};

struct revoke_role_by_name_response {
    bool success = false;
    std::string error_message;
};

struct revoke_role_by_name_request {
    using response_type = struct revoke_role_by_name_response;
    static constexpr std::string_view nats_subject = "iam.v1.roles.revoke-by-name";
    std::string principal;
    std::string role_name;
};

struct get_account_roles_request {
    using response_type = struct get_account_roles_response;
    static constexpr std::string_view nats_subject = "iam.v1.roles.by-account";
    std::string account_id;
};

struct get_account_roles_response {
    std::vector<ores::iam::domain::role> roles;
};

struct get_account_permissions_request {
    std::string account_id;
};

struct get_account_permissions_response {
    std::vector<std::string> permission_codes;
};

struct suggest_role_commands_request {
    using response_type = struct suggest_role_commands_response;
    static constexpr std::string_view nats_subject = "iam.v1.roles.suggest-commands";
    std::string username;
    std::string tenant_id;
    std::string hostname;
};

struct suggest_role_commands_response {
    std::vector<std::string> commands;
};

}

#endif
