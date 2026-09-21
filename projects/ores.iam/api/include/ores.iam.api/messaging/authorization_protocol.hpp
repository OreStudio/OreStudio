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
/**
 * AUTO-GENERATED FILE - DO NOT EDIT MANUALLY
 * Template: cpp_protocol.hpp.mustache
 * To modify, update the template and regenerate.
 */
#ifndef ORES_IAM_MESSAGING_AUTHORIZATION_PROTOCOL_HPP
#define ORES_IAM_MESSAGING_AUTHORIZATION_PROTOCOL_HPP

#include "ores.iam.api/domain/role.hpp"
#include <string>
#include <string_view>
#include <vector>

namespace ores::iam::messaging {

struct assign_role_request {
    using response_type = struct assign_role_response;
    static constexpr std::string_view nats_subject = "iam.v1.roles.assign";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
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
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::string principal;
    std::string role_name;
};

struct revoke_role_request {
    using response_type = struct revoke_role_response;
    static constexpr std::string_view nats_subject = "iam.v1.roles.revoke";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
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
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::string principal;
    std::string role_name;
};

struct get_account_roles_request {
    using response_type = struct get_account_roles_response;
    static constexpr std::string_view nats_subject = "iam.v1.roles.by-account";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::string account_id;
};

struct get_account_roles_response {
    std::vector<ores::iam::domain::role> roles;
};

struct get_account_permissions_request {
    using response_type = struct get_account_permissions_response;
    static constexpr std::string_view nats_subject = "iam.v1.roles.permissions-by-account";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::string account_id;
};

struct get_account_permissions_response {
    std::vector<std::string> permission_codes;
};

struct get_role_permissions_request {
    using response_type = struct get_role_permissions_response;
    static constexpr std::string_view nats_subject = "iam.v1.roles.permissions";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::string role_id;
};

struct get_role_permissions_response {
    std::vector<std::string> permission_codes;
};

struct suggest_role_commands_request {
    using response_type = struct suggest_role_commands_response;
    static constexpr std::string_view nats_subject = "iam.v1.roles.suggest-commands";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::string username;
    std::string tenant_id;
    std::string hostname;
};

struct suggest_role_commands_response {
    std::vector<std::string> commands;
};

}

#endif
