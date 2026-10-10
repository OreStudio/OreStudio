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
#ifndef ORES_IAM_API_MESSAGING_AUTHORIZATION_PROTOCOL_HPP
#define ORES_IAM_API_MESSAGING_AUTHORIZATION_PROTOCOL_HPP

#include "ores.iam.api/domain/role.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <chrono>
#include <string>
#include <string_view>
#include <vector>

namespace ores::iam::messaging {

struct assign_role_request {
    using response_type = struct assign_role_response;
    static constexpr std::string_view nats_subject = "iam.v1.ops.assign_role";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::string account_id;
    std::string role_id;
    std::string change_reason_code;
    std::string change_commentary;
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
    static constexpr std::string_view nats_subject = "iam.v1.ops.assign_role_by_name";
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
    static constexpr std::string_view nats_subject = "iam.v1.ops.revoke_role";
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
    static constexpr std::string_view nats_subject = "iam.v1.ops.revoke_role_by_name";
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
    static constexpr std::string_view nats_subject = "iam.v1.ops.get_account_roles";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::string account_id;
};

struct get_my_roles_request {
    using response_type = struct get_account_roles_response;
    static constexpr std::string_view nats_subject = "iam.v1.ops.get_my_roles";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
};

/**
 * @brief One resource of the permission catalogue and what an account holds of it.
 *
 * The unit of a page. A screen draws a resource as a row with its actions as
 * columns, so a page holds whole rows and never half of one. =actions= are the
 * actions the catalogue defines for the resource, =held= the ones the account's
 * roles grant, and =roles= the roles that grant any of them, so a screen can
 * say why without reading the roles again.
 */
struct permission_resource_row {
    std::string component;
    std::string resource;
    std::vector<std::string> actions;
    std::vector<std::string> held;
    std::vector<std::string> roles;
};

/**
 * @brief One area (a component) in which an account holds something, and how
 * many of its resources they hold.
 *
 * The areas a screen offers to choose from. An area the account holds nothing
 * in is not listed, so there is nothing to choose that would show an empty page.
 */
struct permission_area_count {
    std::string component;
    int resources = 0;
};

/**
 * @brief One page of what an account's roles let it do, by resource.
 *
 * The caller needs iam::roles:read, as reading the account's roles does. The
 * page is the server's: =offset= and =limit= bound the rows, =area= narrows
 * them to one component, and =search= to a resource or component name. An empty
 * =area= means the first area the account holds something in, so a page is
 * always of one area.
 */
struct list_account_permissions_request {
    using response_type = struct permission_page_response;
    static constexpr std::string_view nats_subject = "iam.v1.ops.list_account_permissions";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::string account_id;
    std::string area;
    std::string search;
    int offset = 0;
    int limit = 15;
};

/**
 * @brief One page of what the caller's own roles let them do, by resource.
 *
 * The session names the account, so the request names none and the read needs
 * no permission: it is a self read on the allow-list of Authorised reads. The
 * paging fields are those of iam.v1.ops.list_account_permissions.
 */
struct list_my_permissions_request {
    using response_type = struct permission_page_response;
    static constexpr std::string_view nats_subject = "iam.v1.ops.list_my_permissions";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::string area;
    std::string search;
    int offset = 0;
    int limit = 15;
};

struct permission_page_response {
    ores::utility::domain::result result;
    /**
     * @brief The area the rows belong to: the one asked for, or the first the
     * account holds something in when none was.
     */
    std::string area;
    std::vector<permission_resource_row> rows;
    /**
     * @brief How many rows match the area and the search, not how many this page
     * holds.
     */
    int total_count = 0;
    std::vector<permission_area_count> areas;
    /**
     * @brief Whether the chosen area is granted whole, by =component::*= or by
     * everything, so a screen can say so and not tick every row.
     */
    bool area_whole = false;
    /**
     * @brief Whether the roles grant everything (=*=).
     */
    bool everything = false;
};

/**
 * @brief One page of the permission catalogue against what one role grants.
 *
 * It serves the role editor, which must offer every permission to tick, not
 * only the ones the role holds, so =include_unheld= asks for the rows the role
 * does not grant too. The paging fields are those of
 * iam.v1.ops.list_account_permissions. The caller needs iam::roles:read.
 */
struct list_role_permissions_request {
    using response_type = struct permission_page_response;
    static constexpr std::string_view nats_subject = "iam.v1.ops.list_role_permissions";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::string role_id;
    std::string area;
    std::string search;
    bool include_unheld = false;
    int offset = 0;
    int limit = 15;
};

/**
 * @brief One role as the roles list draws it.
 *
 * The permissions are counted, not listed: the list says how much a role lets
 * people do, and the role's own page pages what it lets them do. A service
 * role's count is zero, because it is not given to people and is not changed
 * from a screen.
 */
struct role_page_row {
    std::string id;
    int version = 0;
    std::string name;
    std::string description;
    bool service = false;
    bool registration_default = false;
    bool requestable = false;
    int permission_count = 0;
    bool everything = false;
};

/**
 * @brief One page of the tenant's roles.
 *
 * =search= matches a role's name or description, =area= keeps the roles that
 * grant something in that area, and =include_service= brings in the platform's
 * own service roles, which are left out otherwise. A =role_id= names one role
 * and ignores the rest, so the role's own page reads its header the same way.
 * The caller needs iam::roles:read.
 */
struct list_roles_page_request {
    using response_type = struct role_page_response;
    static constexpr std::string_view nats_subject = "iam.v1.ops.list_roles_page";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::string role_id;
    std::string search;
    std::string area;
    bool include_service = false;
    int offset = 0;
    int limit = 15;
};

struct role_page_response {
    ores::utility::domain::result result;
    std::vector<role_page_row> roles;
    int total_count = 0;
    /**
     * @brief How many service roles were left out, so the list can say so.
     */
    int service_hidden = 0;
    /**
     * @brief The areas some listed role grants something in, with how many roles do.
     * An area no role grants is not offered, so choosing one never shows an empty list.
     */
    std::vector<permission_area_count> areas;
};

/**
 * @brief One person who holds a role, as the role's page draws them.
 *
 * Carries what a screen needs to name and show the person and say who gave the
 * role, so the page makes no second read per person. The picture is the
 * identifier of the account's image, empty when it has none.
 */
struct role_holder {
    std::string account_id;
    std::string username;
    std::string full_name;
    std::string image_id;
    std::string assigned_by;
};

/**
 * @brief One page of the people who hold a role, by name.
 *
 * The caller needs iam::roles:read, as reading who holds a role does. The page
 * is the server's: =offset= and =limit= bound the rows and =total_count= says
 * how many people hold the role.
 */
struct list_role_holders_request {
    using response_type = struct role_holders_response;
    static constexpr std::string_view nats_subject = "iam.v1.ops.list_role_holders";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::string role_id;
    int offset = 0;
    int limit = 15;
};

struct role_holders_response {
    ores::utility::domain::result result;
    std::vector<role_holder> holders;
    int total_count = 0;
};

/**
 * @brief One role an account holds, with the permissions it grants and the
 * record of its assignment.
 *
 * The element both access reads answer with, so the member's screen and the
 * administrator's render one type. The permissions are attributed to the
 * role rather than flattened into one list, and the tail is the junction
 * row's own record of who granted the role, when and why.
 */
struct account_role_access {
    ores::iam::domain::role role;
    std::vector<std::string> permission_codes;
    std::string assigned_by;
    std::chrono::system_clock::time_point assigned_at;
    std::string change_reason_code;
    std::string change_commentary;
};

struct get_account_roles_response {
    ores::utility::domain::result result;
    std::vector<account_role_access> roles;
};

struct get_role_permissions_request {
    using response_type = struct get_role_permissions_response;
    static constexpr std::string_view nats_subject = "iam.v1.ops.get_role_permissions";
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
    ores::utility::domain::result result;
    std::vector<std::string> permission_codes;
};

/**
 * @brief Replaces the permissions a role bundles.
 *
 * The codes are the whole bundle the role should carry, not an increment: a
 * code the role bundles and this request omits is removed, and a code both
 * name stays. The write answers with the bundle as stored, so the caller
 * reads back what it wrote rather than assuming the two agree.
 *
 * The change reason and commentary are the junction row's own record of who
 * changed the bundle, when and why; the actor is stamped from the request.
 */
struct put_role_permissions_request {
    using response_type = struct get_role_permissions_response;
    static constexpr std::string_view nats_subject = "iam.v1.roles_permissions.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::string role_id;
    std::vector<std::string> permission_codes;
    std::string change_reason_code;
    std::string change_commentary;
};

struct suggest_role_commands_request {
    using response_type = struct suggest_role_commands_response;
    static constexpr std::string_view nats_subject = "iam.v1.ops.suggest_role_commands";
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
