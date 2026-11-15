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
#include "ores.iam.api/export.hpp"
#include "ores.utility/codec/wire_fields.hpp"
#include "ores.utility/codec/wire_traits_fwd.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <chrono>
#include <cstddef>
#include <span>
#include <string>
#include <string_view>
#include <tuple>
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

    friend bool operator==(const assign_role_request&, const assign_role_request&) = default;
};

struct assign_role_response {
    bool success = false;
    std::string error_message;

    friend bool operator==(const assign_role_response&, const assign_role_response&) = default;
};

struct assign_role_by_name_response {
    bool success = false;
    std::string error_message;

    friend bool operator==(const assign_role_by_name_response&,
                           const assign_role_by_name_response&) = default;
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

    friend bool operator==(const assign_role_by_name_request&,
                           const assign_role_by_name_request&) = default;
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

    friend bool operator==(const revoke_role_request&, const revoke_role_request&) = default;
};

struct revoke_role_response {
    bool success = false;
    std::string error_message;

    friend bool operator==(const revoke_role_response&, const revoke_role_response&) = default;
};

struct revoke_role_by_name_response {
    bool success = false;
    std::string error_message;

    friend bool operator==(const revoke_role_by_name_response&,
                           const revoke_role_by_name_response&) = default;
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

    friend bool operator==(const revoke_role_by_name_request&,
                           const revoke_role_by_name_request&) = default;
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

    friend bool operator==(const get_account_roles_request&,
                           const get_account_roles_request&) = default;
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

    friend bool operator==(const get_my_roles_request&, const get_my_roles_request&) = default;
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

    friend bool operator==(const permission_resource_row&,
                           const permission_resource_row&) = default;
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

    friend bool operator==(const permission_area_count&, const permission_area_count&) = default;
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

    friend bool operator==(const list_account_permissions_request&,
                           const list_account_permissions_request&) = default;
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

    friend bool operator==(const list_my_permissions_request&,
                           const list_my_permissions_request&) = default;
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

    friend bool operator==(const permission_page_response&,
                           const permission_page_response&) = default;
};

/**
 * @brief One page of the permission catalogue against what one role grants.
 *
 * It serves the role editor, which must offer every permission to tick, not
 * only the ones the role holds, so =include_unheld= asks for the rows the role
 * does not grant too. The paging fields are those of
 * iam.v1.ops.list_account_permissions. The caller needs iam::roles:read.
 */
struct list_role_permissions_page_request {
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

    friend bool operator==(const list_role_permissions_page_request&,
                           const list_role_permissions_page_request&) = default;
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

    friend bool operator==(const role_page_row&, const role_page_row&) = default;
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

    friend bool operator==(const list_roles_page_request&,
                           const list_roles_page_request&) = default;
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

    friend bool operator==(const role_page_response&, const role_page_response&) = default;
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

    friend bool operator==(const role_holder&, const role_holder&) = default;
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

    friend bool operator==(const list_role_holders_request&,
                           const list_role_holders_request&) = default;
};

struct role_holders_response {
    ores::utility::domain::result result;
    std::vector<role_holder> holders;
    int total_count = 0;

    friend bool operator==(const role_holders_response&, const role_holders_response&) = default;
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

    friend bool operator==(const account_role_access&, const account_role_access&) = default;
};

struct get_account_roles_response {
    ores::utility::domain::result result;
    std::vector<account_role_access> roles;

    friend bool operator==(const get_account_roles_response&,
                           const get_account_roles_response&) = default;
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

    friend bool operator==(const get_role_permissions_request&,
                           const get_role_permissions_request&) = default;
};

struct get_role_permissions_response {
    ores::utility::domain::result result;
    std::vector<std::string> permission_codes;

    friend bool operator==(const get_role_permissions_response&,
                           const get_role_permissions_response&) = default;
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

    friend bool operator==(const put_role_permissions_request&,
                           const put_role_permissions_request&) = default;
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

    friend bool operator==(const suggest_role_commands_request&,
                           const suggest_role_commands_request&) = default;
};

struct suggest_role_commands_response {
    std::vector<std::string> commands;

    friend bool operator==(const suggest_role_commands_response&,
                           const suggest_role_commands_response&) = default;
};

/**
 * @brief The members of assign_role_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<assign_role_request>) {
    return ::ores::utility::codec::field_list{
        ::ores::utility::codec::field{"account_id", &assign_role_request::account_id},
        ::ores::utility::codec::field{"role_id", &assign_role_request::role_id},
        ::ores::utility::codec::field{"change_reason_code",
                                      &assign_role_request::change_reason_code},
        ::ores::utility::codec::field{"change_commentary", &assign_role_request::change_commentary},
    };
}

/**
 * @brief The members of assign_role_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<assign_role_response>) {
    return ::ores::utility::codec::field_list{
        ::ores::utility::codec::field{"success", &assign_role_response::success},
        ::ores::utility::codec::field{"error_message", &assign_role_response::error_message},
    };
}

/**
 * @brief The members of assign_role_by_name_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<assign_role_by_name_response>) {
    return ::ores::utility::codec::field_list{
        ::ores::utility::codec::field{"success", &assign_role_by_name_response::success},
        ::ores::utility::codec::field{"error_message",
                                      &assign_role_by_name_response::error_message},
    };
}

/**
 * @brief The members of assign_role_by_name_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<assign_role_by_name_request>) {
    return ::ores::utility::codec::field_list{
        ::ores::utility::codec::field{"principal", &assign_role_by_name_request::principal},
        ::ores::utility::codec::field{"role_name", &assign_role_by_name_request::role_name},
    };
}

/**
 * @brief The members of revoke_role_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<revoke_role_request>) {
    return ::ores::utility::codec::field_list{
        ::ores::utility::codec::field{"account_id", &revoke_role_request::account_id},
        ::ores::utility::codec::field{"role_id", &revoke_role_request::role_id},
    };
}

/**
 * @brief The members of revoke_role_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<revoke_role_response>) {
    return ::ores::utility::codec::field_list{
        ::ores::utility::codec::field{"success", &revoke_role_response::success},
        ::ores::utility::codec::field{"error_message", &revoke_role_response::error_message},
    };
}

/**
 * @brief The members of revoke_role_by_name_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<revoke_role_by_name_response>) {
    return ::ores::utility::codec::field_list{
        ::ores::utility::codec::field{"success", &revoke_role_by_name_response::success},
        ::ores::utility::codec::field{"error_message",
                                      &revoke_role_by_name_response::error_message},
    };
}

/**
 * @brief The members of revoke_role_by_name_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<revoke_role_by_name_request>) {
    return ::ores::utility::codec::field_list{
        ::ores::utility::codec::field{"principal", &revoke_role_by_name_request::principal},
        ::ores::utility::codec::field{"role_name", &revoke_role_by_name_request::role_name},
    };
}

/**
 * @brief The members of get_account_roles_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_account_roles_request>) {
    return ::ores::utility::codec::field_list{
        ::ores::utility::codec::field{"account_id", &get_account_roles_request::account_id},
    };
}

/**
 * @brief The members of get_my_roles_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_my_roles_request>) {
    return ::ores::utility::codec::field_list{};
}

/**
 * @brief The members of permission_resource_row, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<permission_resource_row>) {
    return ::ores::utility::codec::field_list{
        ::ores::utility::codec::field{"component", &permission_resource_row::component},
        ::ores::utility::codec::field{"resource", &permission_resource_row::resource},
        ::ores::utility::codec::field{"actions", &permission_resource_row::actions},
        ::ores::utility::codec::field{"held", &permission_resource_row::held},
        ::ores::utility::codec::field{"roles", &permission_resource_row::roles},
    };
}

/**
 * @brief The members of permission_area_count, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<permission_area_count>) {
    return ::ores::utility::codec::field_list{
        ::ores::utility::codec::field{"component", &permission_area_count::component},
        ::ores::utility::codec::field{"resources", &permission_area_count::resources},
    };
}

/**
 * @brief The members of list_account_permissions_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<list_account_permissions_request>) {
    return ::ores::utility::codec::field_list{
        ::ores::utility::codec::field{"account_id", &list_account_permissions_request::account_id},
        ::ores::utility::codec::field{"area", &list_account_permissions_request::area},
        ::ores::utility::codec::field{"search", &list_account_permissions_request::search},
        ::ores::utility::codec::field{"offset", &list_account_permissions_request::offset},
        ::ores::utility::codec::field{"limit", &list_account_permissions_request::limit},
    };
}

/**
 * @brief The members of list_my_permissions_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<list_my_permissions_request>) {
    return ::ores::utility::codec::field_list{
        ::ores::utility::codec::field{"area", &list_my_permissions_request::area},
        ::ores::utility::codec::field{"search", &list_my_permissions_request::search},
        ::ores::utility::codec::field{"offset", &list_my_permissions_request::offset},
        ::ores::utility::codec::field{"limit", &list_my_permissions_request::limit},
    };
}

/**
 * @brief The members of permission_page_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<permission_page_response>) {
    return ::ores::utility::codec::field_list{
        ::ores::utility::codec::field{"result", &permission_page_response::result},
        ::ores::utility::codec::field{"area", &permission_page_response::area},
        ::ores::utility::codec::field{"rows", &permission_page_response::rows},
        ::ores::utility::codec::field{"total_count", &permission_page_response::total_count},
        ::ores::utility::codec::field{"areas", &permission_page_response::areas},
        ::ores::utility::codec::field{"area_whole", &permission_page_response::area_whole},
        ::ores::utility::codec::field{"everything", &permission_page_response::everything},
    };
}

/**
 * @brief The members of list_role_permissions_page_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<list_role_permissions_page_request>) {
    return ::ores::utility::codec::field_list{
        ::ores::utility::codec::field{"role_id", &list_role_permissions_page_request::role_id},
        ::ores::utility::codec::field{"area", &list_role_permissions_page_request::area},
        ::ores::utility::codec::field{"search", &list_role_permissions_page_request::search},
        ::ores::utility::codec::field{"include_unheld",
                                      &list_role_permissions_page_request::include_unheld},
        ::ores::utility::codec::field{"offset", &list_role_permissions_page_request::offset},
        ::ores::utility::codec::field{"limit", &list_role_permissions_page_request::limit},
    };
}

/**
 * @brief The members of role_page_row, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<role_page_row>) {
    return ::ores::utility::codec::field_list{
        ::ores::utility::codec::field{"id", &role_page_row::id},
        ::ores::utility::codec::field{"version", &role_page_row::version},
        ::ores::utility::codec::field{"name", &role_page_row::name},
        ::ores::utility::codec::field{"description", &role_page_row::description},
        ::ores::utility::codec::field{"service", &role_page_row::service},
        ::ores::utility::codec::field{"registration_default", &role_page_row::registration_default},
        ::ores::utility::codec::field{"requestable", &role_page_row::requestable},
        ::ores::utility::codec::field{"permission_count", &role_page_row::permission_count},
        ::ores::utility::codec::field{"everything", &role_page_row::everything},
    };
}

/**
 * @brief The members of list_roles_page_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<list_roles_page_request>) {
    return ::ores::utility::codec::field_list{
        ::ores::utility::codec::field{"role_id", &list_roles_page_request::role_id},
        ::ores::utility::codec::field{"search", &list_roles_page_request::search},
        ::ores::utility::codec::field{"area", &list_roles_page_request::area},
        ::ores::utility::codec::field{"include_service", &list_roles_page_request::include_service},
        ::ores::utility::codec::field{"offset", &list_roles_page_request::offset},
        ::ores::utility::codec::field{"limit", &list_roles_page_request::limit},
    };
}

/**
 * @brief The members of role_page_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<role_page_response>) {
    return ::ores::utility::codec::field_list{
        ::ores::utility::codec::field{"result", &role_page_response::result},
        ::ores::utility::codec::field{"roles", &role_page_response::roles},
        ::ores::utility::codec::field{"total_count", &role_page_response::total_count},
        ::ores::utility::codec::field{"service_hidden", &role_page_response::service_hidden},
        ::ores::utility::codec::field{"areas", &role_page_response::areas},
    };
}

/**
 * @brief The members of role_holder, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<role_holder>) {
    return ::ores::utility::codec::field_list{
        ::ores::utility::codec::field{"account_id", &role_holder::account_id},
        ::ores::utility::codec::field{"username", &role_holder::username},
        ::ores::utility::codec::field{"full_name", &role_holder::full_name},
        ::ores::utility::codec::field{"image_id", &role_holder::image_id},
        ::ores::utility::codec::field{"assigned_by", &role_holder::assigned_by},
    };
}

/**
 * @brief The members of list_role_holders_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<list_role_holders_request>) {
    return ::ores::utility::codec::field_list{
        ::ores::utility::codec::field{"role_id", &list_role_holders_request::role_id},
        ::ores::utility::codec::field{"offset", &list_role_holders_request::offset},
        ::ores::utility::codec::field{"limit", &list_role_holders_request::limit},
    };
}

/**
 * @brief The members of role_holders_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<role_holders_response>) {
    return ::ores::utility::codec::field_list{
        ::ores::utility::codec::field{"result", &role_holders_response::result},
        ::ores::utility::codec::field{"holders", &role_holders_response::holders},
        ::ores::utility::codec::field{"total_count", &role_holders_response::total_count},
    };
}

/**
 * @brief The members of account_role_access, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<account_role_access>) {
    return ::ores::utility::codec::field_list{
        ::ores::utility::codec::field{"role", &account_role_access::role},
        ::ores::utility::codec::field{"permission_codes", &account_role_access::permission_codes},
        ::ores::utility::codec::field{"assigned_by", &account_role_access::assigned_by},
        ::ores::utility::codec::field{"assigned_at", &account_role_access::assigned_at},
        ::ores::utility::codec::field{"change_reason_code",
                                      &account_role_access::change_reason_code},
        ::ores::utility::codec::field{"change_commentary", &account_role_access::change_commentary},
    };
}

/**
 * @brief The members of get_account_roles_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_account_roles_response>) {
    return ::ores::utility::codec::field_list{
        ::ores::utility::codec::field{"result", &get_account_roles_response::result},
        ::ores::utility::codec::field{"roles", &get_account_roles_response::roles},
    };
}

/**
 * @brief The members of get_role_permissions_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_role_permissions_request>) {
    return ::ores::utility::codec::field_list{
        ::ores::utility::codec::field{"role_id", &get_role_permissions_request::role_id},
    };
}

/**
 * @brief The members of get_role_permissions_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_role_permissions_response>) {
    return ::ores::utility::codec::field_list{
        ::ores::utility::codec::field{"result", &get_role_permissions_response::result},
        ::ores::utility::codec::field{"permission_codes",
                                      &get_role_permissions_response::permission_codes},
    };
}

/**
 * @brief The members of put_role_permissions_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<put_role_permissions_request>) {
    return ::ores::utility::codec::field_list{
        ::ores::utility::codec::field{"role_id", &put_role_permissions_request::role_id},
        ::ores::utility::codec::field{"permission_codes",
                                      &put_role_permissions_request::permission_codes},
        ::ores::utility::codec::field{"change_reason_code",
                                      &put_role_permissions_request::change_reason_code},
        ::ores::utility::codec::field{"change_commentary",
                                      &put_role_permissions_request::change_commentary},
    };
}

/**
 * @brief The members of suggest_role_commands_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<suggest_role_commands_request>) {
    return ::ores::utility::codec::field_list{
        ::ores::utility::codec::field{"username", &suggest_role_commands_request::username},
        ::ores::utility::codec::field{"tenant_id", &suggest_role_commands_request::tenant_id},
        ::ores::utility::codec::field{"hostname", &suggest_role_commands_request::hostname},
    };
}

/**
 * @brief The members of suggest_role_commands_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<suggest_role_commands_response>) {
    return ::ores::utility::codec::field_list{
        ::ores::utility::codec::field{"commands", &suggest_role_commands_response::commands},
    };
}

}
namespace ores::utility::codec {

template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::assign_role_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::assign_role_request& v);
    static ores::iam::messaging::assign_role_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::assign_role_request& v);
    static ores::iam::messaging::assign_role_request read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::assign_role_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::assign_role_response& v);
    static ores::iam::messaging::assign_role_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::assign_role_response& v);
    static ores::iam::messaging::assign_role_response read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::assign_role_by_name_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::assign_role_by_name_response& v);
    static ores::iam::messaging::assign_role_by_name_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::assign_role_by_name_response& v);
    static ores::iam::messaging::assign_role_by_name_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::assign_role_by_name_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::assign_role_by_name_request& v);
    static ores::iam::messaging::assign_role_by_name_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::assign_role_by_name_request& v);
    static ores::iam::messaging::assign_role_by_name_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::revoke_role_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::revoke_role_request& v);
    static ores::iam::messaging::revoke_role_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::revoke_role_request& v);
    static ores::iam::messaging::revoke_role_request read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::revoke_role_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::revoke_role_response& v);
    static ores::iam::messaging::revoke_role_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::revoke_role_response& v);
    static ores::iam::messaging::revoke_role_response read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::revoke_role_by_name_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::revoke_role_by_name_response& v);
    static ores::iam::messaging::revoke_role_by_name_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::revoke_role_by_name_response& v);
    static ores::iam::messaging::revoke_role_by_name_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::revoke_role_by_name_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::revoke_role_by_name_request& v);
    static ores::iam::messaging::revoke_role_by_name_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::revoke_role_by_name_request& v);
    static ores::iam::messaging::revoke_role_by_name_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::get_account_roles_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::get_account_roles_request& v);
    static ores::iam::messaging::get_account_roles_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::get_account_roles_request& v);
    static ores::iam::messaging::get_account_roles_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::get_my_roles_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::get_my_roles_request& v);
    static ores::iam::messaging::get_my_roles_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::get_my_roles_request& v);
    static ores::iam::messaging::get_my_roles_request read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::permission_resource_row> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::permission_resource_row& v);
    static ores::iam::messaging::permission_resource_row read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::permission_resource_row& v);
    static ores::iam::messaging::permission_resource_row
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::permission_area_count> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::permission_area_count& v);
    static ores::iam::messaging::permission_area_count read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::permission_area_count& v);
    static ores::iam::messaging::permission_area_count
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::list_account_permissions_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::list_account_permissions_request& v);
    static ores::iam::messaging::list_account_permissions_request read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::iam::messaging::list_account_permissions_request& v);
    static ores::iam::messaging::list_account_permissions_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::list_my_permissions_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::list_my_permissions_request& v);
    static ores::iam::messaging::list_my_permissions_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::list_my_permissions_request& v);
    static ores::iam::messaging::list_my_permissions_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::permission_page_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::permission_page_response& v);
    static ores::iam::messaging::permission_page_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::permission_page_response& v);
    static ores::iam::messaging::permission_page_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::list_role_permissions_page_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::iam::messaging::list_role_permissions_page_request& v);
    static ores::iam::messaging::list_role_permissions_page_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::iam::messaging::list_role_permissions_page_request& v);
    static ores::iam::messaging::list_role_permissions_page_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::role_page_row> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::role_page_row& v);
    static ores::iam::messaging::role_page_row read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::role_page_row& v);
    static ores::iam::messaging::role_page_row read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::list_roles_page_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::list_roles_page_request& v);
    static ores::iam::messaging::list_roles_page_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::list_roles_page_request& v);
    static ores::iam::messaging::list_roles_page_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::role_page_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::role_page_response& v);
    static ores::iam::messaging::role_page_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::role_page_response& v);
    static ores::iam::messaging::role_page_response read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::role_holder> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::role_holder& v);
    static ores::iam::messaging::role_holder read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::role_holder& v);
    static ores::iam::messaging::role_holder read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::list_role_holders_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::list_role_holders_request& v);
    static ores::iam::messaging::list_role_holders_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::list_role_holders_request& v);
    static ores::iam::messaging::list_role_holders_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::role_holders_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::role_holders_response& v);
    static ores::iam::messaging::role_holders_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::role_holders_response& v);
    static ores::iam::messaging::role_holders_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::account_role_access> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::account_role_access& v);
    static ores::iam::messaging::account_role_access read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::account_role_access& v);
    static ores::iam::messaging::account_role_access read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::get_account_roles_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::get_account_roles_response& v);
    static ores::iam::messaging::get_account_roles_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::get_account_roles_response& v);
    static ores::iam::messaging::get_account_roles_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::get_role_permissions_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::get_role_permissions_request& v);
    static ores::iam::messaging::get_role_permissions_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::get_role_permissions_request& v);
    static ores::iam::messaging::get_role_permissions_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::get_role_permissions_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::get_role_permissions_response& v);
    static ores::iam::messaging::get_role_permissions_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::get_role_permissions_response& v);
    static ores::iam::messaging::get_role_permissions_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::put_role_permissions_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::put_role_permissions_request& v);
    static ores::iam::messaging::put_role_permissions_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::put_role_permissions_request& v);
    static ores::iam::messaging::put_role_permissions_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::suggest_role_commands_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::suggest_role_commands_request& v);
    static ores::iam::messaging::suggest_role_commands_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::suggest_role_commands_request& v);
    static ores::iam::messaging::suggest_role_commands_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::suggest_role_commands_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::suggest_role_commands_response& v);
    static ores::iam::messaging::suggest_role_commands_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::suggest_role_commands_response& v);
    static ores::iam::messaging::suggest_role_commands_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif
