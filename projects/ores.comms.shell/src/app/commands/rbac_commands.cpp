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
#include "ores.comms.shell/app/commands/rbac_commands.hpp"

#include <optional>
#include <ostream>
#include <string_view>
#include <functional>
#include <boost/lexical_cast.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <boost/algorithm/string/join.hpp>
#include <cli/cli.h>
#include "ores.iam/messaging/authorization_protocol.hpp"
#include "ores.iam/domain/role_table_io.hpp"  // IWYU pragma: keep.
#include "ores.iam/domain/permission_table_io.hpp"  // IWYU pragma: keep.

namespace ores::comms::shell::app::commands {

using namespace ores::logging;
using comms::messaging::message_type;
using comms::net::client_session;

namespace {

/**
 * @brief Logger for UUID parsing helper.
 */
[[nodiscard]] auto& parse_uuid_lg() {
    static auto instance = make_logger("ores.comms.shell.app.commands.rbac_commands");
    return instance;
}

/**
 * @brief Parse a string as a UUID, logging and outputting errors on failure.
 *
 * @param out Output stream for error messages
 * @param id_str The string to parse as UUID
 * @param id_name Human-readable name for the ID (e.g., "account ID", "role ID")
 * @return The parsed UUID if successful, or std::nullopt on failure
 */
std::optional<boost::uuids::uuid> parse_uuid(std::ostream& out,
    const std::string& id_str, std::string_view id_name) {
    try {
        return boost::lexical_cast<boost::uuids::uuid>(id_str);
    } catch (const boost::bad_lexical_cast&) {
        BOOST_LOG_SEV(parse_uuid_lg(), error) << "Invalid " << id_name
                                              << " format: " << id_str;
        out << "X Invalid " << id_name << " format. Expected UUID." << std::endl;
        return std::nullopt;
    }
}

/**
 * @brief Parse a string as a UUID without producing any output on failure.
 *
 * @param id_str The string to parse as UUID
 * @return The parsed UUID if successful, or std::nullopt on failure
 */
std::optional<boost::uuids::uuid> parse_uuid_quiet(const std::string& id_str) {
    try {
        return boost::lexical_cast<boost::uuids::uuid>(id_str);
    } catch (const boost::bad_lexical_cast&) {
        return std::nullopt;
    }
}

/**
 * @brief Format and output a list of strings with a title.
 *
 * @param out Output stream
 * @param title The title to display above the list
 * @param items The list of items to display
 * @param empty_message Message to display when the list is empty
 */
void format_string_list(std::ostream& out, std::string_view title,
    const std::vector<std::string>& items, std::string_view empty_message) {
    out << std::endl;
    out << title << std::endl;
    out << std::string(title.size(), '=') << std::endl;
    out << std::endl;

    if (items.empty()) {
        out << "  " << empty_message << std::endl;
    } else {
        for (const auto& item : items) {
            out << "  - " << item << std::endl;
        }
    }
    out << std::endl;
    out << "Total: " << items.size() << " item(s)" << std::endl;
}

}

void rbac_commands::
register_commands(cli::Menu& root_menu, client_session& session,
                  pagination_context& /*pagination*/) {
    // Note: RBAC protocol doesn't support pagination yet, so pagination_context
    // is unused but passed for API consistency.

    // =========================================================================
    // Permissions submenu
    // =========================================================================
    auto permissions_menu = std::make_unique<cli::Menu>("permissions");

    permissions_menu->Insert("list", [&session](std::ostream& out) {
        process_list_permissions(std::ref(out), std::ref(session));
    }, "List all permissions in the system");

    permissions_menu->Insert("suggest", [&session](std::ostream& out,
            std::string username, std::string identifier) {
        process_suggest_role_commands(std::ref(out), std::ref(session),
            std::move(username), std::move(identifier));
    }, "Generate role assignment commands (username hostname_or_tenant_id)");

    root_menu.Insert(std::move(permissions_menu));

    // =========================================================================
    // Roles submenu
    // =========================================================================
    auto roles_menu = std::make_unique<cli::Menu>("roles");

    roles_menu->Insert("list", [&session](std::ostream& out) {
        process_list_roles(std::ref(out), std::ref(session));
    }, "List all roles in the system");

    roles_menu->Insert("get", [&session](std::ostream& out,
            std::string role_identifier) {
        process_get_role(std::ref(out), std::ref(session),
            std::move(role_identifier));
    }, "Get role details (role_name or role_id)");

    root_menu.Insert(std::move(roles_menu));
}

// =============================================================================
// Permissions Commands Implementation
// =============================================================================

void rbac_commands::
process_list_permissions(std::ostream& out, client_session& session) {
    BOOST_LOG_SEV(lg(), debug) << "Initiating list permissions request.";

    using iam::messaging::list_permissions_request;
    using iam::messaging::list_permissions_response;
    auto result = session.process_authenticated_request<list_permissions_request,
                                                        list_permissions_response,
                                                        message_type::list_permissions_request>
        (list_permissions_request{});

    if (!result) {
        out << "X " << to_string(result.error()) << std::endl;
        return;
    }

    BOOST_LOG_SEV(lg(), info) << "Successfully retrieved "
                              << result->permissions.size() << " permissions.";
    out << result->permissions << std::endl;
}

// =============================================================================
// Roles Commands Implementation
// =============================================================================

void rbac_commands::
process_list_roles(std::ostream& out, client_session& session) {
    BOOST_LOG_SEV(lg(), debug) << "Initiating list roles request.";

    using iam::messaging::list_roles_request;
    using iam::messaging::list_roles_response;
    auto result = session.process_authenticated_request<list_roles_request,
                                                        list_roles_response,
                                                        message_type::list_roles_request>
        (list_roles_request{});

    if (!result) {
        out << "X " << to_string(result.error()) << std::endl;
        return;
    }

    BOOST_LOG_SEV(lg(), info) << "Successfully retrieved "
                              << result->roles.size() << " roles.";
    out << result->roles << std::endl;
}

void rbac_commands::
process_get_role(std::ostream& out, client_session& session,
    std::string role_identifier) {
    BOOST_LOG_SEV(lg(), debug) << "Initiating get role request for: "
                               << role_identifier;

    using iam::messaging::get_role_request;
    using iam::messaging::get_role_response;
    auto result = session.process_authenticated_request<get_role_request,
                                                        get_role_response,
                                                        message_type::get_role_request>
        (get_role_request{.identifier = role_identifier});

    if (!result) {
        out << "X " << to_string(result.error()) << std::endl;
        return;
    }

    if (!result->found || !result->role) {
        out << "X " << result->error_message << std::endl;
        return;
    }

    const auto& found_role = *result->role;
    BOOST_LOG_SEV(lg(), info) << "Found role: " << found_role.name;

    // Display role details
    out << std::endl;
    out << "Role Details" << std::endl;
    out << "============" << std::endl;
    out << "ID:            " << boost::uuids::to_string(found_role.id) << std::endl;
    out << "Name:          " << found_role.name << std::endl;
    out << "Description:   " << found_role.description << std::endl;
    out << "Version:       " << found_role.version << std::endl;
    out << "Change Reason: " << found_role.change_reason_code << std::endl;
    out << "Commentary:    " << found_role.change_commentary << std::endl;
    out << "Recorded By:   " << found_role.recorded_by << std::endl;
    out << "Recorded At:   " << found_role.recorded_at << std::endl;
    out << std::endl;
    out << "Permissions (" << found_role.permission_codes.size() << "):" << std::endl;
    out << "-------------" << std::endl;
    for (const auto& code : found_role.permission_codes) {
        out << "  - " << code << std::endl;
    }
    out << std::endl;
}

namespace {

/**
 * @brief Process a role operation (assign or revoke) with auto-detection of
 * UUID vs name-based arguments.
 *
 * If both arguments parse as UUIDs, uses the UUID-based request type.
 * Otherwise, uses the name-based request type with principal and role name.
 *
 * @tparam UuidRequest UUID-based request type (e.g., assign_role_request)
 * @tparam ByNameRequest Name-based request type (e.g., assign_role_by_name_request)
 * @tparam Response Response type (e.g., assign_role_response)
 * @tparam uuid_msg_type Message type enum for the UUID-based request
 * @tparam name_msg_type Message type enum for the name-based request
 * @param out Output stream for results
 * @param session Client session for connectivity
 * @param account_id_or_principal First argument (UUID or principal)
 * @param role_id_or_name Second argument (UUID or role name)
 * @param operation_verb Verb for log/output messages (e.g., "assigned", "revoked")
 */
template<typename UuidRequest, typename ByNameRequest, typename Response,
         message_type uuid_msg_type, message_type name_msg_type>
void process_role_operation(std::ostream& out, client_session& session,
    const std::string& account_id_or_principal,
    const std::string& role_id_or_name,
    std::string_view operation_verb) {
    auto& lg = parse_uuid_lg();

    auto parsed_account_id = parse_uuid_quiet(account_id_or_principal);
    auto parsed_role_id = parse_uuid_quiet(role_id_or_name);

    if (parsed_account_id && parsed_role_id) {
        // Both are UUIDs - use the UUID-based path
        BOOST_LOG_SEV(lg, debug) << "Role " << operation_verb << " (UUID): "
                                 << role_id_or_name << " / "
                                 << account_id_or_principal;

        auto result = session.process_authenticated_request<UuidRequest,
                                                            Response,
                                                            uuid_msg_type>
            (UuidRequest{
                .account_id = *parsed_account_id,
                .role_id = *parsed_role_id
            });

        if (!result) {
            out << "X " << comms::net::to_string(result.error()) << std::endl;
            return;
        }

        const auto& response = *result;
        if (response.success) {
            BOOST_LOG_SEV(lg, info) << "Successfully " << operation_verb
                                    << " role " << role_id_or_name
                                    << " for account " << account_id_or_principal;
            out << "V Role " << operation_verb << " successfully!" << std::endl;
            out << "  Account ID: " << account_id_or_principal << std::endl;
            out << "  Role ID:    " << role_id_or_name << std::endl;
        } else {
            BOOST_LOG_SEV(lg, warn) << "Failed to " << operation_verb
                                    << " role: " << response.error_message;
            out << "X Failed to " << operation_verb << " role: "
                << response.error_message << std::endl;
        }
    } else {
        // At least one argument is not a UUID - use name-based path
        BOOST_LOG_SEV(lg, debug) << "Role " << operation_verb << " (name): "
                                 << role_id_or_name << " / "
                                 << account_id_or_principal;

        auto result = session.process_authenticated_request<ByNameRequest,
                                                            Response,
                                                            name_msg_type>
            (ByNameRequest{
                .principal = account_id_or_principal,
                .role_name = role_id_or_name
            });

        if (!result) {
            out << "X " << comms::net::to_string(result.error()) << std::endl;
            return;
        }

        const auto& response = *result;
        if (response.success) {
            BOOST_LOG_SEV(lg, info) << "Successfully " << operation_verb
                                    << " role " << role_id_or_name
                                    << " for " << account_id_or_principal;
            out << "V Role " << operation_verb << " successfully!" << std::endl;
            out << "  Principal: " << account_id_or_principal << std::endl;
            out << "  Role:      " << role_id_or_name << std::endl;
        } else {
            BOOST_LOG_SEV(lg, warn) << "Failed to " << operation_verb
                                    << " role: " << response.error_message;
            out << "X Failed to " << operation_verb << " role: "
                << response.error_message << std::endl;
        }
    }
}

}

// =============================================================================
// Account-Role Assignment Commands Implementation
// =============================================================================

void rbac_commands::
process_assign_role(std::ostream& out, client_session& session,
    std::string account_id_or_principal, std::string role_id_or_name) {
    using iam::messaging::assign_role_request;
    using iam::messaging::assign_role_by_name_request;
    using iam::messaging::assign_role_response;
    process_role_operation<assign_role_request, assign_role_by_name_request,
                           assign_role_response,
                           message_type::assign_role_request,
                           message_type::assign_role_by_name_request>
        (out, session, account_id_or_principal, role_id_or_name, "assigned");
}

void rbac_commands::
process_revoke_role(std::ostream& out, client_session& session,
    std::string account_id_or_principal, std::string role_id_or_name) {
    using iam::messaging::revoke_role_request;
    using iam::messaging::revoke_role_by_name_request;
    using iam::messaging::revoke_role_response;
    process_role_operation<revoke_role_request, revoke_role_by_name_request,
                           revoke_role_response,
                           message_type::revoke_role_request,
                           message_type::revoke_role_by_name_request>
        (out, session, account_id_or_principal, role_id_or_name, "revoked");
}

void rbac_commands::
process_get_account_roles(std::ostream& out, client_session& session,
    std::string account_id) {
    auto parsed_account_id = parse_uuid(out, account_id, "account ID");
    if (!parsed_account_id) {
        return;
    }

    BOOST_LOG_SEV(lg(), debug) << "Getting roles for account " << account_id;

    using iam::messaging::get_account_roles_request;
    using iam::messaging::get_account_roles_response;
    auto result = session.process_authenticated_request<get_account_roles_request,
                                                        get_account_roles_response,
                                                        message_type::get_account_roles_request>
        (get_account_roles_request{.account_id = *parsed_account_id});

    if (!result) {
        out << "X " << comms::net::to_string(result.error()) << std::endl;
        return;
    }

    BOOST_LOG_SEV(lg(), info) << "Successfully retrieved "
                              << result->roles.size() << " roles for account "
                              << account_id;

    out << std::endl;
    out << "Roles for Account: " << account_id << std::endl;
    out << result->roles << std::endl;
}

void rbac_commands::
process_get_account_permissions(std::ostream& out, client_session& session,
    std::string account_id) {
    auto parsed_account_id = parse_uuid(out, account_id, "account ID");
    if (!parsed_account_id) {
        return;
    }

    BOOST_LOG_SEV(lg(), debug) << "Getting permissions for account " << account_id;

    using iam::messaging::get_account_permissions_request;
    using iam::messaging::get_account_permissions_response;
    auto result = session.process_authenticated_request<get_account_permissions_request,
                                                        get_account_permissions_response,
                                                        message_type::get_account_permissions_request>
        (get_account_permissions_request{.account_id = *parsed_account_id});

    if (!result) {
        out << "X " << comms::net::to_string(result.error()) << std::endl;
        return;
    }

    BOOST_LOG_SEV(lg(), info) << "Successfully retrieved "
                              << result->permission_codes.size()
                              << " permissions for account " << account_id;

    std::string title = "Effective Permissions for Account: " + account_id;
    format_string_list(out, title, result->permission_codes, "(no permissions)");
}

void rbac_commands::
process_suggest_role_commands(std::ostream& out, client_session& session,
    std::string username, std::string identifier) {
    BOOST_LOG_SEV(lg(), debug) << "Generating role commands for: "
                               << username << "@" << identifier;

    using iam::messaging::suggest_role_commands_request;
    using iam::messaging::suggest_role_commands_response;

    // Try to determine if identifier is a UUID (tenant_id) or hostname
    suggest_role_commands_request request{.username = username};

    // Check if identifier looks like a UUID
    try {
        boost::lexical_cast<boost::uuids::uuid>(identifier);
        // It's a valid UUID, use as tenant_id
        request.tenant_id = identifier;
    } catch (const boost::bad_lexical_cast&) {
        // Not a UUID, treat as hostname
        request.hostname = identifier;
    }

    auto result = session.process_authenticated_request<suggest_role_commands_request,
                                                        suggest_role_commands_response,
                                                        message_type::suggest_role_commands_request>
        (request);

    if (!result) {
        out << "X " << comms::net::to_string(result.error()) << std::endl;
        return;
    }

    BOOST_LOG_SEV(lg(), info) << "Generated " << result->commands.size()
                              << " role commands.";

    for (const auto& command : result->commands) {
        out << command << std::endl;
    }
}

}
