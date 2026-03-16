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
#include "ores.shell/app/commands/rbac_commands.hpp"

#include <optional>
#include <ostream>
#include <string_view>
#include <functional>
#include <rfl/json.hpp>
#include <boost/lexical_cast.hpp>
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include <boost/uuid/uuid_io.hpp>
#include <cli/cli.h>
#include "ores.iam/messaging/authorization_protocol.hpp"
#include "ores.iam/domain/role_table_io.hpp"  // IWYU pragma: keep.
#include "ores.iam/domain/permission_table_io.hpp"  // IWYU pragma: keep.

namespace ores::shell::app::commands {

using namespace ores::logging;
using service::nats_session;

namespace {

auto& parse_uuid_lg() {
    static auto instance = make_logger("ores.shell.app.commands.rbac_commands");
    return instance;
}

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

std::optional<boost::uuids::uuid> parse_uuid_quiet(const std::string& id_str) {
    try {
        return boost::lexical_cast<boost::uuids::uuid>(id_str);
    } catch (const boost::bad_lexical_cast&) {
        return std::nullopt;
    }
}

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

template<typename Response>
std::optional<Response> do_auth_request(std::ostream& out, nats_session& session,
    const std::string& subject, const std::string& body) {
    try {
        auto reply = session.authenticated_request(subject, body);
        auto data_str = std::string(
            reinterpret_cast<const char*>(reply.data.data()), reply.data.size());
        auto result = rfl::json::read<Response>(data_str);
        if (!result) {
            out << "✗ Failed to parse response" << std::endl;
            return std::nullopt;
        }
        return *result;
    } catch (const std::exception& e) {
        out << "✗ Request failed: " << e.what() << std::endl;
        return std::nullopt;
    }
}

} // anonymous namespace

void rbac_commands::
register_commands(cli::Menu& root_menu, nats_session& session,
                  pagination_context& /*pagination*/) {
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

void rbac_commands::
process_list_permissions(std::ostream& out, nats_session& session) {
    BOOST_LOG_SEV(lg(), debug) << "Initiating list permissions request.";

    auto result = do_auth_request<iam::messaging::list_permissions_response>(
        out, session, "iam.v1.permissions.list",
        rfl::json::write(iam::messaging::list_permissions_request{}));
    if (!result) return;

    BOOST_LOG_SEV(lg(), info) << "Successfully retrieved "
                              << result->permissions.size() << " permissions.";
    out << result->permissions << std::endl;
}

void rbac_commands::
process_list_roles(std::ostream& out, nats_session& session) {
    BOOST_LOG_SEV(lg(), debug) << "Initiating list roles request.";

    auto result = do_auth_request<iam::messaging::list_roles_response>(
        out, session, "iam.v1.roles.list",
        rfl::json::write(iam::messaging::list_roles_request{}));
    if (!result) return;

    BOOST_LOG_SEV(lg(), info) << "Successfully retrieved "
                              << result->roles.size() << " roles.";
    out << result->roles << std::endl;
}

void rbac_commands::
process_get_role(std::ostream& out, nats_session& session,
    std::string role_identifier) {
    BOOST_LOG_SEV(lg(), debug) << "Initiating get role request for: "
                               << role_identifier;

    iam::messaging::get_role_request req;
    req.identifier = role_identifier;

    auto result = do_auth_request<iam::messaging::get_role_response>(
        out, session, "iam.v1.roles.get", rfl::json::write(req));
    if (!result) return;

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
    out << "Modified By:   " << found_role.modified_by << std::endl;
    out << "Recorded At:   " << found_role.recorded_at << std::endl;
    out << std::endl;
    out << "Permissions (" << found_role.permission_codes.size() << "):" << std::endl;
    out << "-------------" << std::endl;
    for (const auto& code : found_role.permission_codes) {
        out << "  - " << code << std::endl;
    }
    out << std::endl;
}

void rbac_commands::
process_assign_role(std::ostream& out, nats_session& session,
    std::string account_id_or_principal, std::string role_id_or_name) {
    auto parsed_account_id = parse_uuid_quiet(account_id_or_principal);
    auto parsed_role_id = parse_uuid_quiet(role_id_or_name);

    if (parsed_account_id && parsed_role_id) {
        BOOST_LOG_SEV(lg(), debug) << "Assign role (UUID): "
                                   << role_id_or_name << " / "
                                   << account_id_or_principal;

        iam::messaging::assign_role_request req;
        req.account_id = account_id_or_principal;
        req.role_id = role_id_or_name;

        auto result = do_auth_request<iam::messaging::assign_role_response>(
            out, session, "iam.v1.roles.assign", rfl::json::write(req));
        if (!result) return;

        if (result->success) {
            out << "✓ Role assigned successfully!" << std::endl;
            out << "  Account ID: " << account_id_or_principal << std::endl;
            out << "  Role ID:    " << role_id_or_name << std::endl;
        } else {
            out << "X Failed to assign role: " << result->error_message << std::endl;
        }
    } else {
        BOOST_LOG_SEV(lg(), debug) << "Assign role (name): "
                                   << role_id_or_name << " / "
                                   << account_id_or_principal;

        iam::messaging::assign_role_by_name_request req;
        req.principal = account_id_or_principal;
        req.role_name = role_id_or_name;

        auto result = do_auth_request<iam::messaging::assign_role_response>(
            out, session, "iam.v1.roles.assign-by-name", rfl::json::write(req));
        if (!result) return;

        if (result->success) {
            out << "✓ Role assigned successfully!" << std::endl;
            out << "  Principal: " << account_id_or_principal << std::endl;
            out << "  Role:      " << role_id_or_name << std::endl;
        } else {
            out << "X Failed to assign role: " << result->error_message << std::endl;
        }
    }
}

void rbac_commands::
process_revoke_role(std::ostream& out, nats_session& session,
    std::string account_id_or_principal, std::string role_id_or_name) {
    auto parsed_account_id = parse_uuid_quiet(account_id_or_principal);
    auto parsed_role_id = parse_uuid_quiet(role_id_or_name);

    if (parsed_account_id && parsed_role_id) {
        BOOST_LOG_SEV(lg(), debug) << "Revoke role (UUID): "
                                   << role_id_or_name << " / "
                                   << account_id_or_principal;

        iam::messaging::revoke_role_request req;
        req.account_id = account_id_or_principal;
        req.role_id = role_id_or_name;

        auto result = do_auth_request<iam::messaging::revoke_role_response>(
            out, session, "iam.v1.roles.revoke", rfl::json::write(req));
        if (!result) return;

        if (result->success) {
            out << "✓ Role revoked successfully!" << std::endl;
            out << "  Account ID: " << account_id_or_principal << std::endl;
            out << "  Role ID:    " << role_id_or_name << std::endl;
        } else {
            out << "X Failed to revoke role: " << result->error_message << std::endl;
        }
    } else {
        BOOST_LOG_SEV(lg(), debug) << "Revoke role (name): "
                                   << role_id_or_name << " / "
                                   << account_id_or_principal;

        iam::messaging::revoke_role_by_name_request req;
        req.principal = account_id_or_principal;
        req.role_name = role_id_or_name;

        auto result = do_auth_request<iam::messaging::revoke_role_response>(
            out, session, "iam.v1.roles.revoke-by-name", rfl::json::write(req));
        if (!result) return;

        if (result->success) {
            out << "✓ Role revoked successfully!" << std::endl;
            out << "  Principal: " << account_id_or_principal << std::endl;
            out << "  Role:      " << role_id_or_name << std::endl;
        } else {
            out << "X Failed to revoke role: " << result->error_message << std::endl;
        }
    }
}

void rbac_commands::
process_get_account_roles(std::ostream& out, nats_session& session,
    std::string account_id) {
    auto parsed_account_id = parse_uuid(out, account_id, "account ID");
    if (!parsed_account_id) {
        return;
    }

    BOOST_LOG_SEV(lg(), debug) << "Getting roles for account " << account_id;

    iam::messaging::get_account_roles_request req;
    req.account_id = account_id;

    auto result = do_auth_request<iam::messaging::get_account_roles_response>(
        out, session, "iam.v1.roles.for-account", rfl::json::write(req));
    if (!result) return;

    BOOST_LOG_SEV(lg(), info) << "Successfully retrieved "
                              << result->roles.size() << " roles for account "
                              << account_id;

    out << std::endl;
    out << "Roles for Account: " << account_id << std::endl;
    out << result->roles << std::endl;
}

void rbac_commands::
process_get_account_permissions(std::ostream& out, nats_session& session,
    std::string account_id) {
    auto parsed_account_id = parse_uuid(out, account_id, "account ID");
    if (!parsed_account_id) {
        return;
    }

    BOOST_LOG_SEV(lg(), debug) << "Getting permissions for account " << account_id;

    iam::messaging::get_account_permissions_request req;
    req.account_id = account_id;

    auto result = do_auth_request<iam::messaging::get_account_permissions_response>(
        out, session, "iam.v1.permissions.for-account", rfl::json::write(req));
    if (!result) return;

    BOOST_LOG_SEV(lg(), info) << "Successfully retrieved "
                              << result->permission_codes.size()
                              << " permissions for account " << account_id;

    std::string title = "Effective Permissions for Account: " + account_id;
    format_string_list(out, title, result->permission_codes, "(no permissions)");
}

void rbac_commands::
process_suggest_role_commands(std::ostream& out, nats_session& session,
    std::string username, std::string identifier) {
    BOOST_LOG_SEV(lg(), debug) << "Generating role commands for: "
                               << username << "@" << identifier;

    iam::messaging::suggest_role_commands_request req;
    req.username = username;

    // Check if identifier looks like a UUID
    try {
        boost::lexical_cast<boost::uuids::uuid>(identifier);
        // It's a valid UUID, use as tenant_id
        req.tenant_id = identifier;
    } catch (const boost::bad_lexical_cast&) {
        // Not a UUID, treat as hostname
        req.hostname = identifier;
    }

    auto result = do_auth_request<iam::messaging::suggest_role_commands_response>(
        out, session, "iam.v1.roles.suggest-commands", rfl::json::write(req));
    if (!result) return;

    BOOST_LOG_SEV(lg(), info) << "Generated " << result->commands.size()
                              << " role commands.";

    for (const auto& command : result->commands) {
        out << command << std::endl;
    }
}

}
