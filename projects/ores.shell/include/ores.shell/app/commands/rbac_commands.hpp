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
#ifndef ORES_SHELL_APP_COMMANDS_RBAC_COMMANDS_HPP
#define ORES_SHELL_APP_COMMANDS_RBAC_COMMANDS_HPP

#include <string>
#include "ores.telemetry/log/make_logger.hpp"
#include "ores.comms/net/client_session.hpp"

namespace cli {

class Menu;

}

namespace ores::shell::app::commands {

/**
 * @brief Manages commands related to RBAC (Role-Based Access Control).
 *
 * Provides shell commands for managing roles, permissions, and account-role
 * assignments.
 */
class rbac_commands {
private:
    inline static std::string_view logger_name =
        "ores.shell.app.commands.rbac_commands";

    static auto& lg() {
        using namespace ores::telemetry::log;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    /**
     * @brief Register RBAC-related commands.
     *
     * Creates the roles and permissions submenus and adds RBAC operations.
     * Also extends the accounts menu with role assignment commands.
     */
    static void register_commands(cli::Menu& root_menu,
        comms::net::client_session& session);

    // =========================================================================
    // Permissions Commands
    // =========================================================================

    /**
     * @brief Process a list permissions request.
     *
     * Retrieves all permissions from the server and displays them.
     *
     * @param out Output stream for results
     * @param session Client session for connectivity.
     */
    static void process_list_permissions(std::ostream& out,
        comms::net::client_session& session);

    // =========================================================================
    // Roles Commands
    // =========================================================================

    /**
     * @brief Process a list roles request.
     *
     * Retrieves all roles from the server and displays them.
     *
     * @param out Output stream for results
     * @param session Client session for connectivity.
     */
    static void process_list_roles(std::ostream& out,
        comms::net::client_session& session);

    /**
     * @brief Process a show role request.
     *
     * Retrieves a specific role by name or ID and displays its details
     * including all assigned permissions.
     *
     * @param out Output stream for results
     * @param session Client session for connectivity.
     * @param role_identifier Role name or UUID
     */
    static void process_show_role(std::ostream& out,
        comms::net::client_session& session,
        std::string role_identifier);

    // =========================================================================
    // Account-Role Assignment Commands
    // =========================================================================

    /**
     * @brief Process an assign role request.
     *
     * Assigns a role to an account. Requires roles:assign permission.
     *
     * @param out Output stream for results
     * @param session Client session for connectivity.
     * @param account_id Account ID UUID as a string.
     * @param role_id Role ID UUID as a string.
     */
    static void process_assign_role(std::ostream& out,
        comms::net::client_session& session,
        std::string account_id, std::string role_id);

    /**
     * @brief Process a revoke role request.
     *
     * Revokes a role from an account. Requires roles:revoke permission.
     *
     * @param out Output stream for results
     * @param session Client session for connectivity.
     * @param account_id Account ID UUID as a string.
     * @param role_id Role ID UUID as a string.
     */
    static void process_revoke_role(std::ostream& out,
        comms::net::client_session& session,
        std::string account_id, std::string role_id);

    /**
     * @brief Process a get account roles request.
     *
     * Retrieves all roles assigned to a specific account.
     *
     * @param out Output stream for results
     * @param session Client session for connectivity.
     * @param account_id Account ID UUID as a string.
     */
    static void process_get_account_roles(std::ostream& out,
        comms::net::client_session& session,
        std::string account_id);

    /**
     * @brief Process a get account permissions request.
     *
     * Retrieves all effective permissions for a specific account.
     *
     * @param out Output stream for results
     * @param session Client session for connectivity.
     * @param account_id Account ID UUID as a string.
     */
    static void process_get_account_permissions(std::ostream& out,
        comms::net::client_session& session,
        std::string account_id);
};

}

#endif
