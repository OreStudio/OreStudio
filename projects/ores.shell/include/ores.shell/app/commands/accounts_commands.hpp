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
#ifndef ORES_SHELL_APP_COMMANDS_ACCOUNTS_COMMANDS_HPP
#define ORES_SHELL_APP_COMMANDS_ACCOUNTS_COMMANDS_HPP

#include <string>
#include "ores.telemetry/log/make_logger.hpp"
#include "ores.comms/net/client_session.hpp"

namespace cli {

class Menu;

}

namespace ores::shell::app::commands {

/**
 * @brief Manages commands related to accounts.
 */
class accounts_commands {
private:
    inline static std::string_view logger_name =
        "ores.shell.app.commands.accounts_commands";

    static auto& lg() {
        using namespace ores::telemetry::log;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    /**
     * @brief Register account-related commands.
     *
     * Creates the accounts submenu and adds account operations.
     */
    static void register_commands(cli::Menu& root_menu,
        comms::net::client_session& session);

    /**
     * @brief Process a create account request.
     *
     * Creates a new account with the provided details.
     * Note: Admin privileges are now managed via RBAC role assignments.
     *
     * @param out Output stream for results
     * @param session Client session for connectivity.
     * @param username Account username
     * @param password Account password
     * @param totp_secret TOTP secret for 2FA
     * @param email Account email
     */
    static void process_create_account(std::ostream& out,
        comms::net::client_session& session,
        std::string username, std::string password, std::string totp_secret,
        std::string email);

    /**
     * @brief Process a list accounts request.
     *
     * Retrieves all accounts from the server and displays them.
     *
     * @param out Output stream for results
     * @param session Client session for connectivity.
     */
    static void process_list_accounts(std::ostream& out,
        comms::net::client_session& session);

    /**
     * @brief Process a login request.
     *
     * Authenticates a user with the provided credentials.
     *
     * @param out Output stream for results
     * @param session Client session for connectivity.
     * @param username Account username
     * @param password Account password
     */
    static void process_login(std::ostream& out,
        comms::net::client_session& session,
        std::string username, std::string password);

    /**
     * @brief Process a lock account request.
     *
     * Locks an account by account ID. Requires accounts:lock permission.
     *
     * @param out Output stream for results
     * @param session Client session for connectivity.
     * @param account_id Account ID UUID as a string.
     */
    static void process_lock_account(std::ostream& out,
        comms::net::client_session& session,
        std::string account_id);

    /**
     * @brief Process an unlock account request.
     *
     * Unlocks a locked account by account ID. Requires accounts:unlock permission.
     *
     * @param out Output stream for results
     * @param session Client session for connectivity.
     * @param account_id Account ID UUID as a string.
     */
    static void process_unlock_account(std::ostream& out,
        comms::net::client_session& session,
        std::string account_id);

    /**
     * @brief Process a list login info request.
     *
     * Retrieves all login info records from the server and displays them.
     *
     * @param out Output stream for results
     * @param session Client session for connectivity.
     */
    static void process_list_login_info(std::ostream& out,
        comms::net::client_session& session);

    /**
     * @brief Process a logout request.
     *
     * Logs out the currently logged-in user.
     *
     * @param out Output stream for results
     * @param session Client session for connectivity.
     */
    static void process_logout(std::ostream& out,
        comms::net::client_session& session);

    /**
     * @brief Process a bootstrap request.
     *
     * Creates the initial administrator account when the system is in bootstrap
     * mode. This command is only available when no admin accounts exist.
     *
     * @param out Output stream for results
     * @param session Client session for connectivity.
     * @param username Admin account username
     * @param password Admin account password
     * @param email Admin account email
     */
    static void process_bootstrap(std::ostream& out,
        comms::net::client_session& session,
        std::string username, std::string password, std::string email);

    /**
     * @brief Process a list sessions request.
     *
     * Lists session history for the current user or specified account.
     *
     * @param out Output stream for results
     * @param session Client session for connectivity.
     * @param account_id Optional account ID (empty for own sessions)
     */
    static void process_list_sessions(std::ostream& out,
        comms::net::client_session& session,
        std::string account_id = "");

    /**
     * @brief Process an active sessions request.
     *
     * Lists currently active sessions for the current user.
     *
     * @param out Output stream for results
     * @param session Client session for connectivity.
     */
    static void process_active_sessions(std::ostream& out,
        comms::net::client_session& session);

    /**
     * @brief Process a session statistics request.
     *
     * Displays session statistics for the specified time range.
     *
     * @param out Output stream for results
     * @param session Client session for connectivity.
     * @param days Number of days to retrieve statistics for (default 30)
     */
    static void process_session_stats(std::ostream& out,
        comms::net::client_session& session,
        int days = 30);
};

}

#endif
