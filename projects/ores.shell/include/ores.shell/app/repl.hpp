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
#ifndef ORES_SHELL_APP_REPL_HPP
#define ORES_SHELL_APP_REPL_HPP

#include <memory>
#include "ores.utility/log/make_logger.hpp"
#include "ores.shell/app/client_manager.hpp"

namespace cli {

class Cli;
class Menu;

}

namespace ores::shell::app {

/**
 * @brief Interactive REPL (Read-Eval-Print Loop) for the ORE Studio client.
 *
 * Provides a command-line interface for interacting with the ORE Studio server,
 * including commands for connection management and data retrieval.
 */
class repl final {
private:
    static auto& lg() {
        using namespace ores::utility::log;
        static auto instance = make_logger("ores.shell.app.repl");
        return instance;
    }

public:
    /**
     * @brief Construct a REPL instance with configuration.
     *
     * @param client_manager Reference to a client manager.
     */
    explicit repl(client_manager& client_manager_);

    repl(const repl&) = delete;
    repl& operator=(const repl&) = delete;
    repl(repl&&) = delete;
    repl& operator=(repl&&) = delete;

    /**
     * @brief Run the REPL session.
     *
     * Starts the I/O thread, displays the welcome message, and enters
     * the interactive command loop. Blocks until the user exits.
     */
    void run();

private:
    /**
     * @brief Setup the command menu structure.
     *
     * Creates the root menu and all submenus, wiring them together.
     *
     * @return The configured CLI instance
     */
    std::unique_ptr<::cli::Cli> setup_menus();

    /**
     * @brief Register account-related commands.
     *
     * Creates the accounts submenu and adds account operations.
     */
    void register_account_commands(::cli::Menu& root_menu);

    /**
     * @brief Process a create account request.
     *
     * Creates a new account with the provided details.
     *
     * @param out Output stream for results
     * @param username Account username
     * @param password_hash Hashed password
     * @param password_salt Password salt
     * @param totp_secret TOTP secret for 2FA
     * @param email Account email
     * @param is_admin Whether the account has admin privileges
     */
    void process_create_account(std::ostream& out, std::string username,
        std::string password, std::string totp_secret, std::string email,
        bool is_admin);

    /**
     * @brief Process a list accounts request.
     *
     * Retrieves all accounts from the server and displays them.
     *
     * @param out Output stream for results
     */
    void process_list_accounts(std::ostream& out);

    /**
     * @brief Process a login request.
     *
     * Authenticates a user with the provided credentials.
     *
     * @param username Account username
     * @param password Account password
     */
    void process_login(std::string username, std::string password);

    /**
     * @brief Process an unlock account request.
     *
     * Unlocks a locked account by account ID.
     *
     * @param out Output stream for results
     * @param account_id Account ID UUID as a string.
     */
    void process_unlock_account(std::ostream& out, std::string account_id);

    /**
     * @brief Display the welcome message.
     */
    void display_welcome() const;
    client_manager& client_manager_;
};

}

#endif
