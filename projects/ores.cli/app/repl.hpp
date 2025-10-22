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
#ifndef ORES_CLI_APP_REPL_HPP
#define ORES_CLI_APP_REPL_HPP

#if defined(_MSC_VER) && (_MSC_VER >= 1200)
#pragma once
#endif

#include <memory>
#include <thread>
#include <boost/asio/io_context.hpp>
#include <boost/asio/executor_work_guard.hpp>
#include "ores.comms/client.hpp"

namespace cli {
class Cli;
class Menu;
}

namespace ores::cli::app {

/**
 * @brief Interactive REPL (Read-Eval-Print Loop) for the ORE Studio client.
 *
 * Provides a command-line interface for interacting with the ORE Studio server,
 * including commands for connection management and data retrieval.
 */
class repl final {
public:
    /**
     * @brief Construct a REPL instance.
     */
    repl();

    /**
     * @brief Destructor - ensures clean shutdown.
     */
    ~repl();

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
     * @brief Register connection management commands.
     *
     * Adds connect and disconnect commands to the root menu.
     */
    void register_connection_commands(::cli::Menu& root_menu);

    /**
     * @brief Register currency-related commands.
     *
     * Creates the currencies submenu and adds currency operations.
     */
    void register_currency_commands(::cli::Menu& root_menu);

    /**
     * @brief Register account-related commands.
     *
     * Creates the accounts submenu and adds account operations.
     */
    void register_account_commands(::cli::Menu& root_menu);

    /**
     * @brief Process a connection request.
     *
     * Handles the async connection workflow including configuration updates
     * and existing connection cleanup.
     *
     * @param out Output stream for user feedback
     * @param host New host (empty to keep current)
     * @param port New port (empty to keep current)
     * @param identifier New client identifier (empty to keep current)
     */
    boost::asio::awaitable<void> process_connect(
        std::ostream& out, std::string host, std::string port, std::string identifier);

    /**
     * @brief Process a disconnect request.
     *
     * Cleanly disconnects from the server if connected.
     */
    void process_disconnect();

    /**
     * @brief Process a get currencies request.
     *
     * Retrieves all currencies from the server and displays them.
     *
     * @param out Output stream for results
     */
    boost::asio::awaitable<void> process_get_currencies(std::ostream& out);

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
    boost::asio::awaitable<void>
    process_create_account(std::ostream& out, std::string username,
        std::string password, std::string totp_secret, std::string email,
        bool is_admin);

    /**
     * @brief Process a list accounts request.
     *
     * Retrieves all accounts from the server and displays them.
     *
     * @param out Output stream for results
     */
    boost::asio::awaitable<void> process_list_accounts(std::ostream& out);

    /**
     * @brief Process a login request.
     *
     * Authenticates a user with the provided credentials.
     *
     * @param out Output stream for results
     * @param username Account username
     * @param password Account password
     */
    boost::asio::awaitable<void>
    process_login(std::ostream& out, std::string username, std::string password);

    /**
     * @brief Start the I/O context thread.
     *
     * Launches a background thread to handle async operations.
     */
    void start_io_thread();

    /**
     * @brief Stop the I/O context thread.
     *
     * Stops the I/O context and joins the background thread.
     */
    void stop_io_thread();

    /**
     * @brief Display the welcome message.
     */
    void display_welcome() const;

    comms::client_options config_;
    std::shared_ptr<comms::client> client_;
    std::unique_ptr<boost::asio::io_context> io_ctx_;
    std::unique_ptr<boost::asio::executor_work_guard<boost::asio::any_io_executor>> work_guard_;
    std::unique_ptr<std::thread> io_thread_;
};

}

#endif
