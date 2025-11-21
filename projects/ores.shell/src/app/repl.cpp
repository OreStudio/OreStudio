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
#include "ores.shell/app/repl.hpp"

#include <iostream>
#include <boost/lexical_cast.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <cli/cli.h>
#include <cli/clifilesession.h>
#include "ores.utility/version/version.hpp"
#include "ores.utility/streaming/std_vector.hpp" // IWYU pragma: keep.
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include "ores.risk/messaging/protocol.hpp"
#include "ores.accounts/messaging/protocol.hpp"
// #include "ores.accounts/domain/account_table_io.hpp"
// #include "ores.risk/domain/currency_table_io.hpp"

namespace ores::shell::app {


using namespace ores::utility::log;
using comms::protocol::message_type;

repl::repl(client_manager& client_manager)
    : client_manager_(client_manager) {
    BOOST_LOG_SEV(lg(), info) << "REPL created.";
}

void repl::run() {
    BOOST_LOG_SEV(lg(), info) << "REPL session started.";

    display_welcome();
    auto cli_instance = setup_menus();
    cli::CliFileSession session(*cli_instance, std::cin, std::cout);
    session.Start();

    BOOST_LOG_SEV(lg(), info) << "REPL session ended.";
}

std::unique_ptr<cli::Cli> repl::setup_menus() {
    auto root_menu =
        std::make_unique<cli::Menu>("ores-shell");

    register_connection_commands(*root_menu);
    register_currency_commands(*root_menu);
    register_account_commands(*root_menu);

    auto cli_instance =
        std::make_unique<cli::Cli>(std::move(root_menu));
    cli_instance->ExitAction([](auto& out) {
        out << "Bye!" << std::endl;
    });

    return cli_instance;
}

void repl::register_connection_commands(cli::Menu& root_menu) {
    root_menu.Insert("connect", [this](std::ostream& /*out*/, std::string host,
            std::string port, std::string identifier) {
            this->process_connect(std::move(host), std::move(port),
                std::move(identifier));
        }, "Connect to server (optional: host port identifier)");

    root_menu.Insert("disconnect", [this](std::ostream& /*out*/) {
        process_disconnect();
    }, "Disconnect from server");
}

void repl::register_currency_commands(cli::Menu& root_menu) {
    auto currencies_menu =
        std::make_unique<cli::Menu>("currencies");

    currencies_menu->Insert("get", [this](std::ostream& out) {
        process_get_currencies(std::ref(out));
    }, "Retrieve all currencies from the server");

    root_menu.Insert(std::move(currencies_menu));
}

void repl::register_account_commands(cli::Menu& root_menu) {
    auto accounts_menu =
        std::make_unique<cli::Menu>("accounts");

    accounts_menu->Insert("create", [this](std::ostream & out, std::string username,
            std::string password, std::string totp_secret,
            std::string email, std::string is_admin_str) {
        bool is_admin = (is_admin_str == "true" || is_admin_str == "1");
        process_create_account(std::ref(out), std::move(username),
                std::move(password), std::move(totp_secret),
                std::move(email), is_admin);
    }, "Create a new account (username password totp_secret email is_admin)");

    accounts_menu->Insert("list", [this](std::ostream& out) {
        process_list_accounts(std::ref(out));
    }, "Retrieve all accounts from the server");

    accounts_menu->Insert("login", [this](std::ostream& /*out*/,
            std::string username, std::string password) {
        process_login(std::move(username),
            std::move(password));
    }, "Login with username and password");

    accounts_menu->Insert("unlock", [this](std::ostream & out,
            std::string account_id_str) {
        process_unlock_account(std::ref(out), std::move(account_id_str));
    }, "Unlock a locked account (account_id)");

    root_menu.Insert(std::move(accounts_menu));
}

void repl::process_connect(std::string host, std::string port,
    std::string identifier) {
    client_manager_.connect(host, port, identifier);
}

void repl::process_disconnect() {
    client_manager_.disconnect();
}

void repl::process_get_currencies(std::ostream& out) {
    try {
        BOOST_LOG_SEV(lg(), debug) << "Initiating get currencies request.";

        using risk::messaging::get_currencies_request;
        using risk::messaging::get_currencies_response;
        client_manager_.process_request<get_currencies_request,
                                        get_currencies_response,
                                        message_type::get_currencies_request>
            (get_currencies_request{})
            .and_then([&](const auto& response) {
                out << response.currencies << std::endl;
                return std::optional{response};
            });
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "Get currencies exception: " << e.what();
        out << "✗ Error: " << e.what() << std::endl;
    }
}

void repl::display_welcome() const {
    std::cout << "ORE Studio Shell REPL v" << ORES_VERSION << std::endl;
    std::cout << "Type 'help' for available commands, 'exit' to quit" << std::endl;
    std::cout << std::endl;
}

void repl::process_create_account(std::ostream& out, std::string username,
    std::string password, std::string totp_secret, std::string email,
    bool is_admin) {
    try {
        BOOST_LOG_SEV(lg(), debug) << "Initiating create account request.";

        using accounts::messaging::create_account_request;
        using accounts::messaging::create_account_response;
        client_manager_.process_request<create_account_request,
                                        create_account_response,
                                        message_type::create_account_request>
            (create_account_request {
                .username = std::move(username),
                .password = std::move(password),
                .totp_secret = std::move(totp_secret),
                .email = std::move(email),
                .is_admin = is_admin
            })
            .and_then([&](const auto& response) {
                BOOST_LOG_SEV(lg(), info) << "Successfully created account with ID: "
                                          << response.account_id;
                out << "✓ Account created with ID: " << response.account_id << std::endl;
                return std::optional{response};
            });
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "Create account exception: " << e.what();
        out << "✗ Error: " << e.what() << std::endl;
    }
}

void repl::process_list_accounts(std::ostream& out) {
    try {
        BOOST_LOG_SEV(lg(), debug) << "Initiating list account request.";
        using accounts::messaging::list_accounts_request;
        using accounts::messaging::list_accounts_response;
        client_manager_.process_request<list_accounts_request,
                                        list_accounts_response,
                                        message_type::list_accounts_request>
            (list_accounts_request{})
            .and_then([&](const auto& response) {
                BOOST_LOG_SEV(lg(), info) << "Successfully retrieved "
                                          << response.accounts.size() << " accounts";

                out << response.accounts << std::endl;
                return std::optional{response};
            });
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "List accounts exception: " << e.what();
        out << "✗ Error: " << e.what() << std::endl;
    }
}

void repl::process_login(std::string username, std::string password) {
    client_manager_.login(username, password);
}

void repl::process_unlock_account(std::ostream& out, std::string account_id) {
    try {
        BOOST_LOG_SEV(lg(), debug) << "Creating unlock account request for ID: "
                                   << account_id;

        using accounts::messaging::unlock_account_request;
        using accounts::messaging::unlock_account_response;
        client_manager_.process_request<unlock_account_request,
                                        unlock_account_response,
                                        message_type::unlock_account_request>
            (unlock_account_request{
                .account_id = boost::lexical_cast<boost::uuids::uuid>(account_id)})
            .and_then([&](const auto& response) {
                if (response.success) {
                    BOOST_LOG_SEV(lg(), info) << "Successfully unlocked account: "
                                              << account_id;

                    out << "✓ Account unlocked successfully!" << std::endl
                        << "  Account ID: " << account_id << std::endl;
                } else {
                    BOOST_LOG_SEV(lg(), warn)
                        << "Failed to unlock account: " << response.error_message;
                    out << "✗ Failed to unlock account: " << response.error_message << std::endl;
                }
                return std::optional{response};
            });

    } catch (const boost::bad_lexical_cast& e) {
        BOOST_LOG_SEV(lg(), error) << "Invalid account ID format: " << account_id;
        out << "✗ Invalid account ID format. Expected UUID." << std::endl;
        return;
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "Unlock account exception: " << e.what();
        out << "✗ Error: " << e.what() << std::endl;
    }
}

}
