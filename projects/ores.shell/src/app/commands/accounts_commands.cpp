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
#include "ores.shell/app/commands/accounts_commands.hpp"

#include <ostream>
#include <functional>
#include <boost/lexical_cast.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <cli/cli.h>
#include "ores.accounts/messaging/protocol.hpp"
#include "ores.accounts/domain/account_table_io.hpp"  // IWYU pragma: keep.
#include "ores.accounts/domain/login_info_table_io.hpp"  // IWYU pragma: keep.

namespace ores::shell::app::commands {

using namespace ores::utility::log;
using comms::messaging::message_type;

void accounts_commands::
register_commands(cli::Menu& root_menu, client_manager& client_manager) {
    auto accounts_menu =
        std::make_unique<cli::Menu>("accounts");

    accounts_menu->Insert("create", [&client_manager](std::ostream & out,
            std::string username, std::string password, std::string totp_secret,
            std::string email, std::string is_admin_str) {
        bool is_admin = (is_admin_str == "true" || is_admin_str == "1");
        process_create_account(std::ref(out),
            std::ref(client_manager), std::move(username),
            std::move(password), std::move(totp_secret),
                std::move(email), is_admin);
    }, "Create a new account (username password totp_secret email is_admin)");

    accounts_menu->Insert("list", [&client_manager](std::ostream& out) {
        process_list_accounts(std::ref(out), std::ref(client_manager));
    }, "Retrieve all accounts from the server");

    accounts_menu->Insert("login", [&client_manager](std::ostream& /*out*/,
            std::string username, std::string password) {
        process_login(std::ref(client_manager), std::move(username),
            std::move(password));
    }, "Login with username and password");

    accounts_menu->Insert("unlock",
        [&client_manager](std::ostream& out, std::string account_id) {
        process_unlock_account(std::ref(out),
            std::ref(client_manager),
            std::move(account_id));
    }, "Unlock a locked account (account_id)");

    accounts_menu->Insert("list-logins", [&client_manager](std::ostream& out) {
        process_list_login_info(std::ref(out), std::ref(client_manager));
    }, "Retrieve all login info records from the server");

    accounts_menu->Insert("logout", [&client_manager](std::ostream& out) {
        process_logout(std::ref(out), std::ref(client_manager));
    }, "Logout the current user");

    root_menu.Insert(std::move(accounts_menu));
}

void accounts_commands::
process_list_accounts(std::ostream& out, client_manager& client_manager) {
    try {
        BOOST_LOG_SEV(lg(), debug) << "Initiating list account request.";

        using accounts::messaging::list_accounts_request;
        using accounts::messaging::list_accounts_response;
        client_manager.process_request<list_accounts_request,
                                       list_accounts_response,
                                       message_type::list_accounts_request>
            (list_accounts_request{})
            .and_then([&](const auto& response) {
                BOOST_LOG_SEV(lg(), info) << "Successfully retrieved "
                                          << response.accounts.size() << " accounts.";

                out << response.accounts << std::endl;
                return std::optional{response};
            });
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "List accounts exception: " << e.what();
        out << "✗ Error: " << e.what() << std::endl;
    }
}

void accounts_commands::process_login(client_manager& client_manager, std::string username,
    std::string password) {
    client_manager.login(username, password);
}

void accounts_commands::
process_unlock_account(std::ostream& out, client_manager& client_manager,
    std::string account_id) {
    try {
        BOOST_LOG_SEV(lg(), debug) << "Creating unlock account request for ID: "
                                   << account_id;

        using accounts::messaging::unlock_account_request;
        using accounts::messaging::unlock_account_response;
        client_manager.process_request<unlock_account_request,
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

void accounts_commands::process_create_account(std::ostream& out,
    client_manager& client_manager, std::string username,
    std::string password, std::string totp_secret, std::string email,
    bool is_admin) {
    try {
        BOOST_LOG_SEV(lg(), debug) << "Initiating create account request.";

        using accounts::messaging::create_account_request;
        using accounts::messaging::create_account_response;
        client_manager.process_request<create_account_request,
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

void accounts_commands::
process_list_login_info(std::ostream& out, client_manager& client_manager) {
    try {
        BOOST_LOG_SEV(lg(), debug) << "Initiating list login info request.";

        using accounts::messaging::list_login_info_request;
        using accounts::messaging::list_login_info_response;
        client_manager.process_request<list_login_info_request,
                                       list_login_info_response,
                                       message_type::list_login_info_request>
            (list_login_info_request{})
            .and_then([&](const auto& response) {
                BOOST_LOG_SEV(lg(), info) << "Successfully retrieved "
                                          << response.login_infos.size() << " login info records.";

                out << response.login_infos << std::endl;
                return std::optional{response};
            });
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "List login info exception: " << e.what();
        out << "✗ Error: " << e.what() << std::endl;
    }
}

void accounts_commands::
process_logout(std::ostream& /*out*/, client_manager& client_manager) {
    BOOST_LOG_SEV(lg(), debug) << "Processing logout request.";
    client_manager.logout();
}

}
