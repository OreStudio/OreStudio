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
#include "ores.accounts/messaging/bootstrap_protocol.hpp"
#include "ores.accounts/domain/account_table_io.hpp"  // IWYU pragma: keep.
#include "ores.accounts/domain/login_info_table_io.hpp"  // IWYU pragma: keep.

namespace ores::shell::app::commands {

using namespace ores::utility::log;
using comms::messaging::message_type;
using comms::net::client_session;
using comms::net::client_session_error;

void accounts_commands::
register_commands(cli::Menu& root_menu, client_session& session) {
    auto accounts_menu =
        std::make_unique<cli::Menu>("accounts");

    accounts_menu->Insert("create", [&session](std::ostream & out,
            std::string username, std::string password, std::string totp_secret,
            std::string email, std::string is_admin_str) {
        bool is_admin = (is_admin_str == "true" || is_admin_str == "1");
        process_create_account(std::ref(out),
            std::ref(session), std::move(username),
            std::move(password), std::move(totp_secret),
                std::move(email), is_admin);
    }, "Create a new account (username password totp_secret email is_admin)");

    accounts_menu->Insert("list", [&session](std::ostream& out) {
        process_list_accounts(std::ref(out), std::ref(session));
    }, "Retrieve all accounts from the server");

    accounts_menu->Insert("login", [&session](std::ostream& out,
            std::string username, std::string password) {
        process_login(std::ref(out), std::ref(session), std::move(username),
            std::move(password));
    }, "Login with username and password");

    accounts_menu->Insert("unlock",
        [&session](std::ostream& out, std::string account_id) {
        process_unlock_account(std::ref(out),
            std::ref(session),
            std::move(account_id));
    }, "Unlock a locked account (account_id)");

    accounts_menu->Insert("list-logins", [&session](std::ostream& out) {
        process_list_login_info(std::ref(out), std::ref(session));
    }, "Retrieve all login info records from the server");

    accounts_menu->Insert("logout", [&session](std::ostream& out) {
        process_logout(std::ref(out), std::ref(session));
    }, "Logout the current user");

    root_menu.Insert(std::move(accounts_menu));

    // Bootstrap command at root level (doesn't require authentication)
    root_menu.Insert("bootstrap", [&session](std::ostream& out,
            std::string username, std::string password, std::string email) {
        process_bootstrap(std::ref(out), std::ref(session),
            std::move(username), std::move(password), std::move(email));
    }, "Create initial admin account (username password email) - only works in bootstrap mode");
}

void accounts_commands::
process_list_accounts(std::ostream& out, client_session& session) {
    BOOST_LOG_SEV(lg(), debug) << "Initiating list account request.";

    using accounts::messaging::list_accounts_request;
    using accounts::messaging::list_accounts_response;
    auto result = session.process_authenticated_request<list_accounts_request,
                                                        list_accounts_response,
                                                        message_type::list_accounts_request>
        (list_accounts_request{});

    if (!result) {
        out << "✗ " << to_string(result.error()) << std::endl;
        return;
    }

    BOOST_LOG_SEV(lg(), info) << "Successfully retrieved "
                              << result->accounts.size() << " accounts.";
    out << result->accounts << std::endl;
}

void accounts_commands::
process_login(std::ostream& out, client_session& session,
    std::string username, std::string password) {
    BOOST_LOG_SEV(lg(), debug) << "Initiating login request for user: "
                               << username;

    using accounts::messaging::login_request;
    using accounts::messaging::login_response;
    auto result = session.process_request<login_request, login_response,
                                          message_type::login_request>
        (login_request{.username = username, .password = std::move(password)});

    if (!result) {
        out << "✗ " << to_string(result.error()) << std::endl;
        return;
    }

    const auto& response = *result;
    if (!response.success) {
        BOOST_LOG_SEV(lg(), warn) << "Login failed: " << response.error_message;
        out << "✗ Login failed: " << response.error_message << std::endl;
        return;
    }

    BOOST_LOG_SEV(lg(), info) << "Login successful for user: " << username;
    out << "✓ Login successful!" << std::endl;

    // Update session state
    comms::net::client_session_info info{
        .account_id = response.account_id,
        .username = std::move(username),
        .is_admin = response.is_admin
    };
    session.set_session_info(std::move(info));
}

void accounts_commands::
process_unlock_account(std::ostream& out, client_session& session,
    std::string account_id) {
    boost::uuids::uuid parsed_id;
    try {
        parsed_id = boost::lexical_cast<boost::uuids::uuid>(account_id);
    } catch (const boost::bad_lexical_cast&) {
        BOOST_LOG_SEV(lg(), error) << "Invalid account ID format: " << account_id;
        out << "✗ Invalid account ID format. Expected UUID." << std::endl;
        return;
    }

    BOOST_LOG_SEV(lg(), debug) << "Creating unlock account request for ID: "
                               << account_id;

    using accounts::messaging::unlock_account_request;
    using accounts::messaging::unlock_account_response;
    auto result = session.process_authenticated_request<unlock_account_request,
                                                        unlock_account_response,
                                                        message_type::unlock_account_request>
        (unlock_account_request{.account_id = parsed_id});

    if (!result) {
        out << "✗ " << to_string(result.error()) << std::endl;
        return;
    }

    const auto& response = *result;
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
}

void accounts_commands::process_create_account(std::ostream& out,
    client_session& session, std::string username,
    std::string password, std::string totp_secret, std::string email,
    bool is_admin) {
    BOOST_LOG_SEV(lg(), debug) << "Initiating create account request.";

    using accounts::messaging::create_account_request;
    using accounts::messaging::create_account_response;
    auto result = session.process_authenticated_request<create_account_request,
                                                        create_account_response,
                                                        message_type::create_account_request>
        (create_account_request {
            .username = std::move(username),
            .password = std::move(password),
            .totp_secret = std::move(totp_secret),
            .email = std::move(email),
            .is_admin = is_admin
        });

    if (!result) {
        out << "✗ " << to_string(result.error()) << std::endl;
        return;
    }

    BOOST_LOG_SEV(lg(), info) << "Successfully created account with ID: "
                              << result->account_id;
    out << "✓ Account created with ID: " << result->account_id << std::endl;
}

void accounts_commands::
process_list_login_info(std::ostream& out, client_session& session) {
    BOOST_LOG_SEV(lg(), debug) << "Initiating list login info request.";

    using accounts::messaging::list_login_info_request;
    using accounts::messaging::list_login_info_response;
    auto result = session.process_authenticated_request<list_login_info_request,
                                                        list_login_info_response,
                                                        message_type::list_login_info_request>
        (list_login_info_request{});

    if (!result) {
        out << "✗ " << to_string(result.error()) << std::endl;
        return;
    }

    BOOST_LOG_SEV(lg(), info) << "Successfully retrieved "
                              << result->login_infos.size() << " login info records.";
    out << result->login_infos << std::endl;
}

void accounts_commands::
process_logout(std::ostream& out, client_session& session) {
    BOOST_LOG_SEV(lg(), debug) << "Processing logout request.";

    using accounts::messaging::logout_request;
    using accounts::messaging::logout_response;
    auto result = session.process_authenticated_request<logout_request,
                                                        logout_response,
                                                        message_type::logout_request>
        (logout_request{});

    if (!result) {
        out << "✗ " << to_string(result.error()) << std::endl;
        return;
    }

    const auto& response = *result;
    if (response.success) {
        BOOST_LOG_SEV(lg(), info) << "Logout successful.";
        out << "✓ Logged out successfully." << std::endl;
        session.clear_session_info();
    } else {
        BOOST_LOG_SEV(lg(), warn) << "Logout failed: " << response.message;
        out << "✗ Logout failed: " << response.message << std::endl;
    }
}

void accounts_commands::
process_bootstrap(std::ostream& out, client_session& session,
    std::string username, std::string password, std::string email) {
    BOOST_LOG_SEV(lg(), debug) << "Initiating bootstrap request for user: "
                               << username;

    using accounts::messaging::create_initial_admin_request;
    using accounts::messaging::create_initial_admin_response;
    auto result = session.process_request<create_initial_admin_request,
                                          create_initial_admin_response,
                                          message_type::create_initial_admin_request>
        (create_initial_admin_request{
            .username = std::move(username),
            .password = std::move(password),
            .email = std::move(email)
        });

    if (!result) {
        out << "✗ " << to_string(result.error()) << std::endl;
        return;
    }

    const auto& response = *result;
    if (response.success) {
        BOOST_LOG_SEV(lg(), info) << "Bootstrap successful. Admin account ID: "
                                  << response.account_id;
        out << "✓ Initial admin account created successfully!" << std::endl;
        out << "  Account ID: " << response.account_id << std::endl;
        out << "  You can now login with the credentials provided." << std::endl;
    } else {
        BOOST_LOG_SEV(lg(), warn) << "Bootstrap failed: " << response.error_message;
        out << "✗ Bootstrap failed: " << response.error_message << std::endl;
    }
}

}
