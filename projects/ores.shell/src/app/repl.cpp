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
#include "ores.comms/protocol/handshake.hpp"
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include "ores.risk/messaging/protocol.hpp"
#include "ores.accounts/messaging/protocol.hpp"
#include "ores.accounts/domain/account_table_io.hpp"
#include "ores.risk/domain/currency_table_io.hpp"

namespace ores::shell::app {

using namespace ores::utility::log;

repl::repl(std::optional<comms::net::client_options> connection_config,
           std::optional<config::login_options> login_config)
    : connection_config_(std::move(connection_config)),
      login_config_(std::move(login_config)),
      config_{
          .host = "localhost",
          .port = 55555,
          .client_identifier = "ores-client",
          .verify_certificate = false
      },
      client_(std::make_shared<comms::net::client>(config_)) {
    BOOST_LOG_SEV(lg(), info) << "REPL created with configuration: " << config_;
}

void repl::run() {
    BOOST_LOG_SEV(lg(), info) << "REPL session started.";

    if (connection_config_) {
        BOOST_LOG_SEV(lg(), debug) << "Will attempt to auto-connect and auto-login.";

        const bool connected = auto_connect();
        if (connected && login_config_)
            auto_login();
    }

    display_welcome();
    auto cli_instance = setup_menus();
    ::cli::CliFileSession session(*cli_instance, std::cin, std::cout);
    session.Start();

    BOOST_LOG_SEV(lg(), info) << "REPL session ended.";
}

std::unique_ptr<::cli::Cli> repl::setup_menus() {
    auto root_menu =
        std::make_unique<::cli::Menu>("ores-client");

    register_connection_commands(*root_menu);
    register_currency_commands(*root_menu);
    register_account_commands(*root_menu);

    auto cli_instance = std::make_unique<::cli::Cli>(std::move(root_menu));
    cli_instance->ExitAction([](auto& out) {
        out << "Bye!" << std::endl;
    });

    return cli_instance;
}

void repl::register_connection_commands(::cli::Menu& root_menu) {
    root_menu.Insert("connect",
        [this](std::ostream& out, std::string host, std::string port,
        std::string identifier) {
            this->process_connect(std::ref(out), std::move(host),
                std::move(port), std::move(identifier));
        }, "Connect to server (optional: host port identifier)");

    root_menu.Insert("disconnect", [this](std::ostream& out) {
        process_disconnect(std::ref(out));
    }, "Disconnect from server");
}

void repl::register_currency_commands(::cli::Menu& root_menu) {
    auto currencies_menu =
        std::make_unique<::cli::Menu>("currencies");

    currencies_menu->Insert("get", [this](std::ostream& out) {
        process_get_currencies(std::ref(out));
    }, "Retrieve all currencies from the server");

    root_menu.Insert(std::move(currencies_menu));
}

void repl::register_account_commands(::cli::Menu& root_menu) {
    auto accounts_menu =
        std::make_unique<::cli::Menu>("accounts");

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

    accounts_menu->Insert("login", [this](std::ostream & out, std::string username,
            std::string password) {
        process_login(std::ref(out), std::move(username),
            std::move(password));
    }, "Login with username and password");

    accounts_menu->Insert("unlock", [this](std::ostream & out,
            std::string account_id_str) {
        process_unlock_account(std::ref(out), std::move(account_id_str));
    }, "Unlock a locked account (account_id)");

    root_menu.Insert(std::move(accounts_menu));
}

void repl::process_connect(std::ostream& out, std::string host, std::string port,
    std::string identifier) {

    try {
        if (!host.empty()) {
            BOOST_LOG_SEV(lg(), debug) << "Updating host to: " << host;
            config_.host = std::move(host);
        }

        if (!port.empty()) {
            try {
                config_.port = static_cast<std::uint16_t>(std::stoi(port));
                BOOST_LOG_SEV(lg(), debug) << "Updating port to: " << config_.port;
            } catch (...) {
                BOOST_LOG_SEV(lg(), error) << "Invalid port number: " << port;
                out << "✗ Invalid port number: " << port << std::endl;
                return;
            }
        }
        if (!identifier.empty()) {
            BOOST_LOG_SEV(lg(), debug) << "Updating client identifier to: "
                                     << identifier;
            config_.client_identifier = std::move(identifier);
        }

        BOOST_LOG_SEV(lg(), info) << "Connecting to " << config_.host << ":"
                                << config_.port << " (identifier: "
                                << config_.client_identifier << ")";

        if (client_ && client_->is_connected()) {
            BOOST_LOG_SEV(lg(), info) << "Disconnecting existing connection";
            client_->disconnect();
        }

        client_ = std::make_shared<comms::net::client>(config_);

        client_->connect_sync();

        BOOST_LOG_SEV(lg(), info) << "Successfully connected.";
        out << "✓ Connected." << std::endl;

    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "Connect exception: " << e.what();
        out << "✗ Error: " << e.what() << std::endl;
    }
}

void repl::process_disconnect(std::ostream& out) {
    if (!client_) {
        BOOST_LOG_SEV(lg(), warn) << "No client instance.";
        return;
    }

    if (!client_->is_connected()) {
        BOOST_LOG_SEV(lg(), debug) << "Already disconnected.";
        return;
    }

    client_->disconnect();
    BOOST_LOG_SEV(lg(), info) << "Disconnected from server.";
    out << "Disconnected from server.";
}

void repl::process_get_currencies(std::ostream& out) {
    try {
        if (!client_ || !client_->is_connected()) {
            out << "✗ Not connected to server. Use 'connect' command first" << std::endl;
            return;
        }

        BOOST_LOG_SEV(lg(), debug) << "Initiating get currencies request";

        BOOST_LOG_SEV(lg(), debug) << "Creating get currencies request";

        risk::messaging::get_currencies_request request;
        auto request_payload = request.serialize();

        comms::protocol::frame request_frame(
            comms::protocol::message_type::get_currencies_request, 0,
            std::move(request_payload));

        BOOST_LOG_SEV(lg(), debug) << "Sending request frame";

        auto response_result =
            client_->send_request_sync(std::move(request_frame));

        if (!response_result) {
            BOOST_LOG_SEV(lg(), error) << "Request failed with error code: "
                                      << static_cast<int>(response_result.error());
            out << "✗ Request failed" << std::endl;
            return;
        }

        // Check if server returned an error response
        if (response_result->header().type == comms::protocol::message_type::error_response) {
            auto err_resp = comms::protocol::error_response::deserialize(
                response_result->payload());
            if (err_resp) {
                BOOST_LOG_SEV(lg(), error) << "Server returned error: "
                                          << err_resp->message;
                out << "✗ Error: " << err_resp->message << std::endl;
            } else {
                BOOST_LOG_SEV(lg(), error) << "Failed to deserialize error response";
                out << "✗ Server error (could not parse error message)" << std::endl;
            }
            return;
        }

        BOOST_LOG_SEV(lg(), debug) << "Deserializing response";

        auto response = risk::messaging::get_currencies_response::deserialize(
            response_result->payload());

        if (!response) {
            BOOST_LOG_SEV(lg(), error) << "Failed to deserialize response: "
                                      << static_cast<int>(response.error());
            out << "✗ Failed to parse response" << std::endl;
            return;
        }

        BOOST_LOG_SEV(lg(), info) << "Successfully retrieved "
                                << response->currencies.size() << " currencies";

        out << response->currencies << std::endl;

    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "Get currencies exception: " << e.what();
        out << "✗ Error: " << e.what() << std::endl;
    }
}

bool repl::auto_connect() {
    try {
        if (connection_config_) {
            config_.host = connection_config_->host;
            config_.port = connection_config_->port;
            config_.client_identifier = connection_config_->client_identifier;
        } else {
            config_.host = "localhost";
            config_.port = 55555;
            config_.client_identifier = "ores-client";
        }

        BOOST_LOG_SEV(lg(), info) << "Auto-connecting to " << config_.host << ":"
                                  << config_.port << " (identifier: "
                                  << config_.client_identifier << ")";

        if (client_ && client_->is_connected()) {
            BOOST_LOG_SEV(lg(), info) << "Disconnecting existing connection";
            client_->disconnect();
        }

        client_ = std::make_shared<comms::net::client>(config_);

        client_->connect_sync();

        BOOST_LOG_SEV(lg(), info) << "Successfully auto-connected";
        std::cout << "✓ Auto-connected to " << config_.host << ":" << config_.port << std::endl;
        return true;

    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "Auto-connect exception: " << e.what();
        std::cout << "✗ Auto-connect error: " << e.what() << std::endl;
        return false;
    }
}

bool repl::auto_login() {
    try {
        if (!login_config_) {
            return false;
        }

        if (!client_ || !client_->is_connected()) {
            BOOST_LOG_SEV(lg(), warn) << "Not connected to server. Cannot auto-login.";
            std::cout << "✗ Not connected to server. Cannot auto-login." << std::endl;
            return false;
        }

        BOOST_LOG_SEV(lg(), debug) << "Creating auto-login request for user: " << login_config_->username;

        accounts::messaging::login_request request{
            .username = login_config_->username,
            .password = login_config_->password
        };

        auto request_payload = request.serialize();

        comms::protocol::frame request_frame(
            comms::protocol::message_type::login_request,
            0,
            std::move(request_payload));

        BOOST_LOG_SEV(lg(), debug) << "Sending auto-login request frame";

        auto response_result =
            client_->send_request_sync(std::move(request_frame));

        if (!response_result) {
            BOOST_LOG_SEV(lg(), error) << "Auto-login request failed with error code: "
                                      << static_cast<int>(response_result.error());
            std::cout << "✗ Auto-login request failed" << std::endl;
            return false;
        }

        BOOST_LOG_SEV(lg(), debug) << "Deserializing auto-login response";

        auto response = accounts::messaging::login_response::deserialize(
            response_result->payload());

        if (!response) {
            BOOST_LOG_SEV(lg(), error) << "Failed to deserialize auto-login response: "
                                      << static_cast<int>(response.error());
            std::cout << "✗ Failed to parse auto-login response" << std::endl;
            return false;
        }

        if (response->success) {
            BOOST_LOG_SEV(lg(), info) << "Auto-login successful for user: " << response->username
                                    << " (ID: " << response->account_id << ")";

            std::cout << "✓ Auto-login successful!" << std::endl;
            std::cout << "  Username: " << response->username << std::endl;
            std::cout << "  Account ID: " << response->account_id << std::endl;
            std::cout << "  Admin: " << (response->is_admin ? "Yes" : "No") << std::endl;
            return true;
        } else {
            BOOST_LOG_SEV(lg(), warn) << "Auto-login failed: " << response->error_message;
            std::cout << "✗ Auto-login failed: " << response->error_message << std::endl;
            return false;
        }

    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "Auto-login exception: " << e.what();
        std::cout << "✗ Auto-login error: " << e.what() << std::endl;
        return false;
    }
}

void repl::display_welcome() const {
    std::cout << "ORE Studio Client REPL v" << ORES_VERSION << std::endl;
    std::cout << "Type 'help' for available commands, 'exit' to quit" << std::endl;
    std::cout << std::endl;
}

void repl::process_create_account(std::ostream& out, std::string username,
    std::string password, std::string totp_secret, std::string email,
    bool is_admin) {
    try {
        if (!client_ || !client_->is_connected()) {
            out << "✗ Not connected to server. Use 'connect' command first."
                << std::endl;
            return;
        }

        BOOST_LOG_SEV(lg(), debug) << "Initiating create account request";

        BOOST_LOG_SEV(lg(), debug) << "Creating create account request.";

        accounts::messaging::create_account_request request{
            .username = std::move(username),
            .password = std::move(password),
            .totp_secret = std::move(totp_secret),
            .email = std::move(email),
            .is_admin = is_admin
        };

        BOOST_LOG_SEV(lg(), debug) << "Request: " << request;
        auto request_payload = request.serialize();

        comms::protocol::frame request_frame(
            comms::protocol::message_type::create_account_request,
            0,
            std::move(request_payload));

        BOOST_LOG_SEV(lg(), debug) << "Sending request frame";

        auto response_result =
            client_->send_request_sync(std::move(request_frame));

        if (!response_result) {
            BOOST_LOG_SEV(lg(), error) << "Request failed with error code: "
                                      << static_cast<int>(response_result.error());
            out << "✗ Request failed." << std::endl;
            return;
        }

        if (response_result->header().type == comms::protocol::message_type::error_response) {
            auto err_resp = comms::protocol::error_response::deserialize(
                response_result->payload());
            if (err_resp) {
                BOOST_LOG_SEV(lg(), error) << "Server returned error: "
                                          << err_resp->message;
                out << "✗ Error: " << err_resp->message << std::endl;
            } else {
                BOOST_LOG_SEV(lg(), error) << "Failed to deserialize error response";
                out << "✗ Server error (could not parse error message)"
                    << std::endl;
            }
            return;
        }

        BOOST_LOG_SEV(lg(), debug) << "Deserializing response";

        auto response = accounts::messaging::create_account_response::deserialize(
            response_result->payload());

        if (!response) {
            BOOST_LOG_SEV(lg(), error) << "Failed to deserialize response: "
                                       << static_cast<int>(response.error());
            out << "✗ Failed to parse response." << std::endl;
            return;
        }

        BOOST_LOG_SEV(lg(), info) << "Successfully created account with ID: "
                                  << response->account_id;

        out << "✓ Account created with ID: " << response->account_id << std::endl;

    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "Create account exception: " << e.what();
        out << "✗ Error: " << e.what() << std::endl;
    }
}

void repl::process_list_accounts(std::ostream& out) {
    try {
        if (!client_ || !client_->is_connected()) {
            out << "✗ Not connected to server. Use 'connect' command first."
                << std::endl;
            return;
        }

        BOOST_LOG_SEV(lg(), debug) << "Creating list accounts request";

        accounts::messaging::list_accounts_request request;
        auto request_payload = request.serialize();

        comms::protocol::frame request_frame(
            comms::protocol::message_type::list_accounts_request, 0,
            std::move(request_payload));

        BOOST_LOG_SEV(lg(), debug) << "Sending request frame";

        auto response_result =
            client_->send_request_sync(std::move(request_frame));

        if (!response_result) {
            BOOST_LOG_SEV(lg(), error) << "Request failed with error code: "
                                      << static_cast<int>(response_result.error());
            out << "✗ Request failed" << std::endl;
            return;
        }

        // Check if server returned an error response
        if (response_result->header().type == comms::protocol::message_type::error_response) {
            auto err_resp = comms::protocol::error_response::deserialize(
                response_result->payload());
            if (err_resp) {
                BOOST_LOG_SEV(lg(), error) << "Server returned error: "
                                          << err_resp->message;
                out << "✗ Error: " << err_resp->message << std::endl;
            } else {
                BOOST_LOG_SEV(lg(), error) << "Failed to deserialize error response";
                out << "✗ Server error (could not parse error message)" << std::endl;
            }
            return;
        }

        BOOST_LOG_SEV(lg(), debug) << "Deserializing response";

        auto response = accounts::messaging::list_accounts_response::deserialize(
            response_result->payload());

        if (!response) {
            BOOST_LOG_SEV(lg(), error) << "Failed to deserialize response: "
                                      << static_cast<int>(response.error());
            out << "✗ Failed to parse response" << std::endl;
            return;
        }

        BOOST_LOG_SEV(lg(), info) << "Successfully retrieved "
                                << response->accounts.size() << " accounts";

        out << response->accounts << std::endl;

    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "List accounts exception: " << e.what();
        out << "✗ Error: " << e.what() << std::endl;
    }
}

void repl::process_login(std::ostream& out, std::string username, std::string password) {
    try {

        if (!client_ || !client_->is_connected()) {
            out << "✗ Not connected to server. Use 'connect' command first."
                << std::endl;
            return;
        }

        BOOST_LOG_SEV(lg(), debug) << "Creating login request for user: " << username;

        accounts::messaging::login_request request{
            .username = std::move(username),
            .password = std::move(password)
        };

        auto request_payload = request.serialize();

        comms::protocol::frame request_frame(
            comms::protocol::message_type::login_request,
            0,
            std::move(request_payload));

        BOOST_LOG_SEV(lg(), debug) << "Sending login request frame";

        auto response_result =
            client_->send_request_sync(std::move(request_frame));

        if (!response_result) {
            BOOST_LOG_SEV(lg(), error) << "Login request failed with error code: "
                                      << static_cast<int>(response_result.error());
            out << "✗ Login request failed" << std::endl;
            return;
        }

        BOOST_LOG_SEV(lg(), debug) << "Deserializing login response";

        auto response = accounts::messaging::login_response::deserialize(
            response_result->payload());

        if (!response) {
            BOOST_LOG_SEV(lg(), error) << "Failed to deserialize login response: "
                                      << static_cast<int>(response.error());
            out << "✗ Failed to parse login response" << std::endl;
            return;
        }

        if (response->success) {
            BOOST_LOG_SEV(lg(), info) << "Login successful for user: " << response->username
                                    << " (ID: " << response->account_id << ")";

            out << "✓ Login successful." << std::endl
                << "  Username: " << response->username << std::endl
                << "  Account ID: " << response->account_id << std::endl
                << "  Admin: " << (response->is_admin ? "Yes" : "No") << std::endl;
        } else {
            BOOST_LOG_SEV(lg(), warn) << "Login failed: " << response->error_message;
            out << "✗ Login failed: " << response->error_message << std::endl;
        }

    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "Login exception: " << e.what();
        out << "✗ Error: " << e.what() << std::endl;
    }
}

void repl::process_unlock_account(std::ostream& out, std::string account_id_str) {
    try {
        if (!client_ || !client_->is_connected()) {
            out << "✗ Not connected to server. Use 'connect' command first."
                << std::endl;
            return;
        }

        BOOST_LOG_SEV(lg(), debug) << "Creating unlock account request for ID: "
                                   << account_id_str;

        boost::uuids::uuid account_id;
        try {
            account_id = boost::lexical_cast<boost::uuids::uuid>(account_id_str);
        } catch (const boost::bad_lexical_cast& e) {
            BOOST_LOG_SEV(lg(), error) << "Invalid account ID format: " << account_id_str;
            out << "✗ Invalid account ID format. Expected UUID." << std::endl;
            return;
        }

        accounts::messaging::unlock_account_request request{
            .account_id = account_id
        };

        auto request_payload = request.serialize();

        comms::protocol::frame request_frame(
            comms::protocol::message_type::unlock_account_request,
            0,
            std::move(request_payload));

        BOOST_LOG_SEV(lg(), debug) << "Sending unlock account request frame";

        auto response_result =
            client_->send_request_sync(std::move(request_frame));

        if (!response_result) {
            BOOST_LOG_SEV(lg(), error) << "Unlock account request failed with error code: "
                                      << static_cast<int>(response_result.error());
            out << "✗ Unlock account request failed" << std::endl;
            return;
        }

        BOOST_LOG_SEV(lg(), debug) << "Deserializing unlock account response";

        auto response = accounts::messaging::unlock_account_response::deserialize(
            response_result->payload());

        if (!response) {
            BOOST_LOG_SEV(lg(), error) << "Failed to deserialize unlock account response: "
                                      << static_cast<int>(response.error());
            out << "✗ Failed to parse unlock account response" << std::endl;
            return;
        }

        if (response->success) {
            BOOST_LOG_SEV(lg(), info) << "Successfully unlocked account: " << account_id;

            out << "✓ Account unlocked successfully!" << std::endl
                << "  Account ID: " << account_id << std::endl;
        } else {
            BOOST_LOG_SEV(lg(), warn) << "Failed to unlock account: " << response->error_message;
            out << "✗ Failed to unlock account: " << response->error_message
                << std::endl;
        }

    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "Unlock account exception: " << e.what();
        out << "✗ Error: " << e.what() << std::endl;
    }
}

}
