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
#include "ores.shell/app/client_manager.hpp"

#include <boost/uuid/uuid_io.hpp>
#include "ores.accounts/messaging/protocol.hpp"

namespace ores::shell::app {

using namespace ores::utility::log;
using comms::messaging::message_type;

void client_manager::connect(comms::net::client_options config) {
    BOOST_LOG_SEV(lg(), info) << "Connecting to " << config.host << ":"
                              << config.port << " (identifier: "
                              << config.client_identifier << ")";

    if (client_ && client_->is_connected()) {
        BOOST_LOG_SEV(lg(), info) << "Disconnecting existing connection";
        client_->disconnect();
    }

    client_ = std::make_shared<comms::net::client>(config);
    client_->connect_sync();

    BOOST_LOG_SEV(lg(), info) << "Successfully connected.";
    output_ << "✓ Connected." << std::endl;
}

bool client_manager::auto_connect() {
    try {
        using comms::net::client_options;
        client_options config = connection_config_
            .transform([](const auto& cfg) {
                return client_options{
                    .host = cfg.host,
                    .port = cfg.port,
                    .client_identifier = cfg.client_identifier,
                    .verify_certificate = false // FIXME: for now
                };
            })
            .value_or(client_options{
                    .host = "localhost",
                    .port = 55555,
                    .client_identifier = "ores-shell" });
        BOOST_LOG_SEV(lg(), info) << "Auto-connecting to " << config.host << ":"
                                  << config.port << " (identifier: "
                                  << config.client_identifier << ")";

        connect(config);
        return true;

    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "Auto-connect exception: " << e.what();
        output_ << "✗ Auto-connect error: " << e.what() << std::endl;
        return false;
    }
}

bool client_manager::auto_login() {
    if (!login_config_)
        return false;

    return login(login_config_->username, login_config_->password);
}

client_manager::client_manager(std::ostream& out,
    std::optional<comms::net::client_options> connection_config,
    std::optional<config::login_options> login_config)
    : output_(out), connection_config_(std::move(connection_config)),
      login_config_(std::move(login_config)) {

    BOOST_LOG_SEV(lg(), debug) << "Initialising client manager.";
    if (connection_config_) {
        BOOST_LOG_SEV(lg(), debug) << "Configuration was provided. "
                                   << "Will attempt to auto-connect and auto-login.";

        const bool connected = auto_connect();
        if (connected && login_config_)
            auto_login();
    }
}

client_manager::~client_manager() {
    disconnect();
}

bool client_manager::connect(std::string host, std::string port, std::string identifier) {
    try {
        comms::net::client_options config;
        if (!host.empty()) {
            BOOST_LOG_SEV(lg(), debug) << "Setting host to: " << host;
            config.host = std::move(host);
        }

        if (!port.empty()) {
            try {
                config.port = static_cast<std::uint16_t>(std::stoi(port));
                BOOST_LOG_SEV(lg(), debug) << "Updating port to: " << config.port;
            } catch (...) {
                BOOST_LOG_SEV(lg(), error) << "Invalid port number: " << port;
                output_ << "✗ Invalid port number: " << port << std::endl;
                return false;
            }
        }

        if (!identifier.empty()) {
            BOOST_LOG_SEV(lg(), debug) << "Updating client identifier to: "
                                     << identifier;
            config.client_identifier = std::move(identifier);
        }

        config.verify_certificate = false; // FIXME: for now
        BOOST_LOG_SEV(lg(), warn) << "Not verifying the servre certificate.";

        connect(config);
        return true;
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "Connect exception: " << e.what();
        output_ << "✗ Error: " << e.what() << std::endl;
    }
    return false;
}

bool client_manager::login(std::string username, std::string password) {
    try {
        BOOST_LOG_SEV(lg(), debug) << "Creating login request for user: "
                                   << username;

        using accounts::messaging::login_request;
        using accounts::messaging::login_response;
        return process_request<login_request, login_response, message_type::login_request>(
            login_request{
                .username = username,
                .password = password})
            .and_then([&](const auto& response) {
                if (response.success) {
                    BOOST_LOG_SEV(lg(), info) << "Login successful for user: "
                                              << response.username
                                              << " ID: " << response.account_id;

                    logged_in_account_id_ = response.account_id;

                    output_ << "✓ Login successful." << std::endl
                            << "  Username: '" << response.username << "'" << std::endl
                            << "  Account ID: '" << response.account_id << "'" << std::endl
                            << "  Admin: " << (response.is_admin ? "Yes" : "No") << std::endl;
                    return std::optional{response};
                } else {
                    BOOST_LOG_SEV(lg(), warn) << "Login failed: "
                                              << response.error_message;
                    output_ << "✗ Login failed: " << response.error_message << std::endl;
                }
                return std::optional<login_response>{};
            }).has_value();
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "Connect exception: " << e.what();
        output_ << "✗ Error: " << e.what() << std::endl;
    }
    return false;
}

bool client_manager::logout() {
    if (!client_ || !client_->is_connected()) {
        BOOST_LOG_SEV(lg(), debug) << "Not connected, nothing to logout.";
        return false;
    }

    if (!logged_in_account_id_) {
        BOOST_LOG_SEV(lg(), debug) << "No logged-in account, skipping logout.";
        return false;
    }

    try {
        BOOST_LOG_SEV(lg(), debug) << "Creating logout request for account: "
                                   << *logged_in_account_id_;

        using accounts::messaging::logout_request;
        using accounts::messaging::logout_response;
        auto result = process_request<logout_request, logout_response,
            message_type::logout_request>(
            logout_request{.account_id = *logged_in_account_id_});

        if (result && result->success) {
            BOOST_LOG_SEV(lg(), info) << "Logout successful for account: "
                                      << *logged_in_account_id_;
            output_ << "✓ Logout successful." << std::endl;
            logged_in_account_id_ = std::nullopt;
            return true;
        } else if (result) {
            BOOST_LOG_SEV(lg(), warn) << "Logout failed: " << result->message;
            output_ << "✗ Logout failed: " << result->message << std::endl;
        }
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "Logout exception: " << e.what();
        output_ << "✗ Logout error: " << e.what() << std::endl;
    }

    logged_in_account_id_ = std::nullopt;
    return false;
}

void client_manager::disconnect() {
    if (!client_) {
        BOOST_LOG_SEV(lg(), warn) << "No client instance.";
        return;
    }

    if (!client_->is_connected()) {
        BOOST_LOG_SEV(lg(), debug) << "Already disconnected.";
        logged_in_account_id_ = std::nullopt;
        return;
    }

    // Send logout message before disconnecting
    logout();

    // The server closes the connection after logout, but we call disconnect
    // to ensure proper cleanup on the client side
    client_->disconnect();
    BOOST_LOG_SEV(lg(), info) << "Disconnected from server.";
    output_ << "✓ Disconnected successfully from the server." << std::endl;
}

}
