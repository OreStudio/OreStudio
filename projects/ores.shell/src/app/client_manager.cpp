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
        comms::net::client_options config;
        if (connection_config_) {
            config.host = connection_config_->host;
            config.port = connection_config_->port;
            config.client_identifier = connection_config_->client_identifier;
        } else {
            config.host = "localhost";
            config.port = 55555;
            config.client_identifier = "ores-shell";
        }

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

    accounts::messaging::login_request request{
        .username = login_config_->username,
        .password = login_config_->password
    };

    using accounts::messaging::login_response;
    std::optional<login_response> response = process_request<
        accounts::messaging::login_request,
        accounts::messaging::login_response,
        comms::protocol::message_type::login_request
    >(request);

    if (!response)
        return false;

    if (response->success) {
        BOOST_LOG_SEV(lg(), info) << "Auto-login successful for user: " << response->username
                                  << " (ID: " << response->account_id << ")";

        output_ << "✓ Auto-login successful!" << std::endl;
        output_ << "  Username: " << response->username << std::endl;
        output_ << "  Account ID: " << response->account_id << std::endl;
        output_ << "  Admin: " << (response->is_admin ? "Yes" : "No") << std::endl;
        return true;
    } else {
        BOOST_LOG_SEV(lg(), warn) << "Auto-login failed: " << response->error_message;
        output_ << "✗ Auto-login failed: " << response->error_message << std::endl;
        return false;
    }
}

client_manager::client_manager(std::ostream& out,
                               std::optional<comms::net::client_options> connection_config,
                               std::optional<config::login_options> login_config)
    : output_(out), connection_config_(std::move(connection_config)),
      login_config_(std::move(login_config)) {}

void client_manager::initialise() {
    BOOST_LOG_SEV(lg(), debug) << "Initialising client manager.";
    if (connection_config_) {
        BOOST_LOG_SEV(lg(), debug) << "Configuration was provided. "
                                   << "Will attempt to auto-connect and auto-login.";

        const bool connected = auto_connect();
        if (connected && login_config_)
            auto_login();
    }
}

void client_manager::connect(std::string host, std::string port, std::string identifier) {
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
                return;
            }
        }
        if (!identifier.empty()) {
            BOOST_LOG_SEV(lg(), debug) << "Updating client identifier to: "
                                     << identifier;
            config.client_identifier = std::move(identifier);
        }

        // FIXME: for now
        config.verify_certificate = false;
        connect(config);
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "Connect exception: " << e.what();
        output_ << "✗ Error: " << e.what() << std::endl;
    }
}

}
