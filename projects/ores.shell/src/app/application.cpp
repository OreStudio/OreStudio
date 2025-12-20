/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2025 Marco Craveiro <marco.craveiro@gmail.com>
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
 * MA 02110-1301, USA.
 *
 */
#include "ores.shell/app/application.hpp"

#include <iostream>
#include "ores.comms/net/client_session.hpp"
#include "ores.iam/messaging/login_protocol.hpp"
#include "ores.iam/messaging/bootstrap_protocol.hpp"
#include "ores.shell/app/repl.hpp"
#include "ores.shell/app/commands/compression_commands.hpp"

namespace ores::shell::app {

using namespace ores::utility::log;
using comms::net::client_session;
using comms::net::client_session_info;

application::application(std::optional<comms::net::client_options> connection_config,
                         std::optional<config::login_options> login_config)
    : connection_config_(std::move(connection_config)),
      login_config_(std::move(login_config)) {
}

namespace {

bool auto_connect(client_session& session, std::ostream& out,
    const std::optional<comms::net::client_options>& connection_config) {
    comms::net::client_options config = connection_config
        .transform([](const auto& cfg) {
            return comms::net::client_options{
                .host = cfg.host,
                .port = cfg.port,
                .client_identifier = cfg.client_identifier,
                .verify_certificate = false // FIXME: for now
            };
        })
        .value_or(comms::net::client_options{
                .host = "localhost",
                .port = 55555,
                .client_identifier = "ores-shell" });

    // Disable heartbeat for shell - it's synchronous request-response
    config.heartbeat_enabled = false;
    // Use compression setting from compression_commands
    config.supported_compression =
        commands::compression_commands::get_supported_compression();

    auto result = session.connect(std::move(config));
    if (result) {
        out << "✓ Connected to " << config.host << ":" << config.port << std::endl;
        return true;
    } else {
        out << "✗ Auto-connect failed: " << to_string(result.error()) << std::endl;
        return false;
    }
}

bool auto_login(client_session& session, std::ostream& out,
    const config::login_options& login_config) {
    using iam::messaging::login_request;

    auto result = session.process_request(login_request{
        .username = login_config.username,
        .password = login_config.password
    });

    if (!result) {
        out << "✗ Auto-login failed: " << to_string(result.error()) << std::endl;
        return false;
    }

    const auto& response = *result;
    if (!response.success) {
        out << "✗ Auto-login failed: " << response.error_message << std::endl;
        return false;
    }

    out << "✓ Logged in as: " << login_config.username << std::endl;

    // Update session state
    session.set_session_info(client_session_info{
        .account_id = response.account_id,
        .username = login_config.username,
        .is_admin = response.is_admin
    });
    return true;
}

void check_bootstrap_status(client_session& session, std::ostream& out) {
    using iam::messaging::bootstrap_status_request;

    auto result = session.process_request(bootstrap_status_request{});

    if (!result) {
        // Silently ignore errors - bootstrap check is optional
        return;
    }

    const auto& response = *result;
    if (response.is_in_bootstrap_mode) {
        out << std::endl;
        out << "⚠ WARNING: System is in BOOTSTRAP MODE" << std::endl;
        out << "  " << response.message << std::endl;
        out << "  Use 'bootstrap <username> <password> <email>' to create the initial admin account." << std::endl;
        out << std::endl;
    }
}

} // anonymous namespace

void application::run() const {
    BOOST_LOG_SEV(lg(), info) << "Starting client REPL";

    try {
        client_session session;

        // Auto-connect and auto-login if configuration was provided
        if (connection_config_) {
            BOOST_LOG_SEV(lg(), debug) << "Configuration provided. "
                                       << "Attempting auto-connect and auto-login.";

            bool connected = auto_connect(session, std::cout, connection_config_);
            if (connected) {
                check_bootstrap_status(session, std::cout);
                if (login_config_) {
                    auto_login(session, std::cout, *login_config_);
                }
            }
        }

        repl client_repl(session);
        client_repl.run();
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "Client REPL error: " << e.what();
        throw;
    }
}

}
