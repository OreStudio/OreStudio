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
#include "ores.shell/app/commands/connection_commands.hpp"

#include <ostream>
#include <functional>
#include <cli/cli.h>
#include "ores.comms/messaging/message_types.hpp"
#include "ores.accounts/messaging/bootstrap_protocol.hpp"
#include "ores.shell/app/commands/compression_commands.hpp"

namespace ores::shell::app::commands {

using namespace ores::utility::log;
using comms::net::client_session;

namespace {

void check_bootstrap_status(client_session& session, std::ostream& out) {
    using accounts::messaging::bootstrap_status_request;
    using accounts::messaging::bootstrap_status_response;
    using comms::messaging::message_type;

    auto result = session.process_request<bootstrap_status_request,
                                          bootstrap_status_response,
                                          message_type::bootstrap_status_request>
        (bootstrap_status_request{});

    if (!result) {
        // Silently ignore errors - bootstrap check is optional
        return;
    }

    const auto& response = *result;
    if (response.is_in_bootstrap_mode) {
        out << std::endl;
        out << "⚠ WARNING: System is in BOOTSTRAP MODE" << std::endl;
        out << "  " << response.message << std::endl;
        out << std::endl;
    }
}

} // anonymous namespace

void connection_commands::
register_commands(cli::Menu& root_menu, client_session& session) {
    root_menu.Insert("connect", [&session](std::ostream & out,
            std::string host, std::string port, std::string identifier) {
        process_connect(std::ref(out), std::ref(session),
            std::move(host), std::move(port), std::move(identifier));
        }, "Connect to server (optional: host port identifier)");

    root_menu.Insert("disconnect", [&session](std::ostream& out) {
        process_disconnect(std::ref(out), std::ref(session));
    }, "Disconnect from server");
}

void connection_commands::
process_connect(std::ostream& out, client_session& session,
    std::string host, std::string port, std::string identifier) {

    comms::net::client_options config;

    // Use provided values or defaults
    config.host = host.empty() ? "localhost" : std::move(host);

    if (!port.empty()) {
        try {
            config.port = static_cast<std::uint16_t>(std::stoi(port));
        } catch (...) {
            out << "✗ Invalid port number: " << port << std::endl;
            return;
        }
    }

    if (!identifier.empty()) {
        config.client_identifier = std::move(identifier);
    } else {
        config.client_identifier = "ores-shell";
    }

    config.verify_certificate = false; // FIXME: for now
    config.heartbeat_enabled = true;   // Enable to receive async notifications
    config.supported_compression = compression_commands::get_supported_compression();

    auto result = session.connect(std::move(config));
    if (result) {
        out << "✓ Connected to " << config.host << ":" << config.port << std::endl;

        // Check bootstrap status after successful connection
        check_bootstrap_status(session, out);
    } else {
        out << "✗ Connection failed: "
            << comms::net::to_string(result.error()) << std::endl;
    }
}

void connection_commands::
process_disconnect(std::ostream& out, client_session& session) {
    if (!session.is_connected()) {
        out << "✗ Not connected." << std::endl;
        return;
    }

    session.disconnect();
    out << "✓ Disconnected." << std::endl;
}

}
