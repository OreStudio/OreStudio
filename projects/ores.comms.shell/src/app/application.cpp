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
#include "ores.comms.shell/app/application.hpp"

#include <iostream>
#include <boost/uuid/uuid_io.hpp>
#include "ores.utility/version/version.hpp"
#include "ores.comms/messaging/message_type.hpp"
#include "ores.comms/messaging/protocol.hpp"
#include "ores.comms/net/client.hpp"
#include "ores.comms/net/client_session.hpp"
#include "ores.iam/messaging/bootstrap_protocol.hpp"
#include "ores.iam/client/auth_helpers.hpp"
#include "ores.comms.shell/app/repl.hpp"
#include "ores.comms.shell/app/commands/compression_commands.hpp"
#include "ores.nats/service/nats_client.hpp"

namespace ores::comms::shell::app {

using namespace ores::logging;
using comms::net::client_session;

application::application(
    std::optional<comms::net::client_options> connection_config,
    std::optional<config::login_options> login_config,
    std::optional<telemetry::domain::telemetry_context> telemetry_ctx,
    std::optional<comms::service::telemetry_streaming_options> streaming_options)
    : connection_config_(std::move(connection_config)),
      login_config_(std::move(login_config)),
      telemetry_ctx_(std::move(telemetry_ctx)),
      streaming_options_(std::move(streaming_options)) {
}

namespace {

inline std::string_view anon_logger_name = "ores.comms.shell.app.application";

auto& anon_lg() {
    static auto instance = make_logger(anon_logger_name);
    return instance;
}

bool auto_connect(client_session& session, std::ostream& out,
    const std::optional<comms::net::client_options>& connection_config) {

    // Build NATS URL from connection config (host:port) or use default
    std::string host = "localhost";
    std::uint16_t port = 4222;
    if (connection_config) {
        host = connection_config->host;
        port = connection_config->port;
    }

    const std::string nats_url = "nats://" + host + ":" + std::to_string(port);

    try {
        ores::nats::config::nats_options opts;
        opts.url = nats_url;
        auto nats_client = std::make_shared<ores::nats::service::nats_client>(opts);
        nats_client->connect_sync();

        auto result = session.attach_client(nats_client);
        if (result) {
            out << "✓ Connected to " << nats_url << std::endl;
            return true;
        } else {
            out << "✗ Auto-connect failed: " << to_string(result.error()) << std::endl;
            return false;
        }
    } catch (const std::exception& e) {
        out << "✗ Auto-connect failed: " << e.what() << std::endl;
        return false;
    }
}

bool auto_login(client_session& session, std::ostream& out,
    const config::login_options& login_config) {
    auto result = iam::client::login(session, login_config.username,
        login_config.password);
    if (result.success) {
        out << "✓ Logged in as: " << result.username << std::endl;
        out << "  Tenant: " << result.tenant_name
            << " (" << result.tenant_id << ")" << std::endl;
        return true;
    }
    out << "✗ Auto-login failed: " << result.error_message << std::endl;
    return false;
}

void check_bootstrap_status(client_session& session, std::ostream& out) {
    using iam::messaging::bootstrap_status_request;

    auto result = session.process_request(bootstrap_status_request{});

    if (!result) {
        BOOST_LOG_SEV(anon_lg(), debug) << "Bootstrap status check failed: "
                                        << to_string(result.error());
        return;
    }

    const auto& response = *result;
    if (response.is_in_bootstrap_mode) {
        BOOST_LOG_SEV(anon_lg(), warn) << "System is in bootstrap mode: "
                                       << response.message;
        out << std::endl;
        out << "⚠ WARNING: System is in BOOTSTRAP MODE" << std::endl;
        out << "  " << response.message << std::endl;
        out << "  Use 'bootstrap <username> <password> <email>' to create the initial admin account." << std::endl;
        out << std::endl;
    } else {
        BOOST_LOG_SEV(anon_lg(), debug) << "Bootstrap status: not in bootstrap mode";
    }
}

} // anonymous namespace

void application::run() {
    BOOST_LOG_SEV(lg(), info) << utility::version::format_startup_message(
        "ORE Studio Shell",
        comms::messaging::PROTOCOL_VERSION_MAJOR,
        comms::messaging::PROTOCOL_VERSION_MINOR);
    if (telemetry_ctx_) {
        BOOST_LOG_SEV(lg(), debug)
            << "Telemetry context active - trace_id: "
            << telemetry_ctx_->get_trace_id().to_hex()
            << ", span_id: " << telemetry_ctx_->get_span_id().to_hex();
    }

    std::unique_ptr<comms::service::telemetry_streaming_service> streaming_service;

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

                // Start telemetry streaming if configured and connected
                if (streaming_options_ && session.is_connected()) {
                    BOOST_LOG_SEV(lg(), info)
                        << "Starting telemetry streaming for: "
                        << streaming_options_->source_name;
                    try {
                        auto ssl_client = std::dynamic_pointer_cast<comms::net::client>(
                            session.get_client());
                        if (ssl_client) {
                            streaming_service =
                                std::make_unique<comms::service::telemetry_streaming_service>(
                                    ssl_client, *streaming_options_);
                            streaming_service->start();
                            std::cout << "✓ Telemetry streaming enabled" << std::endl;
                        } else {
                            BOOST_LOG_SEV(lg(), debug)
                                << "Telemetry streaming not supported with NATS transport";
                        }
                    } catch (const std::exception& e) {
                        BOOST_LOG_SEV(lg(), error)
                            << "Failed to start telemetry streaming: " << e.what();
                    }
                }
            }
        }

        repl client_repl(session);
        client_repl.run();

        // Stop streaming when REPL exits
        if (streaming_service) {
            BOOST_LOG_SEV(lg(), info)
                << "Stopping telemetry streaming. Sent: "
                << streaming_service->total_sent()
                << ", dropped: " << streaming_service->total_dropped();
            streaming_service->stop();
        }
    } catch (const std::exception& e) {
        // Ensure streaming is stopped on error
        if (streaming_service) {
            streaming_service->stop();
        }
        BOOST_LOG_SEV(lg(), error) << "Client REPL error: " << e.what();
        throw;
    }
}

}
