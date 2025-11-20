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
#include <boost/asio/co_spawn.hpp>
#include <boost/asio/detached.hpp>
#include <cli/cli.h>
#include "ores.utility/log/make_logger.hpp"

namespace ores::client::app::commands::connection {

using namespace ores::utility::log;

namespace {
auto& lg() {
    static auto instance = make_logger("ores.client.app.commands.connection");
    return instance;
}
}

void register_commands(
    ::cli::Menu& root_menu,
    boost::asio::io_context& io_ctx,
    std::shared_ptr<comms::net::client>& client,
    comms::net::client_options& config) {

    root_menu.Insert("connect",
        [&io_ctx, &client, &config](std::ostream& out, std::string host,
            std::string port, std::string identifier) {
            try {
                BOOST_LOG_SEV(lg(), debug) << "Initiating connection request";
                auto executor = io_ctx.get_executor();
                boost::asio::co_spawn(executor,
                    process_connect(std::ref(out), client, config,
                        std::move(host), std::move(port), std::move(identifier)),
                    boost::asio::detached);
            } catch (const std::exception& e) {
                BOOST_LOG_SEV(lg(), error) << "Error setting up connection: " << e.what();
                out << "✗ Error: " << e.what() << std::endl;
            }
        },
        "Connect to server (optional: host port identifier)");

    root_menu.Insert("disconnect",
        [&client](std::ostream& out) {
            process_disconnect(client);
            out << "✓ Disconnected from server" << std::endl;
        },
        "Disconnect from server");
}

boost::asio::awaitable<void> process_connect(
    std::ostream& out,
    std::shared_ptr<comms::net::client>& client,
    comms::net::client_options& config,
    std::string host,
    std::string port,
    std::string identifier) {

    try {
        if (!host.empty()) {
            BOOST_LOG_SEV(lg(), debug) << "Updating host to: " << host;
            config.host = std::move(host);
        }

        if (!port.empty()) {
            try {
                config.port = static_cast<std::uint16_t>(std::stoi(port));
                BOOST_LOG_SEV(lg(), debug) << "Updating port to: " << config.port;
            } catch (...) {
                BOOST_LOG_SEV(lg(), error) << "Invalid port number: " << port;
                out << "✗ Invalid port number: " << port << std::endl;
                co_return;
            }
        }

        if (!identifier.empty()) {
            BOOST_LOG_SEV(lg(), debug) << "Updating client identifier to: " << identifier;
            config.client_identifier = std::move(identifier);
        }

        BOOST_LOG_SEV(lg(), info) << "Connecting to " << config.host << ":"
                                << config.port << " (identifier: "
                                << config.client_identifier << ")";

        if (client && client->is_connected()) {
            BOOST_LOG_SEV(lg(), info) << "Disconnecting existing connection";
            client->disconnect();
        }

        client = std::make_shared<comms::net::client>(config);

        co_await client->connect();

        BOOST_LOG_SEV(lg(), info) << "Successfully connected";
        out << "✓ Connected\nores-client> " << std::flush;

    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "Connect exception: " << e.what();
        out << "✗ Error: " << e.what() << "\nores-client> " << std::flush;
    }
}

void process_disconnect(std::shared_ptr<comms::net::client>& client) {
    if (!client) {
        BOOST_LOG_SEV(lg(), warn) << "No client instance.";
        return;
    }

    if (!client->is_connected()) {
        BOOST_LOG_SEV(lg(), debug) << "Already disconnected.";
        return;
    }

    client->disconnect();
    BOOST_LOG_SEV(lg(), info) << "Disconnected from server.";
}

}
