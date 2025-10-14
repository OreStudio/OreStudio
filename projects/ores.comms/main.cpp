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
#include <boost/cobalt/main.hpp>
#include "ores.comms/client.hpp"

namespace cobalt = boost::cobalt;

cobalt::main co_main(int argc, char** argv) {
    try {
        // Configure client
        ores::comms::client_config config;
        config.host = "localhost";
        config.port = 55555;
        config.client_identifier = "test-client";
        config.verify_certificate = false; // For testing with self-signed certificates

        // Create client
        ores::comms::client cli(config, co_await cobalt::this_coro::executor);

        // Connect and perform handshake
        bool connected = co_await cli.connect();
        if (!connected) {
            std::printf("Failed to connect to server\n");
            co_return 1;
        }

        std::printf("Successfully connected and performed handshake!\n");
        std::printf("Client session established (disconnecting immediately for now)\n");

        // Future: Add message processing loop here
        cli.disconnect();

    } catch (const std::exception& e) {
        std::printf("Client error: %s\n", e.what());
        co_return 1;
    }

    co_return 0;
}
