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
#ifndef ORES_COMMS_SERVER_HPP
#define ORES_COMMS_SERVER_HPP

#include <cstdint>
#include <string>
#include <boost/asio/ip/tcp.hpp>
#include <boost/asio/ssl.hpp>
#include <boost/cobalt.hpp>

namespace ores::comms {

namespace cobalt = boost::cobalt;
using tcp = boost::asio::ip::tcp;
namespace ssl = boost::asio::ssl;

/**
 * @brief Configuration for the server.
 */
struct server_config final {
    uint16_t port = 55555;
    uint32_t max_connections = 10;
    std::string certificate_file = "server.crt";
    std::string private_key_file = "server.key";
    std::string server_identifier = "ores-server";
};

/**
 * @brief ORES protocol server.
 *
 * Accepts SSL connections, performs handshake, and manages client sessions.
 */
class server final {
public:
    /**
     * @brief Construct server with configuration.
     */
    explicit server(server_config config);

    /**
     * @brief Run the server.
     *
     * Accepts connections and spawns sessions until stopped.
     */
    cobalt::promise<void> run();

private:
    /**
     * @brief Accept connections and spawn sessions.
     */
    cobalt::promise<void> accept_loop(cobalt::wait_group& workers);

    /**
     * @brief Create and configure SSL context.
     */
    void setup_ssl_context();

    server_config config_;
    ssl::context ssl_ctx_;
};

}

#endif
