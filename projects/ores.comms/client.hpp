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
#ifndef ORES_COMMS_CLIENT_HPP
#define ORES_COMMS_CLIENT_HPP

#include <memory>
#include <string>
#include <cstdint>
#include <boost/asio/io_context.hpp>
#include <boost/asio/ip/tcp.hpp>
#include <boost/asio/ssl.hpp>
#include <boost/cobalt.hpp>
#include "ores.comms/connection.hpp"

namespace ores::comms {

namespace cobalt = boost::cobalt;
using tcp = boost::asio::ip::tcp;
namespace ssl = boost::asio::ssl;

/**
 * @brief Configuration for the client.
 */
struct client_options final {
    /**
     * @brief Host to connect to.
     */
    std::string host = "localhost";

    /**
     * @brief Port to connect to.
     */
    std::uint16_t port = 55555;

    /**
     * @brief Client identifier to send in handshake.
     */
    std::string client_identifier = "ores-client";

    /**
     * @brief Whether to verify server certificate.
     */
    bool verify_certificate = true;
};

std::ostream& operator<<(std::ostream& s, const client_options& v);

/**
 * @brief ORES protocol client.
 *
 * Connects to server via SSL, performs handshake, and manages communication.
 */
class client final {
public:
    /**
     * @brief Construct client with configuration and executor.
     */
    explicit client(client_options config, boost::asio::any_io_executor executor);

    /**
     * @brief Connect to server and perform handshake.
     *
     * Returns true if connection and handshake succeed.
     */
    cobalt::promise<bool> connect();

    /**
     * @brief Disconnect from server.
     */
    void disconnect();

    /**
     * @brief Check if client is connected.
     */
    bool is_connected() const;

private:
    /**
     * @brief Perform protocol handshake with server.
     */
    cobalt::promise<bool> perform_handshake();

    /**
     * @brief Setup SSL context for client.
     */
    void setup_ssl_context();

    client_options config_;
    boost::asio::any_io_executor executor_;
    ssl::context ssl_ctx_;
    std::unique_ptr<connection> conn_;
    std::uint32_t sequence_number_;
    bool connected_;
};

}

#endif
