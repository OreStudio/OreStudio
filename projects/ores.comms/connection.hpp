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
#ifndef ORES_COMMS_CONNECTION_HPP
#define ORES_COMMS_CONNECTION_HPP

#include <boost/asio/ip/tcp.hpp>
#include <boost/asio/ssl.hpp>
#include <boost/cobalt.hpp>
#include "ores.comms/protocol/frame.hpp"

namespace ores::comms {

namespace cobalt = boost::cobalt;
using tcp = boost::asio::ip::tcp;
namespace ssl = boost::asio::ssl;

/**
 * @brief SSL connection wrapper for frame-based communication.
 *
 * Provides async read/write operations for protocol frames over SSL/TLS.
 */
class connection final {
public:
    using ssl_socket = ssl::stream<tcp::socket>;

    /**
     * @brief Construct connection from an SSL socket.
     */
    explicit connection(ssl_socket socket);

    /**
     * @brief Perform SSL handshake as server.
     */
    cobalt::promise<void> ssl_handshake_server();

    /**
     * @brief Perform SSL handshake as client.
     */
    cobalt::promise<void> ssl_handshake_client();

    /**
     * @brief Read a complete frame from the connection.
     *
     * Reads the frame header first, then reads the payload based on
     * the size specified in the header.
     */
    cobalt::promise<std::expected<protocol::frame, protocol::error_code>> read_frame();

    /**
     * @brief Write a frame to the connection.
     *
     * Serializes the frame and writes it to the socket.
     */
    cobalt::promise<void> write_frame(const protocol::frame& frame);

    /**
     * @brief Check if the connection is open.
     */
    bool is_open() const;

    /**
     * @brief Close the connection.
     */
    void close();

    /**
     * @brief Get the remote endpoint address.
     */
    std::string remote_address() const;

private:
    ssl_socket socket_;
};

}

#endif
