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
#ifndef ORES_COMMS_NET_CONNECTION_HPP
#define ORES_COMMS_NET_CONNECTION_HPP

#include <boost/asio/ip/tcp.hpp>
#include <boost/asio/ssl.hpp>
#include <boost/asio/awaitable.hpp>
#include "ores.comms/protocol/frame.hpp"
#include "ores.utility/log/make_logger.hpp"

namespace ores::comms::net {

/**
 * @brief SSL connection wrapper for frame-based communication.
 *
 * Provides async read/write operations for protocol frames over SSL/TLS.
 */
class connection final {
private:
    static auto& lg() {
        using namespace ores::utility::log;
        static auto instance = make_logger("ores.comms.connection");
        return instance;
    }

public:
    using ssl_socket = boost::asio::ssl::stream<boost::asio::ip::tcp::socket>;

    /**
     * @brief Construct connection from an SSL socket.
     */
    explicit connection(ssl_socket socket);

    /**
     * @brief Perform SSL handshake as server.
     *
     * @param cancel_slot Optional cancellation slot for graceful shutdown
     */
    boost::asio::awaitable<void> ssl_handshake_server(
        boost::asio::cancellation_slot cancel_slot = {});

    /**
     * @brief Perform SSL handshake as client.
     *
     * @param cancel_slot Optional cancellation slot for graceful shutdown
     */
    boost::asio::awaitable<void> ssl_handshake_client(
        boost::asio::cancellation_slot cancel_slot = {});

    /**
     * @brief Read a complete frame from the connection.
     *
     * Reads the frame header first, then reads the payload based on
     * the size specified in the header.
     *
     * @param skip_version_check If true, skips protocol version validation.
     * This is useful during handshake to allow the server to send a proper
     * version mismatch response instead of rejecting the frame immediately.
     * @param cancel_slot Optional cancellation slot for graceful shutdown
     */
    boost::asio::awaitable<std::expected<protocol::frame, protocol::error_code>>
    read_frame(bool skip_version_check = false,
        boost::asio::cancellation_slot cancel_slot = {});

    /**
     * @brief Write a frame to the connection.
     *
     * Serializes the frame and writes it to the socket.
     *
     * @param frame The frame to write
     * @param cancel_slot Optional cancellation slot for graceful shutdown
     */
    boost::asio::awaitable<void> write_frame(const protocol::frame& frame,
        boost::asio::cancellation_slot cancel_slot = {});

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
