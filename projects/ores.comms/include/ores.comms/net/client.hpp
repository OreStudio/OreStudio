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
#ifndef ORES_COMMS_NET_CLIENT_HPP
#define ORES_COMMS_NET_CLIENT_HPP

#include <mutex>
#include <memory>
#include <string>
#include <cstdint>
#include <boost/asio/awaitable.hpp>
#include <boost/asio/ssl.hpp>
#include <boost/asio/ip/tcp.hpp>
#include <boost/asio/io_context.hpp>
#include "ores.utility/log/make_logger.hpp"
#include "ores.comms/net/client_options.hpp"
#include "ores.comms/net/connection.hpp"

namespace ores::comms {

/**
 * @brief ORES protocol client.
 *
 * Connects to server via SSL, performs handshake, and manages communication.
 */
class client final {
private:
    static auto& lg() {
        using namespace ores::utility::log;
        static auto instance = make_logger("ores.comms.client");
        return instance;
    }

    /**
     * @brief Setup SSL context for client.
     */
    void setup_ssl_context();

    /**
     * @brief Perform protocol handshake with server.
     *
     * @throws connection_error if handshake fails
     */
    boost::asio::awaitable<void> perform_handshake();

public:
    /**
     * @brief Construct client with configuration.
     *
     * Creates its own io_context for synchronous operations.
     */
    explicit client(client_options config);

    /**
     * @brief Construct client with configuration and executor.
     */
    explicit client(client_options config, boost::asio::any_io_executor executor);

    /**
     * @brief Connect to server and perform handshake (async version).
     *
     * @throws connection_error if connection or handshake fails
     */
    boost::asio::awaitable<void> connect();

    /**
     * @brief Connect to server and perform handshake (blocking version).
     *
     * @throws connection_error if connection or handshake fails
     */
    void connect_sync();

    /**
     * @brief Disconnect from server.
     */
    void disconnect();

    /**
     * @brief Check if client is connected.
     */
    bool is_connected() const;

    /**
     * @brief Send a request frame and receive response frame (async version).
     *
     * Generic method for sending any request and receiving response.
     * Manages sequence numbers automatically.
     *
     * @param request_frame The request frame to send
     * @return Expected containing response frame, or error_code
     */
    boost::asio::awaitable<std::expected<protocol::frame, protocol::error_code>>
    send_request(protocol::frame request_frame);

    /**
     * @brief Send a request frame and receive response frame (blocking version).
     *
     * Generic method for sending any request and receiving response.
     * Manages sequence numbers automatically.
     *
     * @param request_frame The request frame to send
     * @return Expected containing response frame, or error_code
     */
    std::expected<protocol::frame, protocol::error_code>
    send_request_sync(protocol::frame request_frame);

private:
    client_options config_;
    std::unique_ptr<boost::asio::io_context> io_ctx_; // Owned io_context for sync operations
    boost::asio::any_io_executor executor_;
    boost::asio::ssl::context ssl_ctx_;
    std::unique_ptr<connection> conn_;
    std::uint32_t sequence_number_;
    bool connected_;
    mutable std::mutex state_mutex_; // Thread-safe state protection
};

}

#endif
