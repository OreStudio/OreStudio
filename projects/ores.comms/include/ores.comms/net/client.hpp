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
#include <atomic>
#include <memory>
#include <cstdint>
#include <functional>
#include <string_view>
#include <boost/asio/awaitable.hpp>
#include <boost/asio/ssl.hpp>
#include <boost/asio/strand.hpp>
#include <boost/asio/ip/tcp.hpp>
#include <boost/asio/io_context.hpp>
#include "ores.utility/log/make_logger.hpp"
#include "ores.comms/net/client_options.hpp"
#include "ores.comms/net/connection.hpp"
#include "ores.comms/net/pending_request_map.hpp"

namespace ores::comms::net {

/**
 * @brief Callback invoked when client detects server disconnect.
 *
 * Called from the heartbeat coroutine when ping fails or times out.
 * Should be thread-safe as it may be called from different executors.
 */
using disconnect_callback_t = std::function<void()>;

/**
 * @brief ORES protocol client.
 *
 * Connects to server via SSL, performs handshake, and manages communication.
 */
class client final {
private:
    inline static std::string_view logger_name = "ores.comms.net.client";

    static auto& lg() {
        using namespace ores::utility::log;
        static auto instance = make_logger(logger_name);
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

    /**
     * @brief Run heartbeat loop to detect disconnections.
     *
     * Periodically sends ping messages and waits for pong responses.
     * Exits when cancelled or when disconnect is detected.
     */
    boost::asio::awaitable<void> run_heartbeat();

    /**
     * @brief Run the message loop that reads all incoming frames.
     *
     * Single reader coroutine that dispatches frames by type:
     * - Response/pong: completes pending request via correlation ID
     * - Notification: invokes notification callback (future)
     * - Error: fails pending request
     */
    boost::asio::awaitable<void> run_message_loop();

    /**
     * @brief Write a frame through the write strand.
     *
     * Serializes all writes to prevent interleaving.
     *
     * @param f The frame to write
     */
    boost::asio::awaitable<void> write_frame(const messaging::frame& f);

    /**
     * @brief Generate the next correlation ID.
     */
    std::uint32_t next_correlation_id();

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
     * @brief Destructor.
     *
     * Ensures proper cleanup order - strand and pending requests must be
     * destroyed before the io_context they reference.
     */
    ~client();

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
     * @brief Set callback to be invoked when disconnect is detected.
     *
     * The callback will be called from the heartbeat coroutine when
     * a ping fails or times out. It should be thread-safe.
     *
     * @param callback Function to call on disconnect (may be empty to disable)
     */
    void set_disconnect_callback(disconnect_callback_t callback);

    /**
     * @brief Send a request frame and receive response frame (async version).
     *
     * Generic method for sending any request and receiving response.
     * Manages sequence numbers automatically.
     *
     * @param request_frame The request frame to send
     * @return Expected containing response frame, or error_code
     */
    boost::asio::awaitable<std::expected<messaging::frame, messaging::error_code>>
    send_request(messaging::frame request_frame);

    /**
     * @brief Send a request frame and receive response frame (blocking version).
     *
     * Generic method for sending any request and receiving response.
     * Manages sequence numbers automatically.
     *
     * @param request_frame The request frame to send
     * @return Expected containing response frame, or error_code
     */
    std::expected<messaging::frame, messaging::error_code>
    send_request_sync(messaging::frame request_frame);

private:
    client_options config_;
    std::unique_ptr<boost::asio::io_context> io_ctx_; // Owned io_context for sync operations
    boost::asio::any_io_executor executor_;
    boost::asio::ssl::context ssl_ctx_;
    std::unique_ptr<connection> conn_;
    std::uint32_t sequence_number_;
    bool connected_;
    mutable std::mutex state_mutex_; // Thread-safe state protection
    disconnect_callback_t disconnect_callback_;

    // New infrastructure for unified message loop
    std::unique_ptr<boost::asio::strand<boost::asio::any_io_executor>> write_strand_;
    std::unique_ptr<pending_request_map> pending_requests_;
    std::atomic<std::uint32_t> correlation_id_counter_{1};
    bool message_loop_running_{false};
};

}

#endif
