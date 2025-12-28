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
#include <thread>
#include <memory>
#include <iosfwd>
#include <cstdint>
#include <concepts>
#include <expected>
#include <functional>
#include <string_view>
#include <boost/asio/awaitable.hpp>
#include <boost/asio/ssl.hpp>
#include <boost/asio/strand.hpp>
#include <boost/asio/ip/tcp.hpp>
#include <boost/asio/io_context.hpp>
#include "ores.telemetry/log/make_logger.hpp"
#include "ores.eventing/service/event_bus.hpp"
#include "ores.comms/net/client_options.hpp"
#include "ores.comms/net/connection.hpp"
#include "ores.comms/net/pending_request_map.hpp"
#include "ores.comms/messaging/frame.hpp"
#include "ores.comms/messaging/message_traits.hpp"
#include "ores.comms/recording/session_recorder.hpp"

namespace ores::comms::net {

/**
 * @brief Connection state for the client.
 */
enum class connection_state {
    disconnected,   ///< Not connected to server
    connecting,     ///< Initial connection in progress
    connected,      ///< Connected and ready for requests
    reconnecting    ///< Lost connection, attempting to reconnect
};

std::ostream& operator<<(std::ostream& s, connection_state v);

/**
 * @brief Callback invoked when client detects server disconnect.
 *
 * Called from the heartbeat coroutine when ping fails or times out.
 * The callback is invoked on the client's internal executor and should not
 * perform blocking operations. Any UI updates must be dispatched to the
 * appropriate UI thread.
 */
using disconnect_callback_t = std::function<void()>;

/**
 * @brief Callback invoked when client starts reconnection attempts.
 *
 * Called when connection is lost and auto-reconnect is enabled.
 * Allows UI to show reconnecting state to the user.
 * The callback is invoked on the client's internal executor and should not
 * perform blocking operations. Any UI updates must be dispatched to the
 * appropriate UI thread.
 */
using reconnecting_callback_t = std::function<void()>;

/**
 * @brief Callback invoked when client successfully reconnects.
 *
 * Called after auto-reconnect succeeds.
 * Allows UI to restore connected state display.
 * The callback is invoked on the client's internal executor and should not
 * perform blocking operations. Any UI updates must be dispatched to the
 * appropriate UI thread.
 */
using reconnected_callback_t = std::function<void()>;

/**
 * @brief Callback invoked when client receives a notification from server.
 *
 * Called when the server pushes a notification for a subscribed event type.
 * The callback is invoked on the client's internal executor and should not
 * perform blocking operations. Any UI updates must be dispatched to the
 * appropriate UI thread.
 *
 * @param event_type The fully qualified event type name (e.g., "ores.risk.currency_changed_event")
 * @param timestamp When the event occurred (UTC)
 */
using notification_callback_t = std::function<void(
    const std::string& event_type,
    std::chrono::system_clock::time_point timestamp)>;

/**
 * @brief ORES protocol client.
 *
 * Connects to server via SSL, performs handshake, and manages communication.
 */
class client final {
private:
    inline static std::string_view logger_name = "ores.comms.net.client";

    static auto& lg() {
        using namespace ores::telemetry::log;
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
     * @brief Run the reconnection loop after disconnect.
     *
     * Attempts to reconnect using exponential backoff with jitter.
     * Spawned automatically when auto_reconnect is enabled.
     */
    boost::asio::awaitable<void> run_reconnect_loop();

    /**
     * @brief Perform a single connection attempt.
     *
     * Core connection logic extracted for reuse in connect() and reconnect.
     */
    boost::asio::awaitable<void> perform_connection();

    /**
     * @brief Calculate backoff delay for a retry attempt.
     *
     * @param attempt The attempt number (0-based)
     * @return Delay with exponential backoff and jitter applied
     */
    std::chrono::milliseconds calculate_backoff(std::uint32_t attempt) const;

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
     *
     * @param config Client configuration options
     * @param event_bus Optional event bus for publishing connection events
     */
    explicit client(client_options config,
        std::shared_ptr<eventing::service::event_bus> event_bus = nullptr);

    /**
     * @brief Construct client with configuration and executor.
     *
     * @param config Client configuration options
     * @param executor The executor to use for async operations
     * @param event_bus Optional event bus for publishing connection events
     */
    explicit client(client_options config, boost::asio::any_io_executor executor,
        std::shared_ptr<eventing::service::event_bus> event_bus = nullptr);

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
     * Single attempt connection without retries.
     *
     * @throws connection_error if connection or handshake fails
     */
    boost::asio::awaitable<void> connect();

    /**
     * @brief Connect to server and perform handshake (blocking version).
     *
     * Single attempt connection without retries.
     *
     * @throws connection_error if connection or handshake fails
     */
    void connect_sync();

    /**
     * @brief Connect to server with retry (async version).
     *
     * Attempts connection with exponential backoff according to retry_options.
     *
     * @throws connection_error if all retry attempts fail
     */
    boost::asio::awaitable<void> connect_with_retry();

    /**
     * @brief Connect to server with retry (blocking version).
     *
     * Attempts connection with exponential backoff according to retry_options.
     *
     * @throws connection_error if all retry attempts fail
     */
    void connect_with_retry_sync();

    /**
     * @brief Disconnect from server.
     */
    void disconnect();

    /**
     * @brief Check if client is connected.
     *
     * Returns true only when in the connected state, not during reconnection.
     */
    bool is_connected() const;

    /**
     * @brief Get the current connection state.
     */
    connection_state get_state() const;

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
     * @brief Set callback to be invoked when reconnection starts.
     *
     * The callback will be called when auto-reconnect begins after
     * connection loss. It should be thread-safe.
     *
     * @param callback Function to call on reconnection start (may be empty to disable)
     */
    void set_reconnecting_callback(reconnecting_callback_t callback);

    /**
     * @brief Set callback to be invoked when reconnection succeeds.
     *
     * The callback will be called after auto-reconnect successfully
     * restores the connection. It should be thread-safe.
     *
     * @param callback Function to call on reconnection success (may be empty to disable)
     */
    void set_reconnected_callback(reconnected_callback_t callback);

    /**
     * @brief Set callback to be invoked when a notification is received.
     *
     * The callback will be called when the server pushes a notification
     * for an event type the client is subscribed to. It should be thread-safe.
     *
     * @param callback Function to call on notification (may be empty to disable)
     */
    void set_notification_callback(notification_callback_t callback);

    // =========================================================================
    // Session Recording
    // =========================================================================

    /**
     * @brief Enable session recording to the specified directory.
     *
     * Creates a new session recording file in the specified directory.
     * The file will contain all frames sent and received during the session.
     * Recording can be started before or after connecting.
     *
     * @param output_directory Directory where the session file will be created
     * @return Expected containing the full path to the created file, or error
     */
    std::expected<std::filesystem::path, recording::session_file_error>
    enable_recording(const std::filesystem::path& output_directory);

    /**
     * @brief Disable session recording.
     *
     * Stops recording and closes the session file. Safe to call when not
     * recording or already disabled.
     */
    void disable_recording();

    /**
     * @brief Check if session recording is currently active.
     */
    bool is_recording() const;

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

    /**
     * @brief Send typed request and receive typed response (blocking version).
     *
     * Handles serialization, framing, decompression, and deserialization.
     *
     * @tparam RequestType Request message type (must have serialize() method)
     * @tparam ResponseType Response message type (must have static deserialize() method)
     * @tparam RequestMsgType The message_type enum value for this request
     * @param request The request to send
     * @return Expected containing deserialized response, or error_code
     */
    template <typename RequestType, typename ResponseType,
              messaging::message_type RequestMsgType>
    std::expected<ResponseType, messaging::error_code>
    process_request(RequestType request) {
        using namespace ores::telemetry::log;

        auto payload = request.serialize();
        messaging::frame request_frame(RequestMsgType, 0, std::move(payload));

        auto result = send_request_sync(std::move(request_frame));
        if (!result) {
            return std::unexpected(result.error());
        }

        auto response_payload = result->decompressed_payload();
        if (!response_payload) {
            BOOST_LOG_SEV(lg(), error) << "Failed to decompress response";
            return std::unexpected(response_payload.error());
        }

        auto response = ResponseType::deserialize(*response_payload);
        if (!response) {
            BOOST_LOG_SEV(lg(), error) << "Failed to deserialize response";
            return std::unexpected(response.error());
        }

        return *response;
    }

    /**
     * @brief Send typed request and receive typed response (async version).
     *
     * Handles serialization, framing, decompression, and deserialization.
     *
     * @tparam RequestType Request message type (must have serialize() method)
     * @tparam ResponseType Response message type (must have static deserialize() method)
     * @tparam RequestMsgType The message_type enum value for this request
     * @param request The request to send
     * @return Awaitable containing deserialized response, or error_code
     */
    template <typename RequestType, typename ResponseType,
              messaging::message_type RequestMsgType>
    boost::asio::awaitable<std::expected<ResponseType, messaging::error_code>>
    process_request_async(RequestType request) {
        using namespace ores::telemetry::log;

        auto payload = request.serialize();
        messaging::frame request_frame(RequestMsgType, 0, std::move(payload));

        auto result = co_await send_request(std::move(request_frame));
        if (!result) {
            co_return std::unexpected(result.error());
        }

        auto response_payload = result->decompressed_payload();
        if (!response_payload) {
            BOOST_LOG_SEV(lg(), error) << "Failed to decompress response";
            co_return std::unexpected(response_payload.error());
        }

        auto response = ResponseType::deserialize(*response_payload);
        if (!response) {
            BOOST_LOG_SEV(lg(), error) << "Failed to deserialize response";
            co_return std::unexpected(response.error());
        }

        co_return *response;
    }

    // =========================================================================
    // Traits-based process_request overloads
    // =========================================================================
    // These overloads use message_traits to infer the response type and
    // message_type enum from the request type, simplifying the API.

    /**
     * @brief Send typed request using message_traits (blocking version).
     *
     * Uses message_traits to automatically determine the response type and
     * message_type enum value from the request type.
     *
     * @tparam RequestType Request message type (must have message_traits)
     * @param request The request to send
     * @return Expected containing deserialized response, or error_code
     */
    template <typename RequestType>
        requires messaging::has_message_traits<RequestType>
    std::expected<typename messaging::message_traits<RequestType>::response_type,
                  messaging::error_code>
    process_request(RequestType request) {
        using traits = messaging::message_traits<RequestType>;
        return process_request<
            RequestType,
            typename traits::response_type,
            traits::request_message_type>(std::move(request));
    }

    /**
     * @brief Send typed request using message_traits (async version).
     *
     * Uses message_traits to automatically determine the response type and
     * message_type enum value from the request type.
     *
     * @tparam RequestType Request message type (must have message_traits)
     * @param request The request to send
     * @return Awaitable containing deserialized response, or error_code
     */
    template <typename RequestType>
        requires messaging::has_message_traits<RequestType>
    boost::asio::awaitable<
        std::expected<typename messaging::message_traits<RequestType>::response_type,
                      messaging::error_code>>
    process_request_async(RequestType request) {
        using traits = messaging::message_traits<RequestType>;
        return process_request_async<
            RequestType,
            typename traits::response_type,
            traits::request_message_type>(std::move(request));
    }

private:
    client_options config_;
    std::unique_ptr<boost::asio::io_context> io_ctx_; // Owned io_context for sync operations
    boost::asio::any_io_executor executor_;
    boost::asio::ssl::context ssl_ctx_;
    std::unique_ptr<connection> conn_;
    std::uint32_t sequence_number_;
    std::atomic<connection_state> state_;
    mutable std::mutex state_mutex_; // Protects conn_, sequence_number_, callbacks, and recorder_
    disconnect_callback_t disconnect_callback_;
    reconnecting_callback_t reconnecting_callback_;
    reconnected_callback_t reconnected_callback_;
    notification_callback_t notification_callback_;
    std::shared_ptr<eventing::service::event_bus> event_bus_; // Optional event bus for connection events

    // Infrastructure for unified message loop
    std::unique_ptr<boost::asio::strand<boost::asio::any_io_executor>> write_strand_;
    std::unique_ptr<pending_request_map> pending_requests_;
    std::atomic<std::uint32_t> correlation_id_counter_{1};
    std::atomic<bool> message_loop_running_{false};
    std::atomic<bool> reconnect_loop_running_{false};
    std::atomic<bool> heartbeat_loop_running_{false};
    std::unique_ptr<std::thread> io_thread_; // Background thread for io_context
    std::unique_ptr<boost::asio::executor_work_guard<boost::asio::io_context::executor_type>> work_guard_;

    // Session compression type negotiated during handshake
    messaging::compression_type session_compression_{messaging::compression_type::none};

    // Session recording (protected by state_mutex_ for thread-safe access)
    std::shared_ptr<recording::session_recorder> recorder_;
};

}

#endif
