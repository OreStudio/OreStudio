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
#ifndef ORES_COMMS_NET_SERVER_SESSION_HPP
#define ORES_COMMS_NET_SERVER_SESSION_HPP

#include <mutex>
#include <queue>
#include <atomic>
#include <memory>
#include <string>
#include <chrono>
#include <boost/asio/steady_timer.hpp>
#include <boost/asio/strand.hpp>
#include "ores.comms/net/connection.hpp"
#include "ores.telemetry/log/make_logger.hpp"
#include "ores.comms/messaging/message_dispatcher.hpp"
#include "ores.comms/service/auth_session_service.hpp"

namespace ores::comms::service { class subscription_manager; }

namespace ores::comms::net {

/**
 * @brief Represents a client session on the server side.
 *
 * Manages the lifecycle of a single client connection, including
 * handshake, message processing, and cleanup.
 */
class server_session final {
private:
    inline static std::string_view logger_name = "ores.comms.net.server_session";

    static auto& lg() {
        using namespace ores::telemetry::log;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    /**
     * @brief Pending notification to be sent to client.
     */
    struct pending_notification {
        std::string event_type;
        std::chrono::system_clock::time_point timestamp;
    };

    /**
     * @brief Pending database status notification to be sent to client.
     */
    struct pending_database_status {
        bool available;
        std::string error_message;
        std::chrono::system_clock::time_point timestamp;
    };

    /**
     * @brief Construct a session from a connection.
     *
     * @param conn The connection to manage
     * @param server_id Server identifier for handshake
     * @param dispatcher Message dispatcher for handling requests
     * @param io_executor The executor to use for async operations
     * @param sessions Auth session service for storing client info
     * @param subscription_mgr Optional subscription manager for event notifications
     */
    explicit server_session(std::unique_ptr<connection> conn, std::string server_id,
        std::shared_ptr<messaging::message_dispatcher> dispatcher,
        boost::asio::any_io_executor io_executor,
        std::shared_ptr<service::auth_session_service> sessions,
        std::shared_ptr<service::subscription_manager> subscription_mgr = nullptr);

    /**
     * @brief Run the session.
     *
     * Performs handshake and processes messages until connection closes.
     */
    boost::asio::awaitable<void> run();

    /**
     * @brief Stop the session by closing its connection.
     *
     * This will cause any pending I/O operations to fail and the session to end.
     */
    void stop();

    /**
     * @brief Queue a notification to be sent to this session's client.
     *
     * Thread-safe. The notification will be sent on the next iteration of
     * the message processing loop.
     *
     * @param event_type The event type that occurred.
     * @param timestamp The timestamp of the event.
     * @return true if queued successfully, false if session is not active.
     */
    bool queue_notification(const std::string& event_type,
        std::chrono::system_clock::time_point timestamp);

    /**
     * @brief Queue a database status notification to be sent to this client.
     *
     * Thread-safe. Used to inform the client of database availability status.
     *
     * @param available Whether the database is available.
     * @param error_message Error message if unavailable, empty otherwise.
     * @param timestamp The timestamp of the status check.
     * @return true if queued successfully, false if session is not active.
     */
    bool queue_database_status(bool available, const std::string& error_message,
        std::chrono::system_clock::time_point timestamp);

private:
    /**
     * @brief Perform handshake with client.
     *
     * Returns true if handshake succeeds, false otherwise.
     */
    boost::asio::awaitable<bool> perform_handshake();

    /**
     * @brief Process messages from client after handshake.
     */
    boost::asio::awaitable<void> process_messages();

    /**
     * @brief Run the notification writer coroutine.
     *
     * Waits for notifications to be queued and sends them immediately.
     * Runs concurrently with the message processing loop.
     */
    boost::asio::awaitable<void> run_notification_writer();

    /**
     * @brief Send all pending notifications to the client.
     *
     * Drains the notification queue and sends each as a notification message.
     * Must be called while holding the write strand.
     */
    boost::asio::awaitable<void> send_pending_notifications();

    /**
     * @brief Send all pending database status notifications to the client.
     *
     * Drains the database status queue and sends each as a status message.
     */
    boost::asio::awaitable<void> send_pending_database_status();

    /**
     * @brief Register this session with the subscription manager.
     */
    void register_with_subscription_manager();

    /**
     * @brief Unregister this session from the subscription manager.
     */
    void unregister_from_subscription_manager();

    std::unique_ptr<connection> conn_;
    std::string server_id_;
    std::shared_ptr<messaging::message_dispatcher> dispatcher_;
    std::shared_ptr<service::auth_session_service> sessions_;
    std::shared_ptr<service::subscription_manager> subscription_mgr_;
    std::uint32_t sequence_number_;
    bool handshake_complete_;
    std::atomic<bool> active_{false};

    // Strand for serializing write operations
    boost::asio::strand<boost::asio::any_io_executor> write_strand_;

    // Timer used as async signal for notification availability
    boost::asio::steady_timer notification_signal_;

    // Thread-safe notification queue
    mutable std::mutex notification_mutex_;
    std::queue<pending_notification> pending_notifications_;
    std::queue<pending_database_status> pending_database_status_;

    // Session compression type negotiated during handshake
    messaging::compression_type session_compression_{messaging::compression_type::none};
};

}

#endif
