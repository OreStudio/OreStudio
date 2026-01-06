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
#ifndef ORES_EVENTING_SERVICE_POSTGRES_LISTENER_SERVICE_HPP
#define ORES_EVENTING_SERVICE_POSTGRES_LISTENER_SERVICE_HPP

#include <mutex>
#include <atomic>
#include <string>
#include <thread>
#include <vector>
#include <optional>
#include <functional>
#include <condition_variable>
#include <sqlgen/postgres.hpp>
#include "ores.telemetry/log/make_logger.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.eventing/domain/entity_change_event.hpp"

namespace ores::eventing::service {

/**
 * @brief Manages a dedicated PostgreSQL connection to listen for NOTIFY events.
 *
 * This service runs a separate thread to continuously listen for asynchronous
 * notifications on configured channels. When a notification is received, it
 * parses the payload into a domain::entity_change_event object and dispatches
 * it via a callback.
 *
 * The service maintains its own dedicated connection separate from any
 * connection pool, as LISTEN/NOTIFY requires a persistent connection.
 */
class postgres_listener_service final {
private:
    [[nodiscard]] static auto& lg() {
        using namespace ores::telemetry::log;
        static auto instance = make_logger(
            "ores.eventing.service.postgres_listener_service");
        return instance;
    }

public:
    /**
     * @brief Type alias for the notification callback function.
     *
     * The callback is invoked with the parsed domain::entity_change_event
     * object. Note: The callback is invoked from the listener thread, so
     * implementations should be thread-safe and non-blocking.
     */
    using notification_callback_t = std::function<void(const domain::entity_change_event&)>;

    /**
     * @brief Constructs a postgres_listener_service.
     *
     * Creates a dedicated PostgreSQL connection for listening using the
     * credentials from the provided database context.
     *
     * @param ctx The database context containing connection credentials.
     * @param callback The callback function to be invoked when a notification
     *        is received.
     */
    explicit postgres_listener_service(database::context ctx,
        notification_callback_t callback);

    /**
     * @brief Destroys the postgres_listener_service.
     *
     * Stops the listener thread and closes the dedicated connection.
     */
    ~postgres_listener_service();

    // Deleted copy constructor and assignment operator to prevent copies.
    postgres_listener_service(const postgres_listener_service&) = delete;
    postgres_listener_service& operator=(const postgres_listener_service&) = delete;

    /**
     * @brief Starts the listener thread and begins listening for notifications.
     *
     * Any channels subscribed via subscribe() before calling start() will
     * have LISTEN commands issued when the thread starts.
     */
    void start();

    /**
     * @brief Stops the listener thread and waits for it to join.
     */
    void stop();

    /**
     * @brief Subscribes to a PostgreSQL NOTIFY channel.
     *
     * Can be called before or after start(). If called before start(), the
     * LISTEN command will be issued when the listener thread starts. If called
     * after start(), the LISTEN command is issued immediately on the dedicated
     * connection.
     *
     * @param channel_name The name of the channel to listen on (e.g.,
     *        "ores_currencies").
     */
    void subscribe(const std::string& channel_name);

    /**
     * @brief Sends a NOTIFY on a PostgreSQL channel.
     *
     * @param channel_name The name of the channel to notify.
     * @param payload The payload string to send with the notification.
     */
    void notify(const std::string& channel_name, const std::string& payload);

    /**
     * @brief Waits until the listener is ready to receive notifications.
     *
     * Blocks until the listener thread has started and issued all pending
     * LISTEN commands. Returns immediately if not started or already ready.
     *
     * @param timeout Maximum time to wait for the listener to become ready.
     * @return true if the listener is ready, false if timeout occurred.
     */
    bool wait_until_ready(std::chrono::milliseconds timeout = std::chrono::seconds(5));

private:
    /**
     * @brief Opens the dedicated PostgreSQL connection.
     *
     * @return true if connection was successful, false otherwise.
     */
    bool open_connection();

    /**
     * @brief Issues LISTEN commands for all subscribed channels.
     *
     * Called when the listener thread starts to ensure all pending
     * subscriptions are activated.
     */
    void issue_pending_listens();

    /**
     * @brief The main loop for the listener thread.
     *
     * Continuously checks for PostgreSQL notifications.
     */
    void listen_loop();

    /**
     * @brief Handles a received sqlgen notification.
     *
     * Parses the payload and invokes the notification callback.
     *
     * @param notification The sqlgen Notification object.
     */
    void handle_notification(const sqlgen::postgres::Notification& notification);

private:
    database::context ctx_;
    notification_callback_t notification_callback_;

    mutable std::mutex mutex_;              ///< Protects connection and channels
    std::optional<rfl::Ref<sqlgen::postgres::Connection>> connection_;
    std::vector<std::string> subscribed_channels_;

    std::thread listener_thread_;
    std::atomic<bool> running_;

    std::condition_variable ready_cv_;      ///< Signaled when listener is ready
    bool ready_{false};                     ///< True when listener is actively polling
};

}

#endif
