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
#ifndef ORES_COMMS_SERVICE_REMOTE_EVENT_ADAPTER_HPP
#define ORES_COMMS_SERVICE_REMOTE_EVENT_ADAPTER_HPP

#include <set>
#include <mutex>
#include <string>
#include <memory>
#include <functional>
#include <string_view>
#include <boost/asio/awaitable.hpp>
#include "ores.utility/log/make_logger.hpp"
#include "ores.comms/net/client.hpp"

namespace ores::comms::service {

/**
 * @brief Adapts remote server subscriptions to local notification callbacks.
 *
 * This class bridges the gap between client-side event handling and server-side
 * subscription management. It:
 * - Sends SUBSCRIBE protocol messages when subscribing to an event type
 * - Sends UNSUBSCRIBE protocol messages when unsubscribing
 * - Tracks active subscriptions to support re-subscription after reconnect
 * - Forwards received notifications to registered callbacks
 *
 * Thread-safety: All operations are thread-safe.
 */
class remote_event_adapter final {
private:
    inline static std::string_view logger_name =
        "ores.comms.service.remote_event_adapter";

    static auto& lg() {
        using namespace ores::utility::log;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    /**
     * @brief Construct adapter with a client reference.
     *
     * The adapter registers itself as the notification callback on the client.
     *
     * @param client The client to use for communication
     */
    explicit remote_event_adapter(std::shared_ptr<net::client> client);

    /**
     * @brief Destructor.
     *
     * Does not automatically unsubscribe from events.
     */
    ~remote_event_adapter();

    /**
     * @brief Subscribe to notifications for an event type.
     *
     * Sends a SUBSCRIBE protocol message to the server. The subscription
     * is tracked locally to support re-subscription after reconnect.
     *
     * @param event_type The fully qualified event type name
     * @return True if subscription succeeded, false otherwise
     */
    boost::asio::awaitable<bool> subscribe(const std::string& event_type);

    /**
     * @brief Unsubscribe from notifications for an event type.
     *
     * Sends an UNSUBSCRIBE protocol message to the server.
     *
     * @param event_type The fully qualified event type name
     * @return True if unsubscription succeeded, false otherwise
     */
    boost::asio::awaitable<bool> unsubscribe(const std::string& event_type);

    /**
     * @brief Check if currently subscribed to an event type.
     *
     * @param event_type The event type to check
     * @return True if subscribed, false otherwise
     */
    bool is_subscribed(const std::string& event_type) const;

    /**
     * @brief Get the set of currently subscribed event types.
     *
     * @return Set of event type names
     */
    std::set<std::string> get_subscriptions() const;

    /**
     * @brief Re-subscribe to all previously subscribed event types.
     *
     * Useful after reconnection to restore subscription state.
     *
     * @return Number of successful re-subscriptions
     */
    boost::asio::awaitable<std::size_t> resubscribe_all();

    /**
     * @brief Set callback to be invoked when notifications are received.
     *
     * @param callback Function to call with (event_type, timestamp)
     */
    void set_notification_callback(net::notification_callback_t callback);

private:
    /**
     * @brief Handle incoming notification from client.
     */
    void on_notification(const std::string& event_type,
        std::chrono::system_clock::time_point timestamp);

    std::shared_ptr<net::client> client_;
    mutable std::mutex mutex_;
    std::set<std::string> subscriptions_;
    net::notification_callback_t user_callback_;
};

}

#endif
