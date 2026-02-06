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
#ifndef ORES_COMMS_SERVICE_SUBSCRIPTION_MANAGER_HPP
#define ORES_COMMS_SERVICE_SUBSCRIPTION_MANAGER_HPP

#include <mutex>
#include <string>
#include <vector>
#include <chrono>
#include <memory>
#include <functional>
#include <unordered_map>
#include <unordered_set>
#include "ores.logging/make_logger.hpp"
#include "ores.comms/service/auth_session_service.hpp"

namespace ores::comms::service {

/**
 * @brief Callback type for pushing notifications to clients.
 *
 * Takes event_type, timestamp, and entity_ids. Returns true if notification
 * was sent successfully, false otherwise (e.g., connection closed).
 */
using notification_callback =
    std::function<bool(const std::string&, std::chrono::system_clock::time_point,
                       const std::vector<std::string>&)>;

/**
 * @brief Unique identifier for a client session.
 *
 * Uses the remote address (IP:port) as identifier since it's unique per
 * connection.
 */
using session_id = std::string;

/**
 * @brief Manages client subscriptions to event notifications.
 *
 * This class tracks which clients are subscribed to which event types and
 * provides the mechanism to broadcast notifications when events occur.
 *
 * Thread Safety:
 * - All operations are thread-safe.
 * - Notification callbacks are invoked without holding the lock to avoid
 *   deadlocks, so they must be safe to call concurrently.
 *
 * Usage:
 * @code
 *     subscription_manager mgr;
 *
 *     // Register a session when it connects
 *     mgr.register_session("192.168.1.1:45678",
 *         [&conn](const std::string& event_type, auto ts) {
 *             return push_notification(conn, event_type, ts);
 *         });
 *
 *     // Subscribe to events
 *     mgr.subscribe("192.168.1.1:45678", "ores.refdata.currency_changed");
 *
 *     // Notify all subscribers of an event
 *     mgr.notify("ores.refdata.currency_changed", std::chrono::system_clock::now());
 *
 *     // Unregister when session ends
 *     mgr.unregister_session("192.168.1.1:45678");
 * @endcode
 */
class subscription_manager final {
private:
    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(
            "ores.comms.service.subscription_manager");
        return instance;
    }

    struct session_info {
        notification_callback callback;
        std::unordered_set<std::string> subscribed_events;
    };

public:
    subscription_manager() = default;
    ~subscription_manager() = default;

    /**
     * @brief Set the auth session service for tenant-aware filtering.
     *
     * Must be called before notify() is used with tenant_id filtering.
     * Supports deferred binding since the server (which owns sessions)
     * may be created after the subscription manager.
     */
    void set_sessions_service(std::shared_ptr<auth_session_service> sessions);

    subscription_manager(const subscription_manager&) = delete;
    subscription_manager& operator=(const subscription_manager&) = delete;
    subscription_manager(subscription_manager&&) = delete;
    subscription_manager& operator=(subscription_manager&&) = delete;

    /**
     * @brief Register a new session with the subscription manager.
     *
     * Must be called when a session successfully completes handshake.
     *
     * @param id The unique session identifier (typically remote address).
     * @param callback The callback to invoke for pushing notifications.
     */
    void register_session(const session_id& id, notification_callback callback);

    /**
     * @brief Unregister a session from the subscription manager.
     *
     * Removes all subscriptions for this session. Should be called when
     * a session disconnects.
     *
     * @param id The session identifier to unregister.
     */
    void unregister_session(const session_id& id);

    /**
     * @brief Subscribe a session to an event type.
     *
     * @param id The session identifier.
     * @param event_type The event type to subscribe to.
     * @return true if subscription succeeded, false if session not found.
     */
    bool subscribe(const session_id& id, const std::string& event_type);

    /**
     * @brief Unsubscribe a session from an event type.
     *
     * @param id The session identifier.
     * @param event_type The event type to unsubscribe from.
     * @return true if unsubscription succeeded, false if session not found
     *         or was not subscribed.
     */
    bool unsubscribe(const session_id& id, const std::string& event_type);

    /**
     * @brief Notify all subscribers of an event.
     *
     * Invokes the notification callback for each session subscribed to the
     * given event type. When tenant_id is provided and a sessions service
     * is configured, only sessions belonging to the same tenant receive
     * the notification (multi-tenancy isolation). If tenant_id is empty
     * or no sessions service is set, notifications are broadcast to all
     * subscribers.
     *
     * @param event_type The event type that occurred.
     * @param timestamp The timestamp of the event.
     * @param entity_ids Identifiers of entities that changed.
     * @param tenant_id The tenant that owns the changed entity.
     * @return The number of successful notifications sent.
     */
    std::size_t notify(const std::string& event_type,
                       std::chrono::system_clock::time_point timestamp,
                       const std::vector<std::string>& entity_ids = {},
                       const std::string& tenant_id = {});

    /**
     * @brief Get the number of subscribers for an event type.
     *
     * @param event_type The event type to query.
     * @return The number of sessions subscribed to this event type.
     */
    [[nodiscard]] std::size_t subscriber_count(const std::string& event_type) const;

    /**
     * @brief Get the total number of registered sessions.
     *
     * @return The number of active sessions.
     */
    [[nodiscard]] std::size_t session_count() const;

    /**
     * @brief Get the event types a session is subscribed to.
     *
     * @param id The session identifier.
     * @return Vector of event type names, empty if session not found.
     */
    [[nodiscard]] std::vector<std::string> get_subscriptions(const session_id& id) const;

private:
    mutable std::mutex mutex_;
    std::shared_ptr<auth_session_service> sessions_service_;
    std::unordered_map<session_id, session_info> sessions_;
    std::unordered_map<std::string, std::unordered_set<session_id>> event_subscribers_;
};

}

#endif
