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
#ifndef ORES_COMMS_NET_CLIENT_BASE_HPP
#define ORES_COMMS_NET_CLIENT_BASE_HPP

#include <chrono>
#include <cstdint>
#include <expected>
#include <functional>
#include <optional>
#include <string>
#include <vector>
#include "ores.utility/serialization/error_code.hpp"
#include "ores.comms/messaging/frame.hpp"
#include "ores.comms/messaging/subscription_protocol.hpp"

namespace ores::comms::net {

/**
 * @brief Callback invoked when client detects server disconnect.
 *
 * Called when a disconnect is detected. Should not perform blocking operations.
 * Any UI updates must be dispatched to the appropriate UI thread.
 */
using disconnect_callback_t = std::function<void()>;

/**
 * @brief Callback invoked when client starts reconnection attempts.
 *
 * Called when connection is lost and auto-reconnect is enabled.
 * Allows UI to show reconnecting state to the user.
 */
using reconnecting_callback_t = std::function<void()>;

/**
 * @brief Callback invoked when client successfully reconnects.
 *
 * Called after auto-reconnect succeeds.
 * Allows UI to restore connected state display.
 */
using reconnected_callback_t = std::function<void()>;

/**
 * @brief Callback invoked when client receives a notification from server.
 *
 * Called when the server pushes a notification for a subscribed event type.
 *
 * @param event_type The fully qualified event type name
 * @param timestamp When the event occurred (UTC)
 * @param entity_ids Identifiers of specific entities that changed
 * @param tenant_id The tenant that owns the changed entities
 */
using notification_callback_t = std::function<void(
    const std::string& event_type,
    std::chrono::system_clock::time_point timestamp,
    const std::vector<std::string>& entity_ids,
    const std::string& tenant_id,
    messaging::payload_type pt,
    const std::optional<std::vector<std::byte>>& payload)>;

/**
 * @brief Abstract interface for ORES protocol clients.
 *
 * Decouples client_session and other consumers from the concrete transport
 * implementation (ASIO SSL via net::client or NATS via nats::service::nats_client).
 *
 * All blocking methods are synchronous — they run the full request/response
 * cycle on the caller's thread and return when complete.
 */
class client_base {
public:
    virtual ~client_base() = default;

    /**
     * @brief Check if client is currently connected.
     *
     * Returns true only when in the connected state.
     */
    [[nodiscard]] virtual bool is_connected() const noexcept = 0;

    /**
     * @brief Disconnect from the server.
     */
    virtual void disconnect() = 0;

    /**
     * @brief Disable auto-reconnect.
     *
     * Prevents the client from attempting to reconnect if the connection
     * is lost. Transport implementations that do not support auto-reconnect
     * may implement this as a no-op.
     */
    virtual void disable_auto_reconnect() = 0;

    /**
     * @brief Send a request frame and receive response frame (blocking).
     *
     * @param request The request frame to send
     * @return Expected containing response frame, or error_code on failure
     */
    virtual std::expected<messaging::frame, ores::utility::serialization::error_code>
    send_request_sync(messaging::frame request) = 0;

    /**
     * @brief Total bytes sent on the current connection.
     *
     * Returns 0 if not connected or if the transport does not track bytes.
     */
    [[nodiscard]] virtual std::uint64_t bytes_sent() const = 0;

    /**
     * @brief Total bytes received on the current connection.
     *
     * Returns 0 if not connected or if the transport does not track bytes.
     */
    [[nodiscard]] virtual std::uint64_t bytes_received() const = 0;

    /**
     * @brief Last measured round-trip time in milliseconds.
     *
     * Returns 0 for transports that do not track RTT (e.g. NATS).
     */
    [[nodiscard]] virtual std::uint64_t last_rtt_ms() const { return 0; }

    /**
     * @brief Set callback to be invoked when disconnect is detected.
     *
     * @param callback Function to call on disconnect (nullptr to disable)
     */
    virtual void set_disconnect_callback(disconnect_callback_t callback) = 0;

    /**
     * @brief Set callback to be invoked when reconnection starts.
     *
     * @param callback Function to call on reconnection start (nullptr to disable)
     */
    virtual void set_reconnecting_callback(reconnecting_callback_t callback) = 0;

    /**
     * @brief Set callback to be invoked when reconnection succeeds.
     *
     * @param callback Function to call on reconnection success (nullptr to disable)
     */
    virtual void set_reconnected_callback(reconnected_callback_t callback) = 0;

    /**
     * @brief Set callback to be invoked when a notification is received.
     *
     * Transports that do not support server-push notifications may store
     * the callback but never invoke it.
     *
     * @param callback Function to call on notification (nullptr to disable)
     */
    virtual void set_notification_callback(notification_callback_t callback) = 0;
};

}

#endif
