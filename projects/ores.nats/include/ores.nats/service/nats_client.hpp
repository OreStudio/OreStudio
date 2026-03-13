/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2026 Marco Craveiro <marco.craveiro@gmail.com>
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
#ifndef ORES_NATS_SERVICE_NATS_CLIENT_HPP
#define ORES_NATS_SERVICE_NATS_CLIENT_HPP

#include <atomic>
#include <mutex>
#include <string_view>
#include <nats/nats.h>
#include "ores.logging/make_logger.hpp"
#include "ores.comms/net/client_base.hpp"
#include "ores.nats/config/nats_options.hpp"

namespace ores::nats::service {

/**
 * @brief NATS request/reply transport implementing the client_base interface.
 *
 * Uses cnats natsConnection_Request for synchronous request/reply over NATS.
 * Each call to send_request_sync serialises the ORES frame, publishes it to
 * the configured subject, and waits for the reply message (timeout: 30 s).
 *
 * Thread-safety: send_request_sync may be called from any thread; cnats
 * natsConnection_Request is thread-safe.
 */
class nats_client final : public comms::net::client_base {
private:
    inline static std::string_view logger_name = "ores.nats.service.nats_client";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    /**
     * @brief Construct with NATS options.
     *
     * Does not connect immediately; call connect_sync() to establish the
     * connection before use.
     */
    explicit nats_client(config::nats_options options);

    ~nats_client() override;

    // Non-copyable, non-movable
    nats_client(const nats_client&) = delete;
    nats_client& operator=(const nats_client&) = delete;
    nats_client(nats_client&&) = delete;
    nats_client& operator=(nats_client&&) = delete;

    /**
     * @brief Connect to the NATS server (blocking).
     *
     * Establishes the NATS connection, allocates a unique inbox subject, and
     * subscribes to it for push notification delivery.
     *
     * @throws std::runtime_error if the connection fails
     */
    void connect_sync();

    // =========================================================================
    // client_base interface
    // =========================================================================

    [[nodiscard]] bool is_connected() const noexcept override;

    void disconnect() override;

    /**
     * @brief No-op for NATS; cnats handles reconnection internally.
     */
    void disable_auto_reconnect() override;

    std::expected<comms::messaging::frame, ores::utility::serialization::error_code>
    send_request_sync(comms::messaging::frame request) override;

    /**
     * @brief Returns the NATS inbox subject allocated at connect time.
     *
     * The server publishes event notifications to this subject for clients
     * that register with a notification_inbox in their subscribe_request.
     */
    [[nodiscard]] std::string notification_inbox() const override;

    /**
     * @brief Returns 0; cnats does not expose per-connection byte counters.
     */
    [[nodiscard]] std::uint64_t bytes_sent() const override;

    /**
     * @brief Returns 0; cnats does not expose per-connection byte counters.
     */
    [[nodiscard]] std::uint64_t bytes_received() const override;

    /**
     * @brief Store callback; invoked when cnats reports a disconnect event.
     */
    void set_disconnect_callback(comms::net::disconnect_callback_t callback) override;

    /**
     * @brief Store callback; invoked when cnats reports a reconnecting event.
     */
    void set_reconnecting_callback(comms::net::reconnecting_callback_t callback) override;

    /**
     * @brief Store callback; invoked when cnats reports a reconnected event.
     */
    void set_reconnected_callback(comms::net::reconnected_callback_t callback) override;

    /**
     * @brief Store callback; NATS push notifications are not yet implemented.
     *
     * The callback is stored but never invoked in Phase 4. Server-push
     * notification delivery over NATS subjects is planned for a future phase.
     */
    void set_notification_callback(comms::net::notification_callback_t callback) override;

private:
    /**
     * @brief cnats disconnect handler; marshals the callback onto the caller thread.
     */
    static void on_disconnect(natsConnection* conn, void* closure);

    /**
     * @brief cnats reconnected handler.
     */
    static void on_reconnected(natsConnection* conn, void* closure);

    /**
     * @brief cnats subscription callback for push notification delivery.
     *
     * Deserializes the received notification_message and invokes the stored
     * notification_cb_. Called on a cnats internal thread.
     */
    static void on_notification_message(natsConnection* conn,
        natsSubscription* sub, natsMsg* msg, void* closure);

private:
    config::nats_options options_;
    natsConnection* conn_{nullptr};
    natsSubscription* notification_sub_{nullptr};
    std::string inbox_subject_;
    std::atomic<bool> connected_{false};

    mutable std::mutex callbacks_mutex_;
    comms::net::disconnect_callback_t disconnect_cb_;
    comms::net::reconnecting_callback_t reconnecting_cb_;
    comms::net::reconnected_callback_t reconnected_cb_;
    comms::net::notification_callback_t notification_cb_;
};

}

#endif
