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
#include "ores.nats/service/nats_client.hpp"

#include <stdexcept>
#include <boost/throw_exception.hpp>
#include "ores.comms/messaging/subscription_protocol.hpp"
#include "ores.utility/serialization/error_code.hpp"

namespace ores::nats::service {

using namespace ores::logging;
using ores::utility::serialization::error_code;

nats_client::nats_client(config::nats_options options)
    : options_(std::move(options)) {
}

nats_client::~nats_client() {
    disconnect();
}

void nats_client::connect_sync() {
    BOOST_LOG_SEV(lg(), info) << "Connecting to NATS at " << options_.url;

    natsOptions* opts = nullptr;
    natsStatus s = natsOptions_Create(&opts);
    if (s != NATS_OK) {
        BOOST_THROW_EXCEPTION(std::runtime_error(
            std::string("Failed to create NATS options: ") + natsStatus_GetText(s)));
    }

    // Wire up connection event callbacks
    natsOptions_SetDisconnectedCB(opts, on_disconnect, this);
    natsOptions_SetReconnectedCB(opts, on_reconnected, this);

    s = natsConnection_Connect(&conn_, opts);
    natsOptions_Destroy(opts);

    if (s != NATS_OK) {
        BOOST_LOG_SEV(lg(), error) << "NATS connection failed: " << natsStatus_GetText(s);
        BOOST_THROW_EXCEPTION(std::runtime_error(
            std::string("NATS connection failed: ") + natsStatus_GetText(s)));
    }

    connected_.store(true, std::memory_order_release);
    BOOST_LOG_SEV(lg(), info) << "Connected to NATS";

    // Allocate a unique inbox subject for push notification delivery
    natsInbox* raw_inbox = nullptr;
    const natsStatus inbox_s = natsInbox_Create(&raw_inbox);
    if (inbox_s == NATS_OK && raw_inbox) {
        inbox_subject_ = raw_inbox;
        natsInbox_Destroy(raw_inbox);

        // Subscribe to the inbox so the server can push notifications here
        const natsStatus sub_s = natsConnection_Subscribe(
            &notification_sub_, conn_,
            inbox_subject_.c_str(), on_notification_message, this);
        if (sub_s != NATS_OK) {
            BOOST_LOG_SEV(lg(), warn)
                << "Failed to subscribe to notification inbox: "
                << natsStatus_GetText(sub_s);
            notification_sub_ = nullptr;
            inbox_subject_.clear();
        } else {
            BOOST_LOG_SEV(lg(), info)
                << "Subscribed to notification inbox: " << inbox_subject_;
        }
    } else {
        BOOST_LOG_SEV(lg(), warn) << "Failed to create NATS inbox";
    }
}

bool nats_client::is_connected() const noexcept {
    return connected_.load(std::memory_order_acquire);
}

void nats_client::disconnect() {
    if (!connected_.exchange(false, std::memory_order_acq_rel)) {
        return;
    }

    BOOST_LOG_SEV(lg(), info) << "Disconnecting from NATS";

    if (notification_sub_) {
        natsSubscription_Unsubscribe(notification_sub_);
        natsSubscription_Destroy(notification_sub_);
        notification_sub_ = nullptr;
    }
    inbox_subject_.clear();

    if (conn_) {
        natsConnection_Close(conn_);
        natsConnection_Destroy(conn_);
        conn_ = nullptr;
    }
}

void nats_client::disable_auto_reconnect() {
    // No-op: cnats manages reconnection internally.
    // A future phase can call natsOptions_SetMaxReconnect(opts, 0) at connect time.
}

std::expected<comms::messaging::frame, error_code>
nats_client::send_request_sync(comms::messaging::frame request) {
    if (!connected_.load(std::memory_order_acquire)) {
        BOOST_LOG_SEV(lg(), error) << "send_request_sync called while disconnected";
        return std::unexpected(error_code::network_error);
    }

    // Serialize the ORES frame to bytes
    const auto bytes = request.serialize();

    // Send request and wait for reply (30 second timeout)
    natsMsg* reply_msg = nullptr;
    const natsStatus s = natsConnection_Request(
        &reply_msg, conn_,
        options_.subject.c_str(),
        bytes.data(),
        static_cast<int>(bytes.size()),
        30000);  // 30 000 ms

    if (s != NATS_OK) {
        BOOST_LOG_SEV(lg(), error) << "NATS request failed: " << natsStatus_GetText(s);
        if (s == NATS_TIMEOUT) {
            return std::unexpected(error_code::handshake_timeout);
        }
        return std::unexpected(error_code::network_error);
    }

    // Copy reply data before destroying the cnats message
    const int data_len = natsMsg_GetDataLength(reply_msg);
    std::vector<std::byte> body;
    if (data_len > 0) {
        const auto* data_ptr =
            reinterpret_cast<const std::byte*>(natsMsg_GetData(reply_msg));
        body.assign(data_ptr, data_ptr + data_len);
    }
    natsMsg_Destroy(reply_msg);

    if (body.empty()) {
        BOOST_LOG_SEV(lg(), warn) << "Received empty reply from NATS";
        return std::unexpected(error_code::network_error);
    }

    // Deserialize ORES frame header
    auto header_result = comms::messaging::frame::deserialize_header(body);
    if (!header_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize reply frame header";
        return std::unexpected(error_code::network_error);
    }

    // Deserialize the complete frame
    auto frame_result = comms::messaging::frame::deserialize(*header_result, body);
    if (!frame_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize reply frame";
        return std::unexpected(error_code::network_error);
    }

    return std::move(*frame_result);
}

std::string nats_client::notification_inbox() const {
    return inbox_subject_;
}

std::uint64_t nats_client::bytes_sent() const {
    return 0;
}

std::uint64_t nats_client::bytes_received() const {
    return 0;
}

void nats_client::set_disconnect_callback(comms::net::disconnect_callback_t callback) {
    std::lock_guard lock(callbacks_mutex_);
    disconnect_cb_ = std::move(callback);
}

void nats_client::set_reconnecting_callback(comms::net::reconnecting_callback_t callback) {
    std::lock_guard lock(callbacks_mutex_);
    reconnecting_cb_ = std::move(callback);
}

void nats_client::set_reconnected_callback(comms::net::reconnected_callback_t callback) {
    std::lock_guard lock(callbacks_mutex_);
    reconnected_cb_ = std::move(callback);
}

void nats_client::set_notification_callback(comms::net::notification_callback_t callback) {
    std::lock_guard lock(callbacks_mutex_);
    notification_cb_ = std::move(callback);
}

// static
void nats_client::on_disconnect(natsConnection*, void* closure) {
    auto* self = static_cast<nats_client*>(closure);
    self->connected_.store(false, std::memory_order_release);

    BOOST_LOG_SEV(self->lg(), warn) << "NATS connection lost";

    comms::net::disconnect_callback_t cb;
    {
        std::lock_guard lock(self->callbacks_mutex_);
        cb = self->disconnect_cb_;
    }
    if (cb) cb();
}

// static
void nats_client::on_reconnected(natsConnection*, void* closure) {
    auto* self = static_cast<nats_client*>(closure);
    self->connected_.store(true, std::memory_order_release);

    BOOST_LOG_SEV(self->lg(), info) << "NATS connection restored";

    comms::net::reconnected_callback_t cb;
    {
        std::lock_guard lock(self->callbacks_mutex_);
        cb = self->reconnected_cb_;
    }
    if (cb) cb();
}

// static
void nats_client::on_notification_message(natsConnection*, natsSubscription*,
    natsMsg* msg, void* closure) {
    auto* self = static_cast<nats_client*>(closure);

    // Copy body before destroying the cnats message
    const int data_len = natsMsg_GetDataLength(msg);
    std::vector<std::byte> body;
    if (data_len > 0) {
        const auto* data_ptr =
            reinterpret_cast<const std::byte*>(natsMsg_GetData(msg));
        body.assign(data_ptr, data_ptr + data_len);
    }
    natsMsg_Destroy(msg);

    if (body.empty()) return;

    // Deserialize as a notification_message (raw payload, no ORES frame wrapper)
    auto notification =
        comms::messaging::notification_message::deserialize(body);
    if (!notification) {
        BOOST_LOG_SEV(self->lg(), warn)
            << "Failed to deserialize push notification";
        return;
    }

    comms::net::notification_callback_t cb;
    {
        std::lock_guard lock(self->callbacks_mutex_);
        cb = self->notification_cb_;
    }

    if (cb) {
        cb(notification->event_type, notification->timestamp,
           notification->entity_ids, notification->tenant_id,
           notification->pt, notification->payload);
    }
}

}
