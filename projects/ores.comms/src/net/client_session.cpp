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
#include "ores.comms/net/client_session.hpp"
#include "ores.comms/messaging/subscription_protocol.hpp"

namespace ores::comms::net {

using namespace ores::logging;

client_session::client_session() = default;

client_session::~client_session() {
    if (is_connected()) {
        disconnect();
    }
}

std::expected<void, session_error>
client_session::attach_client(std::shared_ptr<client_base> external_client) {
    if (!external_client) {
        BOOST_LOG_SEV(lg(), error) << "Cannot attach null client";
        return std::unexpected(session_error(
            client_session_error::not_connected,
            "Cannot attach null client"));
    }

    if (!external_client->is_connected()) {
        BOOST_LOG_SEV(lg(), error) << "Cannot attach disconnected client";
        return std::unexpected(session_error(
            client_session_error::not_connected,
            "Client is not connected"));
    }

    // Clean up existing connection if any
    if (client_) {
        BOOST_LOG_SEV(lg(), info) << "Detaching existing client before attaching new one";
        detach_client();
    }

    BOOST_LOG_SEV(lg(), info) << "Attaching external client";
    client_ = std::move(external_client);
    external_client_ = true;  // We don't own this client

    // Route push notifications directly from the client's inbox subscription.
    client_->set_notification_callback(
        [this](const std::string& event_type,
               std::chrono::system_clock::time_point timestamp,
               const std::vector<std::string>& entity_ids,
               const std::string& tenant_id,
               messaging::payload_type pt,
               const std::optional<std::vector<std::byte>>& payload) {
            on_notification(event_type, timestamp, entity_ids, tenant_id,
                pt, payload);
        });

    BOOST_LOG_SEV(lg(), info) << "External client attached successfully";
    return {};
}

void client_session::detach_client() {
    BOOST_LOG_SEV(lg(), debug) << "Detaching client";

    // Clear session state
    session_info_.reset();
    nats_subscriptions_.clear();
    {
        std::lock_guard lock(notifications_mutex_);
        pending_notifications_.clear();
    }

    // Release client reference without disconnecting
    client_.reset();
    external_client_ = false;
}

void client_session::disconnect() {
    if (!client_) {
        BOOST_LOG_SEV(lg(), warn) << "No client instance.";
        return;
    }

    if (external_client_) {
        // External client: just detach, don't disconnect
        BOOST_LOG_SEV(lg(), debug) << "Detaching external client (not disconnecting)";
        client_.reset();
    } else if (client_->is_connected()) {
        // Internal client: actually disconnect
        client_->disconnect();
        client_.reset();
        BOOST_LOG_SEV(lg(), info) << "Disconnected from server.";
    } else {
        BOOST_LOG_SEV(lg(), debug) << "Already disconnected.";
        client_.reset();
    }

    external_client_ = false;

    // Clear session state
    session_info_.reset();
    nats_subscriptions_.clear();
    {
        std::lock_guard lock(notifications_mutex_);
        pending_notifications_.clear();
    }
}

bool client_session::is_connected() const noexcept {
    return client_ && client_->is_connected();
}

bool client_session::subscribe(const std::string& event_type) {
    // Send subscribe_request with notification inbox
    if (!client_ || !client_->is_connected()) {
        BOOST_LOG_SEV(lg(), error) << "Cannot subscribe: not connected";
        return false;
    }

    messaging::subscribe_request req;
    req.event_type = event_type;
    req.notification_inbox = client_->notification_inbox();
    auto result = process_request(req);
    if (!result || !result->success) {
        BOOST_LOG_SEV(lg(), error) << "Subscribe failed for " << event_type;
        return false;
    }
    nats_subscriptions_.insert(event_type);
    return true;
}

bool client_session::unsubscribe(const std::string& event_type) {
    if (!client_ || !client_->is_connected()) {
        BOOST_LOG_SEV(lg(), error) << "Cannot unsubscribe: not connected";
        return false;
    }

    messaging::unsubscribe_request req;
    req.event_type = event_type;
    req.notification_inbox = client_->notification_inbox();
    auto result = process_request(req);
    if (!result || !result->success) {
        BOOST_LOG_SEV(lg(), error) << "Unsubscribe failed for " << event_type;
        return false;
    }
    nats_subscriptions_.erase(event_type);
    return true;
}

bool client_session::is_subscribed(const std::string& event_type) const {
    return nats_subscriptions_.contains(event_type);
}

std::set<std::string> client_session::get_subscriptions() const {
    return nats_subscriptions_;
}

std::vector<pending_notification> client_session::take_pending_notifications() {
    std::deque<pending_notification> notifications;
    {
        std::lock_guard lock(notifications_mutex_);
        notifications.swap(pending_notifications_);
    }
    return {std::make_move_iterator(notifications.begin()),
            std::make_move_iterator(notifications.end())};
}

bool client_session::has_pending_notifications() const {
    std::lock_guard lock(notifications_mutex_);
    return !pending_notifications_.empty();
}

void client_session::on_notification(const std::string& event_type,
    std::chrono::system_clock::time_point timestamp,
    const std::vector<std::string>& entity_ids,
    const std::string& tenant_id,
    messaging::payload_type pt,
    const std::optional<std::vector<std::byte>>& payload) {
    BOOST_LOG_SEV(lg(), debug) << "Received notification for " << event_type
                               << ", tenant: " << tenant_id;

    // If external callback is set, use it instead of internal queuing
    if (external_notification_callback_) {
        external_notification_callback_(event_type, timestamp, entity_ids, tenant_id,
            pt, payload);
        return;
    }

    std::lock_guard lock(notifications_mutex_);
    pending_notifications_.push_back({event_type, timestamp, entity_ids, tenant_id,
        pt, payload});
}

void client_session::set_notification_callback(notification_callback_t callback) {
    external_notification_callback_ = std::move(callback);
}

std::string to_string(client_session_error error) {
    switch (error) {
    case client_session_error::not_connected:
        return "Not connected to server";
    case client_session_error::not_logged_in:
        return "Not logged in";
    case client_session_error::login_required:
        return "Login required for this operation";
    case client_session_error::admin_required:
        return "Admin privileges required for this operation";
    case client_session_error::request_failed:
        return "Request failed";
    case client_session_error::deserialization_failed:
        return "Failed to parse server response";
    case client_session_error::server_error:
        return "Server returned an error";
    case client_session_error::connection_lost:
        return "Connection to server lost";
    default:
        return "Unknown error";
    }
}

std::string to_string(const session_error& error) {
    if (!error.message.empty()) {
        return error.message;
    }
    return to_string(error.code);
}

}
