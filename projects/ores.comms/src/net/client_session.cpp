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

using namespace ores::utility::log;

client_session::~client_session() {
    if (is_connected()) {
        disconnect();
    }
}

std::expected<void, session_error>
client_session::connect(client_options options) {
    BOOST_LOG_SEV(lg(), info) << "Connecting to " << options.host << ":"
                              << options.port << " (identifier: "
                              << options.client_identifier << ")";

    if (client_ && client_->is_connected()) {
        BOOST_LOG_SEV(lg(), info) << "Disconnecting existing connection";
        client_->disconnect();
        session_info_.reset();
    }

    try {
        client_ = std::make_shared<client>(std::move(options));
        client_->connect_sync();

        // Register notification callback to queue notifications for display
        client_->set_notification_callback(
            [this](const std::string& event_type,
                   std::chrono::system_clock::time_point timestamp) {
                on_notification(event_type, timestamp);
            });

        BOOST_LOG_SEV(lg(), info) << "Successfully connected.";
        return {};
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "Connection failed: " << e.what();
        client_.reset();
        return std::unexpected(session_error(
            client_session_error::not_connected,
            std::string("Connection failed: ") + e.what()));
    }
}

void client_session::disconnect() {
    if (!client_) {
        BOOST_LOG_SEV(lg(), warn) << "No client instance.";
        return;
    }

    if (!client_->is_connected()) {
        BOOST_LOG_SEV(lg(), debug) << "Already disconnected.";
        session_info_.reset();
        subscriptions_.clear();
        {
            std::lock_guard lock(notifications_mutex_);
            pending_notifications_.clear();
        }
        return;
    }

    // Clear session info and subscriptions on disconnect
    session_info_.reset();
    subscriptions_.clear();
    {
        std::lock_guard lock(notifications_mutex_);
        pending_notifications_.clear();
    }

    // Clear the notification callback before disconnecting
    client_->set_notification_callback(nullptr);

    client_->disconnect();
    BOOST_LOG_SEV(lg(), info) << "Disconnected from server.";
}

bool client_session::is_connected() const noexcept {
    return client_ && client_->is_connected();
}

bool client_session::subscribe(const std::string& event_type) {
    if (!client_ || !client_->is_connected()) {
        BOOST_LOG_SEV(lg(), error) << "Cannot subscribe: not connected";
        return false;
    }

    BOOST_LOG_SEV(lg(), info) << "Subscribing to event type: " << event_type;

    // Send subscribe request
    messaging::subscribe_request request;
    request.event_type = event_type;

    auto payload = request.serialize();
    messaging::frame request_frame(messaging::message_type::subscribe_request,
        0, std::move(payload));

    auto response_result = client_->send_request_sync(std::move(request_frame));
    if (!response_result) {
        BOOST_LOG_SEV(lg(), error) << "Subscribe request failed";
        return false;
    }

    auto decompressed = response_result->decompressed_payload();
    if (!decompressed) {
        BOOST_LOG_SEV(lg(), error) << "Failed to decompress subscribe response";
        return false;
    }

    auto response = messaging::subscribe_response::deserialize(*decompressed);
    if (!response) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize subscribe response";
        return false;
    }

    if (response->success) {
        subscriptions_.insert(event_type);
        BOOST_LOG_SEV(lg(), info) << "Successfully subscribed to " << event_type;
    } else {
        BOOST_LOG_SEV(lg(), warn) << "Subscribe failed: " << response->message;
    }

    return response->success;
}

bool client_session::unsubscribe(const std::string& event_type) {
    if (!client_ || !client_->is_connected()) {
        BOOST_LOG_SEV(lg(), error) << "Cannot unsubscribe: not connected";
        return false;
    }

    BOOST_LOG_SEV(lg(), info) << "Unsubscribing from event type: " << event_type;

    // Send unsubscribe request
    messaging::unsubscribe_request request;
    request.event_type = event_type;

    auto payload = request.serialize();
    messaging::frame request_frame(messaging::message_type::unsubscribe_request,
        0, std::move(payload));

    auto response_result = client_->send_request_sync(std::move(request_frame));
    if (!response_result) {
        BOOST_LOG_SEV(lg(), error) << "Unsubscribe request failed";
        return false;
    }

    auto decompressed = response_result->decompressed_payload();
    if (!decompressed) {
        BOOST_LOG_SEV(lg(), error) << "Failed to decompress unsubscribe response";
        return false;
    }

    auto response = messaging::unsubscribe_response::deserialize(*decompressed);
    if (!response) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize unsubscribe response";
        return false;
    }

    if (response->success) {
        subscriptions_.erase(event_type);
        BOOST_LOG_SEV(lg(), info) << "Successfully unsubscribed from " << event_type;
    } else {
        BOOST_LOG_SEV(lg(), warn) << "Unsubscribe failed: " << response->message;
    }

    return response->success;
}

bool client_session::is_subscribed(const std::string& event_type) const {
    return subscriptions_.contains(event_type);
}

std::set<std::string> client_session::get_subscriptions() const {
    return subscriptions_;
}

std::vector<pending_notification> client_session::take_pending_notifications() {
    std::lock_guard lock(notifications_mutex_);
    std::vector<pending_notification> result(
        pending_notifications_.begin(),
        pending_notifications_.end());
    pending_notifications_.clear();
    return result;
}

bool client_session::has_pending_notifications() const {
    std::lock_guard lock(notifications_mutex_);
    return !pending_notifications_.empty();
}

void client_session::on_notification(const std::string& event_type,
    std::chrono::system_clock::time_point timestamp) {
    BOOST_LOG_SEV(lg(), debug) << "Received notification for " << event_type;

    std::lock_guard lock(notifications_mutex_);
    pending_notifications_.push_back({event_type, timestamp});
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
