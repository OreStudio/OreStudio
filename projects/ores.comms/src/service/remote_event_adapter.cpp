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
#include "ores.comms/service/remote_event_adapter.hpp"

#include <boost/log/sources/record_ostream.hpp>
#include "ores.comms/messaging/subscription_protocol.hpp"

namespace ores::comms::service {

using namespace ores::utility::log;

remote_event_adapter::remote_event_adapter(std::shared_ptr<net::client> client)
    : client_(std::move(client)) {
    BOOST_LOG_SEV(lg(), info) << "Creating remote event adapter";

    // Register ourselves as the notification handler on the client
    client_->set_notification_callback(
        [this](const std::string& event_type,
               std::chrono::system_clock::time_point timestamp) {
            on_notification(event_type, timestamp);
        });
}

remote_event_adapter::~remote_event_adapter() {
    BOOST_LOG_SEV(lg(), debug) << "Destroying remote event adapter";
    // Clear the notification callback to avoid use-after-free
    if (client_) {
        client_->set_notification_callback(nullptr);
    }
}

boost::asio::awaitable<bool>
remote_event_adapter::subscribe(const std::string& event_type) {
    BOOST_LOG_SEV(lg(), info) << "Subscribing to event type: " << event_type;

    messaging::subscribe_request req;
    req.event_type = event_type;

    auto result = co_await client_->process_request_async<
        messaging::subscribe_request,
        messaging::subscribe_response,
        messaging::message_type::subscribe_request>(std::move(req));

    if (!result) {
        BOOST_LOG_SEV(lg(), error) << "Subscribe request failed for "
                                   << event_type << ": " << result.error();
        co_return false;
    }

    if (result->success) {
        BOOST_LOG_SEV(lg(), info) << "Successfully subscribed to " << event_type
                                  << ": " << result->message;
        std::lock_guard lock(mutex_);
        subscriptions_.insert(event_type);
    } else {
        BOOST_LOG_SEV(lg(), warn) << "Subscription failed for " << event_type
                                  << ": " << result->message;
    }

    co_return result->success;
}

bool remote_event_adapter::subscribe_sync(const std::string& event_type) {
    BOOST_LOG_SEV(lg(), info) << "Subscribing to event type (sync): " << event_type;

    messaging::subscribe_request req;
    req.event_type = event_type;

    auto result = client_->process_request<
        messaging::subscribe_request,
        messaging::subscribe_response,
        messaging::message_type::subscribe_request>(std::move(req));

    if (!result) {
        BOOST_LOG_SEV(lg(), error) << "Subscribe request failed for "
                                   << event_type << ": " << result.error();
        return false;
    }

    if (result->success) {
        BOOST_LOG_SEV(lg(), info) << "Successfully subscribed to " << event_type
                                  << ": " << result->message;
        std::lock_guard lock(mutex_);
        subscriptions_.insert(event_type);
    } else {
        BOOST_LOG_SEV(lg(), warn) << "Subscription failed for " << event_type
                                  << ": " << result->message;
    }

    return result->success;
}

boost::asio::awaitable<bool>
remote_event_adapter::unsubscribe(const std::string& event_type) {
    BOOST_LOG_SEV(lg(), info) << "Unsubscribing from event type: " << event_type;

    messaging::unsubscribe_request req;
    req.event_type = event_type;

    auto result = co_await client_->process_request_async<
        messaging::unsubscribe_request,
        messaging::unsubscribe_response,
        messaging::message_type::unsubscribe_request>(std::move(req));

    if (!result) {
        BOOST_LOG_SEV(lg(), error) << "Unsubscribe request failed for "
                                   << event_type << ": " << result.error();
        co_return false;
    }

    if (result->success) {
        BOOST_LOG_SEV(lg(), info) << "Successfully unsubscribed from " << event_type
                                  << ": " << result->message;
        std::lock_guard lock(mutex_);
        subscriptions_.erase(event_type);
    } else {
        BOOST_LOG_SEV(lg(), warn) << "Unsubscription failed for " << event_type
                                  << ": " << result->message;
    }

    co_return result->success;
}

bool remote_event_adapter::unsubscribe_sync(const std::string& event_type) {
    BOOST_LOG_SEV(lg(), info) << "Unsubscribing from event type (sync): " << event_type;

    messaging::unsubscribe_request req;
    req.event_type = event_type;

    auto result = client_->process_request<
        messaging::unsubscribe_request,
        messaging::unsubscribe_response,
        messaging::message_type::unsubscribe_request>(std::move(req));

    if (!result) {
        BOOST_LOG_SEV(lg(), error) << "Unsubscribe request failed for "
                                   << event_type << ": " << result.error();
        return false;
    }

    if (result->success) {
        BOOST_LOG_SEV(lg(), info) << "Successfully unsubscribed from " << event_type
                                  << ": " << result->message;
        std::lock_guard lock(mutex_);
        subscriptions_.erase(event_type);
    } else {
        BOOST_LOG_SEV(lg(), warn) << "Unsubscription failed for " << event_type
                                  << ": " << result->message;
    }

    return result->success;
}

bool remote_event_adapter::is_subscribed(const std::string& event_type) const {
    std::lock_guard lock(mutex_);
    return subscriptions_.contains(event_type);
}

std::set<std::string> remote_event_adapter::get_subscriptions() const {
    std::lock_guard lock(mutex_);
    return subscriptions_;
}

boost::asio::awaitable<std::size_t> remote_event_adapter::resubscribe_all() {
    BOOST_LOG_SEV(lg(), info) << "Re-subscribing to all event types";

    // Get copy of subscriptions under lock
    std::set<std::string> to_resubscribe;
    {
        std::lock_guard lock(mutex_);
        to_resubscribe = subscriptions_;
    }

    if (to_resubscribe.empty()) {
        BOOST_LOG_SEV(lg(), debug) << "No subscriptions to restore";
        co_return 0;
    }

    BOOST_LOG_SEV(lg(), info) << "Re-subscribing to " << to_resubscribe.size()
                              << " event types";

    std::size_t success_count = 0;
    for (const auto& event_type : to_resubscribe) {
        // Clear from local set temporarily - subscribe() will re-add on success
        {
            std::lock_guard lock(mutex_);
            subscriptions_.erase(event_type);
        }

        if (co_await subscribe(event_type)) {
            ++success_count;
        } else {
            BOOST_LOG_SEV(lg(), warn) << "Failed to re-subscribe to " << event_type;
        }
    }

    BOOST_LOG_SEV(lg(), info) << "Re-subscribed to " << success_count << " of "
                              << to_resubscribe.size() << " event types";

    co_return success_count;
}

void remote_event_adapter::set_notification_callback(
    net::notification_callback_t callback) {
    std::lock_guard lock(mutex_);
    user_callback_ = std::move(callback);
}

void remote_event_adapter::on_notification(
    const std::string& event_type,
    std::chrono::system_clock::time_point timestamp) {
    BOOST_LOG_SEV(lg(), debug) << "Received notification for " << event_type;

    net::notification_callback_t callback;
    {
        std::lock_guard lock(mutex_);
        callback = user_callback_;
    }

    if (callback) {
        BOOST_LOG_SEV(lg(), trace) << "Forwarding notification to user callback";
        callback(event_type, timestamp);
    } else {
        BOOST_LOG_SEV(lg(), trace) << "No user callback registered, ignoring notification";
    }
}

}
