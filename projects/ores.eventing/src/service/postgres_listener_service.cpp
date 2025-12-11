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
#include "ores.eventing/service/postgres_listener_service.hpp"

#include <chrono>
#include <thread>
#include <algorithm>
#include <rfl/json.hpp>
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.

namespace ores::eventing::service {

using namespace ores::utility::log;

postgres_listener_service::postgres_listener_service(
    utility::database::context ctx,
    notification_callback_t callback)
    : ctx_(std::move(ctx)),
      notification_callback_(std::move(callback)),
      connection_(std::nullopt),
      running_(false) {
    BOOST_LOG_SEV(lg(), debug) << "Listener service created.";
}

postgres_listener_service::~postgres_listener_service() {
    stop();
    BOOST_LOG_SEV(lg(), debug) << "Listener service destroyed.";
}

bool postgres_listener_service::open_connection() {
    std::lock_guard lock(mutex_);

    if (connection_.has_value()) {
        BOOST_LOG_SEV(lg(), debug) << "Connection already open.";
        return true;
    }

    BOOST_LOG_SEV(lg(), debug) << "Opening dedicated listener connection.";

    auto result = sqlgen::postgres::connect(ctx_.credentials());
    if (!result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to connect to database: "
                                   << result.error().what();
        return false;
    }

    connection_ = std::move(*result);
    BOOST_LOG_SEV(lg(), info) << "Dedicated listener connection established.";
    return true;
}

void postgres_listener_service::issue_pending_listens() {
    // Note: Called from listen_loop with mutex held
    if (!connection_.has_value()) {
        BOOST_LOG_SEV(lg(), error) << "Cannot issue listens: no connection.";
        return;
    }

    for (const auto& channel : subscribed_channels_) {
        auto result = (*connection_)->listen(channel);
        if (!result) {
            BOOST_LOG_SEV(lg(), error) << "Failed to LISTEN on channel '"
                                       << channel << "': "
                                       << result.error().what();
        } else {
            BOOST_LOG_SEV(lg(), info) << "Listening on channel: " << channel;
        }
    }
}

void postgres_listener_service::start() {
    if (running_.exchange(true)) {
        BOOST_LOG_SEV(lg(), warn) << "Listener already running.";
        return;
    }

    if (!open_connection()) {
        running_ = false;
        BOOST_LOG_SEV(lg(), error) << "Cannot start listener: connection failed.";
        return;
    }

    BOOST_LOG_SEV(lg(), info) << "Starting listener thread.";
    listener_thread_ = std::thread(&postgres_listener_service::listen_loop, this);
}

void postgres_listener_service::stop() {
    if (running_.exchange(false)) {
        BOOST_LOG_SEV(lg(), info) << "Stopping listener thread.";
    } else {
        BOOST_LOG_SEV(lg(), debug) << "Listener not running or already stopped.";
    }

    // Always join if joinable, even if thread terminated on its own due to error
    if (listener_thread_.joinable()) {
        listener_thread_.join();
    }
}

void postgres_listener_service::subscribe(const std::string& channel_name) {
    std::lock_guard lock(mutex_);

    // Check if already subscribed
    if (std::find(subscribed_channels_.begin(), subscribed_channels_.end(),
                  channel_name) != subscribed_channels_.end()) {
        BOOST_LOG_SEV(lg(), debug) << "Already subscribed to channel: "
                                   << channel_name;
        return;
    }

    subscribed_channels_.push_back(channel_name);

    // If connection is open and running, issue LISTEN immediately
    if (connection_.has_value() && running_) {
        auto result = (*connection_)->listen(channel_name);
        if (!result) {
            BOOST_LOG_SEV(lg(), error) << "Failed to LISTEN on channel '"
                                       << channel_name << "': "
                                       << result.error().what();
        } else {
            BOOST_LOG_SEV(lg(), info) << "Listening on channel: " << channel_name;
        }
    } else {
        BOOST_LOG_SEV(lg(), debug) << "Subscription queued for channel: "
                                   << channel_name
                                   << " (will activate on start)";
    }
}

void postgres_listener_service::notify(const std::string& channel_name,
                                        const std::string& payload) {
    std::lock_guard lock(mutex_);

    if (!connection_.has_value()) {
        BOOST_LOG_SEV(lg(), error) << "Cannot notify: no connection.";
        return;
    }

    auto result = (*connection_)->notify(channel_name, payload);
    if (!result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to NOTIFY on channel '"
                                   << channel_name << "': "
                                   << result.error().what();
    } else {
        BOOST_LOG_SEV(lg(), debug) << "Sent NOTIFY on channel: " << channel_name;
    }
}

void postgres_listener_service::listen_loop() {
    BOOST_LOG_SEV(lg(), info) << "Listener thread started.";

    // Issue LISTEN for all pre-subscribed channels
    {
        std::lock_guard lock(mutex_);
        issue_pending_listens();
    }

    while (running_) {
        {
            std::lock_guard lock(mutex_);

            if (!connection_.has_value()) {
                BOOST_LOG_SEV(lg(), error) << "Connection lost in listen_loop.";
                running_ = false;
                break;
            }

            // Consume any available input from the server
            if (!(*connection_)->consume_input()) {
                BOOST_LOG_SEV(lg(), error)
                    << "Connection error while consuming input.";
                running_ = false;
                break;
            }

            // Process any pending notifications
            auto notifications = (*connection_)->get_notifications();
            for (const auto& notification : notifications) {
                handle_notification(notification);
            }
        }

        // Sleep briefly before checking again
        std::this_thread::sleep_for(std::chrono::milliseconds(100));
    }

    BOOST_LOG_SEV(lg(), info) << "Listener thread stopped.";
}

void postgres_listener_service::handle_notification(
    const sqlgen::postgres::Notification& notification) {
    if (!notification_callback_) {
        BOOST_LOG_SEV(lg(), warn)
            << "Received notification but no callback registered for channel: "
            << notification.channel << " with payload: " << notification.payload;
        return;
    }

    BOOST_LOG_SEV(lg(), debug) << "Received notification on channel: "
                               << notification.channel
                               << " with payload: " << notification.payload;
    try {
        auto result = rfl::json::read<domain::entity_change_event>(notification.payload);
        if (result) {
            notification_callback_(*result);
        } else {
            BOOST_LOG_SEV(lg(), error)
                << "Failed to deserialize notification payload: "
                << notification.payload;
        }
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "Failed to parse notification payload '"
                                   << notification.payload << "': " << e.what();
    }
}

}
