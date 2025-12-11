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
#include <sstream>
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
      connection_(nullptr),
      running_(false) {
    BOOST_LOG_SEV(lg(), debug) << "Listener service created.";
}

postgres_listener_service::~postgres_listener_service() {
    stop();
    close_connection();
    BOOST_LOG_SEV(lg(), debug) << "Listener service destroyed.";
}

std::string postgres_listener_service::build_connection_string(
    const sqlgen::postgres::Credentials& credentials) {
    std::ostringstream oss;
    oss << "host=" << credentials.host
        << " port=" << credentials.port
        << " dbname=" << credentials.dbname
        << " user=" << credentials.user
        << " password=" << credentials.password;
    return oss.str();
}

bool postgres_listener_service::open_connection() {
    std::lock_guard lock(mutex_);

    if (connection_) {
        BOOST_LOG_SEV(lg(), debug) << "Connection already open.";
        return true;
    }

    const auto conn_str = build_connection_string(ctx_.credentials());
    BOOST_LOG_SEV(lg(), debug) << "Opening dedicated listener connection.";

    connection_ = PQconnectdb(conn_str.c_str());
    if (PQstatus(connection_) != CONNECTION_OK) {
        BOOST_LOG_SEV(lg(), error) << "Failed to connect to database: "
                                   << PQerrorMessage(connection_);
        PQfinish(connection_);
        connection_ = nullptr;
        return false;
    }

    BOOST_LOG_SEV(lg(), info) << "Dedicated listener connection established.";
    return true;
}

void postgres_listener_service::close_connection() {
    std::lock_guard lock(mutex_);

    if (connection_) {
        BOOST_LOG_SEV(lg(), debug) << "Closing dedicated listener connection.";
        PQfinish(connection_);
        connection_ = nullptr;
    }
}

void postgres_listener_service::issue_pending_listens() {
    // Note: Called from listen_loop with mutex held
    for (const auto& channel : subscribed_channels_) {
        std::string command = "LISTEN " + channel + ";";
        PGresult* res = PQexec(connection_, command.c_str());

        if (PQresultStatus(res) != PGRES_COMMAND_OK) {
            BOOST_LOG_SEV(lg(), error) << "Failed to LISTEN on channel '"
                                       << channel << "': "
                                       << PQerrorMessage(connection_);
        } else {
            BOOST_LOG_SEV(lg(), info) << "Listening on channel: " << channel;
        }
        PQclear(res);
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
    if (!running_.exchange(false)) {
        BOOST_LOG_SEV(lg(), debug) << "Listener not running or already stopped.";
        return;
    }

    BOOST_LOG_SEV(lg(), info) << "Stopping listener thread.";
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
    if (connection_ && running_) {
        std::string command = "LISTEN " + channel_name + ";";
        PGresult* res = PQexec(connection_, command.c_str());

        if (PQresultStatus(res) != PGRES_COMMAND_OK) {
            BOOST_LOG_SEV(lg(), error) << "Failed to LISTEN on channel '"
                                       << channel_name << "': "
                                       << PQerrorMessage(connection_);
        } else {
            BOOST_LOG_SEV(lg(), info) << "Listening on channel: " << channel_name;
        }
        PQclear(res);
    } else {
        BOOST_LOG_SEV(lg(), debug) << "Subscription queued for channel: "
                                   << channel_name
                                   << " (will activate on start)";
    }
}

void postgres_listener_service::notify(const std::string& channel_name,
                                        const std::string& payload) {
    std::lock_guard lock(mutex_);

    if (!connection_) {
        BOOST_LOG_SEV(lg(), error) << "Cannot notify: no connection.";
        return;
    }

    // Escape the payload to prevent SQL injection
    char* escaped = PQescapeLiteral(connection_, payload.c_str(), payload.size());
    if (!escaped) {
        BOOST_LOG_SEV(lg(), error) << "Failed to escape payload: "
                                   << PQerrorMessage(connection_);
        return;
    }

    std::string command = "NOTIFY " + channel_name + ", " + escaped + ";";
    PQfreemem(escaped);

    PGresult* res = PQexec(connection_, command.c_str());
    if (PQresultStatus(res) != PGRES_COMMAND_OK) {
        BOOST_LOG_SEV(lg(), error) << "Failed to NOTIFY on channel '"
                                   << channel_name << "': "
                                   << PQerrorMessage(connection_);
    } else {
        BOOST_LOG_SEV(lg(), debug) << "Sent NOTIFY on channel: " << channel_name;
    }
    PQclear(res);
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

            if (!connection_) {
                BOOST_LOG_SEV(lg(), error) << "Connection lost in listen_loop.";
                running_ = false;
                break;
            }

            // Consume any pending input from the server
            if (PQconsumeInput(connection_) == 0) {
                BOOST_LOG_SEV(lg(), error) << "PQconsumeInput failed: "
                                           << PQerrorMessage(connection_);
                running_ = false;
                break;
            }

            // Process any pending notifications
            PGnotify* pg_notify;
            while ((pg_notify = PQnotifies(connection_)) != nullptr) {
                handle_notification(pg_notify);
                PQfreemem(pg_notify);
            }
        }

        // Sleep briefly before checking again
        // TODO: Consider using select/poll on PQsocket() for efficiency
        std::this_thread::sleep_for(std::chrono::milliseconds(100));
    }

    BOOST_LOG_SEV(lg(), info) << "Listener thread stopped.";
}

void postgres_listener_service::handle_notification(PGnotify* pg_notify) {
    if (!notification_callback_) {
        BOOST_LOG_SEV(lg(), warn)
            << "Received notification but no callback registered for channel: "
            << pg_notify->relname << " with payload: " << pg_notify->extra;
        return;
    }

    BOOST_LOG_SEV(lg(), debug) << "Received notification on channel: "
                               << pg_notify->relname
                               << " with payload: " << pg_notify->extra;
    try {
        auto result = rfl::json::read<domain::entity_change_event>(pg_notify->extra);
        if (result) {
            notification_callback_(*result);
        } else {
            BOOST_LOG_SEV(lg(), error)
                << "Failed to deserialize notification payload: "
                << pg_notify->extra;
        }
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "Failed to parse notification payload '"
                                   << pg_notify->extra << "': " << e.what();
    }
}

}
