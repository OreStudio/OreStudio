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
#include "ores.database/service/postgres_listener_service.hpp"
#include "ores.database/pq/store_error.hpp"
#include "ores.platform/process/pid.hpp"
#include <boost/algorithm/string/join.hpp>
#include <algorithm>
#include <chrono>
#include <libpq-fe.h>
#include <thread>
#include <vector>

namespace ores::database::service {

using namespace ores::logging;

postgres_listener_service::postgres_listener_service(context ctx, notification_callback_t callback)
    : ctx_(std::move(ctx))
    , notification_callback_(std::move(callback))
    , application_name_("ores.database.listener." +
                        std::to_string(ores::platform::process::current_pid()) + "." +
                        std::to_string(instance_counter_.fetch_add(1)))
    , running_(false) {
    BOOST_LOG_SEV(lg(), debug) << "Listener service created.";
}

postgres_listener_service::~postgres_listener_service() {
    stop();
    BOOST_LOG_SEV(lg(), debug) << "Listener service destroyed.";
}

namespace {

/**
 * @brief Runs a statement that returns no rows, and says whether it succeeded.
 */
bool run_command(PGconn* c, const std::string& sql, std::string& error) {
    PGresult* r = PQexec(c, sql.c_str());
    const auto status = PQresultStatus(r);
    const bool ok = status == PGRES_COMMAND_OK || status == PGRES_TUPLES_OK;
    if (!ok)
        error = PQerrorMessage(c);
    PQclear(r);
    return ok;
}

/**
 * @brief LISTEN on a channel, its name quoted as an identifier.
 */
bool listen_on(pq::connection& c, const std::string& channel, std::string& error) {
    char* quoted = PQescapeIdentifier(c.native(), channel.c_str(), channel.size());
    if (!quoted) {
        error = PQerrorMessage(c.native());
        return false;
    }
    const std::string sql = std::string("LISTEN ") + quoted;
    PQfreemem(quoted);
    return run_command(c.native(), sql, error);
}

/**
 * @brief Sends a notification, the channel and the payload bound.
 */
bool notify_on(pq::connection& c,
               const std::string& channel,
               const std::string& payload,
               std::string& error) {
    const char* values[] = {channel.c_str(), payload.c_str()};
    PGresult* r = PQexecParams(
        c.native(), "SELECT pg_notify($1, $2)", 2, nullptr, values, nullptr, nullptr, 0);
    const bool ok = PQresultStatus(r) == PGRES_TUPLES_OK;
    if (!ok)
        error = PQerrorMessage(c.native());
    PQclear(r);
    return ok;
}

/**
 * @brief The notifications the connection has read so far.
 */
std::vector<pg_notification> drain(pq::connection& c) {
    std::vector<pg_notification> out;
    while (PGnotify* n = PQnotifies(c.native())) {
        out.push_back({.channel = n->relname, .payload = n->extra, .backend_pid = n->be_pid});
        PQfreemem(n);
    }
    return out;
}

}

std::unique_ptr<pq::connection> connect_utc(const pq::connection_options& options,
                                            const std::string& application_name) {
    auto with_name = options;
    with_name.application_name = application_name;
    auto c = std::make_unique<pq::connection>(with_name);
    std::string problem;
    if (!run_command(c->native(), "SELECT set_config('TimeZone', 'UTC', false)", problem))
        throw pq::store_error(
            "Failed to set session timezone to UTC: " + problem, "", pq::error_kind::refused);
    return c;
}

bool postgres_listener_service::open_connection() {
    std::lock_guard lock(mutex_);

    if (connection_) {
        BOOST_LOG_SEV(lg(), debug) << "Connection already open.";
        return true;
    }

    BOOST_LOG_SEV(lg(), debug) << "Opening dedicated listener connection.";

    try {
        connection_ = connect_utc(ctx_.connection_options(), application_name_);
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "Failed to connect to database: " << e.what();
        return false;
    }
    BOOST_LOG_SEV(lg(), info) << "Dedicated listener connection established.";
    return true;
}

void postgres_listener_service::issue_pending_listens() {
    // Note: Called from listen_loop with mutex held
    if (!connection_) {
        BOOST_LOG_SEV(lg(), error) << "Cannot issue listens: no connection.";
        return;
    }

    for (const auto& channel : subscribed_channels_) {
        std::string problem;
        if (!listen_on(*connection_, channel, problem)) {
            BOOST_LOG_SEV(lg(), error)
                << "Failed to LISTEN on channel '" << channel << "': " << problem;
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

    // Reset ready state
    {
        std::lock_guard lock(mutex_);
        ready_ = false;
    }
}

bool postgres_listener_service::wait_until_ready(std::chrono::milliseconds timeout) {
    std::unique_lock lock(mutex_);
    return ready_cv_.wait_for(lock, timeout, [this] { return ready_; });
}

void postgres_listener_service::subscribe(const std::string& channel_name) {
    std::lock_guard lock(mutex_);

    // Check if already subscribed
    if (std::find(subscribed_channels_.begin(), subscribed_channels_.end(), channel_name) !=
        subscribed_channels_.end()) {
        BOOST_LOG_SEV(lg(), debug) << "Already subscribed to channel: " << channel_name;
        return;
    }

    subscribed_channels_.push_back(channel_name);

    // If connection is open and running, issue LISTEN immediately
    if (connection_ && running_) {
        std::string problem;
        if (!listen_on(*connection_, channel_name, problem)) {
            BOOST_LOG_SEV(lg(), error)
                << "Failed to LISTEN on channel '" << channel_name << "': " << problem;
        } else {
            BOOST_LOG_SEV(lg(), info) << "Listening on channel: " << channel_name;
        }
    } else {
        BOOST_LOG_SEV(lg(), debug)
            << "Subscription queued for channel: " << channel_name << " (will activate on start)";
    }
}

void postgres_listener_service::notify(const std::string& channel_name,
                                       const std::string& payload) {
    std::lock_guard lock(mutex_);

    if (!connection_) {
        BOOST_LOG_SEV(lg(), error) << "Cannot notify: no connection.";
        return;
    }

    std::string problem;
    if (!notify_on(*connection_, channel_name, payload, problem)) {
        BOOST_LOG_SEV(lg(), error)
            << "Failed to NOTIFY on channel '" << channel_name << "': " << problem;
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
        ready_ = true;
    }
    ready_cv_.notify_all();

    static constexpr std::chrono::seconds min_backoff{1};
    // A local PostgreSQL should reconnect in well under a second; 30s was
    // sized for a remote DB and made outages look far longer than they were.
    static constexpr std::chrono::seconds max_backoff{5};
    auto backoff = min_backoff;

    while (running_) {
        bool connection_ok = true;
        // Drain notifications into a local vector under the lock, then
        // process callbacks outside it. Under a heavy bulk import (e.g. the
        // GLEIF party dataset — hundreds of INSERTs each firing a NOTIFY
        // trigger), processing callbacks while holding mutex_ blocks any
        // other thread waiting on it (subscribe(), notify(), stop()) for as
        // long as the whole batch takes, and — because that same mutex
        // guards the 100ms poll cycle above — starves the listener's own
        // next iteration, making it appear to have died under load.
        std::vector<pg_notification> batch;
        {
            std::lock_guard lock(mutex_);

            if (!connection_) {
                connection_ok = false;
            } else if (PQconsumeInput(connection_->native()) != 1) {
                BOOST_LOG_SEV(lg(), error) << "Connection error while consuming input.";
                connection_.reset();
                connection_ok = false;
                ++connection_loss_count_;
                loss_start_ = std::chrono::steady_clock::now();
                BOOST_LOG_SEV(lg(), error)
                    << "Listener connection lost while listening on channels ["
                    << boost::algorithm::join(subscribed_channels_, ", ")
                    << "]; notifications in flight may be lost";
            } else {
                backoff = min_backoff; // reset on successful poll
                batch = drain(*connection_);
            }
        }
        for (const auto& notification : batch)
            handle_notification(notification);

        if (!connection_ok) {
            if (!running_)
                break;

            BOOST_LOG_SEV(lg(), warn)
                << "Listener connection lost. Reconnecting in " << backoff.count() << "s...";
            std::this_thread::sleep_for(backoff);
            backoff = std::min(backoff * 2, max_backoff);

            if (!running_)
                break;

            std::lock_guard lock(mutex_);
            try {
                connection_ = connect_utc(ctx_.connection_options(), application_name_);
            } catch (const std::exception& e) {
                BOOST_LOG_SEV(lg(), error) << "Reconnect failed: " << e.what();
                continue;
            }
            const auto loss_duration = std::chrono::duration_cast<std::chrono::milliseconds>(
                std::chrono::steady_clock::now() - loss_start_);
            BOOST_LOG_SEV(lg(), error) << "Reconnected after " << loss_duration.count()
                                       << "ms; notifications sent on channels ["
                                       << boost::algorithm::join(subscribed_channels_, ", ")
                                       << "] during the outage are lost (PostgreSQL has no replay)";
            BOOST_LOG_SEV(lg(), info) << "Listener reconnected. Reissuing LISTENs.";
            issue_pending_listens();
            continue;
        }

        // Sleep only when the poll found nothing. Each poll reads what the
        // socket holds at that moment, so a fixed pause after every poll caps
        // the listener at one socket read per pause: a bulk import's per-row
        // notifications then pile up for minutes, and PostgreSQL holds this
        // backend in ClientWrite meanwhile, which stalls every DROP DATABASE
        // on the server.
        if (batch.empty())
            std::this_thread::sleep_for(std::chrono::milliseconds(100));
    }

    BOOST_LOG_SEV(lg(), info) << "Listener thread stopped.";
}

void postgres_listener_service::handle_notification(const pg_notification& notification) {
    if (!notification_callback_) {
        BOOST_LOG_SEV(lg(), warn)
            << "Received notification but no callback registered for channel: "
            << notification.channel << " with payload: " << notification.payload;
        return;
    }

    BOOST_LOG_SEV(lg(), debug) << "Received notification on channel: " << notification.channel
                               << " with payload: " << notification.payload;

    notification_callback_(notification.channel, notification.payload);
}

}
