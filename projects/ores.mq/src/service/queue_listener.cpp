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
#include "ores.mq/service/queue_listener.hpp"

#include <boost/log/sources/record_ostream.hpp>
#include "ores.mq/domain/queue_message_event.hpp"

namespace ores::mq::service {

using namespace ores::logging;

namespace {

/// Channel name suffix that pgmq appends for INSERT notifications.
constexpr std::string_view notify_suffix = ".INSERT";

/// Prefix pgmq uses for queue notification channels.
constexpr std::string_view notify_prefix = "pgmq.q_";

}

queue_listener::queue_listener(
    database::context ctx,
    eventing::service::event_bus& bus,
    const std::string& queue_name)
    : ctx_(std::move(ctx))
    , bus_(bus)
    , queue_name_(queue_name)
    , client_()
    , listener_(ctx_,
          [this](const std::string& channel, const std::string& payload) {
              on_notify(channel, payload);
          })
{
    BOOST_LOG_SEV(lg(), info) << "Creating queue_listener for queue: " << queue_name_;

    const auto notify_channel =
        std::string(notify_prefix) + queue_name_ + std::string(notify_suffix);

    BOOST_LOG_SEV(lg(), debug) << "Enabling NOTIFY for queue: " << queue_name_
                               << " on channel: " << notify_channel;
    client_.enable_notify(ctx_, queue_name_);
    listener_.subscribe(notify_channel);
}

queue_listener::~queue_listener() {
    BOOST_LOG_SEV(lg(), debug) << "Destroying queue_listener for queue: " << queue_name_;
    listener_.stop();
    try {
        client_.disable_notify(ctx_, queue_name_);
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), warn) << "Failed to disable NOTIFY for queue "
                                  << queue_name_ << ": " << e.what();
    }
}

void queue_listener::start() {
    BOOST_LOG_SEV(lg(), info) << "Starting queue_listener for queue: " << queue_name_;
    listener_.start();
}

void queue_listener::stop() {
    BOOST_LOG_SEV(lg(), info) << "Stopping queue_listener for queue: " << queue_name_;
    listener_.stop();
}

void queue_listener::on_notify(const std::string& channel,
                               const std::string& /*payload*/) {
    BOOST_LOG_SEV(lg(), debug) << "Received NOTIFY on channel: " << channel
                               << " for queue: " << queue_name_;

    // Drain all available messages. Multiple messages may have been enqueued
    // between NOTIFY signals (e.g., in a batch send), so we keep popping until
    // the queue is empty.
    std::size_t count = 0;
    while (true) {
        std::optional<pgmq::message<std::string>> msg;
        try {
            msg = client_.pop<std::string>(ctx_, queue_name_);
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(lg(), error) << "Failed to pop from queue "
                                       << queue_name_ << ": " << e.what();
            break;
        }

        if (!msg) {
            break;
        }

        domain::queue_message_event ev;
        ev.queue_name = queue_name_;
        ev.msg_id = msg->msg_id;
        ev.timestamp = msg->enqueued_at;
        ev.encoding = domain::payload_encoding::json;

        // Store the raw JSONB body as bytes. Consumers parse tenant_id
        // and other fields from the payload themselves.
        ev.payload.assign(
            reinterpret_cast<const std::byte*>(msg->body.data()),
            reinterpret_cast<const std::byte*>(msg->body.data() + msg->body.size()));

        BOOST_LOG_SEV(lg(), debug) << "Publishing queue_message_event: queue="
                                   << queue_name_ << " msg_id=" << ev.msg_id
                                   << " payload_bytes=" << ev.payload.size();
        bus_.publish(ev);
        ++count;
    }

    if (count > 0) {
        BOOST_LOG_SEV(lg(), info) << "Processed " << count
                                  << " message(s) from queue: " << queue_name_;
    }
}

}
