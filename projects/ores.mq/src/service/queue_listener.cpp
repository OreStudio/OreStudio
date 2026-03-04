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
#include "ores.mq/service/mq_service.hpp"

namespace ores::mq::service {

using namespace ores::logging;

namespace {

/// PostgreSQL NOTIFY channel used by ores_mq_messages_tbl for new messages.
constexpr std::string_view notify_channel = "ores_mq_messages";

} // anonymous namespace

queue_listener::queue_listener(
    database::context ctx,
    eventing::service::event_bus& bus,
    const std::string& queue_name)
    : ctx_(std::move(ctx))
    , bus_(bus)
    , queue_name_(queue_name)
    , listener_(ctx_,
          [this](const std::string& channel, const std::string& payload) {
              on_notify(channel, payload);
          })
{
    BOOST_LOG_SEV(lg(), info) << "Creating queue_listener for queue: "
                              << queue_name_;
    BOOST_LOG_SEV(lg(), debug) << "Subscribing to NOTIFY channel: "
                               << notify_channel;
    listener_.subscribe(std::string(notify_channel));
}

queue_listener::~queue_listener() {
    BOOST_LOG_SEV(lg(), debug) << "Destroying queue_listener for queue: "
                               << queue_name_;
    listener_.stop();
}

void queue_listener::start() {
    BOOST_LOG_SEV(lg(), info) << "Starting queue_listener for queue: "
                              << queue_name_;
    listener_.start();
}

void queue_listener::stop() {
    BOOST_LOG_SEV(lg(), info) << "Stopping queue_listener for queue: "
                              << queue_name_;
    listener_.stop();
}

void queue_listener::on_notify(const std::string& channel,
                               const std::string& /*payload*/) {
    BOOST_LOG_SEV(lg(), debug) << "Received NOTIFY on channel: " << channel
                               << " for queue: " << queue_name_;

    // Look up the queue definition by name. If this listener was constructed
    // for a queue that has since been deactivated we simply log and return.
    mq_service svc(ctx_);
    auto queue_def = svc.find_queue(queue_name_, std::nullopt, std::nullopt);
    if (!queue_def) {
        BOOST_LOG_SEV(lg(), warn) << "Queue not found (or deactivated): "
                                  << queue_name_;
        return;
    }

    // Drain all available messages. Multiple messages may have arrived between
    // NOTIFY signals (e.g., in a batch send), so we read until empty.
    std::size_t count = 0;
    while (true) {
        std::vector<domain::mq_message> msgs;
        try {
            // Read a single message with a short visibility timeout; we will
            // ack it immediately after publishing the event.
            msgs = svc.read(queue_def->id, 1, 30);
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(lg(), error) << "Failed to read from queue "
                                       << queue_name_ << ": " << e.what();
            break;
        }

        if (msgs.empty()) {
            break;
        }

        const auto& msg = msgs.front();

        domain::queue_message_event ev;
        ev.queue_name = queue_name_;
        ev.msg_id = msg.id;
        ev.timestamp = msg.created_at;
        ev.encoding = domain::payload_encoding::json;

        // Store the JSON payload as bytes. Consumers parse tenant_id and
        // other fields from the payload themselves.
        if (msg.payload) {
            const auto& body = *msg.payload;
            ev.payload.assign(
                reinterpret_cast<const std::byte*>(body.data()),
                reinterpret_cast<const std::byte*>(body.data() + body.size()));
        }

        BOOST_LOG_SEV(lg(), debug) << "Publishing queue_message_event: queue="
                                   << queue_name_ << " msg_id=" << ev.msg_id
                                   << " payload_bytes=" << ev.payload.size();
        bus_.publish(ev);

        // Acknowledge (delete) the message after successful publish.
        try {
            svc.ack({msg.id});
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(lg(), error) << "Failed to ack message " << msg.id
                                       << " from queue " << queue_name_
                                       << ": " << e.what();
        }

        ++count;
    }

    if (count > 0) {
        BOOST_LOG_SEV(lg(), info) << "Processed " << count
                                  << " message(s) from queue: " << queue_name_;
    }
}

}
