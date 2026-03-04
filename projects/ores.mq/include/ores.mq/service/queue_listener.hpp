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
#ifndef ORES_MQ_SERVICE_QUEUE_LISTENER_HPP
#define ORES_MQ_SERVICE_QUEUE_LISTENER_HPP

#include <string>
#include "ores.logging/make_logger.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.database/service/postgres_listener_service.hpp"
#include "ores.eventing/service/event_bus.hpp"

namespace ores::mq::service {

/**
 * @brief Bridges PostgreSQL NOTIFY signals to typed domain events on the
 * event bus.
 *
 * queue_listener monitors a single named queue for new messages. When a
 * new message is inserted into ores_mq_messages_tbl the database fires a
 * NOTIFY on the @c ores_mq_messages channel. The listener reads and acks
 * the available messages and publishes a domain::queue_message_event on the
 * event_bus for each one.
 *
 * Lifecycle:
 * @code
 *     queue_listener listener(ctx, bus, "my_queue");
 *     listener.start();
 *     // ...
 *     listener.stop();
 * @endcode
 */
class queue_listener final {
private:
    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger("ores.mq.service.queue_listener");
        return instance;
    }

public:
    /**
     * @brief Constructs a queue_listener for the specified queue.
     *
     * Subscribes to the ores_mq_messages NOTIFY channel and registers the
     * callback. Call start() to begin receiving events.
     *
     * @param ctx        Database context used for reading messages.
     * @param bus        Event bus on which queue_message_event instances will
     *                   be published.
     * @param queue_name Name of the queue to monitor.
     */
    queue_listener(database::context ctx,
                   eventing::service::event_bus& bus,
                   const std::string& queue_name);

    /**
     * @brief Destructor — stops the listener.
     */
    ~queue_listener();

    queue_listener(const queue_listener&) = delete;
    queue_listener& operator=(const queue_listener&) = delete;

    /**
     * @brief Starts the listener thread; begins receiving NOTIFY signals.
     */
    void start();

    /**
     * @brief Stops the listener thread.
     */
    void stop();

private:
    /**
     * @brief Callback invoked by postgres_listener_service on each NOTIFY.
     *
     * Reads available messages from the queue via mq_service::read(),
     * publishes a queue_message_event for each, then acks them all.
     *
     * @param channel  The PostgreSQL channel name (ores_mq_messages).
     * @param payload  The raw NOTIFY payload.
     */
    void on_notify(const std::string& channel, const std::string& payload);

    database::context ctx_;
    eventing::service::event_bus& bus_;
    std::string queue_name_;
    database::service::postgres_listener_service listener_;
};

}

#endif
