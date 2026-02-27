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
#ifndef ORES_MQ_DOMAIN_QUEUE_MESSAGE_EVENT_HPP
#define ORES_MQ_DOMAIN_QUEUE_MESSAGE_EVENT_HPP

#include <chrono>
#include <cstddef>
#include <cstdint>
#include <string>
#include <vector>
#include "ores.eventing/domain/event_traits.hpp"

namespace ores::mq::domain {

/**
 * @brief Encoding of the message payload bytes.
 */
enum class payload_encoding : std::uint8_t {
    json = 0, ///< JSON-encoded payload (pgmq default: JSONB).
    bson = 1  ///< BSON-encoded payload.
};

/**
 * @brief Domain event representing a message popped from a pgmq queue.
 *
 * Published by queue_listener when it pops a message from a monitored
 * pgmq queue. Consumers subscribe to this event on the event_bus and
 * may forward the payload to remote subscribers via subscription_manager.
 */
struct queue_message_event final {
    /// Name of the pgmq queue this message came from.
    std::string queue_name;

    /// Unique monotonically increasing message ID within the queue.
    std::int64_t msg_id{0};

    /// Timestamp when the message was enqueued (UTC).
    std::chrono::system_clock::time_point timestamp;

    /// Encoding of the payload bytes (always json for pgmq JSONB queues).
    payload_encoding encoding{payload_encoding::json};

    /// Raw message payload bytes.
    std::vector<std::byte> payload;

    /// Tenant this message belongs to (may be empty for system messages).
    std::string tenant_id;
};

}

namespace ores::eventing::domain {

/**
 * @brief Event traits specialization for queue_message_event.
 */
template<>
struct event_traits<ores::mq::domain::queue_message_event> {
    static constexpr std::string_view name = "ores.mq.queue_message_event";
};

}

#endif
