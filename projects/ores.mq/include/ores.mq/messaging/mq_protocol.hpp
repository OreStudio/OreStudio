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
#ifndef ORES_MQ_MESSAGING_MQ_PROTOCOL_HPP
#define ORES_MQ_MESSAGING_MQ_PROTOCOL_HPP

#include <cstdint>
#include <string>
#include <string_view>
#include <vector>
#include "ores.mq/domain/queue_definition.hpp"
#include "ores.mq/domain/queue_stats.hpp"

namespace ores::mq::messaging {

/**
 * @brief Represents a single message in a queue.
 */
struct queue_message {
    std::int64_t msg_id = 0;
    int read_count = 0;
    std::string created_at;
    std::string visible_after;
    std::string payload;
};

struct send_message_request {
    using response_type = struct send_message_response;
    static constexpr std::string_view nats_subject = "mq.v1.messages.send";
    std::string queue_id;
    std::string payload;
    int delay_seconds = 0;
};

struct send_message_response {
    bool success = false;
    std::string message;
    std::int64_t msg_id = 0;
};

struct read_messages_request {
    using response_type = struct read_messages_response;
    static constexpr std::string_view nats_subject = "mq.v1.messages.read";
    std::string queue_id;
    int count = 10;
    int vt_seconds = 30;
};

struct read_messages_response {
    bool success = false;
    std::string message;
    std::vector<queue_message> messages;
};

struct pop_messages_request {
    using response_type = struct pop_messages_response;
    static constexpr std::string_view nats_subject = "mq.v1.messages.pop";
    std::string queue_id;
    int count = 10;
};

struct pop_messages_response {
    bool success = false;
    std::string message;
    std::vector<queue_message> messages;
};

struct delete_messages_request {
    using response_type = struct delete_messages_response;
    static constexpr std::string_view nats_subject = "mq.v1.messages.delete";
    std::string queue_name;
    std::vector<std::int64_t> msg_ids;
};

struct delete_messages_response {
    bool success = false;
    std::string message;
    int deleted_count = 0;
};

struct get_queues_request {
    using response_type = struct get_queues_response;
    static constexpr std::string_view nats_subject = "mq.v1.queues.list";
    int offset = 0;
    int limit = 100;
};

struct get_queues_response {
    bool success = false;
    std::string message;
    std::vector<ores::mq::domain::queue_definition> queues;
};

struct get_queue_stats_request {
    using response_type = struct get_queue_stats_response;
    static constexpr std::string_view nats_subject = "mq.v1.queues.stats";
    int offset = 0;
    int limit = 100;
};

struct get_queue_stats_response {
    bool success = false;
    std::string message;
    std::vector<ores::mq::domain::queue_stats> stats;
};

}

#endif
