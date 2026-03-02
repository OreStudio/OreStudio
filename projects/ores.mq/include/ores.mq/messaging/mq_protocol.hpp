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

#include <span>
#include <iosfwd>
#include <string>
#include <vector>
#include <expected>
#include "ores.comms/messaging/message_type.hpp"
#include "ores.comms/messaging/message_traits.hpp"
#include "ores.utility/serialization/error_code.hpp"
#include "ores.mq/pgmq/queue_info.hpp"
#include "ores.mq/pgmq/queue_metrics.hpp"

namespace ores::mq::messaging {

// ============================================================================
// get_queues messages
// ============================================================================

/**
 * @brief Request to retrieve all pgmq queues.
 */
struct get_queues_request final {
    std::vector<std::byte> serialize() const;
    static std::expected<get_queues_request,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const get_queues_request& v);

/**
 * @brief Response containing all pgmq queues.
 */
struct get_queues_response final {
    bool success{false};
    std::string message;
    std::vector<pgmq::queue_info> queues;

    std::vector<std::byte> serialize() const;
    static std::expected<get_queues_response,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const get_queues_response& v);

// ============================================================================
// get_queue_metrics messages
// ============================================================================

/**
 * @brief Request to retrieve metrics for all pgmq queues.
 */
struct get_queue_metrics_request final {
    std::vector<std::byte> serialize() const;
    static std::expected<get_queue_metrics_request,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const get_queue_metrics_request& v);

/**
 * @brief Response containing metrics for all pgmq queues.
 */
struct get_queue_metrics_response final {
    bool success{false};
    std::string message;
    std::vector<pgmq::queue_metrics> metrics;

    std::vector<std::byte> serialize() const;
    static std::expected<get_queue_metrics_response,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const get_queue_metrics_response& v);

}

namespace ores::comms::messaging {

template<>
struct message_traits<mq::messaging::get_queues_request> {
    using request_type = mq::messaging::get_queues_request;
    using response_type = mq::messaging::get_queues_response;
    static constexpr message_type request_message_type =
        message_type::get_queues_request;
};

template<>
struct message_traits<mq::messaging::get_queue_metrics_request> {
    using request_type = mq::messaging::get_queue_metrics_request;
    using response_type = mq::messaging::get_queue_metrics_response;
    static constexpr message_type request_message_type =
        message_type::get_queue_metrics_request;
};

}

#endif
