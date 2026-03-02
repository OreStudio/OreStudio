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
#include <optional>
#include <vector>
#include <chrono>
#include <expected>
#include "ores.comms/messaging/message_type.hpp"
#include "ores.comms/messaging/message_traits.hpp"
#include "ores.utility/serialization/error_code.hpp"
#include "ores.mq/pgmq/metrics_sample.hpp"
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

// ============================================================================
// get_queue_metric_samples messages
// ============================================================================

/**
 * @brief Request to retrieve time-series metric samples for a specific queue.
 *
 * Returns rows from ores_mq_metrics_samples_tbl, ordered by sample_time ASC.
 * The optional from/to fields define an inclusive time window. When omitted,
 * all available samples for the queue are returned (capped at 10,000 rows).
 */
struct get_queue_metric_samples_request final {
    std::string queue_name;
    std::optional<std::chrono::system_clock::time_point> from;
    std::optional<std::chrono::system_clock::time_point> to;

    /**
     * @brief Serialize request to bytes.
     *
     * Format:
     * - 2+N bytes: queue_name (string)
     * - 1 byte:   has_from flag
     * - (if has_from) 2+N bytes: from timestamp (ISO UTC string)
     * - 1 byte:   has_to flag
     * - (if has_to) 2+N bytes: to timestamp (ISO UTC string)
     */
    std::vector<std::byte> serialize() const;
    static std::expected<get_queue_metric_samples_request,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s,
    const get_queue_metric_samples_request& v);

/**
 * @brief Response containing time-series metric samples for a queue.
 */
struct get_queue_metric_samples_response final {
    bool success{false};
    std::string message;
    std::string queue_name;
    std::vector<pgmq::metrics_sample> samples;

    /**
     * @brief Serialize response to bytes.
     *
     * Format:
     * - 1 byte:   success
     * - 2+N bytes: message (error text, empty on success)
     * - 2+N bytes: queue_name
     * - 4 bytes:  sample count
     * - Per sample:
     *   - 2+N bytes: sample_time (ISO UTC string)
     *   - 8 bytes:   queue_length (int64)
     *   - 8 bytes:   total_messages (int64)
     */
    std::vector<std::byte> serialize() const;
    static std::expected<get_queue_metric_samples_response,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s,
    const get_queue_metric_samples_response& v);

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

template<>
struct message_traits<mq::messaging::get_queue_metric_samples_request> {
    using request_type = mq::messaging::get_queue_metric_samples_request;
    using response_type = mq::messaging::get_queue_metric_samples_response;
    static constexpr message_type request_message_type =
        message_type::get_queue_metric_samples_request;
};

}

#endif
