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
#include <cstdint>
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

// ============================================================================
// Shared wire type for messages read from a queue
// ============================================================================

/**
 * @brief A single pgmq message as returned over the wire.
 */
struct queue_message final {
    std::int64_t msg_id{0};
    std::int32_t read_ct{0};
    std::string enqueued_at;  // ISO UTC string
    std::string vt;           // ISO UTC visibility timeout string
    std::string payload;      // Raw JSON body
};

// ============================================================================
// create_queue messages
// ============================================================================

struct create_queue_request final {
    std::string queue_name;
    bool is_unlogged{false};

    std::vector<std::byte> serialize() const;
    static std::expected<create_queue_request,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const create_queue_request& v);

struct create_queue_response final {
    bool success{false};
    std::string message;

    std::vector<std::byte> serialize() const;
    static std::expected<create_queue_response,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const create_queue_response& v);

// ============================================================================
// drop_queue messages
// ============================================================================

struct drop_queue_request final {
    std::string queue_name;

    std::vector<std::byte> serialize() const;
    static std::expected<drop_queue_request,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const drop_queue_request& v);

struct drop_queue_response final {
    bool success{false};
    std::string message;

    std::vector<std::byte> serialize() const;
    static std::expected<drop_queue_response,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const drop_queue_response& v);

// ============================================================================
// purge_queue messages
// ============================================================================

struct purge_queue_request final {
    std::string queue_name;

    std::vector<std::byte> serialize() const;
    static std::expected<purge_queue_request,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const purge_queue_request& v);

struct purge_queue_response final {
    bool success{false};
    std::string message;
    std::int64_t purged_count{0};

    std::vector<std::byte> serialize() const;
    static std::expected<purge_queue_response,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const purge_queue_response& v);

// ============================================================================
// send_message messages
// ============================================================================

struct send_message_request final {
    std::string queue_name;
    std::string payload;         // Raw JSON body
    std::int32_t delay_seconds{0};

    std::vector<std::byte> serialize() const;
    static std::expected<send_message_request,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const send_message_request& v);

struct send_message_response final {
    bool success{false};
    std::string message;
    std::int64_t msg_id{0};

    std::vector<std::byte> serialize() const;
    static std::expected<send_message_response,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const send_message_response& v);

// ============================================================================
// read_messages messages  (non-destructive peek)
// ============================================================================

struct read_messages_request final {
    std::string queue_name;
    std::int32_t count{1};
    std::int32_t vt_seconds{30};  // visibility timeout extension in seconds

    std::vector<std::byte> serialize() const;
    static std::expected<read_messages_request,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const read_messages_request& v);

struct read_messages_response final {
    bool success{false};
    std::string message;
    std::vector<queue_message> messages;

    std::vector<std::byte> serialize() const;
    static std::expected<read_messages_response,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const read_messages_response& v);

// ============================================================================
// pop_messages messages  (atomic read + delete)
// ============================================================================

struct pop_messages_request final {
    std::string queue_name;
    std::int32_t count{1};

    std::vector<std::byte> serialize() const;
    static std::expected<pop_messages_request,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const pop_messages_request& v);

struct pop_messages_response final {
    bool success{false};
    std::string message;
    std::vector<queue_message> messages;

    std::vector<std::byte> serialize() const;
    static std::expected<pop_messages_response,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const pop_messages_response& v);

// ============================================================================
// delete_messages messages
// ============================================================================

struct delete_messages_request final {
    std::string queue_name;
    std::vector<std::int64_t> msg_ids;

    std::vector<std::byte> serialize() const;
    static std::expected<delete_messages_request,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const delete_messages_request& v);

struct delete_messages_response final {
    bool success{false};
    std::string message;
    std::int32_t deleted_count{0};

    std::vector<std::byte> serialize() const;
    static std::expected<delete_messages_response,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const delete_messages_response& v);

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

template<>
struct message_traits<mq::messaging::create_queue_request> {
    using request_type = mq::messaging::create_queue_request;
    using response_type = mq::messaging::create_queue_response;
    static constexpr message_type request_message_type =
        message_type::create_queue_request;
};

template<>
struct message_traits<mq::messaging::drop_queue_request> {
    using request_type = mq::messaging::drop_queue_request;
    using response_type = mq::messaging::drop_queue_response;
    static constexpr message_type request_message_type =
        message_type::drop_queue_request;
};

template<>
struct message_traits<mq::messaging::purge_queue_request> {
    using request_type = mq::messaging::purge_queue_request;
    using response_type = mq::messaging::purge_queue_response;
    static constexpr message_type request_message_type =
        message_type::purge_queue_request;
};

template<>
struct message_traits<mq::messaging::send_message_request> {
    using request_type = mq::messaging::send_message_request;
    using response_type = mq::messaging::send_message_response;
    static constexpr message_type request_message_type =
        message_type::send_message_request;
};

template<>
struct message_traits<mq::messaging::read_messages_request> {
    using request_type = mq::messaging::read_messages_request;
    using response_type = mq::messaging::read_messages_response;
    static constexpr message_type request_message_type =
        message_type::read_messages_request;
};

template<>
struct message_traits<mq::messaging::pop_messages_request> {
    using request_type = mq::messaging::pop_messages_request;
    using response_type = mq::messaging::pop_messages_response;
    static constexpr message_type request_message_type =
        message_type::pop_messages_request;
};

template<>
struct message_traits<mq::messaging::delete_messages_request> {
    using request_type = mq::messaging::delete_messages_request;
    using response_type = mq::messaging::delete_messages_response;
    static constexpr message_type request_message_type =
        message_type::delete_messages_request;
};

}

#endif
