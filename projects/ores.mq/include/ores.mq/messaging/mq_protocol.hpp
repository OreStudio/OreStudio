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
#include "ores.mq/domain/queue_definition.hpp"
#include "ores.mq/domain/queue_stats.hpp"

namespace ores::mq::messaging {

// ============================================================================
// get_queues messages
// ============================================================================

/**
 * @brief Request to retrieve all active queues.
 */
struct get_queues_request final {
    std::vector<std::byte> serialize() const;
    static std::expected<get_queues_request,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const get_queues_request& v);

/**
 * @brief Response containing all active queues.
 */
struct get_queues_response final {
    bool success{false};
    std::string message;
    std::vector<domain::queue_definition> queues;

    std::vector<std::byte> serialize() const;
    static std::expected<get_queues_response,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const get_queues_response& v);

// ============================================================================
// get_queue_stats messages
// ============================================================================

/**
 * @brief Request to retrieve statistics for all queues.
 */
struct get_queue_stats_request final {
    std::vector<std::byte> serialize() const;
    static std::expected<get_queue_stats_request,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const get_queue_stats_request& v);

/**
 * @brief Response containing statistics for all queues.
 */
struct get_queue_stats_response final {
    bool success{false};
    std::string message;
    std::vector<domain::queue_stats> stats;

    std::vector<std::byte> serialize() const;
    static std::expected<get_queue_stats_response,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const get_queue_stats_response& v);

// ============================================================================
// get_queue_stats_samples messages
// ============================================================================

/**
 * @brief Request to retrieve time-series statistics samples for a specific queue.
 *
 * The optional from/to fields define an inclusive time window. When omitted,
 * all available samples for the queue are returned.
 */
struct get_queue_stats_samples_request final {
    std::string queue_id;   // UUID as string
    std::optional<std::chrono::system_clock::time_point> from;
    std::optional<std::chrono::system_clock::time_point> to;

    std::vector<std::byte> serialize() const;
    static std::expected<get_queue_stats_samples_request,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s,
    const get_queue_stats_samples_request& v);

/**
 * @brief Response containing time-series statistics samples for a queue.
 */
struct get_queue_stats_samples_response final {
    bool success{false};
    std::string message;
    std::string queue_id;   // UUID as string
    std::vector<domain::queue_stats> samples;

    std::vector<std::byte> serialize() const;
    static std::expected<get_queue_stats_samples_response,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s,
    const get_queue_stats_samples_response& v);

// ============================================================================
// Shared wire type for messages read from a queue
// ============================================================================

/**
 * @brief A single queue message as returned over the wire.
 */
struct queue_message final {
    std::int64_t msg_id{0};
    std::string queue_id;       // UUID as string
    std::string message_type;
    std::string payload_type;   // "json" or "binary"
    std::string status;         // "pending", "processing", "done", "failed"
    std::string created_at;     // ISO UTC string
    std::string visible_after;  // ISO UTC string
    std::int32_t read_count{0};
    std::string payload;        // JSON body when payload_type == "json"
};

// ============================================================================
// create_queue messages
// ============================================================================

struct create_queue_request final {
    std::string queue_name;
    std::string scope_type;     // "party", "tenant", "system"
    std::string queue_type;     // "task", "channel"
    std::string description;

    std::vector<std::byte> serialize() const;
    static std::expected<create_queue_request,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const create_queue_request& v);

struct create_queue_response final {
    bool success{false};
    std::string message;
    std::string queue_id;   // UUID as string, assigned on success

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
    std::string queue_id;       // UUID as string
    std::string message_type;
    std::string payload;        // JSON body
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
// read_messages messages  (non-destructive peek with visibility lock)
// ============================================================================

struct read_messages_request final {
    std::string queue_id;       // UUID as string
    std::int32_t count{1};
    std::int32_t vt_seconds{30};

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
// pop_messages messages  (read + immediate ack)
// ============================================================================

struct pop_messages_request final {
    std::string queue_id;       // UUID as string
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
// ack_messages messages  (acknowledge / permanently delete)
// ============================================================================

struct ack_messages_request final {
    std::vector<std::int64_t> message_ids;

    std::vector<std::byte> serialize() const;
    static std::expected<ack_messages_request,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const ack_messages_request& v);

struct ack_messages_response final {
    bool success{false};
    std::string message;

    std::vector<std::byte> serialize() const;
    static std::expected<ack_messages_response,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const ack_messages_response& v);

// ============================================================================
// nack_message messages  (negative acknowledge)
// ============================================================================

struct nack_message_request final {
    std::int64_t message_id{0};
    std::string error;

    std::vector<std::byte> serialize() const;
    static std::expected<nack_message_request,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const nack_message_request& v);

struct nack_message_response final {
    bool success{false};
    std::string message;

    std::vector<std::byte> serialize() const;
    static std::expected<nack_message_response,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const nack_message_response& v);

// ============================================================================
// delete_messages messages (kept for backward compatibility)
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
struct message_traits<mq::messaging::get_queue_stats_request> {
    using request_type = mq::messaging::get_queue_stats_request;
    using response_type = mq::messaging::get_queue_stats_response;
    static constexpr message_type request_message_type =
        message_type::get_queue_stats_request;
};

template<>
struct message_traits<mq::messaging::get_queue_stats_samples_request> {
    using request_type = mq::messaging::get_queue_stats_samples_request;
    using response_type = mq::messaging::get_queue_stats_samples_response;
    static constexpr message_type request_message_type =
        message_type::get_queue_stats_samples_request;
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
struct message_traits<mq::messaging::ack_messages_request> {
    using request_type = mq::messaging::ack_messages_request;
    using response_type = mq::messaging::ack_messages_response;
    static constexpr message_type request_message_type =
        message_type::ack_messages_request;
};

template<>
struct message_traits<mq::messaging::nack_message_request> {
    using request_type = mq::messaging::nack_message_request;
    using response_type = mq::messaging::nack_message_response;
    static constexpr message_type request_message_type =
        message_type::nack_message_request;
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
