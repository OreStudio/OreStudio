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
#ifndef ORES_COMMS_MESSAGING_SUBSCRIPTION_PROTOCOL_HPP
#define ORES_COMMS_MESSAGING_SUBSCRIPTION_PROTOCOL_HPP

#include <span>
#include <chrono>
#include <iosfwd>
#include <vector>
#include <cstdint>
#include <expected>
#include <string>
#include "ores.comms/messaging/message_types.hpp"
#include "ores.comms/messaging/message_traits.hpp"

namespace ores::comms::messaging {

/**
 * @brief Request to subscribe to entity change notifications.
 *
 * The client sends this to register interest in receiving notifications
 * when a particular entity type changes.
 */
struct subscribe_request final {
    /**
     * @brief The event type to subscribe to.
     *
     * Uses the logical event name from event_traits (e.g., "ores.risk.currency_changed").
     */
    std::string event_type;

    /**
     * @brief Serialize request to bytes.
     *
     * Format:
     * - 2 bytes: event_type length
     * - N bytes: event_type (UTF-8)
     */
    std::vector<std::byte> serialize() const;

    /**
     * @brief Deserialize request from bytes.
     */
    static std::expected<subscribe_request, ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const subscribe_request& v);

/**
 * @brief Response confirming subscription request.
 */
struct subscribe_response final {
    bool success;
    std::string message;

    /**
     * @brief Serialize response to bytes.
     *
     * Format:
     * - 1 byte: success (0 or 1)
     * - 2 bytes: message length
     * - N bytes: message (UTF-8)
     */
    std::vector<std::byte> serialize() const;

    /**
     * @brief Deserialize response from bytes.
     */
    static std::expected<subscribe_response, ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const subscribe_response& v);

/**
 * @brief Request to unsubscribe from entity change notifications.
 */
struct unsubscribe_request final {
    /**
     * @brief The event type to unsubscribe from.
     */
    std::string event_type;

    /**
     * @brief Serialize request to bytes.
     *
     * Format:
     * - 2 bytes: event_type length
     * - N bytes: event_type (UTF-8)
     */
    std::vector<std::byte> serialize() const;

    /**
     * @brief Deserialize request from bytes.
     */
    static std::expected<unsubscribe_request, ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const unsubscribe_request& v);

/**
 * @brief Response confirming unsubscription request.
 */
struct unsubscribe_response final {
    bool success;
    std::string message;

    /**
     * @brief Serialize response to bytes.
     *
     * Format:
     * - 1 byte: success (0 or 1)
     * - 2 bytes: message length
     * - N bytes: message (UTF-8)
     */
    std::vector<std::byte> serialize() const;

    /**
     * @brief Deserialize response from bytes.
     */
    static std::expected<unsubscribe_response, ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const unsubscribe_response& v);

/**
 * @brief Server-initiated notification of an entity change.
 *
 * This is a push message from the server to subscribed clients.
 * It does not expect a response.
 */
struct notification_message final {
    /**
     * @brief The event type that occurred.
     */
    std::string event_type;

    /**
     * @brief Timestamp of when the change occurred (UTC).
     *
     * Clients can use this to query for changes since this timestamp.
     */
    std::chrono::system_clock::time_point timestamp;

    /**
     * @brief Identifiers of entities that changed.
     *
     * For currencies, this contains ISO codes (e.g., "USD", "EUR").
     * For accounts, this contains account IDs (UUIDs as strings).
     * May be empty for bulk operations or when specific IDs are not available.
     */
    std::vector<std::string> entity_ids;

    /**
     * @brief Serialize notification to bytes.
     *
     * Format:
     * - 2 bytes: event_type length
     * - N bytes: event_type (UTF-8)
     * - 8 bytes: timestamp (milliseconds since epoch, int64_t)
     * - 4 bytes: entity_ids count
     * - For each entity_id:
     *   - 2 bytes: length
     *   - N bytes: entity_id (UTF-8)
     */
    std::vector<std::byte> serialize() const;

    /**
     * @brief Deserialize notification from bytes.
     */
    static std::expected<notification_message, ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const notification_message& v);

/**
 * @brief Server-initiated notification of database status.
 *
 * This is a push message from the server to connected clients.
 * It informs clients about database connectivity status.
 */
struct database_status_message final {
    /**
     * @brief Whether the database is available.
     */
    bool available;

    /**
     * @brief Error message if unavailable, empty otherwise.
     */
    std::string error_message;

    /**
     * @brief Timestamp of when the status was checked (UTC).
     */
    std::chrono::system_clock::time_point timestamp;

    /**
     * @brief Serialize notification to bytes.
     *
     * Format:
     * - 1 byte: available (0 or 1)
     * - 2 bytes: error_message length
     * - N bytes: error_message (UTF-8)
     * - 8 bytes: timestamp (milliseconds since epoch, int64_t)
     */
    std::vector<std::byte> serialize() const;

    /**
     * @brief Deserialize notification from bytes.
     */
    static std::expected<database_status_message, ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const database_status_message& v);

/**
 * @brief Message traits specialization for subscribe_request.
 */
template<>
struct message_traits<subscribe_request> {
    using request_type = subscribe_request;
    using response_type = subscribe_response;
    static constexpr message_type request_message_type =
        message_type::subscribe_request;
};

/**
 * @brief Message traits specialization for unsubscribe_request.
 */
template<>
struct message_traits<unsubscribe_request> {
    using request_type = unsubscribe_request;
    using response_type = unsubscribe_response;
    static constexpr message_type request_message_type =
        message_type::unsubscribe_request;
};

}

#endif
