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
#ifndef ORES_TRADING_MESSAGING_LIFECYCLE_EVENT_PROTOCOL_HPP
#define ORES_TRADING_MESSAGING_LIFECYCLE_EVENT_PROTOCOL_HPP

#include <span>
#include <iosfwd>
#include <vector>
#include <expected>
#include "ores.comms/messaging/message_type.hpp"
#include "ores.comms/messaging/save_result.hpp"
#include "ores.comms/messaging/message_traits.hpp"
#include "ores.utility/serialization/error_code.hpp"
#include "ores.trading/domain/lifecycle_event.hpp"

namespace ores::trading::messaging {

// ============================================================================
// Lifecycle Event Messages
// ============================================================================

/**
 * @brief Request to retrieve all lifecycle events.
 */
struct get_lifecycle_events_request final {
    std::vector<std::byte> serialize() const;
    static std::expected<get_lifecycle_events_request,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const get_lifecycle_events_request& v);

/**
 * @brief Response containing all lifecycle events.
 */
struct get_lifecycle_events_response final {
    std::vector<domain::lifecycle_event> events;

    std::vector<std::byte> serialize() const;
    static std::expected<get_lifecycle_events_response,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const get_lifecycle_events_response& v);

/**
 * @brief Request to save one or more lifecycle events (create or update).
 */
struct save_lifecycle_event_request final {
    std::vector<domain::lifecycle_event> events;

    static save_lifecycle_event_request from(domain::lifecycle_event event);
    static save_lifecycle_event_request from(std::vector<domain::lifecycle_event> events);

    std::vector<std::byte> serialize() const;
    static std::expected<save_lifecycle_event_request,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const save_lifecycle_event_request& v);

/**
 * @brief Response confirming lifecycle event save operation(s).
 */
struct save_lifecycle_event_response final {
    bool success = false;
    std::string message;

    std::vector<std::byte> serialize() const;
    static std::expected<save_lifecycle_event_response,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const save_lifecycle_event_response& v);

/**
 * @brief Request to delete one or more lifecycle events.
 */
struct delete_lifecycle_event_request final {
    std::vector<std::string> codes;  ///< Primary keys

    std::vector<std::byte> serialize() const;
    static std::expected<delete_lifecycle_event_request,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const delete_lifecycle_event_request& v);

/**
 * @brief Response confirming lifecycle event deletion(s).
 */
struct delete_lifecycle_event_response final {
    bool success = false;
    std::string message;

    std::vector<std::byte> serialize() const;
    static std::expected<delete_lifecycle_event_response,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const delete_lifecycle_event_response& v);

/**
 * @brief Request to retrieve version history for a lifecycle event.
 */
struct get_lifecycle_event_history_request final {
    std::string code;  ///< Primary key

    std::vector<std::byte> serialize() const;
    static std::expected<get_lifecycle_event_history_request,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const get_lifecycle_event_history_request& v);

/**
 * @brief Response containing lifecycle event version history.
 */
struct get_lifecycle_event_history_response final {
    bool success;
    std::string message;
    std::vector<domain::lifecycle_event> versions;

    std::vector<std::byte> serialize() const;
    static std::expected<get_lifecycle_event_history_response,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const get_lifecycle_event_history_response& v);

}

namespace ores::comms::messaging {

// Lifecycle Event traits
template<>
struct message_traits<trading::messaging::get_lifecycle_events_request> {
    using request_type = trading::messaging::get_lifecycle_events_request;
    using response_type = trading::messaging::get_lifecycle_events_response;
    static constexpr message_type request_message_type =
        message_type::get_lifecycle_events_request;
};

template<>
struct message_traits<trading::messaging::save_lifecycle_event_request> {
    using request_type = trading::messaging::save_lifecycle_event_request;
    using response_type = trading::messaging::save_lifecycle_event_response;
    static constexpr message_type request_message_type =
        message_type::save_lifecycle_event_request;
};

template<>
struct message_traits<trading::messaging::delete_lifecycle_event_request> {
    using request_type = trading::messaging::delete_lifecycle_event_request;
    using response_type = trading::messaging::delete_lifecycle_event_response;
    static constexpr message_type request_message_type =
        message_type::delete_lifecycle_event_request;
};

template<>
struct message_traits<trading::messaging::get_lifecycle_event_history_request> {
    using request_type = trading::messaging::get_lifecycle_event_history_request;
    using response_type = trading::messaging::get_lifecycle_event_history_response;
    static constexpr message_type request_message_type =
        message_type::get_lifecycle_event_history_request;
};

}

#endif
