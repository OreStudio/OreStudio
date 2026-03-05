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
#ifndef ORES_MQ_MESSAGING_BROKER_PROTOCOL_HPP
#define ORES_MQ_MESSAGING_BROKER_PROTOCOL_HPP

#include <span>
#include <iosfwd>
#include <string>
#include <vector>
#include <cstdint>
#include <expected>
#include "ores.comms/messaging/message_type.hpp"
#include "ores.comms/messaging/message_traits.hpp"
#include "ores.utility/serialization/error_code.hpp"

namespace ores::mq::messaging {

// ============================================================================
// register_service messages (0xC000 / 0xC001)
// ============================================================================

/**
 * @brief Sent by a domain service to the broker to register itself.
 *
 * The service announces which message-type ranges it handles so the broker
 * can build its routing table. Sent once on startup; re-sent after reconnect.
 */
struct register_service_request final {
    /// Human-readable service name (e.g. "ores.comms.service")
    std::string service_name;
    /// Message-type ranges handled by this service [min, max] pairs
    std::vector<std::pair<std::uint16_t, std::uint16_t>> handled_ranges;

    std::vector<std::byte> serialize() const;
    static std::expected<register_service_request,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const register_service_request& v);

/**
 * @brief Broker's reply to a register_service_request.
 */
struct register_service_response final {
    bool success{false};
    /// UUID assigned to this service registration (used for deregistration)
    std::string assigned_id;
    std::string error_message;

    std::vector<std::byte> serialize() const;
    static std::expected<register_service_response,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const register_service_response& v);

// ============================================================================
// token_refresh messages (0xC002 / 0xC003)
// ============================================================================

/**
 * @brief Sent by a service to the broker to request a new JWT for an expired token.
 *
 * The broker forwards the request to the IAM service, which mints a fresh
 * token and returns it. The requesting service can then use the new token.
 */
struct token_refresh_request final {
    /// The expired JWT to be replaced
    std::string expired_jwt;

    std::vector<std::byte> serialize() const;
    static std::expected<token_refresh_request,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const token_refresh_request& v);

/**
 * @brief Response to a token_refresh_request.
 */
struct token_refresh_response final {
    bool success{false};
    std::string new_jwt;
    std::string error_message;

    std::vector<std::byte> serialize() const;
    static std::expected<token_refresh_response,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const token_refresh_response& v);

}

// ============================================================================
// message_traits specialisations
// ============================================================================

namespace ores::comms::messaging {

template<>
struct message_traits<mq::messaging::register_service_request> {
    using request_type = mq::messaging::register_service_request;
    using response_type = mq::messaging::register_service_response;
    static constexpr message_type request_message_type =
        message_type::register_service_request;
};

template<>
struct message_traits<mq::messaging::token_refresh_request> {
    using request_type = mq::messaging::token_refresh_request;
    using response_type = mq::messaging::token_refresh_response;
    static constexpr message_type request_message_type =
        message_type::token_refresh_request;
};

}

#endif
