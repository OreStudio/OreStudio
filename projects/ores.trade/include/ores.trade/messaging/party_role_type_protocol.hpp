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
#ifndef ORES_TRADE_MESSAGING_PARTY_ROLE_TYPE_PROTOCOL_HPP
#define ORES_TRADE_MESSAGING_PARTY_ROLE_TYPE_PROTOCOL_HPP

#include <span>
#include <iosfwd>
#include <vector>
#include <expected>
#include "ores.comms/messaging/message_type.hpp"
#include "ores.comms/messaging/message_traits.hpp"
#include "ores.utility/serialization/error_code.hpp"
#include "ores.trade/domain/party_role_type.hpp"

namespace ores::trade::messaging {

// ============================================================================
// Party Role Type Messages
// ============================================================================

/**
 * @brief Request to retrieve all party role types.
 */
struct get_party_role_types_request final {
    std::vector<std::byte> serialize() const;
    static std::expected<get_party_role_types_request,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const get_party_role_types_request& v);

/**
 * @brief Response containing all party role types.
 */
struct get_party_role_types_response final {
    std::vector<domain::party_role_type> role_types;

    std::vector<std::byte> serialize() const;
    static std::expected<get_party_role_types_response,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const get_party_role_types_response& v);

/**
 * @brief Request to save a party role type (create or update).
 */
struct save_party_role_type_request final {
    domain::party_role_type role_type;

    std::vector<std::byte> serialize() const;
    static std::expected<save_party_role_type_request,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const save_party_role_type_request& v);

/**
 * @brief Response confirming party role type save operation.
 */
struct save_party_role_type_response final {
    bool success;
    std::string message;

    std::vector<std::byte> serialize() const;
    static std::expected<save_party_role_type_response,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const save_party_role_type_response& v);

/**
 * @brief Result for a single party role type deletion.
 */
struct delete_party_role_type_result final {
    std::string code;  ///< Primary key
    bool success;
    std::string message;
};

std::ostream& operator<<(std::ostream& s, const delete_party_role_type_result& v);

/**
 * @brief Request to delete one or more party role types.
 */
struct delete_party_role_type_request final {
    std::vector<std::string> codes;  ///< Primary keys

    std::vector<std::byte> serialize() const;
    static std::expected<delete_party_role_type_request,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const delete_party_role_type_request& v);

/**
 * @brief Response confirming party role type deletion(s).
 */
struct delete_party_role_type_response final {
    std::vector<delete_party_role_type_result> results;

    std::vector<std::byte> serialize() const;
    static std::expected<delete_party_role_type_response,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const delete_party_role_type_response& v);

/**
 * @brief Request to retrieve version history for a party role type.
 */
struct get_party_role_type_history_request final {
    std::string code;  ///< Primary key

    std::vector<std::byte> serialize() const;
    static std::expected<get_party_role_type_history_request,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const get_party_role_type_history_request& v);

/**
 * @brief Response containing party role type version history.
 */
struct get_party_role_type_history_response final {
    bool success;
    std::string message;
    std::vector<domain::party_role_type> versions;

    std::vector<std::byte> serialize() const;
    static std::expected<get_party_role_type_history_response,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const get_party_role_type_history_response& v);

}

namespace ores::comms::messaging {

// Party Role Type traits
template<>
struct message_traits<trade::messaging::get_party_role_types_request> {
    using request_type = trade::messaging::get_party_role_types_request;
    using response_type = trade::messaging::get_party_role_types_response;
    static constexpr message_type request_message_type =
        message_type::get_party_role_types_request;
};

template<>
struct message_traits<trade::messaging::save_party_role_type_request> {
    using request_type = trade::messaging::save_party_role_type_request;
    using response_type = trade::messaging::save_party_role_type_response;
    static constexpr message_type request_message_type =
        message_type::save_party_role_type_request;
};

template<>
struct message_traits<trade::messaging::delete_party_role_type_request> {
    using request_type = trade::messaging::delete_party_role_type_request;
    using response_type = trade::messaging::delete_party_role_type_response;
    static constexpr message_type request_message_type =
        message_type::delete_party_role_type_request;
};

template<>
struct message_traits<trade::messaging::get_party_role_type_history_request> {
    using request_type = trade::messaging::get_party_role_type_history_request;
    using response_type = trade::messaging::get_party_role_type_history_response;
    static constexpr message_type request_message_type =
        message_type::get_party_role_type_history_request;
};

}

#endif
