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
#ifndef ORES_TRADING_MESSAGING_TRADE_ID_TYPE_PROTOCOL_HPP
#define ORES_TRADING_MESSAGING_TRADE_ID_TYPE_PROTOCOL_HPP

#include <span>
#include <iosfwd>
#include <vector>
#include <expected>
#include "ores.comms/messaging/message_type.hpp"
#include "ores.comms/messaging/message_traits.hpp"
#include "ores.utility/serialization/error_code.hpp"
#include "ores.trading/domain/trade_id_type.hpp"

namespace ores::trading::messaging {

// ============================================================================
// Trade ID Type Messages
// ============================================================================

/**
 * @brief Request to retrieve all trade ID types.
 */
struct get_trade_id_types_request final {
    std::vector<std::byte> serialize() const;
    static std::expected<get_trade_id_types_request,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const get_trade_id_types_request& v);

/**
 * @brief Response containing all trade ID types.
 */
struct get_trade_id_types_response final {
    std::vector<domain::trade_id_type> id_types;

    std::vector<std::byte> serialize() const;
    static std::expected<get_trade_id_types_response,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const get_trade_id_types_response& v);

/**
 * @brief Request to save one or more trade ID types (create or update).
 */
struct save_trade_id_type_request final {
    std::vector<domain::trade_id_type> id_types;

    static save_trade_id_type_request from(domain::trade_id_type id_type);
    static save_trade_id_type_request from(std::vector<domain::trade_id_type> id_types);

    std::vector<std::byte> serialize() const;
    static std::expected<save_trade_id_type_request,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const save_trade_id_type_request& v);

/**
 * @brief Response confirming trade ID type save operation(s).
 */
struct save_trade_id_type_response final {
    bool success = false;
    std::string message;

    std::vector<std::byte> serialize() const;
    static std::expected<save_trade_id_type_response,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const save_trade_id_type_response& v);

/**
 * @brief Request to delete one or more trade ID types.
 */
struct delete_trade_id_type_request final {
    std::vector<std::string> codes;  ///< Primary keys

    std::vector<std::byte> serialize() const;
    static std::expected<delete_trade_id_type_request,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const delete_trade_id_type_request& v);

/**
 * @brief Response confirming trade ID type deletion(s).
 */
struct delete_trade_id_type_response final {
    bool success = false;
    std::string message;

    std::vector<std::byte> serialize() const;
    static std::expected<delete_trade_id_type_response,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const delete_trade_id_type_response& v);

/**
 * @brief Request to retrieve version history for a trade ID type.
 */
struct get_trade_id_type_history_request final {
    std::string code;  ///< Primary key

    std::vector<std::byte> serialize() const;
    static std::expected<get_trade_id_type_history_request,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const get_trade_id_type_history_request& v);

/**
 * @brief Response containing trade ID type version history.
 */
struct get_trade_id_type_history_response final {
    bool success;
    std::string message;
    std::vector<domain::trade_id_type> versions;

    std::vector<std::byte> serialize() const;
    static std::expected<get_trade_id_type_history_response,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const get_trade_id_type_history_response& v);

}

namespace ores::comms::messaging {

// Trade ID Type traits
template<>
struct message_traits<trading::messaging::get_trade_id_types_request> {
    using request_type = trading::messaging::get_trade_id_types_request;
    using response_type = trading::messaging::get_trade_id_types_response;
    static constexpr message_type request_message_type =
        message_type::get_trade_id_types_request;
};

template<>
struct message_traits<trading::messaging::save_trade_id_type_request> {
    using request_type = trading::messaging::save_trade_id_type_request;
    using response_type = trading::messaging::save_trade_id_type_response;
    static constexpr message_type request_message_type =
        message_type::save_trade_id_type_request;
};

template<>
struct message_traits<trading::messaging::delete_trade_id_type_request> {
    using request_type = trading::messaging::delete_trade_id_type_request;
    using response_type = trading::messaging::delete_trade_id_type_response;
    static constexpr message_type request_message_type =
        message_type::delete_trade_id_type_request;
};

template<>
struct message_traits<trading::messaging::get_trade_id_type_history_request> {
    using request_type = trading::messaging::get_trade_id_type_history_request;
    using response_type = trading::messaging::get_trade_id_type_history_response;
    static constexpr message_type request_message_type =
        message_type::get_trade_id_type_history_request;
};

}

#endif
