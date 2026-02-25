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
#ifndef ORES_TRADING_MESSAGING_TRADE_PROTOCOL_HPP
#define ORES_TRADING_MESSAGING_TRADE_PROTOCOL_HPP

#include <span>
#include <iosfwd>
#include <vector>
#include <optional>
#include <expected>
#include <boost/uuid/uuid.hpp>
#include "ores.comms/messaging/message_type.hpp"
#include "ores.comms/messaging/message_traits.hpp"
#include "ores.utility/serialization/error_code.hpp"
#include "ores.trading/domain/trade.hpp"

namespace ores::trading::messaging {

// ============================================================================
// Trade Messages
// ============================================================================

/**
 * @brief Request to retrieve all trades with optional filtering.
 *
 * Wire format:
 * - 4 bytes: offset (uint32)
 * - 4 bytes: limit  (uint32)
 * - 1 byte : has_book_id (bool)
 * - 16 bytes: book_id UUID   [only if has_book_id]
 * - 1 byte : has_portfolio_id (bool)
 * - 16 bytes: portfolio_id UUID [only if has_portfolio_id]
 */
struct get_trades_request final {
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    std::optional<boost::uuids::uuid> book_id;       ///< filter by single book
    std::optional<boost::uuids::uuid> portfolio_id;  ///< filter by portfolio subtree

    std::vector<std::byte> serialize() const;
    static std::expected<get_trades_request,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const get_trades_request& v);

/**
 * @brief Response containing all trades.
 */
struct get_trades_response final {
    std::vector<domain::trade> trades;
    std::uint32_t total_available_count = 0;

    std::vector<std::byte> serialize() const;
    static std::expected<get_trades_response,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const get_trades_response& v);

/**
 * @brief Request to save a trade (create or update).
 */
struct save_trade_request final {
    domain::trade trade;

    std::vector<std::byte> serialize() const;
    static std::expected<save_trade_request,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const save_trade_request& v);

/**
 * @brief Response confirming trade save operation.
 */
struct save_trade_response final {
    bool success;
    std::string message;

    std::vector<std::byte> serialize() const;
    static std::expected<save_trade_response,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const save_trade_response& v);

/**
 * @brief Result for a single trade deletion.
 */
struct delete_trade_result final {
    boost::uuids::uuid id;  ///< Primary key
    bool success;
    std::string message;
};

std::ostream& operator<<(std::ostream& s, const delete_trade_result& v);

/**
 * @brief Request to delete one or more trades.
 */
struct delete_trade_request final {
    std::vector<boost::uuids::uuid> ids;  ///< Primary keys

    std::vector<std::byte> serialize() const;
    static std::expected<delete_trade_request,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const delete_trade_request& v);

/**
 * @brief Response confirming trade deletion(s).
 */
struct delete_trade_response final {
    std::vector<delete_trade_result> results;

    std::vector<std::byte> serialize() const;
    static std::expected<delete_trade_response,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const delete_trade_response& v);

/**
 * @brief Request to retrieve version history for a trade.
 */
struct get_trade_history_request final {
    boost::uuids::uuid id;  ///< Primary key

    std::vector<std::byte> serialize() const;
    static std::expected<get_trade_history_request,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const get_trade_history_request& v);

/**
 * @brief Response containing trade version history.
 */
struct get_trade_history_response final {
    bool success;
    std::string message;
    std::vector<domain::trade> versions;

    std::vector<std::byte> serialize() const;
    static std::expected<get_trade_history_response,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const get_trade_history_response& v);

}

namespace ores::comms::messaging {

// Trade traits
template<>
struct message_traits<trading::messaging::get_trades_request> {
    using request_type = trading::messaging::get_trades_request;
    using response_type = trading::messaging::get_trades_response;
    static constexpr message_type request_message_type =
        message_type::get_trades_request;
};

template<>
struct message_traits<trading::messaging::save_trade_request> {
    using request_type = trading::messaging::save_trade_request;
    using response_type = trading::messaging::save_trade_response;
    static constexpr message_type request_message_type =
        message_type::save_trade_request;
};

template<>
struct message_traits<trading::messaging::delete_trade_request> {
    using request_type = trading::messaging::delete_trade_request;
    using response_type = trading::messaging::delete_trade_response;
    static constexpr message_type request_message_type =
        message_type::delete_trade_request;
};

template<>
struct message_traits<trading::messaging::get_trade_history_request> {
    using request_type = trading::messaging::get_trade_history_request;
    using response_type = trading::messaging::get_trade_history_response;
    static constexpr message_type request_message_type =
        message_type::get_trade_history_request;
};

}

#endif
