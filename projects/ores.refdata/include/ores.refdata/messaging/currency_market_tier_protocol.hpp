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
#ifndef ORES_REFDATA_MESSAGING_CURRENCY_MARKET_TIER_PROTOCOL_HPP
#define ORES_REFDATA_MESSAGING_CURRENCY_MARKET_TIER_PROTOCOL_HPP

#include <span>
#include <iosfwd>
#include <vector>
#include <expected>
#include "ores.comms/messaging/message_type.hpp"
#include "ores.comms/messaging/message_traits.hpp"
#include "ores.utility/serialization/error_code.hpp"
#include "ores.refdata/domain/currency_market_tier.hpp"

namespace ores::refdata::messaging {

// ============================================================================
// Currency Market Tier Messages
// ============================================================================

/**
 * @brief Request to retrieve all currency market tiers.
 */
struct get_currency_market_tiers_request final {
    std::vector<std::byte> serialize() const;
    static std::expected<get_currency_market_tiers_request,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const get_currency_market_tiers_request& v);

/**
 * @brief Response containing all currency market tiers.
 */
struct get_currency_market_tiers_response final {
    std::vector<domain::currency_market_tier> types;

    std::vector<std::byte> serialize() const;
    static std::expected<get_currency_market_tiers_response,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const get_currency_market_tiers_response& v);

/**
 * @brief Request to save one or more currency market tiers (create or update).
 */
struct save_currency_market_tier_request final {
    std::vector<domain::currency_market_tier> types;

    static save_currency_market_tier_request from(domain::currency_market_tier type);
    static save_currency_market_tier_request from(std::vector<domain::currency_market_tier> types);

    std::vector<std::byte> serialize() const;
    static std::expected<save_currency_market_tier_request,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const save_currency_market_tier_request& v);

/**
 * @brief Response confirming currency market tier save operation(s).
 */
struct save_currency_market_tier_response final {
    bool success = false;
    std::string message;

    std::vector<std::byte> serialize() const;
    static std::expected<save_currency_market_tier_response,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const save_currency_market_tier_response& v);

/**
 * @brief Request to delete one or more currency market tiers.
 */
struct delete_currency_market_tier_request final {
    std::vector<std::string> codes;  ///< Primary keys

    std::vector<std::byte> serialize() const;
    static std::expected<delete_currency_market_tier_request,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const delete_currency_market_tier_request& v);

/**
 * @brief Response confirming currency market tier deletion(s).
 */
struct delete_currency_market_tier_response final {
    bool success = false;
    std::string message;

    std::vector<std::byte> serialize() const;
    static std::expected<delete_currency_market_tier_response,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const delete_currency_market_tier_response& v);

}

namespace ores::comms::messaging {

// Currency Market Tier traits
template<>
struct message_traits<refdata::messaging::get_currency_market_tiers_request> {
    using request_type = refdata::messaging::get_currency_market_tiers_request;
    using response_type = refdata::messaging::get_currency_market_tiers_response;
    static constexpr message_type request_message_type =
        message_type::get_currency_market_tiers_request;
};

template<>
struct message_traits<refdata::messaging::save_currency_market_tier_request> {
    using request_type = refdata::messaging::save_currency_market_tier_request;
    using response_type = refdata::messaging::save_currency_market_tier_response;
    static constexpr message_type request_message_type =
        message_type::save_currency_market_tier_request;
};

template<>
struct message_traits<refdata::messaging::delete_currency_market_tier_request> {
    using request_type = refdata::messaging::delete_currency_market_tier_request;
    using response_type = refdata::messaging::delete_currency_market_tier_response;
    static constexpr message_type request_message_type =
        message_type::delete_currency_market_tier_request;
};

}

#endif
