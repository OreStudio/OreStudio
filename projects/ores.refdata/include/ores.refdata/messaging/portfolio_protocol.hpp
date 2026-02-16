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
#ifndef ORES_REFDATA_MESSAGING_PORTFOLIO_PROTOCOL_HPP
#define ORES_REFDATA_MESSAGING_PORTFOLIO_PROTOCOL_HPP

#include <span>
#include <iosfwd>
#include <vector>
#include <expected>
#include <boost/uuid/uuid.hpp>
#include "ores.comms/messaging/message_type.hpp"
#include "ores.comms/messaging/message_traits.hpp"
#include "ores.utility/serialization/error_code.hpp"
#include "ores.refdata/domain/portfolio.hpp"

namespace ores::refdata::messaging {

// ============================================================================
// Portfolio Messages
// ============================================================================

/**
 * @brief Request to retrieve portfolios with pagination support.
 */
struct get_portfolios_request final {
    /// Number of records to skip (0-based)
    std::uint32_t offset = 0;
    /// Maximum number of records to return
    std::uint32_t limit = 100;

    std::vector<std::byte> serialize() const;
    static std::expected<get_portfolios_request,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const get_portfolios_request& v);

/**
 * @brief Response containing portfolios with pagination metadata.
 */
struct get_portfolios_response final {
    std::vector<domain::portfolio> portfolios;
    /// Total number of portfolios available (not just in this page)
    std::uint32_t total_available_count = 0;

    std::vector<std::byte> serialize() const;
    static std::expected<get_portfolios_response,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const get_portfolios_response& v);

/**
 * @brief Request to save a portfolio (create or update).
 */
struct save_portfolio_request final {
    domain::portfolio portfolio;

    std::vector<std::byte> serialize() const;
    static std::expected<save_portfolio_request,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const save_portfolio_request& v);

/**
 * @brief Response confirming portfolio save operation.
 */
struct save_portfolio_response final {
    bool success;
    std::string message;

    std::vector<std::byte> serialize() const;
    static std::expected<save_portfolio_response,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const save_portfolio_response& v);

/**
 * @brief Result for a single portfolio deletion.
 */
struct delete_portfolio_result final {
    boost::uuids::uuid id;  ///< Primary key
    bool success;
    std::string message;
};

std::ostream& operator<<(std::ostream& s, const delete_portfolio_result& v);

/**
 * @brief Request to delete one or more portfolios.
 */
struct delete_portfolio_request final {
    std::vector<boost::uuids::uuid> ids;  ///< Primary keys

    std::vector<std::byte> serialize() const;
    static std::expected<delete_portfolio_request,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const delete_portfolio_request& v);

/**
 * @brief Response confirming portfolio deletion(s).
 */
struct delete_portfolio_response final {
    std::vector<delete_portfolio_result> results;

    std::vector<std::byte> serialize() const;
    static std::expected<delete_portfolio_response,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const delete_portfolio_response& v);

/**
 * @brief Request to retrieve version history for a portfolio.
 */
struct get_portfolio_history_request final {
    boost::uuids::uuid id;  ///< Primary key

    std::vector<std::byte> serialize() const;
    static std::expected<get_portfolio_history_request,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const get_portfolio_history_request& v);

/**
 * @brief Response containing portfolio version history.
 */
struct get_portfolio_history_response final {
    bool success;
    std::string message;
    std::vector<domain::portfolio> versions;

    std::vector<std::byte> serialize() const;
    static std::expected<get_portfolio_history_response,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const get_portfolio_history_response& v);

}

namespace ores::comms::messaging {

// Portfolio traits
template<>
struct message_traits<refdata::messaging::get_portfolios_request> {
    using request_type = refdata::messaging::get_portfolios_request;
    using response_type = refdata::messaging::get_portfolios_response;
    static constexpr message_type request_message_type =
        message_type::get_portfolios_request;
};

template<>
struct message_traits<refdata::messaging::save_portfolio_request> {
    using request_type = refdata::messaging::save_portfolio_request;
    using response_type = refdata::messaging::save_portfolio_response;
    static constexpr message_type request_message_type =
        message_type::save_portfolio_request;
};

template<>
struct message_traits<refdata::messaging::delete_portfolio_request> {
    using request_type = refdata::messaging::delete_portfolio_request;
    using response_type = refdata::messaging::delete_portfolio_response;
    static constexpr message_type request_message_type =
        message_type::delete_portfolio_request;
};

template<>
struct message_traits<refdata::messaging::get_portfolio_history_request> {
    using request_type = refdata::messaging::get_portfolio_history_request;
    using response_type = refdata::messaging::get_portfolio_history_response;
    static constexpr message_type request_message_type =
        message_type::get_portfolio_history_request;
};

}

#endif
