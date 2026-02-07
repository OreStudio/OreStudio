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
#ifndef ORES_REFDATA_MESSAGING_COUNTERPARTY_PROTOCOL_HPP
#define ORES_REFDATA_MESSAGING_COUNTERPARTY_PROTOCOL_HPP

#include <span>
#include <iosfwd>
#include <vector>
#include <expected>
#include <boost/uuid/uuid.hpp>
#include "ores.comms/messaging/message_type.hpp"
#include "ores.comms/messaging/message_traits.hpp"
#include "ores.utility/serialization/error_code.hpp"
#include "ores.refdata/domain/counterparty.hpp"

namespace ores::refdata::messaging {

// ============================================================================
// Counterparty Messages
// ============================================================================

/**
 * @brief Request to retrieve all counterparties.
 */
struct get_counterparties_request final {
    std::vector<std::byte> serialize() const;
    static std::expected<get_counterparties_request,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const get_counterparties_request& v);

/**
 * @brief Response containing all counterparties.
 */
struct get_counterparties_response final {
    std::vector<domain::counterparty> counterparties;

    std::vector<std::byte> serialize() const;
    static std::expected<get_counterparties_response,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const get_counterparties_response& v);

/**
 * @brief Request to save a counterparty (create or update).
 */
struct save_counterparty_request final {
    domain::counterparty counterparty;

    std::vector<std::byte> serialize() const;
    static std::expected<save_counterparty_request,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const save_counterparty_request& v);

/**
 * @brief Response confirming counterparty save operation.
 */
struct save_counterparty_response final {
    bool success;
    std::string message;

    std::vector<std::byte> serialize() const;
    static std::expected<save_counterparty_response,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const save_counterparty_response& v);

/**
 * @brief Result for a single counterparty deletion.
 */
struct delete_counterparty_result final {
    boost::uuids::uuid id;  ///< Primary key
    bool success;
    std::string message;
};

std::ostream& operator<<(std::ostream& s, const delete_counterparty_result& v);

/**
 * @brief Request to delete one or more counterparties.
 */
struct delete_counterparty_request final {
    std::vector<boost::uuids::uuid> ids;  ///< Primary keys

    std::vector<std::byte> serialize() const;
    static std::expected<delete_counterparty_request,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const delete_counterparty_request& v);

/**
 * @brief Response confirming counterparty deletion(s).
 */
struct delete_counterparty_response final {
    std::vector<delete_counterparty_result> results;

    std::vector<std::byte> serialize() const;
    static std::expected<delete_counterparty_response,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const delete_counterparty_response& v);

/**
 * @brief Request to retrieve version history for a counterparty.
 */
struct get_counterparty_history_request final {
    boost::uuids::uuid id;  ///< Primary key

    std::vector<std::byte> serialize() const;
    static std::expected<get_counterparty_history_request,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const get_counterparty_history_request& v);

/**
 * @brief Response containing counterparty version history.
 */
struct get_counterparty_history_response final {
    bool success;
    std::string message;
    std::vector<domain::counterparty> versions;

    std::vector<std::byte> serialize() const;
    static std::expected<get_counterparty_history_response,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const get_counterparty_history_response& v);

}

namespace ores::comms::messaging {

// Counterparty traits
template<>
struct message_traits<refdata::messaging::get_counterparties_request> {
    using request_type = refdata::messaging::get_counterparties_request;
    using response_type = refdata::messaging::get_counterparties_response;
    static constexpr message_type request_message_type =
        message_type::get_counterparties_request;
};

template<>
struct message_traits<refdata::messaging::save_counterparty_request> {
    using request_type = refdata::messaging::save_counterparty_request;
    using response_type = refdata::messaging::save_counterparty_response;
    static constexpr message_type request_message_type =
        message_type::save_counterparty_request;
};

template<>
struct message_traits<refdata::messaging::delete_counterparty_request> {
    using request_type = refdata::messaging::delete_counterparty_request;
    using response_type = refdata::messaging::delete_counterparty_response;
    static constexpr message_type request_message_type =
        message_type::delete_counterparty_request;
};

template<>
struct message_traits<refdata::messaging::get_counterparty_history_request> {
    using request_type = refdata::messaging::get_counterparty_history_request;
    using response_type = refdata::messaging::get_counterparty_history_response;
    static constexpr message_type request_message_type =
        message_type::get_counterparty_history_request;
};

}

#endif
