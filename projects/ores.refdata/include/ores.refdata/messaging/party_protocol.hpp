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
#ifndef ORES_REFDATA_MESSAGING_PARTY_PROTOCOL_HPP
#define ORES_REFDATA_MESSAGING_PARTY_PROTOCOL_HPP

#include <span>
#include <iosfwd>
#include <vector>
#include <cstdint>
#include <expected>
#include <boost/uuid/uuid.hpp>
#include "ores.comms/messaging/message_type.hpp"
#include "ores.comms/messaging/message_traits.hpp"
#include "ores.utility/serialization/error_code.hpp"
#include "ores.refdata/domain/party.hpp"

namespace ores::refdata::messaging {

// ============================================================================
// Party Messages
// ============================================================================

/**
 * @brief Request to retrieve parties with pagination support.
 */
struct get_parties_request final {
    /// Number of records to skip (0-based)
    std::uint32_t offset = 0;
    /// Maximum number of records to return
    std::uint32_t limit = 100;

    std::vector<std::byte> serialize() const;
    static std::expected<get_parties_request,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const get_parties_request& v);

/**
 * @brief Response containing parties with pagination metadata.
 */
struct get_parties_response final {
    std::vector<domain::party> parties;
    /// Total number of parties available (not just in this page)
    std::uint32_t total_available_count = 0;

    std::vector<std::byte> serialize() const;
    static std::expected<get_parties_response,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const get_parties_response& v);

/**
 * @brief Request to save one or more parties (create or update).
 */
struct save_party_request final {
    std::vector<domain::party> parties;

    static save_party_request from(domain::party party);
    static save_party_request from(std::vector<domain::party> parties);

    std::vector<std::byte> serialize() const;
    static std::expected<save_party_request,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const save_party_request& v);

/**
 * @brief Response confirming party save operation(s).
 */
struct save_party_response final {
    bool success = false;
    std::string message;

    std::vector<std::byte> serialize() const;
    static std::expected<save_party_response,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const save_party_response& v);

/**
 * @brief Request to delete one or more parties.
 */
struct delete_party_request final {
    std::vector<boost::uuids::uuid> ids;  ///< Primary keys

    std::vector<std::byte> serialize() const;
    static std::expected<delete_party_request,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const delete_party_request& v);

/**
 * @brief Response confirming party deletion(s).
 */
struct delete_party_response final {
    bool success = false;
    std::string message;

    std::vector<std::byte> serialize() const;
    static std::expected<delete_party_response,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const delete_party_response& v);

/**
 * @brief Request to retrieve version history for a party.
 */
struct get_party_history_request final {
    boost::uuids::uuid id;  ///< Primary key

    std::vector<std::byte> serialize() const;
    static std::expected<get_party_history_request,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const get_party_history_request& v);

/**
 * @brief Response containing party version history.
 */
struct get_party_history_response final {
    bool success;
    std::string message;
    std::vector<domain::party> versions;

    std::vector<std::byte> serialize() const;
    static std::expected<get_party_history_response,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const get_party_history_response& v);

}

namespace ores::comms::messaging {

// Party traits
template<>
struct message_traits<refdata::messaging::get_parties_request> {
    using request_type = refdata::messaging::get_parties_request;
    using response_type = refdata::messaging::get_parties_response;
    static constexpr message_type request_message_type =
        message_type::get_parties_request;
};

template<>
struct message_traits<refdata::messaging::save_party_request> {
    using request_type = refdata::messaging::save_party_request;
    using response_type = refdata::messaging::save_party_response;
    static constexpr message_type request_message_type =
        message_type::save_party_request;
};

template<>
struct message_traits<refdata::messaging::delete_party_request> {
    using request_type = refdata::messaging::delete_party_request;
    using response_type = refdata::messaging::delete_party_response;
    static constexpr message_type request_message_type =
        message_type::delete_party_request;
};

template<>
struct message_traits<refdata::messaging::get_party_history_request> {
    using request_type = refdata::messaging::get_party_history_request;
    using response_type = refdata::messaging::get_party_history_response;
    static constexpr message_type request_message_type =
        message_type::get_party_history_request;
};

}

#endif
