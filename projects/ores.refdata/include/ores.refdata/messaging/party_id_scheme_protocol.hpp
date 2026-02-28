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
#ifndef ORES_REFDATA_MESSAGING_PARTY_ID_SCHEME_PROTOCOL_HPP
#define ORES_REFDATA_MESSAGING_PARTY_ID_SCHEME_PROTOCOL_HPP

#include <span>
#include <iosfwd>
#include <vector>
#include <expected>
#include "ores.comms/messaging/message_type.hpp"
#include "ores.comms/messaging/save_result.hpp"
#include "ores.comms/messaging/message_traits.hpp"
#include "ores.utility/serialization/error_code.hpp"
#include "ores.refdata/domain/party_id_scheme.hpp"

namespace ores::refdata::messaging {

// ============================================================================
// Party ID Scheme Messages
// ============================================================================

/**
 * @brief Request to retrieve all party ID schemes.
 */
struct get_party_id_schemes_request final {
    std::vector<std::byte> serialize() const;
    static std::expected<get_party_id_schemes_request,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const get_party_id_schemes_request& v);

/**
 * @brief Response containing all party ID schemes.
 */
struct get_party_id_schemes_response final {
    std::vector<domain::party_id_scheme> schemes;

    std::vector<std::byte> serialize() const;
    static std::expected<get_party_id_schemes_response,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const get_party_id_schemes_response& v);

/**
 * @brief Request to save one or more party ID schemes (create or update).
 */
struct save_party_id_scheme_request final {
    std::vector<domain::party_id_scheme> schemes;

    static save_party_id_scheme_request from(domain::party_id_scheme scheme);
    static save_party_id_scheme_request from(std::vector<domain::party_id_scheme> schemes);

    std::vector<std::byte> serialize() const;
    static std::expected<save_party_id_scheme_request,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const save_party_id_scheme_request& v);

/**
 * @brief Response confirming party ID scheme save operation(s).
 */
struct save_party_id_scheme_response final {
    bool success = false;
    std::string message;

    std::vector<std::byte> serialize() const;
    static std::expected<save_party_id_scheme_response,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const save_party_id_scheme_response& v);

/**
 * @brief Request to delete one or more party ID schemes.
 */
struct delete_party_id_scheme_request final {
    std::vector<std::string> codes;  ///< Primary keys

    std::vector<std::byte> serialize() const;
    static std::expected<delete_party_id_scheme_request,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const delete_party_id_scheme_request& v);

/**
 * @brief Response confirming party ID scheme deletion(s).
 */
struct delete_party_id_scheme_response final {
    bool success = false;
    std::string message;

    std::vector<std::byte> serialize() const;
    static std::expected<delete_party_id_scheme_response,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const delete_party_id_scheme_response& v);

/**
 * @brief Request to retrieve version history for a party ID scheme.
 */
struct get_party_id_scheme_history_request final {
    std::string code;  ///< Primary key

    std::vector<std::byte> serialize() const;
    static std::expected<get_party_id_scheme_history_request,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const get_party_id_scheme_history_request& v);

/**
 * @brief Response containing party ID scheme version history.
 */
struct get_party_id_scheme_history_response final {
    bool success;
    std::string message;
    std::vector<domain::party_id_scheme> versions;

    std::vector<std::byte> serialize() const;
    static std::expected<get_party_id_scheme_history_response,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const get_party_id_scheme_history_response& v);

}

namespace ores::comms::messaging {

// Party ID Scheme traits
template<>
struct message_traits<refdata::messaging::get_party_id_schemes_request> {
    using request_type = refdata::messaging::get_party_id_schemes_request;
    using response_type = refdata::messaging::get_party_id_schemes_response;
    static constexpr message_type request_message_type =
        message_type::get_party_id_schemes_request;
};

template<>
struct message_traits<refdata::messaging::save_party_id_scheme_request> {
    using request_type = refdata::messaging::save_party_id_scheme_request;
    using response_type = refdata::messaging::save_party_id_scheme_response;
    static constexpr message_type request_message_type =
        message_type::save_party_id_scheme_request;
};

template<>
struct message_traits<refdata::messaging::delete_party_id_scheme_request> {
    using request_type = refdata::messaging::delete_party_id_scheme_request;
    using response_type = refdata::messaging::delete_party_id_scheme_response;
    static constexpr message_type request_message_type =
        message_type::delete_party_id_scheme_request;
};

template<>
struct message_traits<refdata::messaging::get_party_id_scheme_history_request> {
    using request_type = refdata::messaging::get_party_id_scheme_history_request;
    using response_type = refdata::messaging::get_party_id_scheme_history_response;
    static constexpr message_type request_message_type =
        message_type::get_party_id_scheme_history_request;
};

}

#endif
