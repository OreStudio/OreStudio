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
#ifndef ORES_REFDATA_MESSAGING_PARTY_CONTACT_INFORMATION_PROTOCOL_HPP
#define ORES_REFDATA_MESSAGING_PARTY_CONTACT_INFORMATION_PROTOCOL_HPP

#include <span>
#include <iosfwd>
#include <vector>
#include <expected>
#include <boost/uuid/uuid.hpp>
#include "ores.comms/messaging/message_type.hpp"
#include "ores.comms/messaging/message_traits.hpp"
#include "ores.utility/serialization/error_code.hpp"
#include "ores.refdata/domain/party_contact_information.hpp"

namespace ores::refdata::messaging {

// ============================================================================
// Party Contact Information Messages
// ============================================================================

/**
 * @brief Request to retrieve all party contact informations.
 */
struct get_party_contact_informations_request final {
    std::vector<std::byte> serialize() const;
    static std::expected<get_party_contact_informations_request,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const get_party_contact_informations_request& v);

/**
 * @brief Response containing all party contact informations.
 */
struct get_party_contact_informations_response final {
    std::vector<domain::party_contact_information> party_contact_informations;

    std::vector<std::byte> serialize() const;
    static std::expected<get_party_contact_informations_response,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const get_party_contact_informations_response& v);

/**
 * @brief Request to save a party contact information (create or update).
 */
struct save_party_contact_information_request final {
    domain::party_contact_information party_contact_information;

    std::vector<std::byte> serialize() const;
    static std::expected<save_party_contact_information_request,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const save_party_contact_information_request& v);

/**
 * @brief Response confirming party contact information save operation.
 */
struct save_party_contact_information_response final {
    bool success;
    std::string message;

    std::vector<std::byte> serialize() const;
    static std::expected<save_party_contact_information_response,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const save_party_contact_information_response& v);

/**
 * @brief Result for a single party contact information deletion.
 */
struct delete_party_contact_information_result final {
    boost::uuids::uuid id;  ///< Primary key
    bool success;
    std::string message;
};

std::ostream& operator<<(std::ostream& s, const delete_party_contact_information_result& v);

/**
 * @brief Request to delete one or more party contact informations.
 */
struct delete_party_contact_information_request final {
    std::vector<boost::uuids::uuid> ids;  ///< Primary keys

    std::vector<std::byte> serialize() const;
    static std::expected<delete_party_contact_information_request,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const delete_party_contact_information_request& v);

/**
 * @brief Response confirming party contact information deletion(s).
 */
struct delete_party_contact_information_response final {
    std::vector<delete_party_contact_information_result> results;

    std::vector<std::byte> serialize() const;
    static std::expected<delete_party_contact_information_response,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const delete_party_contact_information_response& v);

/**
 * @brief Request to retrieve version history for a party contact information.
 */
struct get_party_contact_information_history_request final {
    boost::uuids::uuid id;  ///< Primary key

    std::vector<std::byte> serialize() const;
    static std::expected<get_party_contact_information_history_request,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const get_party_contact_information_history_request& v);

/**
 * @brief Response containing party contact information version history.
 */
struct get_party_contact_information_history_response final {
    bool success;
    std::string message;
    std::vector<domain::party_contact_information> versions;

    std::vector<std::byte> serialize() const;
    static std::expected<get_party_contact_information_history_response,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const get_party_contact_information_history_response& v);

}

namespace ores::comms::messaging {

// Party Contact Information traits
template<>
struct message_traits<refdata::messaging::get_party_contact_informations_request> {
    using request_type = refdata::messaging::get_party_contact_informations_request;
    using response_type = refdata::messaging::get_party_contact_informations_response;
    static constexpr message_type request_message_type =
        message_type::get_party_contact_informations_request;
};

template<>
struct message_traits<refdata::messaging::save_party_contact_information_request> {
    using request_type = refdata::messaging::save_party_contact_information_request;
    using response_type = refdata::messaging::save_party_contact_information_response;
    static constexpr message_type request_message_type =
        message_type::save_party_contact_information_request;
};

template<>
struct message_traits<refdata::messaging::delete_party_contact_information_request> {
    using request_type = refdata::messaging::delete_party_contact_information_request;
    using response_type = refdata::messaging::delete_party_contact_information_response;
    static constexpr message_type request_message_type =
        message_type::delete_party_contact_information_request;
};

template<>
struct message_traits<refdata::messaging::get_party_contact_information_history_request> {
    using request_type = refdata::messaging::get_party_contact_information_history_request;
    using response_type = refdata::messaging::get_party_contact_information_history_response;
    static constexpr message_type request_message_type =
        message_type::get_party_contact_information_history_request;
};

}

#endif
