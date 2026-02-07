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
#ifndef ORES_REFDATA_MESSAGING_CONTACT_TYPE_PROTOCOL_HPP
#define ORES_REFDATA_MESSAGING_CONTACT_TYPE_PROTOCOL_HPP

#include <span>
#include <iosfwd>
#include <vector>
#include <expected>
#include "ores.comms/messaging/message_type.hpp"
#include "ores.comms/messaging/message_traits.hpp"
#include "ores.utility/serialization/error_code.hpp"
#include "ores.refdata/domain/contact_type.hpp"

namespace ores::refdata::messaging {

// ============================================================================
// Contact Type Messages
// ============================================================================

/**
 * @brief Request to retrieve all contact types.
 */
struct get_contact_types_request final {
    std::vector<std::byte> serialize() const;
    static std::expected<get_contact_types_request,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const get_contact_types_request& v);

/**
 * @brief Response containing all contact types.
 */
struct get_contact_types_response final {
    std::vector<domain::contact_type> types;

    std::vector<std::byte> serialize() const;
    static std::expected<get_contact_types_response,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const get_contact_types_response& v);

/**
 * @brief Request to save a contact type (create or update).
 */
struct save_contact_type_request final {
    domain::contact_type type;

    std::vector<std::byte> serialize() const;
    static std::expected<save_contact_type_request,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const save_contact_type_request& v);

/**
 * @brief Response confirming contact type save operation.
 */
struct save_contact_type_response final {
    bool success;
    std::string message;

    std::vector<std::byte> serialize() const;
    static std::expected<save_contact_type_response,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const save_contact_type_response& v);

/**
 * @brief Result for a single contact type deletion.
 */
struct delete_contact_type_result final {
    std::string code;  ///< Primary key
    bool success;
    std::string message;
};

std::ostream& operator<<(std::ostream& s, const delete_contact_type_result& v);

/**
 * @brief Request to delete one or more contact types.
 */
struct delete_contact_type_request final {
    std::vector<std::string> codes;  ///< Primary keys

    std::vector<std::byte> serialize() const;
    static std::expected<delete_contact_type_request,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const delete_contact_type_request& v);

/**
 * @brief Response confirming contact type deletion(s).
 */
struct delete_contact_type_response final {
    std::vector<delete_contact_type_result> results;

    std::vector<std::byte> serialize() const;
    static std::expected<delete_contact_type_response,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const delete_contact_type_response& v);

/**
 * @brief Request to retrieve version history for a contact type.
 */
struct get_contact_type_history_request final {
    std::string code;  ///< Primary key

    std::vector<std::byte> serialize() const;
    static std::expected<get_contact_type_history_request,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const get_contact_type_history_request& v);

/**
 * @brief Response containing contact type version history.
 */
struct get_contact_type_history_response final {
    bool success;
    std::string message;
    std::vector<domain::contact_type> versions;

    std::vector<std::byte> serialize() const;
    static std::expected<get_contact_type_history_response,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const get_contact_type_history_response& v);

}

namespace ores::comms::messaging {

// Contact Type traits
template<>
struct message_traits<refdata::messaging::get_contact_types_request> {
    using request_type = refdata::messaging::get_contact_types_request;
    using response_type = refdata::messaging::get_contact_types_response;
    static constexpr message_type request_message_type =
        message_type::get_contact_types_request;
};

template<>
struct message_traits<refdata::messaging::save_contact_type_request> {
    using request_type = refdata::messaging::save_contact_type_request;
    using response_type = refdata::messaging::save_contact_type_response;
    static constexpr message_type request_message_type =
        message_type::save_contact_type_request;
};

template<>
struct message_traits<refdata::messaging::delete_contact_type_request> {
    using request_type = refdata::messaging::delete_contact_type_request;
    using response_type = refdata::messaging::delete_contact_type_response;
    static constexpr message_type request_message_type =
        message_type::delete_contact_type_request;
};

template<>
struct message_traits<refdata::messaging::get_contact_type_history_request> {
    using request_type = refdata::messaging::get_contact_type_history_request;
    using response_type = refdata::messaging::get_contact_type_history_response;
    static constexpr message_type request_message_type =
        message_type::get_contact_type_history_request;
};

}

#endif
