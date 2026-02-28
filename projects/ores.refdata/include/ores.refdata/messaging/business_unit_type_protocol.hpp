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
#ifndef ORES_REFDATA_MESSAGING_BUSINESS_UNIT_TYPE_PROTOCOL_HPP
#define ORES_REFDATA_MESSAGING_BUSINESS_UNIT_TYPE_PROTOCOL_HPP

#include <span>
#include <iosfwd>
#include <vector>
#include <expected>
#include <boost/uuid/uuid.hpp>
#include "ores.comms/messaging/message_type.hpp"
#include "ores.comms/messaging/save_result.hpp"
#include "ores.comms/messaging/message_traits.hpp"
#include "ores.utility/serialization/error_code.hpp"
#include "ores.refdata/domain/business_unit_type.hpp"

namespace ores::refdata::messaging {

// ============================================================================
// Business Unit Type Messages
// ============================================================================

/**
 * @brief Request to retrieve business unit types.
 */
struct get_business_unit_types_request final {
    std::vector<std::byte> serialize() const;
    static std::expected<get_business_unit_types_request,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const get_business_unit_types_request& v);

/**
 * @brief Response containing business unit types.
 */
struct get_business_unit_types_response final {
    std::vector<domain::business_unit_type> types;

    std::vector<std::byte> serialize() const;
    static std::expected<get_business_unit_types_response,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const get_business_unit_types_response& v);

/**
 * @brief Request to save one or more business unit types (create or update).
 */
struct save_business_unit_type_request final {
    std::vector<domain::business_unit_type> types;

    static save_business_unit_type_request from(domain::business_unit_type type);
    static save_business_unit_type_request from(std::vector<domain::business_unit_type> types);

    std::vector<std::byte> serialize() const;
    static std::expected<save_business_unit_type_request,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const save_business_unit_type_request& v);

/**
 * @brief Response confirming business unit type save operation(s).
 */
struct save_business_unit_type_response final {
    bool success = false;
    std::string message;

    std::vector<std::byte> serialize() const;
    static std::expected<save_business_unit_type_response,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const save_business_unit_type_response& v);

/**
 * @brief Request to delete one or more business unit types.
 */
struct delete_business_unit_type_request final {
    std::vector<boost::uuids::uuid> ids;  ///< Primary keys

    std::vector<std::byte> serialize() const;
    static std::expected<delete_business_unit_type_request,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const delete_business_unit_type_request& v);

/**
 * @brief Response confirming business unit type deletion(s).
 */
struct delete_business_unit_type_response final {
    bool success = false;
    std::string message;

    std::vector<std::byte> serialize() const;
    static std::expected<delete_business_unit_type_response,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const delete_business_unit_type_response& v);

/**
 * @brief Request to retrieve version history for a business unit type.
 */
struct get_business_unit_type_history_request final {
    boost::uuids::uuid id;  ///< Primary key

    std::vector<std::byte> serialize() const;
    static std::expected<get_business_unit_type_history_request,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const get_business_unit_type_history_request& v);

/**
 * @brief Response containing business unit type version history.
 */
struct get_business_unit_type_history_response final {
    bool success;
    std::string message;
    std::vector<domain::business_unit_type> versions;

    std::vector<std::byte> serialize() const;
    static std::expected<get_business_unit_type_history_response,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const get_business_unit_type_history_response& v);

}

namespace ores::comms::messaging {

// Business Unit Type traits
template<>
struct message_traits<refdata::messaging::get_business_unit_types_request> {
    using request_type = refdata::messaging::get_business_unit_types_request;
    using response_type = refdata::messaging::get_business_unit_types_response;
    static constexpr message_type request_message_type =
        message_type::get_business_unit_types_request;
};

template<>
struct message_traits<refdata::messaging::save_business_unit_type_request> {
    using request_type = refdata::messaging::save_business_unit_type_request;
    using response_type = refdata::messaging::save_business_unit_type_response;
    static constexpr message_type request_message_type =
        message_type::save_business_unit_type_request;
};

template<>
struct message_traits<refdata::messaging::delete_business_unit_type_request> {
    using request_type = refdata::messaging::delete_business_unit_type_request;
    using response_type = refdata::messaging::delete_business_unit_type_response;
    static constexpr message_type request_message_type =
        message_type::delete_business_unit_type_request;
};

template<>
struct message_traits<refdata::messaging::get_business_unit_type_history_request> {
    using request_type = refdata::messaging::get_business_unit_type_history_request;
    using response_type = refdata::messaging::get_business_unit_type_history_response;
    static constexpr message_type request_message_type =
        message_type::get_business_unit_type_history_request;
};

}

#endif
