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
#ifndef ORES_REFDATA_MESSAGING_BUSINESS_UNIT_PROTOCOL_HPP
#define ORES_REFDATA_MESSAGING_BUSINESS_UNIT_PROTOCOL_HPP

#include <span>
#include <iosfwd>
#include <vector>
#include <expected>
#include <boost/uuid/uuid.hpp>
#include "ores.comms/messaging/message_type.hpp"
#include "ores.comms/messaging/message_traits.hpp"
#include "ores.utility/serialization/error_code.hpp"
#include "ores.refdata/domain/business_unit.hpp"

namespace ores::refdata::messaging {

// ============================================================================
// Business Unit Messages
// ============================================================================

/**
 * @brief Request to retrieve all business units.
 */
struct get_business_units_request final {
    std::vector<std::byte> serialize() const;
    static std::expected<get_business_units_request,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const get_business_units_request& v);

/**
 * @brief Response containing all business units.
 */
struct get_business_units_response final {
    std::vector<domain::business_unit> business_units;

    std::vector<std::byte> serialize() const;
    static std::expected<get_business_units_response,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const get_business_units_response& v);

/**
 * @brief Request to save a business unit (create or update).
 */
struct save_business_unit_request final {
    domain::business_unit business_unit;

    std::vector<std::byte> serialize() const;
    static std::expected<save_business_unit_request,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const save_business_unit_request& v);

/**
 * @brief Response confirming business unit save operation.
 */
struct save_business_unit_response final {
    bool success;
    std::string message;

    std::vector<std::byte> serialize() const;
    static std::expected<save_business_unit_response,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const save_business_unit_response& v);

/**
 * @brief Result for a single business unit deletion.
 */
struct delete_business_unit_result final {
    boost::uuids::uuid id;  ///< Primary key
    bool success;
    std::string message;
};

std::ostream& operator<<(std::ostream& s, const delete_business_unit_result& v);

/**
 * @brief Request to delete one or more business units.
 */
struct delete_business_unit_request final {
    std::vector<boost::uuids::uuid> ids;  ///< Primary keys

    std::vector<std::byte> serialize() const;
    static std::expected<delete_business_unit_request,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const delete_business_unit_request& v);

/**
 * @brief Response confirming business unit deletion(s).
 */
struct delete_business_unit_response final {
    std::vector<delete_business_unit_result> results;

    std::vector<std::byte> serialize() const;
    static std::expected<delete_business_unit_response,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const delete_business_unit_response& v);

/**
 * @brief Request to retrieve version history for a business unit.
 */
struct get_business_unit_history_request final {
    boost::uuids::uuid id;  ///< Primary key

    std::vector<std::byte> serialize() const;
    static std::expected<get_business_unit_history_request,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const get_business_unit_history_request& v);

/**
 * @brief Response containing business unit version history.
 */
struct get_business_unit_history_response final {
    bool success;
    std::string message;
    std::vector<domain::business_unit> versions;

    std::vector<std::byte> serialize() const;
    static std::expected<get_business_unit_history_response,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const get_business_unit_history_response& v);

}

namespace ores::comms::messaging {

// Business Unit traits
template<>
struct message_traits<refdata::messaging::get_business_units_request> {
    using request_type = refdata::messaging::get_business_units_request;
    using response_type = refdata::messaging::get_business_units_response;
    static constexpr message_type request_message_type =
        message_type::get_business_units_request;
};

template<>
struct message_traits<refdata::messaging::save_business_unit_request> {
    using request_type = refdata::messaging::save_business_unit_request;
    using response_type = refdata::messaging::save_business_unit_response;
    static constexpr message_type request_message_type =
        message_type::save_business_unit_request;
};

template<>
struct message_traits<refdata::messaging::delete_business_unit_request> {
    using request_type = refdata::messaging::delete_business_unit_request;
    using response_type = refdata::messaging::delete_business_unit_response;
    static constexpr message_type request_message_type =
        message_type::delete_business_unit_request;
};

template<>
struct message_traits<refdata::messaging::get_business_unit_history_request> {
    using request_type = refdata::messaging::get_business_unit_history_request;
    using response_type = refdata::messaging::get_business_unit_history_response;
    static constexpr message_type request_message_type =
        message_type::get_business_unit_history_request;
};

}

#endif
