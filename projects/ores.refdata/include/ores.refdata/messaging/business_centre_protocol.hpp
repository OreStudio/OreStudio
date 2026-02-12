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
#ifndef ORES_REFDATA_MESSAGING_BUSINESS_CENTRE_PROTOCOL_HPP
#define ORES_REFDATA_MESSAGING_BUSINESS_CENTRE_PROTOCOL_HPP

#include <span>
#include <iosfwd>
#include <vector>
#include <cstdint>
#include <expected>
#include <rfl.hpp>
#include <rfl/json.hpp>
#include "ores.comms/messaging/message_type.hpp"
#include "ores.utility/serialization/error_code.hpp"
#include "ores.comms/messaging/message_traits.hpp"
#include "ores.refdata/domain/business_centre.hpp"

namespace ores::refdata::messaging {

/**
 * @brief Request to retrieve business centres with pagination support.
 */
struct get_business_centres_request final {
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;

    std::vector<std::byte> serialize() const;

    static std::expected<get_business_centres_request, ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const get_business_centres_request& v);

/**
 * @brief Response containing business centres with pagination metadata.
 */
struct get_business_centres_response final {
    std::vector<domain::business_centre> business_centres;
    std::uint32_t total_available_count = 0;

    std::vector<std::byte> serialize() const;

    static std::expected<get_business_centres_response, ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const get_business_centres_response& v);

/**
 * @brief Request to save a business centre (create or update).
 */
struct save_business_centre_request final {
    domain::business_centre business_centre;

    std::vector<std::byte> serialize() const;

    static std::expected<save_business_centre_request, ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const save_business_centre_request& v);

/**
 * @brief Response confirming business centre save operation.
 */
struct save_business_centre_response final {
    bool success;
    std::string message;

    std::vector<std::byte> serialize() const;

    static std::expected<save_business_centre_response, ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const save_business_centre_response& v);

/**
 * @brief Request to delete one or more business centres.
 */
struct delete_business_centre_request final {
    std::vector<std::string> codes;

    std::vector<std::byte> serialize() const;

    static std::expected<delete_business_centre_request, ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const delete_business_centre_request& v);

/**
 * @brief Result for a single business centre deletion.
 */
struct delete_business_centre_result final {
    std::string code;
    bool success;
    std::string message;
};

std::ostream& operator<<(std::ostream& s, const delete_business_centre_result& v);

/**
 * @brief Response confirming business centre deletion(s).
 */
struct delete_business_centre_response final {
    std::vector<delete_business_centre_result> results;

    std::vector<std::byte> serialize() const;

    static std::expected<delete_business_centre_response, ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const delete_business_centre_response& v);

/**
 * @brief Request to retrieve version history for a business centre.
 */
struct get_business_centre_history_request final {
    std::string code;

    std::vector<std::byte> serialize() const;

    static std::expected<get_business_centre_history_request, ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const get_business_centre_history_request& v);

/**
 * @brief Response containing business centre version history.
 */
struct get_business_centre_history_response final {
    bool success;
    std::string message;
    std::vector<domain::business_centre> history;

    std::vector<std::byte> serialize() const;

    static std::expected<get_business_centre_history_response, ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const get_business_centre_history_response& v);

}

namespace ores::comms::messaging {

template<>
struct message_traits<refdata::messaging::get_business_centres_request> {
    using request_type = refdata::messaging::get_business_centres_request;
    using response_type = refdata::messaging::get_business_centres_response;
    static constexpr message_type request_message_type =
        message_type::get_business_centres_request;
};

template<>
struct message_traits<refdata::messaging::save_business_centre_request> {
    using request_type = refdata::messaging::save_business_centre_request;
    using response_type = refdata::messaging::save_business_centre_response;
    static constexpr message_type request_message_type =
        message_type::save_business_centre_request;
};

template<>
struct message_traits<refdata::messaging::delete_business_centre_request> {
    using request_type = refdata::messaging::delete_business_centre_request;
    using response_type = refdata::messaging::delete_business_centre_response;
    static constexpr message_type request_message_type =
        message_type::delete_business_centre_request;
};

template<>
struct message_traits<refdata::messaging::get_business_centre_history_request> {
    using request_type = refdata::messaging::get_business_centre_history_request;
    using response_type = refdata::messaging::get_business_centre_history_response;
    static constexpr message_type request_message_type =
        message_type::get_business_centre_history_request;
};

}

#endif
