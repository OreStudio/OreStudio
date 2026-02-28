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
#ifndef ORES_REFDATA_MESSAGING_MONETARY_NATURE_PROTOCOL_HPP
#define ORES_REFDATA_MESSAGING_MONETARY_NATURE_PROTOCOL_HPP

#include <span>
#include <iosfwd>
#include <vector>
#include <expected>
#include "ores.comms/messaging/message_type.hpp"
#include "ores.comms/messaging/message_traits.hpp"
#include "ores.utility/serialization/error_code.hpp"
#include "ores.refdata/domain/monetary_nature.hpp"

namespace ores::refdata::messaging {

// ============================================================================
// Currency Asset Class Messages
// ============================================================================

/**
 * @brief Request to retrieve all currency asset classes.
 */
struct get_monetary_natures_request final {
    std::vector<std::byte> serialize() const;
    static std::expected<get_monetary_natures_request,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const get_monetary_natures_request& v);

/**
 * @brief Response containing all currency asset classes.
 */
struct get_monetary_natures_response final {
    std::vector<domain::monetary_nature> types;

    std::vector<std::byte> serialize() const;
    static std::expected<get_monetary_natures_response,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const get_monetary_natures_response& v);

/**
 * @brief Request to save one or more monetary natures (create or update).
 */
struct save_monetary_nature_request final {
    std::vector<domain::monetary_nature> types;

    static save_monetary_nature_request from(domain::monetary_nature type);
    static save_monetary_nature_request from(std::vector<domain::monetary_nature> types);

    std::vector<std::byte> serialize() const;
    static std::expected<save_monetary_nature_request,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const save_monetary_nature_request& v);

/**
 * @brief Response confirming monetary nature save operation(s).
 */
struct save_monetary_nature_response final {
    bool success = false;
    std::string message;

    std::vector<std::byte> serialize() const;
    static std::expected<save_monetary_nature_response,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const save_monetary_nature_response& v);

/**
 * @brief Request to delete one or more currency asset classes.
 */
struct delete_monetary_nature_request final {
    std::vector<std::string> codes;  ///< Primary keys

    std::vector<std::byte> serialize() const;
    static std::expected<delete_monetary_nature_request,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const delete_monetary_nature_request& v);

/**
 * @brief Response confirming currency asset class deletion(s).
 */
struct delete_monetary_nature_response final {
    bool success = false;
    std::string message;

    std::vector<std::byte> serialize() const;
    static std::expected<delete_monetary_nature_response,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const delete_monetary_nature_response& v);

}

namespace ores::comms::messaging {

// Currency Asset Class traits
template<>
struct message_traits<refdata::messaging::get_monetary_natures_request> {
    using request_type = refdata::messaging::get_monetary_natures_request;
    using response_type = refdata::messaging::get_monetary_natures_response;
    static constexpr message_type request_message_type =
        message_type::get_monetary_natures_request;
};

template<>
struct message_traits<refdata::messaging::save_monetary_nature_request> {
    using request_type = refdata::messaging::save_monetary_nature_request;
    using response_type = refdata::messaging::save_monetary_nature_response;
    static constexpr message_type request_message_type =
        message_type::save_monetary_nature_request;
};

template<>
struct message_traits<refdata::messaging::delete_monetary_nature_request> {
    using request_type = refdata::messaging::delete_monetary_nature_request;
    using response_type = refdata::messaging::delete_monetary_nature_response;
    static constexpr message_type request_message_type =
        message_type::delete_monetary_nature_request;
};

}

#endif
