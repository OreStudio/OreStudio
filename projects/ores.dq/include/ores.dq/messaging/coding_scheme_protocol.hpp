/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2025 Marco Craveiro <marco.craveiro@gmail.com>
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
#ifndef ORES_DQ_MESSAGING_CODING_SCHEME_PROTOCOL_HPP
#define ORES_DQ_MESSAGING_CODING_SCHEME_PROTOCOL_HPP

#include <span>
#include <iosfwd>
#include <vector>
#include <expected>
#include "ores.comms/messaging/message_type.hpp"
#include "ores.comms/messaging/save_result.hpp"
#include "ores.utility/serialization/error_code.hpp"
#include "ores.comms/messaging/message_traits.hpp"
#include "ores.dq/domain/coding_scheme.hpp"
#include "ores.dq/domain/coding_scheme_authority_type.hpp"

namespace ores::dq::messaging {

// ============================================================================
// Coding Scheme Messages
// ============================================================================

/**
 * @brief Request to retrieve all coding schemes.
 */
struct get_coding_schemes_request final {
    std::vector<std::byte> serialize() const;
    static std::expected<get_coding_schemes_request,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const get_coding_schemes_request& v);

/**
 * @brief Response containing all coding schemes.
 */
struct get_coding_schemes_response final {
    std::vector<domain::coding_scheme> schemes;

    std::vector<std::byte> serialize() const;
    static std::expected<get_coding_schemes_response,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const get_coding_schemes_response& v);

/**
 * @brief Request to retrieve coding schemes for a specific authority type.
 */
struct get_coding_schemes_by_authority_type_request final {
    std::string authority_type;

    std::vector<std::byte> serialize() const;
    static std::expected<get_coding_schemes_by_authority_type_request,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const get_coding_schemes_by_authority_type_request& v);

/**
 * @brief Response containing coding schemes for an authority type.
 */
struct get_coding_schemes_by_authority_type_response final {
    std::vector<domain::coding_scheme> schemes;

    std::vector<std::byte> serialize() const;
    static std::expected<get_coding_schemes_by_authority_type_response,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const get_coding_schemes_by_authority_type_response& v);

/**
 * @brief Request to save one or more coding schemes (create or update).
 */
struct save_coding_scheme_request final {
    std::vector<domain::coding_scheme> schemes;

    static save_coding_scheme_request from(domain::coding_scheme scheme);
    static save_coding_scheme_request from(std::vector<domain::coding_scheme> schemes);

    std::vector<std::byte> serialize() const;
    static std::expected<save_coding_scheme_request,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const save_coding_scheme_request& v);

/**
 * @brief Response confirming coding scheme save operation(s).
 */
struct save_coding_scheme_response final {
    bool success = false;
    std::string message;

    std::vector<std::byte> serialize() const;
    static std::expected<save_coding_scheme_response,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const save_coding_scheme_response& v);

/**
 * @brief Request to delete one or more coding schemes.
 */
struct delete_coding_scheme_request final {
    std::vector<std::string> codes;

    std::vector<std::byte> serialize() const;
    static std::expected<delete_coding_scheme_request,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const delete_coding_scheme_request& v);

/**
 * @brief Response confirming coding scheme deletion(s).
 */
struct delete_coding_scheme_response final {
    bool success = false;
    std::string message;

    std::vector<std::byte> serialize() const;
    static std::expected<delete_coding_scheme_response,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const delete_coding_scheme_response& v);

/**
 * @brief Request to retrieve version history for a coding scheme.
 */
struct get_coding_scheme_history_request final {
    std::string code;

    std::vector<std::byte> serialize() const;
    static std::expected<get_coding_scheme_history_request,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const get_coding_scheme_history_request& v);

/**
 * @brief Response containing coding scheme version history.
 */
struct get_coding_scheme_history_response final {
    bool success;
    std::string message;
    std::vector<domain::coding_scheme> versions;

    std::vector<std::byte> serialize() const;
    static std::expected<get_coding_scheme_history_response,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const get_coding_scheme_history_response& v);

// ============================================================================
// Coding Scheme Authority Type Messages
// ============================================================================

/**
 * @brief Request to retrieve all coding scheme authority types.
 */
struct get_coding_scheme_authority_types_request final {
    std::vector<std::byte> serialize() const;
    static std::expected<get_coding_scheme_authority_types_request,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const get_coding_scheme_authority_types_request& v);

/**
 * @brief Response containing all coding scheme authority types.
 */
struct get_coding_scheme_authority_types_response final {
    std::vector<domain::coding_scheme_authority_type> authority_types;

    std::vector<std::byte> serialize() const;
    static std::expected<get_coding_scheme_authority_types_response,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const get_coding_scheme_authority_types_response& v);

/**
 * @brief Request to save one or more coding scheme authority types (create or update).
 */
struct save_coding_scheme_authority_type_request final {
    std::vector<domain::coding_scheme_authority_type> authority_types;

    static save_coding_scheme_authority_type_request from(domain::coding_scheme_authority_type authority_type);
    static save_coding_scheme_authority_type_request from(std::vector<domain::coding_scheme_authority_type> authority_types);

    std::vector<std::byte> serialize() const;
    static std::expected<save_coding_scheme_authority_type_request,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const save_coding_scheme_authority_type_request& v);

/**
 * @brief Response confirming coding scheme authority type save operation(s).
 */
struct save_coding_scheme_authority_type_response final {
    bool success = false;
    std::string message;

    std::vector<std::byte> serialize() const;
    static std::expected<save_coding_scheme_authority_type_response,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const save_coding_scheme_authority_type_response& v);

/**
 * @brief Request to delete one or more coding scheme authority types.
 */
struct delete_coding_scheme_authority_type_request final {
    std::vector<std::string> codes;

    std::vector<std::byte> serialize() const;
    static std::expected<delete_coding_scheme_authority_type_request,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const delete_coding_scheme_authority_type_request& v);

/**
 * @brief Response confirming coding scheme authority type deletion(s).
 */
struct delete_coding_scheme_authority_type_response final {
    bool success = false;
    std::string message;

    std::vector<std::byte> serialize() const;
    static std::expected<delete_coding_scheme_authority_type_response,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const delete_coding_scheme_authority_type_response& v);

/**
 * @brief Request to retrieve version history for a coding scheme authority type.
 */
struct get_coding_scheme_authority_type_history_request final {
    std::string code;

    std::vector<std::byte> serialize() const;
    static std::expected<get_coding_scheme_authority_type_history_request,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const get_coding_scheme_authority_type_history_request& v);

/**
 * @brief Response containing coding scheme authority type version history.
 */
struct get_coding_scheme_authority_type_history_response final {
    bool success;
    std::string message;
    std::vector<domain::coding_scheme_authority_type> versions;

    std::vector<std::byte> serialize() const;
    static std::expected<get_coding_scheme_authority_type_history_response,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const get_coding_scheme_authority_type_history_response& v);

}

namespace ores::comms::messaging {

// Coding Scheme traits
template<>
struct message_traits<dq::messaging::get_coding_schemes_request> {
    using request_type = dq::messaging::get_coding_schemes_request;
    using response_type = dq::messaging::get_coding_schemes_response;
    static constexpr message_type request_message_type =
        message_type::get_coding_schemes_request;
};

template<>
struct message_traits<dq::messaging::get_coding_schemes_by_authority_type_request> {
    using request_type = dq::messaging::get_coding_schemes_by_authority_type_request;
    using response_type = dq::messaging::get_coding_schemes_by_authority_type_response;
    static constexpr message_type request_message_type =
        message_type::get_coding_schemes_by_authority_type_request;
};

template<>
struct message_traits<dq::messaging::save_coding_scheme_request> {
    using request_type = dq::messaging::save_coding_scheme_request;
    using response_type = dq::messaging::save_coding_scheme_response;
    static constexpr message_type request_message_type =
        message_type::save_coding_scheme_request;
};

template<>
struct message_traits<dq::messaging::delete_coding_scheme_request> {
    using request_type = dq::messaging::delete_coding_scheme_request;
    using response_type = dq::messaging::delete_coding_scheme_response;
    static constexpr message_type request_message_type =
        message_type::delete_coding_scheme_request;
};

template<>
struct message_traits<dq::messaging::get_coding_scheme_history_request> {
    using request_type = dq::messaging::get_coding_scheme_history_request;
    using response_type = dq::messaging::get_coding_scheme_history_response;
    static constexpr message_type request_message_type =
        message_type::get_coding_scheme_history_request;
};

// Coding Scheme Authority Type traits
template<>
struct message_traits<dq::messaging::get_coding_scheme_authority_types_request> {
    using request_type = dq::messaging::get_coding_scheme_authority_types_request;
    using response_type = dq::messaging::get_coding_scheme_authority_types_response;
    static constexpr message_type request_message_type =
        message_type::get_coding_scheme_authority_types_request;
};

template<>
struct message_traits<dq::messaging::save_coding_scheme_authority_type_request> {
    using request_type = dq::messaging::save_coding_scheme_authority_type_request;
    using response_type = dq::messaging::save_coding_scheme_authority_type_response;
    static constexpr message_type request_message_type =
        message_type::save_coding_scheme_authority_type_request;
};

template<>
struct message_traits<dq::messaging::delete_coding_scheme_authority_type_request> {
    using request_type = dq::messaging::delete_coding_scheme_authority_type_request;
    using response_type = dq::messaging::delete_coding_scheme_authority_type_response;
    static constexpr message_type request_message_type =
        message_type::delete_coding_scheme_authority_type_request;
};

template<>
struct message_traits<dq::messaging::get_coding_scheme_authority_type_history_request> {
    using request_type = dq::messaging::get_coding_scheme_authority_type_history_request;
    using response_type = dq::messaging::get_coding_scheme_authority_type_history_response;
    static constexpr message_type request_message_type =
        message_type::get_coding_scheme_authority_type_history_request;
};

}

#endif
