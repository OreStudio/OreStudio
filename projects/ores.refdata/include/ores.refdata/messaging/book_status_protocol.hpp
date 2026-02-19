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
#ifndef ORES_REFDATA_MESSAGING_BOOK_STATUS_PROTOCOL_HPP
#define ORES_REFDATA_MESSAGING_BOOK_STATUS_PROTOCOL_HPP

#include <span>
#include <iosfwd>
#include <vector>
#include <expected>
#include "ores.comms/messaging/message_type.hpp"
#include "ores.comms/messaging/message_traits.hpp"
#include "ores.utility/serialization/error_code.hpp"
#include "ores.refdata/domain/book_status.hpp"

namespace ores::refdata::messaging {

// ============================================================================
// Book Status Messages
// ============================================================================

/**
 * @brief Request to retrieve all book statuses.
 */
struct get_book_statuses_request final {
    std::vector<std::byte> serialize() const;
    static std::expected<get_book_statuses_request,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const get_book_statuses_request& v);

/**
 * @brief Response containing all book statuses.
 */
struct get_book_statuses_response final {
    std::vector<domain::book_status> statuses;

    std::vector<std::byte> serialize() const;
    static std::expected<get_book_statuses_response,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const get_book_statuses_response& v);

/**
 * @brief Request to save a book status (create or update).
 */
struct save_book_status_request final {
    domain::book_status status;

    std::vector<std::byte> serialize() const;
    static std::expected<save_book_status_request,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const save_book_status_request& v);

/**
 * @brief Response confirming book status save operation.
 */
struct save_book_status_response final {
    bool success;
    std::string message;

    std::vector<std::byte> serialize() const;
    static std::expected<save_book_status_response,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const save_book_status_response& v);

/**
 * @brief Result for a single book status deletion.
 */
struct delete_book_status_result final {
    std::string code;  ///< Primary key
    bool success;
    std::string message;
};

std::ostream& operator<<(std::ostream& s, const delete_book_status_result& v);

/**
 * @brief Request to delete one or more book statuses.
 */
struct delete_book_status_request final {
    std::vector<std::string> codes;  ///< Primary keys

    std::vector<std::byte> serialize() const;
    static std::expected<delete_book_status_request,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const delete_book_status_request& v);

/**
 * @brief Response confirming book status deletion(s).
 */
struct delete_book_status_response final {
    std::vector<delete_book_status_result> results;

    std::vector<std::byte> serialize() const;
    static std::expected<delete_book_status_response,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const delete_book_status_response& v);

/**
 * @brief Request to retrieve version history for a book status.
 */
struct get_book_status_history_request final {
    std::string code;  ///< Primary key

    std::vector<std::byte> serialize() const;
    static std::expected<get_book_status_history_request,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const get_book_status_history_request& v);

/**
 * @brief Response containing book status version history.
 */
struct get_book_status_history_response final {
    bool success;
    std::string message;
    std::vector<domain::book_status> versions;

    std::vector<std::byte> serialize() const;
    static std::expected<get_book_status_history_response,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const get_book_status_history_response& v);

}

namespace ores::comms::messaging {

// Book Status traits
template<>
struct message_traits<refdata::messaging::get_book_statuses_request> {
    using request_type = refdata::messaging::get_book_statuses_request;
    using response_type = refdata::messaging::get_book_statuses_response;
    static constexpr message_type request_message_type =
        message_type::get_book_statuses_request;
};

template<>
struct message_traits<refdata::messaging::save_book_status_request> {
    using request_type = refdata::messaging::save_book_status_request;
    using response_type = refdata::messaging::save_book_status_response;
    static constexpr message_type request_message_type =
        message_type::save_book_status_request;
};

template<>
struct message_traits<refdata::messaging::delete_book_status_request> {
    using request_type = refdata::messaging::delete_book_status_request;
    using response_type = refdata::messaging::delete_book_status_response;
    static constexpr message_type request_message_type =
        message_type::delete_book_status_request;
};

template<>
struct message_traits<refdata::messaging::get_book_status_history_request> {
    using request_type = refdata::messaging::get_book_status_history_request;
    using response_type = refdata::messaging::get_book_status_history_response;
    static constexpr message_type request_message_type =
        message_type::get_book_status_history_request;
};

}

#endif
