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
#ifndef ORES_DQ_MESSAGING_CHANGE_MANAGEMENT_PROTOCOL_HPP
#define ORES_DQ_MESSAGING_CHANGE_MANAGEMENT_PROTOCOL_HPP

#include <span>
#include <iosfwd>
#include <vector>
#include <expected>
#include "ores.comms/messaging/message_types.hpp"
#include "ores.comms/messaging/message_traits.hpp"
#include "ores.dq/domain/change_reason_category.hpp"
#include "ores.dq/domain/change_reason.hpp"

namespace ores::dq::messaging {

/**
 * @brief Request to retrieve all change reason categories.
 */
struct get_change_reason_categories_request final {
    std::vector<std::byte> serialize() const;
    static std::expected<get_change_reason_categories_request,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s,
    const get_change_reason_categories_request& v);

/**
 * @brief Response containing all change reason categories.
 */
struct get_change_reason_categories_response final {
    std::vector<domain::change_reason_category> categories;

    std::vector<std::byte> serialize() const;
    static std::expected<get_change_reason_categories_response,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s,
    const get_change_reason_categories_response& v);

/**
 * @brief Request to retrieve all change reasons.
 */
struct get_change_reasons_request final {
    std::vector<std::byte> serialize() const;
    static std::expected<get_change_reasons_request,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s,
    const get_change_reasons_request& v);

/**
 * @brief Response containing all change reasons.
 */
struct get_change_reasons_response final {
    std::vector<domain::change_reason> reasons;

    std::vector<std::byte> serialize() const;
    static std::expected<get_change_reasons_response,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s,
    const get_change_reasons_response& v);

/**
 * @brief Request to retrieve change reasons for a specific category.
 */
struct get_change_reasons_by_category_request final {
    std::string category_code;

    std::vector<std::byte> serialize() const;
    static std::expected<get_change_reasons_by_category_request,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s,
    const get_change_reasons_by_category_request& v);

/**
 * @brief Response containing change reasons for a category.
 */
struct get_change_reasons_by_category_response final {
    std::vector<domain::change_reason> reasons;

    std::vector<std::byte> serialize() const;
    static std::expected<get_change_reasons_by_category_response,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s,
    const get_change_reasons_by_category_response& v);

// ============================================================================
// Change Reason CRUD Operations
// ============================================================================

/**
 * @brief Request to save a change reason (create or update).
 *
 * Due to bitemporal storage, both create and update operations
 * result in writing a new record. Database triggers handle temporal
 * versioning automatically. The code field is the natural key.
 */
struct save_change_reason_request final {
    domain::change_reason reason;

    std::vector<std::byte> serialize() const;
    static std::expected<save_change_reason_request,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const save_change_reason_request& v);

/**
 * @brief Response confirming change reason save operation.
 */
struct save_change_reason_response final {
    bool success;
    std::string message;

    std::vector<std::byte> serialize() const;
    static std::expected<save_change_reason_response,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const save_change_reason_response& v);

/**
 * @brief Result for a single change reason deletion.
 */
struct delete_change_reason_result final {
    std::string code;
    bool success;
    std::string message;
};

std::ostream& operator<<(std::ostream& s, const delete_change_reason_result& v);

/**
 * @brief Request to delete one or more change reasons.
 *
 * Supports batch deletion by accepting a vector of codes.
 * Each reason is processed independently - partial success is possible.
 */
struct delete_change_reason_request final {
    std::vector<std::string> codes;

    std::vector<std::byte> serialize() const;
    static std::expected<delete_change_reason_request,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const delete_change_reason_request& v);

/**
 * @brief Response confirming change reason deletion(s).
 *
 * Contains one result per requested reason, indicating individual
 * success or failure. Supports partial success in batch operations.
 */
struct delete_change_reason_response final {
    std::vector<delete_change_reason_result> results;

    std::vector<std::byte> serialize() const;
    static std::expected<delete_change_reason_response,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const delete_change_reason_response& v);

/**
 * @brief Request to retrieve version history for a change reason.
 */
struct get_change_reason_history_request final {
    std::string code;

    std::vector<std::byte> serialize() const;
    static std::expected<get_change_reason_history_request,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s,
    const get_change_reason_history_request& v);

/**
 * @brief Response containing change reason version history.
 */
struct get_change_reason_history_response final {
    bool success;
    std::string message;
    std::vector<domain::change_reason> versions;

    std::vector<std::byte> serialize() const;
    static std::expected<get_change_reason_history_response,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s,
    const get_change_reason_history_response& v);

// ============================================================================
// Change Reason Category CRUD Operations
// ============================================================================

/**
 * @brief Request to save a change reason category (create or update).
 *
 * Due to bitemporal storage, both create and update operations
 * result in writing a new record. Database triggers handle temporal
 * versioning automatically. The code field is the natural key.
 */
struct save_change_reason_category_request final {
    domain::change_reason_category category;

    std::vector<std::byte> serialize() const;
    static std::expected<save_change_reason_category_request,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s,
    const save_change_reason_category_request& v);

/**
 * @brief Response confirming change reason category save operation.
 */
struct save_change_reason_category_response final {
    bool success;
    std::string message;

    std::vector<std::byte> serialize() const;
    static std::expected<save_change_reason_category_response,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s,
    const save_change_reason_category_response& v);

/**
 * @brief Result for a single change reason category deletion.
 */
struct delete_change_reason_category_result final {
    std::string code;
    bool success;
    std::string message;
};

std::ostream& operator<<(std::ostream& s,
    const delete_change_reason_category_result& v);

/**
 * @brief Request to delete one or more change reason categories.
 *
 * Supports batch deletion by accepting a vector of codes.
 * Each category is processed independently - partial success is possible.
 */
struct delete_change_reason_category_request final {
    std::vector<std::string> codes;

    std::vector<std::byte> serialize() const;
    static std::expected<delete_change_reason_category_request,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s,
    const delete_change_reason_category_request& v);

/**
 * @brief Response confirming change reason category deletion(s).
 *
 * Contains one result per requested category, indicating individual
 * success or failure. Supports partial success in batch operations.
 */
struct delete_change_reason_category_response final {
    std::vector<delete_change_reason_category_result> results;

    std::vector<std::byte> serialize() const;
    static std::expected<delete_change_reason_category_response,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s,
    const delete_change_reason_category_response& v);

/**
 * @brief Request to retrieve version history for a change reason category.
 */
struct get_change_reason_category_history_request final {
    std::string code;

    std::vector<std::byte> serialize() const;
    static std::expected<get_change_reason_category_history_request,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s,
    const get_change_reason_category_history_request& v);

/**
 * @brief Response containing change reason category version history.
 */
struct get_change_reason_category_history_response final {
    bool success;
    std::string message;
    std::vector<domain::change_reason_category> versions;

    std::vector<std::byte> serialize() const;
    static std::expected<get_change_reason_category_history_response,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s,
    const get_change_reason_category_history_response& v);

}

namespace ores::comms::messaging {

/**
 * @brief Message traits specialization for get_change_reason_categories_request.
 */
template<>
struct message_traits<dq::messaging::get_change_reason_categories_request> {
    using request_type = dq::messaging::get_change_reason_categories_request;
    using response_type = dq::messaging::get_change_reason_categories_response;
    static constexpr message_type request_message_type =
        message_type::get_change_reason_categories_request;
};

/**
 * @brief Message traits specialization for get_change_reasons_request.
 */
template<>
struct message_traits<dq::messaging::get_change_reasons_request> {
    using request_type = dq::messaging::get_change_reasons_request;
    using response_type = dq::messaging::get_change_reasons_response;
    static constexpr message_type request_message_type =
        message_type::get_change_reasons_request;
};

/**
 * @brief Message traits specialization for get_change_reasons_by_category_request.
 */
template<>
struct message_traits<dq::messaging::get_change_reasons_by_category_request> {
    using request_type = dq::messaging::get_change_reasons_by_category_request;
    using response_type = dq::messaging::get_change_reasons_by_category_response;
    static constexpr message_type request_message_type =
        message_type::get_change_reasons_by_category_request;
};

/**
 * @brief Message traits specialization for save_change_reason_request.
 */
template<>
struct message_traits<dq::messaging::save_change_reason_request> {
    using request_type = dq::messaging::save_change_reason_request;
    using response_type = dq::messaging::save_change_reason_response;
    static constexpr message_type request_message_type =
        message_type::save_change_reason_request;
};

/**
 * @brief Message traits specialization for delete_change_reason_request.
 */
template<>
struct message_traits<dq::messaging::delete_change_reason_request> {
    using request_type = dq::messaging::delete_change_reason_request;
    using response_type = dq::messaging::delete_change_reason_response;
    static constexpr message_type request_message_type =
        message_type::delete_change_reason_request;
};

/**
 * @brief Message traits specialization for get_change_reason_history_request.
 */
template<>
struct message_traits<dq::messaging::get_change_reason_history_request> {
    using request_type = dq::messaging::get_change_reason_history_request;
    using response_type = dq::messaging::get_change_reason_history_response;
    static constexpr message_type request_message_type =
        message_type::get_change_reason_history_request;
};

/**
 * @brief Message traits specialization for save_change_reason_category_request.
 */
template<>
struct message_traits<dq::messaging::save_change_reason_category_request> {
    using request_type = dq::messaging::save_change_reason_category_request;
    using response_type = dq::messaging::save_change_reason_category_response;
    static constexpr message_type request_message_type =
        message_type::save_change_reason_category_request;
};

/**
 * @brief Message traits specialization for delete_change_reason_category_request.
 */
template<>
struct message_traits<dq::messaging::delete_change_reason_category_request> {
    using request_type = dq::messaging::delete_change_reason_category_request;
    using response_type = dq::messaging::delete_change_reason_category_response;
    static constexpr message_type request_message_type =
        message_type::delete_change_reason_category_request;
};

/**
 * @brief Message traits specialization for get_change_reason_category_history_request.
 */
template<>
struct message_traits<dq::messaging::get_change_reason_category_history_request> {
    using request_type = dq::messaging::get_change_reason_category_history_request;
    using response_type = dq::messaging::get_change_reason_category_history_response;
    static constexpr message_type request_message_type =
        message_type::get_change_reason_category_history_request;
};

}

#endif
