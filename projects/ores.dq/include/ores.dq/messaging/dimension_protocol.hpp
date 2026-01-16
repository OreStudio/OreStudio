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
#ifndef ORES_DQ_MESSAGING_DIMENSION_PROTOCOL_HPP
#define ORES_DQ_MESSAGING_DIMENSION_PROTOCOL_HPP

#include <span>
#include <iosfwd>
#include <vector>
#include <expected>
#include "ores.comms/messaging/message_types.hpp"
#include "ores.comms/messaging/message_traits.hpp"
#include "ores.dq/domain/nature_dimension.hpp"
#include "ores.dq/domain/origin_dimension.hpp"
#include "ores.dq/domain/treatment_dimension.hpp"

namespace ores::dq::messaging {

// ============================================================================
// Nature Dimension Messages
// ============================================================================

/**
 * @brief Request to retrieve all nature dimensions.
 */
struct get_nature_dimensions_request final {
    std::vector<std::byte> serialize() const;
    static std::expected<get_nature_dimensions_request,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const get_nature_dimensions_request& v);

/**
 * @brief Response containing all nature dimensions.
 */
struct get_nature_dimensions_response final {
    std::vector<domain::nature_dimension> dimensions;

    std::vector<std::byte> serialize() const;
    static std::expected<get_nature_dimensions_response,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const get_nature_dimensions_response& v);

/**
 * @brief Request to save a nature dimension (create or update).
 */
struct save_nature_dimension_request final {
    domain::nature_dimension dimension;

    std::vector<std::byte> serialize() const;
    static std::expected<save_nature_dimension_request,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const save_nature_dimension_request& v);

/**
 * @brief Response confirming nature dimension save operation.
 */
struct save_nature_dimension_response final {
    bool success;
    std::string message;

    std::vector<std::byte> serialize() const;
    static std::expected<save_nature_dimension_response,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const save_nature_dimension_response& v);

/**
 * @brief Result for a single nature dimension deletion.
 */
struct delete_nature_dimension_result final {
    std::string code;
    bool success;
    std::string message;
};

std::ostream& operator<<(std::ostream& s, const delete_nature_dimension_result& v);

/**
 * @brief Request to delete one or more nature dimensions.
 */
struct delete_nature_dimension_request final {
    std::vector<std::string> codes;

    std::vector<std::byte> serialize() const;
    static std::expected<delete_nature_dimension_request,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const delete_nature_dimension_request& v);

/**
 * @brief Response confirming nature dimension deletion(s).
 */
struct delete_nature_dimension_response final {
    std::vector<delete_nature_dimension_result> results;

    std::vector<std::byte> serialize() const;
    static std::expected<delete_nature_dimension_response,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const delete_nature_dimension_response& v);

/**
 * @brief Request to retrieve version history for a nature dimension.
 */
struct get_nature_dimension_history_request final {
    std::string code;

    std::vector<std::byte> serialize() const;
    static std::expected<get_nature_dimension_history_request,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const get_nature_dimension_history_request& v);

/**
 * @brief Response containing nature dimension version history.
 */
struct get_nature_dimension_history_response final {
    bool success;
    std::string message;
    std::vector<domain::nature_dimension> versions;

    std::vector<std::byte> serialize() const;
    static std::expected<get_nature_dimension_history_response,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const get_nature_dimension_history_response& v);

// ============================================================================
// Origin Dimension Messages
// ============================================================================

/**
 * @brief Request to retrieve all origin dimensions.
 */
struct get_origin_dimensions_request final {
    std::vector<std::byte> serialize() const;
    static std::expected<get_origin_dimensions_request,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const get_origin_dimensions_request& v);

/**
 * @brief Response containing all origin dimensions.
 */
struct get_origin_dimensions_response final {
    std::vector<domain::origin_dimension> dimensions;

    std::vector<std::byte> serialize() const;
    static std::expected<get_origin_dimensions_response,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const get_origin_dimensions_response& v);

/**
 * @brief Request to save an origin dimension (create or update).
 */
struct save_origin_dimension_request final {
    domain::origin_dimension dimension;

    std::vector<std::byte> serialize() const;
    static std::expected<save_origin_dimension_request,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const save_origin_dimension_request& v);

/**
 * @brief Response confirming origin dimension save operation.
 */
struct save_origin_dimension_response final {
    bool success;
    std::string message;

    std::vector<std::byte> serialize() const;
    static std::expected<save_origin_dimension_response,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const save_origin_dimension_response& v);

/**
 * @brief Result for a single origin dimension deletion.
 */
struct delete_origin_dimension_result final {
    std::string code;
    bool success;
    std::string message;
};

std::ostream& operator<<(std::ostream& s, const delete_origin_dimension_result& v);

/**
 * @brief Request to delete one or more origin dimensions.
 */
struct delete_origin_dimension_request final {
    std::vector<std::string> codes;

    std::vector<std::byte> serialize() const;
    static std::expected<delete_origin_dimension_request,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const delete_origin_dimension_request& v);

/**
 * @brief Response confirming origin dimension deletion(s).
 */
struct delete_origin_dimension_response final {
    std::vector<delete_origin_dimension_result> results;

    std::vector<std::byte> serialize() const;
    static std::expected<delete_origin_dimension_response,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const delete_origin_dimension_response& v);

/**
 * @brief Request to retrieve version history for an origin dimension.
 */
struct get_origin_dimension_history_request final {
    std::string code;

    std::vector<std::byte> serialize() const;
    static std::expected<get_origin_dimension_history_request,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const get_origin_dimension_history_request& v);

/**
 * @brief Response containing origin dimension version history.
 */
struct get_origin_dimension_history_response final {
    bool success;
    std::string message;
    std::vector<domain::origin_dimension> versions;

    std::vector<std::byte> serialize() const;
    static std::expected<get_origin_dimension_history_response,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const get_origin_dimension_history_response& v);

// ============================================================================
// Treatment Dimension Messages
// ============================================================================

/**
 * @brief Request to retrieve all treatment dimensions.
 */
struct get_treatment_dimensions_request final {
    std::vector<std::byte> serialize() const;
    static std::expected<get_treatment_dimensions_request,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const get_treatment_dimensions_request& v);

/**
 * @brief Response containing all treatment dimensions.
 */
struct get_treatment_dimensions_response final {
    std::vector<domain::treatment_dimension> dimensions;

    std::vector<std::byte> serialize() const;
    static std::expected<get_treatment_dimensions_response,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const get_treatment_dimensions_response& v);

/**
 * @brief Request to save a treatment dimension (create or update).
 */
struct save_treatment_dimension_request final {
    domain::treatment_dimension dimension;

    std::vector<std::byte> serialize() const;
    static std::expected<save_treatment_dimension_request,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const save_treatment_dimension_request& v);

/**
 * @brief Response confirming treatment dimension save operation.
 */
struct save_treatment_dimension_response final {
    bool success;
    std::string message;

    std::vector<std::byte> serialize() const;
    static std::expected<save_treatment_dimension_response,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const save_treatment_dimension_response& v);

/**
 * @brief Result for a single treatment dimension deletion.
 */
struct delete_treatment_dimension_result final {
    std::string code;
    bool success;
    std::string message;
};

std::ostream& operator<<(std::ostream& s, const delete_treatment_dimension_result& v);

/**
 * @brief Request to delete one or more treatment dimensions.
 */
struct delete_treatment_dimension_request final {
    std::vector<std::string> codes;

    std::vector<std::byte> serialize() const;
    static std::expected<delete_treatment_dimension_request,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const delete_treatment_dimension_request& v);

/**
 * @brief Response confirming treatment dimension deletion(s).
 */
struct delete_treatment_dimension_response final {
    std::vector<delete_treatment_dimension_result> results;

    std::vector<std::byte> serialize() const;
    static std::expected<delete_treatment_dimension_response,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const delete_treatment_dimension_response& v);

/**
 * @brief Request to retrieve version history for a treatment dimension.
 */
struct get_treatment_dimension_history_request final {
    std::string code;

    std::vector<std::byte> serialize() const;
    static std::expected<get_treatment_dimension_history_request,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const get_treatment_dimension_history_request& v);

/**
 * @brief Response containing treatment dimension version history.
 */
struct get_treatment_dimension_history_response final {
    bool success;
    std::string message;
    std::vector<domain::treatment_dimension> versions;

    std::vector<std::byte> serialize() const;
    static std::expected<get_treatment_dimension_history_response,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const get_treatment_dimension_history_response& v);

}

namespace ores::comms::messaging {

// Nature Dimension traits
template<>
struct message_traits<dq::messaging::get_nature_dimensions_request> {
    using request_type = dq::messaging::get_nature_dimensions_request;
    using response_type = dq::messaging::get_nature_dimensions_response;
    static constexpr message_type request_message_type =
        message_type::get_nature_dimensions_request;
};

template<>
struct message_traits<dq::messaging::save_nature_dimension_request> {
    using request_type = dq::messaging::save_nature_dimension_request;
    using response_type = dq::messaging::save_nature_dimension_response;
    static constexpr message_type request_message_type =
        message_type::save_nature_dimension_request;
};

template<>
struct message_traits<dq::messaging::delete_nature_dimension_request> {
    using request_type = dq::messaging::delete_nature_dimension_request;
    using response_type = dq::messaging::delete_nature_dimension_response;
    static constexpr message_type request_message_type =
        message_type::delete_nature_dimension_request;
};

template<>
struct message_traits<dq::messaging::get_nature_dimension_history_request> {
    using request_type = dq::messaging::get_nature_dimension_history_request;
    using response_type = dq::messaging::get_nature_dimension_history_response;
    static constexpr message_type request_message_type =
        message_type::get_nature_dimension_history_request;
};

// Origin Dimension traits
template<>
struct message_traits<dq::messaging::get_origin_dimensions_request> {
    using request_type = dq::messaging::get_origin_dimensions_request;
    using response_type = dq::messaging::get_origin_dimensions_response;
    static constexpr message_type request_message_type =
        message_type::get_origin_dimensions_request;
};

template<>
struct message_traits<dq::messaging::save_origin_dimension_request> {
    using request_type = dq::messaging::save_origin_dimension_request;
    using response_type = dq::messaging::save_origin_dimension_response;
    static constexpr message_type request_message_type =
        message_type::save_origin_dimension_request;
};

template<>
struct message_traits<dq::messaging::delete_origin_dimension_request> {
    using request_type = dq::messaging::delete_origin_dimension_request;
    using response_type = dq::messaging::delete_origin_dimension_response;
    static constexpr message_type request_message_type =
        message_type::delete_origin_dimension_request;
};

template<>
struct message_traits<dq::messaging::get_origin_dimension_history_request> {
    using request_type = dq::messaging::get_origin_dimension_history_request;
    using response_type = dq::messaging::get_origin_dimension_history_response;
    static constexpr message_type request_message_type =
        message_type::get_origin_dimension_history_request;
};

// Treatment Dimension traits
template<>
struct message_traits<dq::messaging::get_treatment_dimensions_request> {
    using request_type = dq::messaging::get_treatment_dimensions_request;
    using response_type = dq::messaging::get_treatment_dimensions_response;
    static constexpr message_type request_message_type =
        message_type::get_treatment_dimensions_request;
};

template<>
struct message_traits<dq::messaging::save_treatment_dimension_request> {
    using request_type = dq::messaging::save_treatment_dimension_request;
    using response_type = dq::messaging::save_treatment_dimension_response;
    static constexpr message_type request_message_type =
        message_type::save_treatment_dimension_request;
};

template<>
struct message_traits<dq::messaging::delete_treatment_dimension_request> {
    using request_type = dq::messaging::delete_treatment_dimension_request;
    using response_type = dq::messaging::delete_treatment_dimension_response;
    static constexpr message_type request_message_type =
        message_type::delete_treatment_dimension_request;
};

template<>
struct message_traits<dq::messaging::get_treatment_dimension_history_request> {
    using request_type = dq::messaging::get_treatment_dimension_history_request;
    using response_type = dq::messaging::get_treatment_dimension_history_response;
    static constexpr message_type request_message_type =
        message_type::get_treatment_dimension_history_request;
};

}

#endif
