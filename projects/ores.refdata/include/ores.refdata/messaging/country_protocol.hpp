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
#ifndef ORES_REFDATA_MESSAGING_COUNTRY_PROTOCOL_HPP
#define ORES_REFDATA_MESSAGING_COUNTRY_PROTOCOL_HPP

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
#include "ores.refdata/domain/country.hpp"

namespace ores::refdata::messaging {

/**
 * @brief Request to retrieve countries with pagination support.
 *
 * Supports pagination through offset and limit parameters.
 */
struct get_countries_request final {
    /// Number of records to skip (0-based)
    std::uint32_t offset = 0;
    /// Maximum number of records to return
    std::uint32_t limit = 100;

    /**
     * @brief Serialize request to bytes.
     *
     * Format:
     * - 4 bytes: offset (uint32_t)
     * - 4 bytes: limit (uint32_t)
     */
    std::vector<std::byte> serialize() const;

    /**
     * @brief Deserialize request from bytes.
     */
    static std::expected<get_countries_request, ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const get_countries_request& v);

/**
 * @brief Response containing countries with pagination metadata.
 */
struct get_countries_response final {
    std::vector<domain::country> countries;
    /// Total number of countries available (not just in this page)
    std::uint32_t total_available_count = 0;

    /**
     * @brief Serialize response to bytes.
     *
     * Format:
     * - 4 bytes: total_available_count (uint32_t)
     * - 4 bytes: count (number of countries in this response)
     * - For each country:
     *   - 4 bytes: version (int)
     *   - 2 bytes: alpha2_code length
     *   - N bytes: alpha2_code (UTF-8)
     *   - 2 bytes: alpha3_code length
     *   - N bytes: alpha3_code (UTF-8)
     *   - 2 bytes: numeric_code length
     *   - N bytes: numeric_code (UTF-8)
     *   - 2 bytes: name length
     *   - N bytes: name (UTF-8)
     *   - 2 bytes: official_name length
     *   - N bytes: official_name (UTF-8)
     *   - 1 byte: has_image_id (0 or 1)
     *   - 16 bytes: image_id (UUID, only if has_image_id)
     *   - 2 bytes: recorded_at length
     *   - N bytes: recorded_at (UTF-8, ISO 8601 format)
     */
    std::vector<std::byte> serialize() const;

    /**
     * @brief Deserialize response from bytes.
     */
    static std::expected<get_countries_response, ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const get_countries_response& v);

/**
 * @brief Request to save a country (create or update).
 *
 * Due to bitemporal storage, both create and update operations
 * result in writing a new record. Database triggers handle temporal
 * versioning automatically.
 */
struct save_country_request final {
    domain::country country;

    /**
     * @brief Serialize request to bytes.
     */
    std::vector<std::byte> serialize() const;

    /**
     * @brief Deserialize request from bytes.
     */
    static std::expected<save_country_request, ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const save_country_request& v);

/**
 * @brief Response confirming country save operation.
 */
struct save_country_response final {
    bool success;
    std::string message;

    /**
     * @brief Serialize response to bytes.
     */
    std::vector<std::byte> serialize() const;

    /**
     * @brief Deserialize response from bytes.
     */
    static std::expected<save_country_response, ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const save_country_response& v);

/**
 * @brief Request to delete one or more countries.
 *
 * Supports batch deletion by accepting a vector of alpha-2 codes.
 * Each country is processed independently - partial success is possible.
 */
struct delete_country_request final {
    std::vector<std::string> alpha2_codes;

    /**
     * @brief Serialize request to bytes.
     *
     * Format:
     * - 4 bytes: count (number of alpha-2 codes)
     * - For each alpha-2 code:
     *   - 2 bytes: length
     *   - N bytes: alpha2_code (UTF-8)
     */
    std::vector<std::byte> serialize() const;

    /**
     * @brief Deserialize request from bytes.
     */
    static std::expected<delete_country_request, ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const delete_country_request& v);

/**
 * @brief Result for a single country deletion.
 */
struct delete_country_result final {
    std::string alpha2_code;
    bool success;
    std::string message;
};

std::ostream& operator<<(std::ostream& s, const delete_country_result& v);

/**
 * @brief Response confirming country deletion(s).
 *
 * Contains one result per requested country, indicating individual
 * success or failure. Supports partial success in batch operations.
 */
struct delete_country_response final {
    std::vector<delete_country_result> results;

    /**
     * @brief Serialize response to bytes.
     *
     * Format:
     * - 4 bytes: count (number of results)
     * - For each result:
     *   - 2 bytes: alpha2_code length
     *   - N bytes: alpha2_code (UTF-8)
     *   - 1 byte: success (0 or 1)
     *   - 2 bytes: message length
     *   - N bytes: message (UTF-8)
     */
    std::vector<std::byte> serialize() const;

    /**
     * @brief Deserialize response from bytes.
     */
    static std::expected<delete_country_response, ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const delete_country_response& v);

/**
 * @brief Request to retrieve version history for a country.
 */
struct get_country_history_request final {
    std::string alpha2_code;

    /**
     * @brief Serialize request to bytes.
     */
    std::vector<std::byte> serialize() const;

    /**
     * @brief Deserialize request from bytes.
     */
    static std::expected<get_country_history_request, ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const get_country_history_request& v);

/**
 * @brief Response containing country version history.
 */
struct get_country_history_response final {
    bool success;
    std::string message;
    std::vector<domain::country> history;

    /**
     * @brief Serialize response to bytes.
     */
    std::vector<std::byte> serialize() const;

    /**
     * @brief Deserialize response from bytes.
     */
    static std::expected<get_country_history_response, ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const get_country_history_response& v);

}

namespace ores::comms::messaging {

/**
 * @brief Message traits specialization for get_countries_request.
 */
template<>
struct message_traits<refdata::messaging::get_countries_request> {
    using request_type = refdata::messaging::get_countries_request;
    using response_type = refdata::messaging::get_countries_response;
    static constexpr message_type request_message_type =
        message_type::get_countries_request;
};

/**
 * @brief Message traits specialization for save_country_request.
 */
template<>
struct message_traits<refdata::messaging::save_country_request> {
    using request_type = refdata::messaging::save_country_request;
    using response_type = refdata::messaging::save_country_response;
    static constexpr message_type request_message_type =
        message_type::save_country_request;
};

/**
 * @brief Message traits specialization for delete_country_request.
 */
template<>
struct message_traits<refdata::messaging::delete_country_request> {
    using request_type = refdata::messaging::delete_country_request;
    using response_type = refdata::messaging::delete_country_response;
    static constexpr message_type request_message_type =
        message_type::delete_country_request;
};

/**
 * @brief Message traits specialization for get_country_history_request.
 */
template<>
struct message_traits<refdata::messaging::get_country_history_request> {
    using request_type = refdata::messaging::get_country_history_request;
    using response_type = refdata::messaging::get_country_history_response;
    static constexpr message_type request_message_type =
        message_type::get_country_history_request;
};

}

#endif
