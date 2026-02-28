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
#ifndef ORES_REFDATA_MESSAGING_CURRENCY_PROTOCOL_HPP
#define ORES_REFDATA_MESSAGING_CURRENCY_PROTOCOL_HPP

#include <span>
#include <iosfwd>
#include <vector>
#include <cstdint>
#include <expected>
#include <rfl.hpp>
#include <rfl/json.hpp>
#include "ores.comms/messaging/message_type.hpp"
#include "ores.comms/messaging/save_result.hpp"
#include "ores.utility/serialization/error_code.hpp"
#include "ores.comms/messaging/message_traits.hpp"
#include "ores.refdata/domain/currency.hpp"

namespace ores::refdata::messaging {

/**
 * @brief Request to retrieve currencies with pagination support.
 *
 * Supports pagination through offset and limit parameters.
 */
struct get_currencies_request final {
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
    static std::expected<get_currencies_request, ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const get_currencies_request& v);

/**
 * @brief Response containing currencies with pagination metadata.
 */
struct get_currencies_response final {
    std::vector<domain::currency> currencies;
    /// Total number of currencies available (not just in this page)
    std::uint32_t total_available_count = 0;

    /**
     * @brief Serialize response to bytes.
     *
     * Format:
     * - 4 bytes: total_available_count (uint32_t)
     * - 4 bytes: count (number of currencies in this response)
     * - For each currency:
     *   - 2 bytes: iso_code length
     *   - N bytes: iso_code (UTF-8)
     *   - 2 bytes: name length
     *   - N bytes: name (UTF-8)
     *   - 2 bytes: numeric_code length
     *   - N bytes: numeric_code (UTF-8)
     *   - 2 bytes: symbol length
     *   - N bytes: symbol (UTF-8)
     *   - 2 bytes: fraction_symbol length
     *   - N bytes: fraction_symbol (UTF-8)
     *   - 4 bytes: fractions_per_unit (int)
     *   - 2 bytes: rounding_type length
     *   - N bytes: rounding_type (UTF-8)
     *   - 4 bytes: rounding_precision (int)
     *   - 2 bytes: format length
     *   - N bytes: format (UTF-8)
     *   - 2 bytes: asset_class length
     *   - N bytes: asset_class (UTF-8)
     *   - 2 bytes: market_tier length
     *   - N bytes: market_tier (UTF-8)
     *   - 2 bytes: recorded_at length
     *   - N bytes: recorded_at (UTF-8)
     */
    std::vector<std::byte> serialize() const;

    /**
     * @brief Deserialize response from bytes.
     */
    static std::expected<get_currencies_response, ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const get_currencies_response& v);

/**
 * @brief Request to save one or more currencies (create or update).
 *
 * Due to bitemporal storage, both create and update operations
 * result in writing a new record. Database triggers handle temporal
 * versioning automatically. All entities in the batch are saved in a
 * single DB transaction — all succeed or all fail.
 */
struct save_currency_request final {
    std::vector<domain::currency> currencies;

    /**
     * @brief Factory for single-entity saves (existing callers).
     */
    static save_currency_request from(domain::currency currency);

    /**
     * @brief Factory for batch saves.
     */
    static save_currency_request from(std::vector<domain::currency> currencies);

    /**
     * @brief Serialize request to bytes.
     */
    std::vector<std::byte> serialize() const;

    /**
     * @brief Deserialize request from bytes.
     */
    static std::expected<save_currency_request, ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const save_currency_request& v);

/**
 * @brief Response confirming currency save operation.
 */
struct save_currency_response final {
    bool success = false;
    std::string message;

    /**
     * @brief Serialize response to bytes.
     */
    std::vector<std::byte> serialize() const;

    /**
     * @brief Deserialize response from bytes.
     */
    static std::expected<save_currency_response, ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const save_currency_response& v);

/**
 * @brief Request to delete one or more currencies.
 *
 * Supports batch deletion by accepting a vector of ISO codes.
 * Each currency is processed independently - partial success is possible.
 */
struct delete_currency_request final {
    std::vector<std::string> iso_codes;

    /**
     * @brief Serialize request to bytes.
     *
     * Format:
     * - 4 bytes: count (number of ISO codes)
     * - For each ISO code:
     *   - 2 bytes: length
     *   - N bytes: iso_code (UTF-8)
     */
    std::vector<std::byte> serialize() const;

    /**
     * @brief Deserialize request from bytes.
     */
    static std::expected<delete_currency_request, ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const delete_currency_request& v);

/**
 * @brief Response confirming currency deletion(s).
 *
 * Contains one result per requested currency, indicating individual
 * success or failure. Supports partial success in batch operations.
 */
struct delete_currency_response final {
    bool success = false;
    std::string message;

    /**
     * @brief Serialize response to bytes.
     *
     * Format:
     * - 4 bytes: count (number of results)
     * - For each result:
     *   - 2 bytes: iso_code length
     *   - N bytes: iso_code (UTF-8)
     *   - 1 byte: success (0 or 1)
     *   - 2 bytes: message length
     *   - N bytes: message (UTF-8)
     */
    std::vector<std::byte> serialize() const;

    /**
     * @brief Deserialize response from bytes.
     */
    static std::expected<delete_currency_response, ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const delete_currency_response& v);

}

namespace ores::comms::messaging {

/**
 * @brief Message traits specialization for get_currencies_request.
 */
template<>
struct message_traits<refdata::messaging::get_currencies_request> {
    using request_type = refdata::messaging::get_currencies_request;
    using response_type = refdata::messaging::get_currencies_response;
    static constexpr message_type request_message_type =
        message_type::get_currencies_request;
};

/**
 * @brief Message traits specialization for save_currency_request.
 */
template<>
struct message_traits<refdata::messaging::save_currency_request> {
    using request_type = refdata::messaging::save_currency_request;
    using response_type = refdata::messaging::save_currency_response;
    static constexpr message_type request_message_type =
        message_type::save_currency_request;
};

/**
 * @brief Message traits specialization for delete_currency_request.
 */
template<>
struct message_traits<refdata::messaging::delete_currency_request> {
    using request_type = refdata::messaging::delete_currency_request;
    using response_type = refdata::messaging::delete_currency_response;
    static constexpr message_type request_message_type =
        message_type::delete_currency_request;
};

}

#endif
