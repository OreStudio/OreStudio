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
#ifndef ORES_RISK_MESSAGING_PROTOCOL_HPP
#define ORES_RISK_MESSAGING_PROTOCOL_HPP

#include <span>
#include <iosfwd>
#include <vector>
#include <cstdint>
#include <expected>
#include <rfl.hpp>
#include <rfl/json.hpp>
#include "ores.comms/protocol/message_types.hpp"
#include "ores.risk/domain/currency.hpp"
#include "ores.risk/domain/currency_version.hpp"
#include "ores.risk/domain/currency_version_history.hpp"

namespace ores::risk::messaging {

/**
 * @brief Request to retrieve all currencies.
 *
 * This request has no parameters - it retrieves all currencies in the system.
 */
struct get_currencies_request final {
    /**
     * @brief Serialize request to bytes.
     */
    std::vector<std::uint8_t> serialize() const;

    /**
     * @brief Deserialize request from bytes.
     */
    static std::expected<get_currencies_request, comms::protocol::error_code>
    deserialize(std::span<const std::uint8_t> data);
};

std::ostream& operator<<(std::ostream& s, const get_currencies_request& v);

/**
 * @brief Response containing all currencies.
 */
struct get_currencies_response final {
    std::vector<domain::currency> currencies;

    /**
     * @brief Serialize response to bytes.
     *
     * Format:
     * - 4 bytes: count (number of currencies)
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
     *   - 2 bytes: currency_type length
     *   - N bytes: currency_type (UTF-8)
     *   - 2 bytes: modified_by length
     *   - N bytes: modified_by (UTF-8)
     *   - 2 bytes: valid_from length
     *   - N bytes: valid_from (UTF-8)
     *   - 2 bytes: valid_to length
     *   - N bytes: valid_to (UTF-8)
     */
    std::vector<std::uint8_t> serialize() const;

    /**
     * @brief Deserialize response from bytes.
     */
    static std::expected<get_currencies_response, comms::protocol::error_code>
    deserialize(std::span<const std::uint8_t> data);
};

std::ostream& operator<<(std::ostream& s, const get_currencies_response& v);

/**
 * @brief Request to save a currency (create or update).
 *
 * Due to bitemporal storage, both create and update operations
 * result in writing a new record. Database triggers handle temporal
 * versioning automatically.
 */
struct save_currency_request final {
    domain::currency currency;

    /**
     * @brief Serialize request to bytes.
     */
    std::vector<std::uint8_t> serialize() const;

    /**
     * @brief Deserialize request from bytes.
     */
    static std::expected<save_currency_request, comms::protocol::error_code>
    deserialize(std::span<const std::uint8_t> data);
};

std::ostream& operator<<(std::ostream& s, const save_currency_request& v);

/**
 * @brief Response confirming currency save operation.
 */
struct save_currency_response final {
    bool success;
    std::string message;

    /**
     * @brief Serialize response to bytes.
     */
    std::vector<std::uint8_t> serialize() const;

    /**
     * @brief Deserialize response from bytes.
     */
    static std::expected<save_currency_response, comms::protocol::error_code>
    deserialize(std::span<const std::uint8_t> data);
};

std::ostream& operator<<(std::ostream& s, const save_currency_response& v);

/**
 * @brief Request to delete a currency.
 */
struct delete_currency_request final {
    std::string iso_code;

    /**
     * @brief Serialize request to bytes.
     */
    std::vector<std::uint8_t> serialize() const;

    /**
     * @brief Deserialize request from bytes.
     */
    static std::expected<delete_currency_request, comms::protocol::error_code>
    deserialize(std::span<const std::uint8_t> data);
};

std::ostream& operator<<(std::ostream& s, const delete_currency_request& v);

/**
 * @brief Response confirming currency deletion.
 */
struct delete_currency_response final {
    bool success;
    std::string message;

    /**
     * @brief Serialize response to bytes.
     */
    std::vector<std::uint8_t> serialize() const;

    /**
     * @brief Deserialize response from bytes.
     */
    static std::expected<delete_currency_response, comms::protocol::error_code>
    deserialize(std::span<const std::uint8_t> data);
};

std::ostream& operator<<(std::ostream& s, const delete_currency_response& v);

/**
 * @brief Request to retrieve version history for a currency.
 */
struct get_currency_history_request final {
    std::string iso_code;

    /**
     * @brief Serialize request to bytes.
     */
    std::vector<std::uint8_t> serialize() const;

    /**
     * @brief Deserialize request from bytes.
     */
    static std::expected<get_currency_history_request, comms::protocol::error_code>
    deserialize(std::span<const std::uint8_t> data);
};

std::ostream& operator<<(std::ostream& s, const get_currency_history_request& v);

/**
 * @brief Response containing currency version history.
 */
struct get_currency_history_response final {
    bool success;
    std::string message;
    domain::currency_version_history history;

    /**
     * @brief Serialize response to bytes.
     */
    std::vector<std::uint8_t> serialize() const;

    /**
     * @brief Deserialize response from bytes.
     */
    static std::expected<get_currency_history_response, comms::protocol::error_code>
    deserialize(std::span<const std::uint8_t> data);
};

std::ostream& operator<<(std::ostream& s, const get_currency_history_response& v);

}

#endif
