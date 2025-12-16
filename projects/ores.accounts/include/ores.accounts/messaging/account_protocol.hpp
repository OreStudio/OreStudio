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
#ifndef ORES_ACCOUNTS_MESSAGING_ACCOUNT_PROTOCOL_HPP
#define ORES_ACCOUNTS_MESSAGING_ACCOUNT_PROTOCOL_HPP

#include <span>
#include <iosfwd>
#include <vector>
#include <expected>
#include <boost/uuid/uuid.hpp>
#include "ores.comms/messaging/message_types.hpp"
#include "ores.accounts/domain/account.hpp"

namespace ores::accounts::messaging {

/**
 * @brief Request to create a new account.
 */
struct create_account_request final {
    std::string username;
    std::string password;
    std::string totp_secret;
    std::string email;
    std::string recorded_by;
    bool is_admin = false;

    /**
     * @brief Serialize request to bytes.
     *
     * Format:
     * - 2 bytes: username length
     * - N bytes: username (UTF-8)
     * - 2 bytes: password length
     * - N bytes: password (UTF-8)
     * - 2 bytes: totp_secret length
     * - N bytes: totp_secret (UTF-8)
     * - 2 bytes: email length
     * - N bytes: email (UTF-8)
     * - 2 bytes: recorded_by length
     * - N bytes: recorded_by (UTF-8)
     * - 1 byte: is_admin (boolean)
     */
    std::vector<std::byte> serialize() const;

    /**
     * @brief Deserialize request from bytes.
     */
    static std::expected<create_account_request, comms::messaging::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const create_account_request& v);

/**
 * @brief Response containing the created account ID.
 */
struct create_account_response final {
    boost::uuids::uuid account_id;

    /**
     * @brief Serialize response to bytes.
     *
     * Format:
     * - 16 bytes: account_id (UUID)
     */
    std::vector<std::byte> serialize() const;

    /**
     * @brief Deserialize response from bytes.
     */
    static std::expected<create_account_response, comms::messaging::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const create_account_response& v);

/**
 * @brief Request to retrieve accounts with pagination support.
 *
 * Supports pagination through offset and limit parameters.
 */
struct list_accounts_request final {
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
    static std::expected<list_accounts_request, comms::messaging::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const list_accounts_request& v);

/**
 * @brief Response containing accounts with pagination metadata.
 */
struct list_accounts_response final {
    std::vector<domain::account> accounts;
    /// Total number of accounts available (not just in this page)
    std::uint32_t total_available_count = 0;

    /**
     * @brief Serialize response to bytes.
     *
     * Format:
     * - 4 bytes: total_available_count (uint32_t)
     * - 4 bytes: count (number of accounts in this response)
     * - For each account:
     *   - 4 bytes: version
     *   - 2 bytes: recorded_by length
     *   - N bytes: recorded_by (UTF-8)
     *   - 16 bytes: id (UUID)
     *   - 2 bytes: username length
     *   - N bytes: username (UTF-8)
     *   - 2 bytes: password_hash length
     *   - N bytes: password_hash (UTF-8)
     *   - 2 bytes: password_salt length
     *   - N bytes: password_salt (UTF-8)
     *   - 2 bytes: totp_secret length
     *   - N bytes: totp_secret (UTF-8)
     *   - 2 bytes: email length
     *   - N bytes: email (UTF-8)
     *   - 1 byte: is_admin (boolean)
     */
    std::vector<std::byte> serialize() const;

    /**
     * @brief Deserialize response from bytes.
     */
    static std::expected<list_accounts_response, comms::messaging::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const list_accounts_response& v);

/**
 * @brief Request to unlock one or more locked accounts.
 *
 * Supports batch operations by accepting a vector of account IDs.
 * Each account is processed independently - partial success is possible.
 * Requires admin privileges.
 */
struct unlock_account_request final {
    std::vector<boost::uuids::uuid> account_ids;

    /**
     * @brief Serialize request to bytes.
     *
     * Format:
     * - 4 bytes: count (number of account IDs)
     * - For each account:
     *   - 16 bytes: account_id (UUID)
     */
    std::vector<std::byte> serialize() const;

    /**
     * @brief Deserialize request from bytes.
     */
    static std::expected<unlock_account_request, comms::messaging::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const unlock_account_request& v);

/**
 * @brief Result for a single account unlock operation.
 */
struct unlock_account_result final {
    boost::uuids::uuid account_id;
    bool success = false;
    std::string message;
};

std::ostream& operator<<(std::ostream& s, const unlock_account_result& v);

/**
 * @brief Response containing results for unlock operation(s).
 *
 * Contains one result per requested account, indicating individual
 * success or failure. Supports partial success in batch operations.
 */
struct unlock_account_response final {
    std::vector<unlock_account_result> results;

    /**
     * @brief Serialize response to bytes.
     *
     * Format:
     * - 4 bytes: count (number of results)
     * - For each result:
     *   - 16 bytes: account_id (UUID)
     *   - 1 byte: success (boolean)
     *   - 2 bytes: message length
     *   - N bytes: message (UTF-8)
     */
    std::vector<std::byte> serialize() const;

    /**
     * @brief Deserialize response from bytes.
     */
    static std::expected<unlock_account_response, comms::messaging::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const unlock_account_response& v);

/**
 * @brief Request to delete an account.
 */
struct delete_account_request final {
    boost::uuids::uuid account_id;

    /**
     * @brief Serialize request to bytes.
     *
     * Format:
     * - 16 bytes: account_id (UUID)
     */
    std::vector<std::byte> serialize() const;

    /**
     * @brief Deserialize request from bytes.
     */
    static std::expected<delete_account_request, comms::messaging::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const delete_account_request& v);

/**
 * @brief Response confirming account deletion.
 */
struct delete_account_response final {
    bool success = false;
    std::string message;

    /**
     * @brief Serialize response to bytes.
     *
     * Format:
     * - 1 byte: success (boolean)
     * - 2 bytes: message length
     * - N bytes: message (UTF-8)
     */
    std::vector<std::byte> serialize() const;

    /**
     * @brief Deserialize response from bytes.
     */
    static std::expected<delete_account_response, comms::messaging::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const delete_account_response& v);

/**
 * @brief Request to lock one or more accounts.
 *
 * Supports batch operations by accepting a vector of account IDs.
 * Each account is processed independently - partial success is possible.
 * Requires admin privileges.
 */
struct lock_account_request final {
    std::vector<boost::uuids::uuid> account_ids;

    /**
     * @brief Serialize request to bytes.
     *
     * Format:
     * - 4 bytes: count (number of account IDs)
     * - For each account:
     *   - 16 bytes: account_id (UUID)
     */
    std::vector<std::byte> serialize() const;

    /**
     * @brief Deserialize request from bytes.
     */
    static std::expected<lock_account_request, comms::messaging::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const lock_account_request& v);

/**
 * @brief Result for a single account lock operation.
 */
struct lock_account_result final {
    boost::uuids::uuid account_id;
    bool success = false;
    std::string message;
};

std::ostream& operator<<(std::ostream& s, const lock_account_result& v);

/**
 * @brief Response containing results for lock operation(s).
 *
 * Contains one result per requested account, indicating individual
 * success or failure. Supports partial success in batch operations.
 */
struct lock_account_response final {
    std::vector<lock_account_result> results;

    /**
     * @brief Serialize response to bytes.
     *
     * Format:
     * - 4 bytes: count (number of results)
     * - For each result:
     *   - 16 bytes: account_id (UUID)
     *   - 1 byte: success (boolean)
     *   - 2 bytes: message length
     *   - N bytes: message (UTF-8)
     */
    std::vector<std::byte> serialize() const;

    /**
     * @brief Deserialize response from bytes.
     */
    static std::expected<lock_account_response, comms::messaging::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const lock_account_response& v);

/**
 * @brief Request to update an existing account.
 *
 * Only email and is_admin fields can be updated. Username cannot be changed.
 * Requires admin privileges.
 */
struct update_account_request final {
    boost::uuids::uuid account_id;
    std::string email;
    std::string recorded_by;
    bool is_admin = false;

    /**
     * @brief Serialize request to bytes.
     *
     * Format:
     * - 16 bytes: account_id (UUID)
     * - 2 bytes: email length
     * - N bytes: email (UTF-8)
     * - 2 bytes: recorded_by length
     * - N bytes: recorded_by (UTF-8)
     * - 1 byte: is_admin (boolean)
     */
    std::vector<std::byte> serialize() const;

    /**
     * @brief Deserialize request from bytes.
     */
    static std::expected<update_account_request, comms::messaging::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const update_account_request& v);

/**
 * @brief Response indicating whether update operation succeeded.
 */
struct update_account_response final {
    bool success = false;
    std::string error_message;

    /**
     * @brief Serialize response to bytes.
     *
     * Format:
     * - 1 byte: success (boolean)
     * - 2 bytes: error_message length
     * - N bytes: error_message (UTF-8)
     */
    std::vector<std::byte> serialize() const;

    /**
     * @brief Deserialize response from bytes.
     */
    static std::expected<update_account_response, comms::messaging::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const update_account_response& v);

}

#endif
