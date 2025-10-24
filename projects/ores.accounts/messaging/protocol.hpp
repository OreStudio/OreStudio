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
#ifndef ORES_ACCOUNTS_MESSAGING_PROTOCOL_HPP
#define ORES_ACCOUNTS_MESSAGING_PROTOCOL_HPP

#if defined(_MSC_VER) && (_MSC_VER >= 1200)
#pragma once
#endif

#include <span>
#include <iosfwd>
#include <vector>
#include <cstdint>
#include <expected>
#include "ores.comms/protocol/message_types.hpp"
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
    std::string modified_by;
    bool is_admin;

    /**
     * @brief Serialize request to bytes.
     *
     * Format:
     * - 2 bytes: username length
     * - N bytes: username (UTF-8)
     * - 2 bytes: password_hash length
     * - N bytes: password_hash (UTF-8)
     * - 2 bytes: password_salt length
     * - N bytes: password_salt (UTF-8)
     * - 2 bytes: totp_secret length
     * - N bytes: totp_secret (UTF-8)
     * - 2 bytes: email length
     * - N bytes: email (UTF-8)
     * - 1 byte: is_admin (boolean)
     */
    std::vector<std::uint8_t> serialize() const;

    /**
     * @brief Deserialize request from bytes.
     */
    static std::expected<create_account_request, comms::protocol::error_code>
    deserialize(std::span<const std::uint8_t> data);
};

std::ostream& operator<<(std::ostream& s, const create_account_request& V);

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
    std::vector<std::uint8_t> serialize() const;

    /**
     * @brief Deserialize response from bytes.
     */
    static std::expected<create_account_response, comms::protocol::error_code>
    deserialize(std::span<const std::uint8_t> data);
};

std::ostream& operator<<(std::ostream& s, const create_account_response& v);

/**
 * @brief Request to retrieve all accounts.
 *
 * This request has no parameters - it retrieves all accounts in the system.
 */
struct list_accounts_request final {
    /**
     * @brief Serialize request to bytes.
     */
    std::vector<std::uint8_t> serialize() const;

    /**
     * @brief Deserialize request from bytes.
     */
    static std::expected<list_accounts_request, comms::protocol::error_code>
    deserialize(std::span<const std::uint8_t> data);
};

std::ostream& operator<<(std::ostream& s, const list_accounts_request& v);

/**
 * @brief Response containing all accounts.
 */
struct list_accounts_response final {
    std::vector<domain::account> accounts;

    /**
     * @brief Serialize response to bytes.
     *
     * Format:
     * - 4 bytes: count (number of accounts)
     * - For each account:
     *   - 4 bytes: version
     *   - 2 bytes: modified_by length
     *   - N bytes: modified_by (UTF-8)
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
    std::vector<std::uint8_t> serialize() const;

    /**
     * @brief Deserialize response from bytes.
     */
    static std::expected<list_accounts_response, comms::protocol::error_code>
    deserialize(std::span<const std::uint8_t> data);
};

std::ostream& operator<<(std::ostream& s, const list_accounts_response& v);

/**
 * @brief Request to authenticate a user.
 */
struct login_request final {
    std::string username;
    std::string password;

    /**
     * @brief Serialize request to bytes.
     *
     * Format:
     * - 2 bytes: username length
     * - N bytes: username (UTF-8)
     * - 2 bytes: password length
     * - N bytes: password (UTF-8)
     */
    std::vector<std::uint8_t> serialize() const;

    /**
     * @brief Deserialize request from bytes.
     */
    static std::expected<login_request, comms::protocol::error_code>
    deserialize(std::span<const std::uint8_t> data);
};

std::ostream& operator<<(std::ostream& s, const login_request& v);

/**
 * @brief Response containing authentication result and account information.
 */
struct login_response final {
    bool success;
    std::string error_message;
    boost::uuids::uuid account_id;
    std::string username;
    bool is_admin;

    /**
     * @brief Serialize response to bytes.
     *
     * Format:
     * - 1 byte: success (boolean)
     * - 2 bytes: error_message length
     * - N bytes: error_message (UTF-8)
     * - 16 bytes: account_id (UUID)
     * - 2 bytes: username length
     * - N bytes: username (UTF-8)
     * - 1 byte: is_admin (boolean)
     */
    std::vector<std::uint8_t> serialize() const;

    /**
     * @brief Deserialize response from bytes.
     */
    static std::expected<login_response, comms::protocol::error_code>
    deserialize(std::span<const std::uint8_t> data);
};

std::ostream& operator<<(std::ostream& s, const login_response& v);

/**
 * @brief Request to unlock a locked account.
 */
struct unlock_account_request final {
    boost::uuids::uuid account_id;

    /**
     * @brief Serialize request to bytes.
     *
     * Format:
     * - 16 bytes: account_id (UUID)
     */
    std::vector<std::uint8_t> serialize() const;

    /**
     * @brief Deserialize request from bytes.
     */
    static std::expected<unlock_account_request, comms::protocol::error_code>
    deserialize(std::span<const std::uint8_t> data);
};

std::ostream& operator<<(std::ostream& s, const unlock_account_request& v);

/**
 * @brief Response indicating whether unlock operation succeeded.
 */
struct unlock_account_response final {
    bool success;
    std::string error_message;

    /**
     * @brief Serialize response to bytes.
     *
     * Format:
     * - 1 byte: success (boolean)
     * - 2 bytes: error_message length
     * - N bytes: error_message (UTF-8)
     */
    std::vector<std::uint8_t> serialize() const;

    /**
     * @brief Deserialize response from bytes.
     */
    static std::expected<unlock_account_response, comms::protocol::error_code>
    deserialize(std::span<const std::uint8_t> data);
};

std::ostream& operator<<(std::ostream& s, const unlock_account_response& v);

}

#endif
