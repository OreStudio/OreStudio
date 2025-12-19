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
#ifndef ORES_ACCOUNTS_MESSAGING_SIGNUP_PROTOCOL_HPP
#define ORES_ACCOUNTS_MESSAGING_SIGNUP_PROTOCOL_HPP

#include <span>
#include <iosfwd>
#include <vector>
#include <expected>
#include <boost/uuid/uuid.hpp>
#include "ores.comms/messaging/message_types.hpp"

namespace ores::accounts::messaging {

/**
 * @brief Request to create a new user account via self-registration.
 *
 * This message allows unauthenticated users to create their own accounts
 * when the system.user_signups feature flag is enabled. Unlike
 * create_account_request, this does not require admin privileges.
 *
 * The request will fail if:
 * - system.user_signups is disabled (signup_disabled error)
 * - system.signup_requires_authorization is enabled (not yet implemented)
 * - username is already taken (username_taken error)
 * - email is already in use (email_taken error)
 * - password does not meet policy requirements (weak_password error)
 */
struct signup_request final {
    std::string username;
    std::string email;
    std::string password;

    /**
     * @brief Serialize request to bytes.
     *
     * Format:
     * - 2 bytes: username length
     * - N bytes: username (UTF-8)
     * - 2 bytes: email length
     * - N bytes: email (UTF-8)
     * - 2 bytes: password length
     * - N bytes: password (UTF-8)
     */
    std::vector<std::byte> serialize() const;

    /**
     * @brief Deserialize request from bytes.
     */
    static std::expected<signup_request, comms::messaging::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const signup_request& v);

/**
 * @brief Response containing signup result.
 *
 * On success, contains the newly created account's ID and username.
 * On failure, contains an error message describing why signup failed.
 */
struct signup_response final {
    bool success = false;
    std::string error_message;
    boost::uuids::uuid account_id;
    std::string username;

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
     */
    std::vector<std::byte> serialize() const;

    /**
     * @brief Deserialize response from bytes.
     */
    static std::expected<signup_response, comms::messaging::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const signup_response& v);

}

#endif
