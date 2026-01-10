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
#ifndef ORES_IAM_MESSAGING_ACCOUNT_PROTOCOL_HPP
#define ORES_IAM_MESSAGING_ACCOUNT_PROTOCOL_HPP

#include <span>
#include <iosfwd>
#include <vector>
#include <expected>
#include <boost/uuid/uuid.hpp>
#include "ores.comms/messaging/message_types.hpp"
#include "ores.comms/messaging/message_traits.hpp"
#include "ores.iam/domain/account.hpp"

namespace ores::iam::messaging {

/**
 * @brief Request to save an account (create or update).
 *
 * For creates: leave account_id as nil UUID, provide username and password.
 * For updates: set account_id to the existing account, password is optional.
 *
 * Note: Administrative privileges are managed through RBAC roles.
 * Use assign_role_request to grant roles after account creation.
 */
struct save_account_request final {
    boost::uuids::uuid account_id;  // Nil for create, valid UUID for update
    std::string username;
    std::string password;      // Plain text for new accounts, optional for updates
    std::string totp_secret;
    std::string email;
    std::string recorded_by;
    std::string change_reason_code;
    std::string change_commentary;

    /**
     * @brief Serialize request to bytes.
     *
     * Format:
     * - 16 bytes: account_id (UUID, nil for create)
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
     * - 2 bytes: change_reason_code length
     * - N bytes: change_reason_code (UTF-8)
     * - 2 bytes: change_commentary length
     * - N bytes: change_commentary (UTF-8)
     */
    std::vector<std::byte> serialize() const;

    /**
     * @brief Deserialize request from bytes.
     */
    static std::expected<save_account_request, ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const save_account_request& v);

/**
 * @brief Response from saving an account.
 */
struct save_account_response final {
    bool success = false;
    std::string message;
    boost::uuids::uuid account_id;  // ID of created/updated account

    /**
     * @brief Serialize response to bytes.
     *
     * Format:
     * - 1 byte: success (boolean)
     * - 2 bytes: message length
     * - N bytes: message (UTF-8)
     * - 16 bytes: account_id (UUID)
     */
    std::vector<std::byte> serialize() const;

    /**
     * @brief Deserialize response from bytes.
     */
    static std::expected<save_account_response, ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const save_account_response& v);

/**
 * @brief Request to retrieve accounts with pagination support.
 *
 * Supports pagination through offset and limit parameters.
 */
struct get_accounts_request final {
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
    static std::expected<get_accounts_request, ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const get_accounts_request& v);

/**
 * @brief Response containing accounts with pagination metadata.
 */
struct get_accounts_response final {
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
     *   - 8 bytes: recorded_at (nanoseconds since epoch)
     *   - 2 bytes: change_reason_code length
     *   - N bytes: change_reason_code (UTF-8)
     *   - 2 bytes: change_commentary length
     *   - N bytes: change_commentary (UTF-8)
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
     */
    std::vector<std::byte> serialize() const;

    /**
     * @brief Deserialize response from bytes.
     */
    static std::expected<get_accounts_response, ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const get_accounts_response& v);

/**
 * @brief Request to unlock one or more locked accounts.
 *
 * Supports batch operations by accepting a vector of account IDs.
 * Each account is processed independently - partial success is possible.
 * Requires accounts:unlock permission.
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
    static std::expected<unlock_account_request, ores::utility::serialization::error_code>
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
    static std::expected<unlock_account_response, ores::utility::serialization::error_code>
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
    static std::expected<delete_account_request, ores::utility::serialization::error_code>
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
    static std::expected<delete_account_response, ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const delete_account_response& v);

/**
 * @brief Request to lock one or more accounts.
 *
 * Supports batch operations by accepting a vector of account IDs.
 * Each account is processed independently - partial success is possible.
 * Requires accounts:lock permission.
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
    static std::expected<lock_account_request, ores::utility::serialization::error_code>
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
    static std::expected<lock_account_response, ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const lock_account_response& v);

/**
 * @brief Request to reset password for one or more accounts.
 *
 * Sets the password_reset_required flag on the target accounts, forcing
 * the user to change their password on next login. Supports batch operations.
 * Requires accounts:reset_password permission.
 */
struct reset_password_request final {
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
    static std::expected<reset_password_request, ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const reset_password_request& v);

/**
 * @brief Result for a single password reset operation.
 */
struct reset_password_result final {
    boost::uuids::uuid account_id;
    bool success = false;
    std::string message;
};

std::ostream& operator<<(std::ostream& s, const reset_password_result& v);

/**
 * @brief Response containing results for password reset operation(s).
 *
 * Contains one result per requested account, indicating individual
 * success or failure. Supports partial success in batch operations.
 */
struct reset_password_response final {
    std::vector<reset_password_result> results;

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
    static std::expected<reset_password_response, ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const reset_password_response& v);

/**
 * @brief Request to change the current user's password.
 *
 * Used by users to change their own password, typically after being required
 * to reset it. The account ID is determined from the session context.
 */
struct change_password_request final {
    std::string new_password;

    /**
     * @brief Serialize request to bytes.
     *
     * Format:
     * - 2 bytes: new_password length
     * - N bytes: new_password (UTF-8)
     */
    std::vector<std::byte> serialize() const;

    /**
     * @brief Deserialize request from bytes.
     */
    static std::expected<change_password_request, ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const change_password_request& v);

/**
 * @brief Response indicating whether password change succeeded.
 */
struct change_password_response final {
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
    static std::expected<change_password_response, ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const change_password_response& v);

/**
 * @brief Request to update the current user's email address.
 *
 * Used by users to change their own email. The account ID is determined
 * from the session context.
 */
struct update_my_email_request final {
    std::string new_email;

    /**
     * @brief Serialize request to bytes.
     *
     * Format:
     * - 2 bytes: new_email length
     * - N bytes: new_email (UTF-8)
     */
    std::vector<std::byte> serialize() const;

    /**
     * @brief Deserialize request from bytes.
     */
    static std::expected<update_my_email_request, ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const update_my_email_request& v);

/**
 * @brief Response indicating whether email update succeeded.
 */
struct update_my_email_response final {
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
    static std::expected<update_my_email_response, ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const update_my_email_response& v);

}

namespace ores::comms::messaging {

/**
 * @brief Message traits specialization for save_account_request.
 */
template<>
struct message_traits<iam::messaging::save_account_request> {
    using request_type = iam::messaging::save_account_request;
    using response_type = iam::messaging::save_account_response;
    static constexpr message_type request_message_type =
        message_type::save_account_request;
};

/**
 * @brief Message traits specialization for get_accounts_request.
 */
template<>
struct message_traits<iam::messaging::get_accounts_request> {
    using request_type = iam::messaging::get_accounts_request;
    using response_type = iam::messaging::get_accounts_response;
    static constexpr message_type request_message_type =
        message_type::get_accounts_request;
};

/**
 * @brief Message traits specialization for unlock_account_request.
 */
template<>
struct message_traits<iam::messaging::unlock_account_request> {
    using request_type = iam::messaging::unlock_account_request;
    using response_type = iam::messaging::unlock_account_response;
    static constexpr message_type request_message_type =
        message_type::unlock_account_request;
};

/**
 * @brief Message traits specialization for delete_account_request.
 */
template<>
struct message_traits<iam::messaging::delete_account_request> {
    using request_type = iam::messaging::delete_account_request;
    using response_type = iam::messaging::delete_account_response;
    static constexpr message_type request_message_type =
        message_type::delete_account_request;
};

/**
 * @brief Message traits specialization for lock_account_request.
 */
template<>
struct message_traits<iam::messaging::lock_account_request> {
    using request_type = iam::messaging::lock_account_request;
    using response_type = iam::messaging::lock_account_response;
    static constexpr message_type request_message_type =
        message_type::lock_account_request;
};

/**
 * @brief Message traits specialization for reset_password_request.
 */
template<>
struct message_traits<iam::messaging::reset_password_request> {
    using request_type = iam::messaging::reset_password_request;
    using response_type = iam::messaging::reset_password_response;
    static constexpr message_type request_message_type =
        message_type::reset_password_request;
};

/**
 * @brief Message traits specialization for change_password_request.
 */
template<>
struct message_traits<iam::messaging::change_password_request> {
    using request_type = iam::messaging::change_password_request;
    using response_type = iam::messaging::change_password_response;
    static constexpr message_type request_message_type =
        message_type::change_password_request;
};

/**
 * @brief Message traits specialization for update_my_email_request.
 */
template<>
struct message_traits<iam::messaging::update_my_email_request> {
    using request_type = iam::messaging::update_my_email_request;
    using response_type = iam::messaging::update_my_email_response;
    static constexpr message_type request_message_type =
        message_type::update_my_email_request;
};

}

#endif
