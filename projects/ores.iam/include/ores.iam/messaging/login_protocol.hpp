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
#ifndef ORES_IAM_MESSAGING_LOGIN_PROTOCOL_HPP
#define ORES_IAM_MESSAGING_LOGIN_PROTOCOL_HPP

#include <span>
#include <iosfwd>
#include <vector>
#include <expected>
#include <boost/asio/ip/address.hpp>
#include <boost/uuid/uuid.hpp>
#include "ores.comms/messaging/message_type.hpp"
#include "ores.utility/serialization/error_code.hpp"
#include "ores.utility/uuid/tenant_id.hpp"
#include "ores.comms/messaging/message_traits.hpp"
#include "ores.iam/domain/login_info.hpp"

namespace ores::iam::messaging {

/**
 * @brief Request to authenticate a user.
 *
 * The principal field identifies both the user and the tenant for multi-tenancy
 * support. The format follows the Kerberos principal convention.
 */
struct login_request final {
    /**
     * @brief User principal identifying the account and tenant.
     *
     * Format: `username@hostname` or just `username`
     *
     * The principal is parsed server-side to extract the username and tenant:
     * - If the principal contains `@`, everything before the last `@` is the
     *   username and everything after is the hostname used to resolve the tenant.
     * - If no `@` is present, the entire string is treated as the username and
     *   the system tenant (ffffffff-ffff-ffff-ffff-ffffffffffff) is used.
     *
     * Examples:
     * - `admin@localhost` - User "admin" in tenant with hostname "localhost"
     * - `admin@acme.example.com` - User "admin" in tenant "acme.example.com"
     * - `admin` - User "admin" in the system tenant
     */
    std::string principal;
    std::string password;

    /**
     * @brief Serialize request to bytes.
     *
     * Format:
     * - 2 bytes: principal length
     * - N bytes: principal (UTF-8)
     * - 2 bytes: password length
     * - N bytes: password (UTF-8)
     */
    std::vector<std::byte> serialize() const;

    /**
     * @brief Deserialize request from bytes.
     */
    static std::expected<login_request, ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const login_request& v);

/**
 * @brief Response containing authentication result and account information.
 *
 * Note: Administrative privileges are now determined via RBAC.
 * Use get_account_permissions_request to retrieve the user's permissions.
 */
struct login_response final {
    bool success = false;
    std::string error_message;
    boost::uuids::uuid account_id;
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();  ///< ID of authenticated tenant
    std::string tenant_name;       ///< Name of authenticated tenant
    std::string username;
    std::string email;
    bool password_reset_required = false;
    bool tenant_bootstrap_mode = false;  ///< True if tenant is in bootstrap mode (needs initial setup)

    /**
     * @brief Serialize response to bytes.
     *
     * Format:
     * - 1 byte: success (boolean)
     * - 2 bytes: error_message length
     * - N bytes: error_message (UTF-8)
     * - 16 bytes: account_id (UUID)
     * - 16 bytes: tenant_id (UUID)
     * - 2 bytes: tenant_name length
     * - N bytes: tenant_name (UTF-8)
     * - 2 bytes: username length
     * - N bytes: username (UTF-8)
     * - 2 bytes: email length
     * - N bytes: email (UTF-8)
     * - 1 byte: password_reset_required (boolean)
     * - 1 byte: tenant_bootstrap_mode (boolean)
     */
    std::vector<std::byte> serialize() const;

    /**
     * @brief Deserialize response from bytes.
     */
    static std::expected<login_response, ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const login_response& v);

/**
 * @brief Request to retrieve all login info records.
 */
struct list_login_info_request final {
    std::vector<std::byte> serialize() const;
    static std::expected<list_login_info_request, ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const list_login_info_request& v);

/**
 * @brief Response containing all login info records.
 */
struct list_login_info_response final {
    std::vector<domain::login_info> login_infos;

    std::vector<std::byte> serialize() const;
    static std::expected<list_login_info_response, ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const list_login_info_response& v);

/**
 * @brief Request to logout the current session.
 *
 * The server will logout the currently authenticated session. The account_id
 * is determined from the session context, not from the request payload.
 * This prevents clients from forging logout requests for other users.
 * After logout, login_info.online is set to false.
 */
struct logout_request final {
    /**
     * @brief Serialize request to bytes.
     *
     * Format: Empty (session context provides account info)
     */
    std::vector<std::byte> serialize() const;

    /**
     * @brief Deserialize request from bytes.
     */
    static std::expected<logout_request, ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const logout_request& v);

/**
 * @brief Response indicating logout result.
 */
struct logout_response final {
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
    static std::expected<logout_response, ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const logout_response& v);

}

namespace ores::comms::messaging {

/**
 * @brief Message traits specialization for login_request.
 */
template<>
struct message_traits<iam::messaging::login_request> {
    using request_type = iam::messaging::login_request;
    using response_type = iam::messaging::login_response;
    static constexpr message_type request_message_type =
        message_type::login_request;
};

/**
 * @brief Message traits specialization for list_login_info_request.
 */
template<>
struct message_traits<iam::messaging::list_login_info_request> {
    using request_type = iam::messaging::list_login_info_request;
    using response_type = iam::messaging::list_login_info_response;
    static constexpr message_type request_message_type =
        message_type::list_login_info_request;
};

/**
 * @brief Message traits specialization for logout_request.
 */
template<>
struct message_traits<iam::messaging::logout_request> {
    using request_type = iam::messaging::logout_request;
    using response_type = iam::messaging::logout_response;
    static constexpr message_type request_message_type =
        message_type::logout_request;
};

}

#endif
