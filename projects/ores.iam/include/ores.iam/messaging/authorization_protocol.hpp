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
#ifndef ORES_ACCOUNTS_MESSAGING_AUTHORIZATION_PROTOCOL_HPP
#define ORES_ACCOUNTS_MESSAGING_AUTHORIZATION_PROTOCOL_HPP

#include <span>
#include <iosfwd>
#include <vector>
#include <expected>
#include <boost/uuid/uuid.hpp>
#include "ores.comms/messaging/message_types.hpp"
#include "ores.comms/messaging/message_traits.hpp"
#include "ores.iam/domain/role.hpp"
#include "ores.iam/domain/permission.hpp"

namespace ores::iam::messaging {

// ============================================================================
// List Roles
// ============================================================================

/**
 * @brief Request to list all roles in the system.
 */
struct list_roles_request final {
    /**
     * @brief Serialize request to bytes.
     *
     * Format: Empty payload.
     */
    std::vector<std::byte> serialize() const;

    /**
     * @brief Deserialize request from bytes.
     */
    static std::expected<list_roles_request, comms::messaging::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const list_roles_request& v);

/**
 * @brief Response containing all roles.
 */
struct list_roles_response final {
    std::vector<domain::role> roles;

    /**
     * @brief Serialize response to bytes.
     *
     * Format:
     * - 4 bytes: count (number of roles)
     * - For each role:
     *   - 4 bytes: version
     *   - 16 bytes: id (UUID)
     *   - 2 bytes: name length
     *   - N bytes: name (UTF-8)
     *   - 2 bytes: description length
     *   - N bytes: description (UTF-8)
     *   - 2 bytes: recorded_by length
     *   - N bytes: recorded_by (UTF-8)
     *   - 2 bytes: recorded_at length
     *   - N bytes: recorded_at (UTF-8)
     *   - 4 bytes: permission_codes count
     *   - For each permission code:
     *     - 2 bytes: code length
     *     - N bytes: code (UTF-8)
     */
    std::vector<std::byte> serialize() const;

    /**
     * @brief Deserialize response from bytes.
     */
    static std::expected<list_roles_response, comms::messaging::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const list_roles_response& v);

// ============================================================================
// List Permissions
// ============================================================================

/**
 * @brief Request to list all permissions in the system.
 */
struct list_permissions_request final {
    /**
     * @brief Serialize request to bytes.
     *
     * Format: Empty payload.
     */
    std::vector<std::byte> serialize() const;

    /**
     * @brief Deserialize request from bytes.
     */
    static std::expected<list_permissions_request, comms::messaging::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const list_permissions_request& v);

/**
 * @brief Response containing all permissions.
 */
struct list_permissions_response final {
    std::vector<domain::permission> permissions;

    /**
     * @brief Serialize response to bytes.
     *
     * Format:
     * - 4 bytes: count (number of permissions)
     * - For each permission:
     *   - 16 bytes: id (UUID)
     *   - 2 bytes: code length
     *   - N bytes: code (UTF-8)
     *   - 2 bytes: description length
     *   - N bytes: description (UTF-8)
     */
    std::vector<std::byte> serialize() const;

    /**
     * @brief Deserialize response from bytes.
     */
    static std::expected<list_permissions_response, comms::messaging::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const list_permissions_response& v);

// ============================================================================
// Assign Role
// ============================================================================

/**
 * @brief Request to assign a role to an account.
 *
 * Requires admin privileges or roles:assign permission.
 */
struct assign_role_request final {
    boost::uuids::uuid account_id;
    boost::uuids::uuid role_id;

    /**
     * @brief Serialize request to bytes.
     *
     * Format:
     * - 16 bytes: account_id (UUID)
     * - 16 bytes: role_id (UUID)
     */
    std::vector<std::byte> serialize() const;

    /**
     * @brief Deserialize request from bytes.
     */
    static std::expected<assign_role_request, comms::messaging::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const assign_role_request& v);

/**
 * @brief Response indicating whether role assignment succeeded.
 */
struct assign_role_response final {
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
    static std::expected<assign_role_response, comms::messaging::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const assign_role_response& v);

// ============================================================================
// Revoke Role
// ============================================================================

/**
 * @brief Request to revoke a role from an account.
 *
 * Requires admin privileges or roles:revoke permission.
 */
struct revoke_role_request final {
    boost::uuids::uuid account_id;
    boost::uuids::uuid role_id;

    /**
     * @brief Serialize request to bytes.
     *
     * Format:
     * - 16 bytes: account_id (UUID)
     * - 16 bytes: role_id (UUID)
     */
    std::vector<std::byte> serialize() const;

    /**
     * @brief Deserialize request from bytes.
     */
    static std::expected<revoke_role_request, comms::messaging::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const revoke_role_request& v);

/**
 * @brief Response indicating whether role revocation succeeded.
 */
struct revoke_role_response final {
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
    static std::expected<revoke_role_response, comms::messaging::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const revoke_role_response& v);

// ============================================================================
// Get Account Roles
// ============================================================================

/**
 * @brief Request to get all roles assigned to an account.
 */
struct get_account_roles_request final {
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
    static std::expected<get_account_roles_request, comms::messaging::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const get_account_roles_request& v);

/**
 * @brief Response containing roles assigned to an account.
 */
struct get_account_roles_response final {
    std::vector<domain::role> roles;

    /**
     * @brief Serialize response to bytes.
     *
     * Format: Same as list_roles_response.
     */
    std::vector<std::byte> serialize() const;

    /**
     * @brief Deserialize response from bytes.
     */
    static std::expected<get_account_roles_response, comms::messaging::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const get_account_roles_response& v);

// ============================================================================
// Get Account Permissions
// ============================================================================

/**
 * @brief Request to get effective permissions for an account.
 */
struct get_account_permissions_request final {
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
    static std::expected<get_account_permissions_request, comms::messaging::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const get_account_permissions_request& v);

/**
 * @brief Response containing effective permissions for an account.
 */
struct get_account_permissions_response final {
    std::vector<std::string> permission_codes;

    /**
     * @brief Serialize response to bytes.
     *
     * Format:
     * - 4 bytes: count (number of permission codes)
     * - For each permission code:
     *   - 2 bytes: code length
     *   - N bytes: code (UTF-8)
     */
    std::vector<std::byte> serialize() const;

    /**
     * @brief Deserialize response from bytes.
     */
    static std::expected<get_account_permissions_response, comms::messaging::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const get_account_permissions_response& v);

}

namespace ores::comms::messaging {

/**
 * @brief Message traits specialization for list_roles_request.
 */
template<>
struct message_traits<iam::messaging::list_roles_request> {
    using request_type = iam::messaging::list_roles_request;
    using response_type = iam::messaging::list_roles_response;
    static constexpr message_type request_message_type =
        message_type::list_roles_request;
};

/**
 * @brief Message traits specialization for list_permissions_request.
 */
template<>
struct message_traits<iam::messaging::list_permissions_request> {
    using request_type = iam::messaging::list_permissions_request;
    using response_type = iam::messaging::list_permissions_response;
    static constexpr message_type request_message_type =
        message_type::list_permissions_request;
};

/**
 * @brief Message traits specialization for assign_role_request.
 */
template<>
struct message_traits<iam::messaging::assign_role_request> {
    using request_type = iam::messaging::assign_role_request;
    using response_type = iam::messaging::assign_role_response;
    static constexpr message_type request_message_type =
        message_type::assign_role_request;
};

/**
 * @brief Message traits specialization for revoke_role_request.
 */
template<>
struct message_traits<iam::messaging::revoke_role_request> {
    using request_type = iam::messaging::revoke_role_request;
    using response_type = iam::messaging::revoke_role_response;
    static constexpr message_type request_message_type =
        message_type::revoke_role_request;
};

/**
 * @brief Message traits specialization for get_account_roles_request.
 */
template<>
struct message_traits<iam::messaging::get_account_roles_request> {
    using request_type = iam::messaging::get_account_roles_request;
    using response_type = iam::messaging::get_account_roles_response;
    static constexpr message_type request_message_type =
        message_type::get_account_roles_request;
};

/**
 * @brief Message traits specialization for get_account_permissions_request.
 */
template<>
struct message_traits<iam::messaging::get_account_permissions_request> {
    using request_type = iam::messaging::get_account_permissions_request;
    using response_type = iam::messaging::get_account_permissions_response;
    static constexpr message_type request_message_type =
        message_type::get_account_permissions_request;
};

}

#endif
