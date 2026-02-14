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
#ifndef ORES_IAM_MESSAGING_AUTHORIZATION_PROTOCOL_HPP
#define ORES_IAM_MESSAGING_AUTHORIZATION_PROTOCOL_HPP

#include <span>
#include <iosfwd>
#include <vector>
#include <optional>
#include <expected>
#include <boost/uuid/uuid.hpp>
#include "ores.comms/messaging/message_type.hpp"
#include "ores.utility/serialization/error_code.hpp"
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
    static std::expected<list_roles_request, ores::utility::serialization::error_code>
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
     *   - 4 bytes: permission_codes count
     *   - For each permission code:
     *     - 2 bytes: code length
     *     - N bytes: code (UTF-8)
     */
    std::vector<std::byte> serialize() const;

    /**
     * @brief Deserialize response from bytes.
     */
    static std::expected<list_roles_response, ores::utility::serialization::error_code>
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
    static std::expected<list_permissions_request, ores::utility::serialization::error_code>
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
    static std::expected<list_permissions_response, ores::utility::serialization::error_code>
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
    static std::expected<assign_role_request, ores::utility::serialization::error_code>
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
    static std::expected<assign_role_response, ores::utility::serialization::error_code>
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
    static std::expected<revoke_role_request, ores::utility::serialization::error_code>
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
    static std::expected<revoke_role_response, ores::utility::serialization::error_code>
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
    static std::expected<get_account_roles_request, ores::utility::serialization::error_code>
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
    static std::expected<get_account_roles_response, ores::utility::serialization::error_code>
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
    static std::expected<get_account_permissions_request, ores::utility::serialization::error_code>
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
    static std::expected<get_account_permissions_response, ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const get_account_permissions_response& v);

// ============================================================================
// Get Role
// ============================================================================

/**
 * @brief Request to get a specific role by ID or name.
 *
 * The identifier can be either a UUID (for lookup by ID) or a string
 * (for lookup by name). The server will attempt UUID parsing first,
 * then fall back to name lookup.
 */
struct get_role_request final {
    std::string identifier;

    /**
     * @brief Serialize request to bytes.
     *
     * Format:
     * - 2 bytes: identifier length
     * - N bytes: identifier (UTF-8, either UUID string or role name)
     */
    std::vector<std::byte> serialize() const;

    /**
     * @brief Deserialize request from bytes.
     */
    static std::expected<get_role_request, ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const get_role_request& v);

/**
 * @brief Response containing the requested role.
 */
struct get_role_response final {
    bool found = false;
    std::optional<domain::role> role;
    std::string error_message;

    /**
     * @brief Serialize response to bytes.
     *
     * Format:
     * - 1 byte: found (boolean)
     * - If found:
     *   - Role data (same format as in list_roles_response)
     * - If not found:
     *   - 2 bytes: error_message length
     *   - N bytes: error_message (UTF-8)
     */
    std::vector<std::byte> serialize() const;

    /**
     * @brief Deserialize response from bytes.
     */
    static std::expected<get_role_response, ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const get_role_response& v);

// ============================================================================
// Suggest Role Commands
// ============================================================================

/**
 * @brief Request to generate shell commands for assigning all roles to an account.
 *
 * This is an administrative helper that generates ores.shell commands to assign
 * roles to a user. The account is identified by username and either hostname
 * or tenant_id.
 */
struct suggest_role_commands_request final {
    std::string username;
    std::string hostname;  ///< Optional: lookup tenant by hostname
    std::string tenant_id; ///< Optional: direct tenant UUID (used if hostname empty)

    /**
     * @brief Serialize request to bytes.
     *
     * Format:
     * - 2 bytes: username length
     * - N bytes: username (UTF-8)
     * - 2 bytes: hostname length
     * - N bytes: hostname (UTF-8)
     * - 2 bytes: tenant_id length
     * - N bytes: tenant_id (UTF-8)
     */
    std::vector<std::byte> serialize() const;

    /**
     * @brief Deserialize request from bytes.
     */
    static std::expected<suggest_role_commands_request, ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const suggest_role_commands_request& v);

/**
 * @brief Response containing suggested shell commands.
 */
struct suggest_role_commands_response final {
    std::vector<std::string> commands;

    /**
     * @brief Serialize response to bytes.
     *
     * Format:
     * - 4 bytes: count (number of commands)
     * - For each command:
     *   - 2 bytes: command length
     *   - N bytes: command (UTF-8)
     */
    std::vector<std::byte> serialize() const;

    /**
     * @brief Deserialize response from bytes.
     */
    static std::expected<suggest_role_commands_response, ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const suggest_role_commands_response& v);

// ============================================================================
// Role By Name Requests (Assign / Revoke)
// ============================================================================

/**
 * @brief Base type for name-based role operation requests.
 *
 * Contains a principal (`username@hostname` or just `username`) and a role
 * name. The server resolves both to IDs before performing the operation.
 *
 * @tparam Tag Empty tag type used to make assign and revoke distinct types.
 */
template<typename Tag>
struct role_by_name_request_base final {
    std::string principal;
    std::string role_name;

    /**
     * @brief Serialize request to bytes.
     *
     * Format:
     * - 2 bytes: principal length
     * - N bytes: principal (UTF-8)
     * - 2 bytes: role_name length
     * - N bytes: role_name (UTF-8)
     */
    std::vector<std::byte> serialize() const;

    /**
     * @brief Deserialize request from bytes.
     */
    static std::expected<role_by_name_request_base, ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

template<typename Tag>
std::ostream& operator<<(std::ostream& s, const role_by_name_request_base<Tag>& v);

struct assign_role_by_name_tag {};
struct revoke_role_by_name_tag {};

using assign_role_by_name_request = role_by_name_request_base<assign_role_by_name_tag>;
using revoke_role_by_name_request = role_by_name_request_base<revoke_role_by_name_tag>;

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

/**
 * @brief Message traits specialization for get_role_request.
 */
template<>
struct message_traits<iam::messaging::get_role_request> {
    using request_type = iam::messaging::get_role_request;
    using response_type = iam::messaging::get_role_response;
    static constexpr message_type request_message_type =
        message_type::get_role_request;
};

/**
 * @brief Message traits specialization for suggest_role_commands_request.
 */
template<>
struct message_traits<iam::messaging::suggest_role_commands_request> {
    using request_type = iam::messaging::suggest_role_commands_request;
    using response_type = iam::messaging::suggest_role_commands_response;
    static constexpr message_type request_message_type =
        message_type::suggest_role_commands_request;
};

/**
 * @brief Message traits specialization for assign_role_by_name_request.
 */
template<>
struct message_traits<iam::messaging::assign_role_by_name_request> {
    using request_type = iam::messaging::assign_role_by_name_request;
    using response_type = iam::messaging::assign_role_response;
    static constexpr message_type request_message_type =
        message_type::assign_role_by_name_request;
};

/**
 * @brief Message traits specialization for revoke_role_by_name_request.
 */
template<>
struct message_traits<iam::messaging::revoke_role_by_name_request> {
    using request_type = iam::messaging::revoke_role_by_name_request;
    using response_type = iam::messaging::revoke_role_response;
    static constexpr message_type request_message_type =
        message_type::revoke_role_by_name_request;
};

}

#endif
