/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2026 Marco Craveiro <marco.craveiro@gmail.com>
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
#ifndef ORES_IAM_MESSAGING_TENANT_PROTOCOL_HPP
#define ORES_IAM_MESSAGING_TENANT_PROTOCOL_HPP

#include <span>
#include <iosfwd>
#include <vector>
#include <expected>
#include <boost/uuid/uuid.hpp>
#include "ores.comms/messaging/message_type.hpp"
#include "ores.comms/messaging/save_result.hpp"
#include "ores.utility/serialization/error_code.hpp"
#include "ores.comms/messaging/message_traits.hpp"
#include "ores.iam/domain/tenant.hpp"

namespace ores::iam::messaging {

// ============================================================================
// Tenant Messages
// ============================================================================

/**
 * @brief Request to retrieve all tenants.
 */
struct get_tenants_request final {
    /**
     * @brief Include soft-deleted tenants in response.
     *
     * When true, returns the latest version of all tenants including deleted.
     * When false (default), returns only active tenants (valid_to = infinity).
     */
    bool include_deleted = false;

    /**
     * @brief Serialize request to bytes.
     *
     * Format:
     * - 1 byte: include_deleted (boolean)
     */
    std::vector<std::byte> serialize() const;

    /**
     * @brief Deserialize request from bytes.
     *
     * Handles both old (empty) and new (with flag) formats for backward
     * compatibility.
     */
    static std::expected<get_tenants_request,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const get_tenants_request& v);

/**
 * @brief Response containing all tenants.
 */
struct get_tenants_response final {
    std::vector<domain::tenant> tenants;

    std::vector<std::byte> serialize() const;
    static std::expected<get_tenants_response,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const get_tenants_response& v);

/**
 * @brief Request to save one or more tenants (create or update).
 */
struct save_tenant_request final {
    std::vector<domain::tenant> tenants;

    static save_tenant_request from(domain::tenant tenant);
    static save_tenant_request from(std::vector<domain::tenant> tenants);

    std::vector<std::byte> serialize() const;
    static std::expected<save_tenant_request,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const save_tenant_request& v);

/**
 * @brief Response confirming tenant save operation(s).
 */
struct save_tenant_response final {
    bool success = false;
    std::string message;

    std::vector<std::byte> serialize() const;
    static std::expected<save_tenant_response,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const save_tenant_response& v);

/**
 * @brief Request to delete one or more tenants.
 */
struct delete_tenant_request final {
    std::vector<boost::uuids::uuid> ids;

    std::vector<std::byte> serialize() const;
    static std::expected<delete_tenant_request,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const delete_tenant_request& v);

/**
 * @brief Response confirming tenant deletion(s).
 */
struct delete_tenant_response final {
    bool success = false;
    std::string message;

    std::vector<std::byte> serialize() const;
    static std::expected<delete_tenant_response,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const delete_tenant_response& v);

/**
 * @brief Request to retrieve version history for a tenant.
 */
struct get_tenant_history_request final {
    boost::uuids::uuid id;

    std::vector<std::byte> serialize() const;
    static std::expected<get_tenant_history_request,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const get_tenant_history_request& v);

/**
 * @brief Response containing tenant version history.
 */
struct get_tenant_history_response final {
    bool success;
    std::string message;
    std::vector<domain::tenant> versions;

    std::vector<std::byte> serialize() const;
    static std::expected<get_tenant_history_response,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const get_tenant_history_response& v);

// ============================================================================
// Tenant Provisioning Messages
// ============================================================================

/**
 * @brief Request to create and fully provision a new tenant.
 *
 * Unlike save_tenant_request which only inserts a row, this calls
 * ores_iam_provision_tenant_fn which also copies base IAM data
 * (permissions, roles, role_permissions) into the new tenant.
 */
struct provision_tenant_request final {
    std::string type;            ///< e.g. "production"
    std::string code;            ///< unique tenant code
    std::string name;            ///< display name
    std::string hostname;        ///< unique hostname
    std::string description;     ///< optional
    std::string admin_username;  ///< optional: admin account username
    std::string admin_password;  ///< optional: admin account password
    std::string admin_email;     ///< optional: admin account email

    std::vector<std::byte> serialize() const;
    static std::expected<provision_tenant_request,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const provision_tenant_request& v);

/**
 * @brief Response from tenant provisioning.
 */
struct provision_tenant_response final {
    bool success = false;
    std::string error_message;
    std::string tenant_id;       ///< UUID of created tenant (as string)

    std::vector<std::byte> serialize() const;
    static std::expected<provision_tenant_response,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const provision_tenant_response& v);

}

namespace ores::comms::messaging {

// Tenant traits
template<>
struct message_traits<iam::messaging::get_tenants_request> {
    using request_type = iam::messaging::get_tenants_request;
    using response_type = iam::messaging::get_tenants_response;
    static constexpr message_type request_message_type =
        message_type::get_tenants_request;
};

template<>
struct message_traits<iam::messaging::save_tenant_request> {
    using request_type = iam::messaging::save_tenant_request;
    using response_type = iam::messaging::save_tenant_response;
    static constexpr message_type request_message_type =
        message_type::save_tenant_request;
};

template<>
struct message_traits<iam::messaging::delete_tenant_request> {
    using request_type = iam::messaging::delete_tenant_request;
    using response_type = iam::messaging::delete_tenant_response;
    static constexpr message_type request_message_type =
        message_type::delete_tenant_request;
};

template<>
struct message_traits<iam::messaging::get_tenant_history_request> {
    using request_type = iam::messaging::get_tenant_history_request;
    using response_type = iam::messaging::get_tenant_history_response;
    static constexpr message_type request_message_type =
        message_type::get_tenant_history_request;
};

template<>
struct message_traits<iam::messaging::provision_tenant_request> {
    using request_type = iam::messaging::provision_tenant_request;
    using response_type = iam::messaging::provision_tenant_response;
    static constexpr message_type request_message_type =
        message_type::provision_tenant_request;
};

}

#endif
