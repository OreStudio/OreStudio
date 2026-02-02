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
#ifndef ORES_IAM_MESSAGING_TENANT_TYPE_PROTOCOL_HPP
#define ORES_IAM_MESSAGING_TENANT_TYPE_PROTOCOL_HPP

#include <span>
#include <iosfwd>
#include <vector>
#include <expected>
#include <boost/uuid/uuid.hpp>
#include "ores.comms/messaging/message_types.hpp"
#include "ores.comms/messaging/message_traits.hpp"
#include "ores.iam/domain/tenant_type.hpp"

namespace ores::iam::messaging {

// ============================================================================
// Tenant Type Messages
// ============================================================================

/**
 * @brief Request to retrieve all tenant types.
 */
struct get_tenant_types_request final {
    std::vector<std::byte> serialize() const;
    static std::expected<get_tenant_types_request,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const get_tenant_types_request& v);

/**
 * @brief Response containing all tenant types.
 */
struct get_tenant_types_response final {
    std::vector<domain::tenant_type> types;

    std::vector<std::byte> serialize() const;
    static std::expected<get_tenant_types_response,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const get_tenant_types_response& v);

/**
 * @brief Request to save a tenant type (create or update).
 */
struct save_tenant_type_request final {
    domain::tenant_type type;

    std::vector<std::byte> serialize() const;
    static std::expected<save_tenant_type_request,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const save_tenant_type_request& v);

/**
 * @brief Response confirming tenant type save operation.
 */
struct save_tenant_type_response final {
    bool success;
    std::string message;

    std::vector<std::byte> serialize() const;
    static std::expected<save_tenant_type_response,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const save_tenant_type_response& v);

/**
 * @brief Result for a single tenant type deletion.
 */
struct delete_tenant_type_result final {
    std::string type;  ///< Text primary key
    bool success;
    std::string message;
};

std::ostream& operator<<(std::ostream& s, const delete_tenant_type_result& v);

/**
 * @brief Request to delete one or more tenant types.
 */
struct delete_tenant_type_request final {
    std::vector<std::string> types;  ///< Text primary keys

    std::vector<std::byte> serialize() const;
    static std::expected<delete_tenant_type_request,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const delete_tenant_type_request& v);

/**
 * @brief Response confirming tenant type deletion(s).
 */
struct delete_tenant_type_response final {
    std::vector<delete_tenant_type_result> results;

    std::vector<std::byte> serialize() const;
    static std::expected<delete_tenant_type_response,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const delete_tenant_type_response& v);

/**
 * @brief Request to retrieve version history for a tenant type.
 */
struct get_tenant_type_history_request final {
    std::string type;  ///< Text primary key

    std::vector<std::byte> serialize() const;
    static std::expected<get_tenant_type_history_request,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const get_tenant_type_history_request& v);

/**
 * @brief Response containing tenant type version history.
 */
struct get_tenant_type_history_response final {
    bool success;
    std::string message;
    std::vector<domain::tenant_type> versions;

    std::vector<std::byte> serialize() const;
    static std::expected<get_tenant_type_history_response,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const get_tenant_type_history_response& v);

}

namespace ores::comms::messaging {

// Tenant Type traits
template<>
struct message_traits<iam::messaging::get_tenant_types_request> {
    using request_type = iam::messaging::get_tenant_types_request;
    using response_type = iam::messaging::get_tenant_types_response;
    static constexpr message_type request_message_type =
        message_type::get_tenant_types_request;
};

template<>
struct message_traits<iam::messaging::save_tenant_type_request> {
    using request_type = iam::messaging::save_tenant_type_request;
    using response_type = iam::messaging::save_tenant_type_response;
    static constexpr message_type request_message_type =
        message_type::save_tenant_type_request;
};

template<>
struct message_traits<iam::messaging::delete_tenant_type_request> {
    using request_type = iam::messaging::delete_tenant_type_request;
    using response_type = iam::messaging::delete_tenant_type_response;
    static constexpr message_type request_message_type =
        message_type::delete_tenant_type_request;
};

template<>
struct message_traits<iam::messaging::get_tenant_type_history_request> {
    using request_type = iam::messaging::get_tenant_type_history_request;
    using response_type = iam::messaging::get_tenant_type_history_response;
    static constexpr message_type request_message_type =
        message_type::get_tenant_type_history_request;
};

}

#endif
